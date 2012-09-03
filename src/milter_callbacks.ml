open Lwt
open Printf

open Log_preemptive
open Util

module O = Release_option

type result
  = No_result
  | Whitelisted of (string * string)
  | Spf_response of SPF.response

type priv =
  { addr      : Unix.inet_addr
  ; helo      : string option
  ; from      : string option
  ; rcpts     : string list
  ; rev_rcpt  : string option
  ; is_bounce : bool
  ; result    : result
  }

module SetOfList = struct
  module type S = sig
    include Set.S
    val of_list : elt list -> t
  end

  module Make (E : Set.OrderedType) : S with type elt = E.t = struct
    include Set.Make(E)
    let of_list =
      List.fold_left (fun s e -> add e s) empty
  end
end

module FlagSet = SetOfList.Make(struct
  type t = Milter.flag
  let compare = compare
end)

module StepSet = SetOfList.Make(struct
  type t = Milter.step
  let compare = compare
end)

type ops =
  { is_remote_sender         : (string -> bool Lwt.t)
  ; count_remote_final_rcpts : (string list -> (string * int) list Lwt.t)
  }

let proxymap_ops = ref None

let init ops =
  proxymap_ops := Some ops

let is_remote_sender sender =
  let run ops =
    Lwt_preemptive.run_in_main (fun () -> ops.is_remote_sender sender) in
  O.may_default false run !proxymap_ops

let count_remote_final_rcpts rcpts =
  let run ops =
    Lwt_preemptive.run_in_main (fun () -> ops.count_remote_final_rcpts rcpts) in
  O.may_default [] run !proxymap_ops

let spf = SPF.server SPF.Dns_cache

let srs_re = Str.regexp "^SRS\\([01]\\)[=+-]"

let with_priv_data z ctx f =
  match Milter.getpriv ctx with
  | None -> z
  | Some p -> let p', r = f p in Milter.setpriv ctx (Some p'); r

let canonicalize a =
  let e = String.length a - 1 in
  let a, e =
    if e > 0 && a.[0] = '<' && a.[e] = '>' then String.sub a 1 (e-1), e-1
    else a, e in
  let a, e =
    if e > 0 && a.[0] = '"' && a.[e] = '"' then String.sub a 1 (e-1), e-1
    else a, e in
  try
    let t = String.rindex a '@' in
    let u = String.sub a 0 (t) in
    let d = String.sub a (t+1) (e-t) in
    let u = if u.[0] = '"' && u.[t-1] = '"' then String.sub u 1 (t-2) else u in
    try
      let v = String.rindex u ':' in
      let u = String.sub u (v+1) (String.length u - v - 1) in
      u ^ "@" ^ d
    with Not_found ->
      u ^ "@" ^ d
  with Not_found ->
    a

let milter_reject ctx msg =
  Milter.setreply ctx "550" (Some "5.7.1") (Some msg);
  Milter.Reject

let milter_tempfail ctx msg =
  Milter.setreply ctx "451" (Some "4.7.1") (Some msg);
  Milter.Tempfail

let detached_in_main f x =
  Lwt_preemptive.run_in_main (fun () -> Lwt_preemptive.detach f x)

let check_helo addr helo =
  detached_in_main (SPF.check_helo spf addr) helo

let check_from addr helo from =
  detached_in_main (SPF.check_from spf addr helo) from

let spf_check_helo ctx priv =
  let addr = priv.addr in
  let helo = O.some (priv.helo) in
  let spf_res = check_helo addr helo in
  let milter_res = match SPF.result spf_res with
  | SPF.Fail c ->
      debug "HELO SPF failure for %s" helo;
      milter_reject ctx (SPF.smtp_comment c)
  | SPF.Temperror ->
      debug "HELO SPF temperror for %s" helo;
      if (Config.spf_fail_on_helo_temperror ()) then
        milter_tempfail ctx (SPF.header_comment spf_res)
      else
        Milter.Continue
  | _ ->
      debug "HELO SPF pass for %s" helo;
      Milter.Continue in
  spf_res, milter_res

let spf_check_from ctx priv from =
  let addr = priv.addr in
  let helo = O.some (priv.helo) in
  let spf_res = check_from addr helo from in
  let milter_res = match SPF.result spf_res with
  | SPF.Fail c ->
      debug "MAIL SPF pass for %s" from;
      milter_reject ctx (SPF.smtp_comment c)
  | SPF.Temperror ->
      debug "MAIL SPF temperror for %s" helo;
      milter_tempfail ctx (SPF.header_comment spf_res)
  | _ ->
      debug "MAIL SPF pass for %s" helo;
      Milter.Continue in
  spf_res, milter_res

let spf_check ctx priv from =
  try
    let spf_res, milter_res = spf_check_helo ctx priv in
    match milter_res with
    | Milter.Continue ->
        let spf_res, milter_res =
          if from <> "" then spf_check_from ctx priv from
          else spf_res, milter_res in
        Some spf_res, milter_res
    | other ->
        Some spf_res, milter_res
  with SPF.SPF_error e ->
    let msg = sprintf "error checking SPF: %s" e in
    warning "%s" msg;
    None, milter_tempfail ctx msg

let milter_add_header ctx (field, value) =
  debug "inserting header: %s: %s" field value;
  Milter.insheader ctx 1 field value

let set_reverse_srs_rcpt ctx rcpt rev_rcpt =
  info "replacing bounce destination '%s' with '%s'" rcpt rev_rcpt;
  Milter.delrcpt ctx rcpt;
  Milter.addrcpt ctx rev_rcpt

let whitelist h =
  Whitelisted h

let authentication_results ctx priv spf_res =
  let myhostname = O.default "localhost" (Milter.getsymval ctx "j") in
  let res = SPF.string_of_result (SPF.result spf_res) in
  let comm = SPF.header_comment spf_res in
  let from = O.some priv.from in
  let helo = O.some priv.helo in
  sprintf "%s; spf=%s (%s) smtp.mailfrom=%s smtp.helo=%s"
    myhostname res comm from helo

let weighted_sample a =
  let tot = float_of_int (Array.fold_left (fun s (_, c) -> s + c) 0 a) in
  let r = tot *. (1.0 -. Random.float 1.0) in
  let rec sample i s =
    if s >= r then
      fst a.(i-1)
    else
      sample (i+1) (s +. float_of_int (snd a.(i))) in
  sample 0 0.0

(*
 * We choose a random domain to be used as the SRS forward domain.
 * This is done because there's no way to associate each of the original
 * recipients to the final addresses after virtual alias translation
 * is done.
 *)
let choose_forward_domain rcpts =
  let arr = Array.of_list rcpts in
  let rcpt = weighted_sample arr in
  let at = String.index rcpt '@' in
  String.sub rcpt (at+1) (String.length rcpt - at - 1)

let srs_forward ctx from remote_rcpt_counts =
  let srs = Milter_srs.current () in
  debug "choosing an SRS domain from %s"
    (join_strings ", " (List.map fst remote_rcpt_counts));
  let fwd = choose_forward_domain remote_rcpt_counts in
  debug "randomly chosen SRS forward domain: %s" fwd;
  let srs_from = SRS.forward srs from fwd in
  info "SRS-forwarding %s as %s" from srs_from;
  Milter.chgfrom ctx srs_from None

(* Callbacks *)

let connect ctx host addr =
  debug "connect callback: host=%s addr=%s"
    (O.default "?" host) (O.may_default "?" string_of_sockaddr addr);
  let addr = O.may_default Unix.inet_addr_loopback inet_addr_of_sockaddr addr in
  let result = O.may_default No_result whitelist (Whitelist.check addr) in
  let priv =
    { addr      = addr
    ; helo      = None
    ; from      = None
    ; rcpts     = []
    ; rev_rcpt  = None
    ; is_bounce = false
    ; result    = result
    } in
  Milter.setpriv ctx (Some priv);
  Milter.Continue

let helo ctx helo =
  debug "helo callback: helo=%s" (O.default "?" helo);
  match helo with
  | None ->
      notice "remote didn't say HELO, rejecting message";
      Milter.setreply ctx "503" (Some "5.0.0") (Some "Please say HELO");
      Milter.Reject
  | Some name ->
      with_priv_data Milter.Tempfail ctx
        (fun priv -> { priv with helo = Some name }, Milter.Continue)

let envfrom ctx from args =
  debug "envfrom callback: from=%s" from;
  with_priv_data Milter.Tempfail ctx
    (fun priv ->
      let from = canonicalize from in
      let priv = { priv with from = Some from; is_bounce = from = "" } in
      match priv.result with
      | No_result | Spf_response _ ->
          (* This callback may be called multiple times in the same
           * connection, so ignore previous results if any. *)
          debug "doing SPF verification";
          let spf_res, milter_res = spf_check ctx priv from in
          let result =
            O.may_default priv.result (fun r -> Spf_response r) spf_res in
          { priv with result = result }, milter_res
      | Whitelisted _ ->
          (* Whitelists are IP-based, so just move on. *)
          debug "connect address is whitelisted";
          priv, Milter.Continue)

let envrcpt ctx rcpt args =
  debug "envrcpt callback: rcpt=%s" rcpt;
  with_priv_data Milter.Tempfail ctx
    (fun priv ->
      let rcpt = canonicalize rcpt in
      let priv = { priv with rcpts = rcpt::priv.rcpts } in
      if priv.is_bounce && Str.string_match srs_re rcpt 0 then begin
        debug "got an SRS-signed bounce";
        try
          let n = 1 + int_of_string (Str.matched_group 1 rcpt) in
          let srs = Milter_srs.current () in
          let rev_rcpt = applyn (SRS.reverse srs) rcpt n in
          info "SRS-reversed address for '%s': '%s'" rcpt rev_rcpt;
          { priv with rev_rcpt = Some rev_rcpt }, Milter.Continue
        with SRS.SRS_error s ->
          notice "SRS failure: %s: %s" rcpt s;
          priv, milter_reject ctx s
      end else
        priv, Milter.Continue)

let eom ctx =
  debug "eom callback";
  with_priv_data Milter.Tempfail ctx
    (fun priv ->
      let from = O.some priv.from in
      O.may (set_reverse_srs_rcpt ctx from) priv.rev_rcpt;
      let rcpts = priv.rcpts in
      if is_remote_sender from then begin
        match count_remote_final_rcpts rcpts with
        | [] -> ()
        | rc -> srs_forward ctx from rc
      end;
      match priv.result with
      | Whitelisted ((_, msg) as header) ->
          info "Whitelisted address: %s" msg;
          milter_add_header ctx header;
          priv, Milter.Continue
      | Spf_response r ->
          info "SPF result: %s" (SPF.string_of_result (SPF.result r));
          let ar = authentication_results ctx priv r in
          milter_add_header ctx ("Authentication-Results", ar);
          milter_add_header ctx ("Received-SPF", SPF.received_spf_value r);
          priv, Milter.Continue
      | No_result ->
          priv, Milter.Continue)

let abort ctx =
  debug "abort callback";
  with_priv_data Milter.Continue ctx
    (fun priv ->
      { priv with result = No_result }, Milter.Continue)

let close ctx =
  debug "close callback";
  O.may (fun () -> Milter.setpriv ctx None) (Milter.getpriv ctx);
  Milter.Continue

let negotiate ctx actions steps =
  debug "negotiate callback";
  let reqactions =
    [Milter.ADDHDRS; Milter.ADDRCPT; Milter.DELRCPT; Milter.CHGFROM] in
  if FlagSet.subset (FlagSet.of_list reqactions) (FlagSet.of_list actions) then
    let unreq_steps =
      StepSet.of_list
        [ Milter.NOHDRS
        ; Milter.NOEOH
        ; Milter.NOBODY
        ; Milter.NOUNKNOWN
        ; Milter.NODATA
        ] in
    let steps = StepSet.of_list steps in
    let unreq_steps = StepSet.elements (StepSet.inter steps unreq_steps) in
    (Milter.Continue, reqactions, unreq_steps)
  else
    (Milter.Reject, [], [])
