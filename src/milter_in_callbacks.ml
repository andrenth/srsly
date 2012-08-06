open Printf
open Milter_util
open Util

module O = Release_option

type result
  = No_result
  | Whitelisted of (string * string)
  | Spf_response of SPF.response

type priv =
  { addr      : Unix.inet_addr
  ; helo      : string option
  ; rcpt      : (string * string) option
  ; is_bounce : bool
  ; result    : result
  }

let spf = SPF.server SPF.Dns_cache

let srs_re = Str.regexp "^SRS\\([01]\\)[=+-]"

let unbox_spf = function
  | `Error e -> failwith (sprintf "error: %s" e)
  | `Response r -> r

let milter_reject ctx msg =
  Milter.setreply ctx "550" (Some "5.7.1") (Some msg);
  Milter.Reject

let milter_tempfail ctx msg =
  Milter.setreply ctx "451" (Some "4.7.1") (Some msg);
  Milter.Tempfail

let spf_check_helo ctx priv =
  let addr = priv.addr in
  let helo = O.some (priv.helo) in
  let spf_res = unbox_spf (SPF.check_helo spf addr helo) in
  let milter_res = match SPF.result spf_res with
  | SPF.Fail c ->
      debug "HELO SPF failure for %s" helo;
      milter_reject ctx (SPF.smtp_comment c)
  | SPF.Temperror ->
      debug "HELO SPF temperror for %s" helo;
      if (Config.fail_on_helo_temperror ()) then
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
  let spf_res = unbox_spf (SPF.check_from spf addr helo from) in
  let milter_res = match SPF.result spf_res with
  | SPF.Fail c ->
      debug "MAIL SPF pass for %s" helo;
      milter_reject ctx (SPF.smtp_comment c)
  | SPF.Temperror ->
      debug "MAIL SPF temperror for %s" helo;
      milter_tempfail ctx (SPF.header_comment spf_res)
  | _ ->
      debug "MAIL SPF pass for %s" helo;
      Milter.Continue in
  spf_res, milter_res

let spf_check ctx priv from =
  let spf_res, milter_res = spf_check_helo ctx priv in
  match milter_res with
  | Milter.Continue -> spf_check_from ctx priv from
  | other -> spf_res, milter_res

let milter_add_header ctx (field, value) =
  Milter.insheader ctx 1 field value

let whitelist s =
  Whitelisted s

(* Callbacks *)

let connect ctx host addr =
  debug "connect callback: host=%s addr=%s"
    (O.default "?" host) (O.may_default "?" string_of_sockaddr addr);
  let addr = O.may_default Unix.inet_addr_loopback inet_addr_of_sockaddr addr in
  let result = O.may_default No_result whitelist (Whitelist.check addr) in
  let priv =
    { addr      = addr
    ; helo      = None
    ; rcpt      = None
    ; is_bounce = false
    ; result    = result
    } in
  Milter.setpriv ctx priv;
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
      let priv = { priv with is_bounce = from = "<>" } in
      match priv.result with
      | No_result | Spf_response _ ->
          (* This callback may be called multiple times in the same
           * connection, so ignore message-specific results. *)
          debug "doing SPF verification";
          let spf_res, milter_res = spf_check ctx priv (canonicalize from) in
          { priv with result = Spf_response spf_res }, milter_res
      | Whitelisted _ ->
          (* Whitelists are IP-based, so just move on. *)
          debug "connect address is whitelisted";
          priv, Milter.Continue)

let envrcpt ctx rcpt args =
  debug "envrcpt callback: rcpt=%s" rcpt;
  with_priv_data Milter.Tempfail ctx
    (fun priv ->
      if priv.is_bounce && Str.string_match srs_re rcpt 0 then begin
        debug "got an SRS-signed bounce";
        try
          let n = 1 + int_of_string (Str.matched_group 1 rcpt) in
          let srs = Milter_srs.current () in
          let rev_rcpt = applyn (SRS.reverse srs) (canonicalize rcpt) n in
          info "SRS-reversed address for '%s': '%s'" rcpt rev_rcpt;
          { priv with rcpt = Some (rcpt, rev_rcpt) }, Milter.Continue
        with SRS.SRS_error s ->
          notice "SRS failure: %s: %s" rcpt s;
          priv, milter_reject ctx s
      end else
        priv, Milter.Continue)

let eom ctx =
  debug "eom callback";
  with_priv_data Milter.Tempfail ctx
    (fun priv ->
      (match priv.rcpt with
      | Some (srs_rcpt, rev_rcpt) ->
          info "replacing bounce destination '%s' with '%s'" srs_rcpt rev_rcpt;
          Milter.delrcpt ctx srs_rcpt;
          Milter.addrcpt ctx rev_rcpt
      | None ->
          ());
      (match priv.result with
      | No_result ->
          ()
      | Whitelisted ((_, msg) as header) ->
          info "Whitelisted address: %s" msg;
          milter_add_header ctx header
      | Spf_response r ->
          info "SPF result: %s" (SPF.string_of_result (SPF.result r));
          milter_add_header ctx (SPF.received_spf r));
      priv, Milter.Continue)

let abort ctx =
  debug "abort callback";
  with_priv_data Milter.Continue ctx
    (fun priv ->
      { priv with result = No_result }, Milter.Continue)

let close ctx =
  debug "close callback";
  O.may (fun () -> Milter.unsetpriv ctx) (Milter.getpriv ctx);
  Milter.Continue

let negotiate ctx actions steps =
  debug "negotiate callback";
  let reqactions = [Milter.ADDHDRS; Milter.DELRCPT; Milter.ADDRCPT] in
  if FlagSet.subset (FlagSet.of_list reqactions) (FlagSet.of_list actions) then
    let unreq_steps =
      StepSet.of_list
        [ Milter.NORCPT
        ; Milter.NOHDRS
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
