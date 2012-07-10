open Printf
open Util

type result
  = No_result
  | Whitelisted of string
  | Spf_response of SPF.response

type priv =
  { addr   : Unix.inet_addr
  ; helo   : string option
  ; result : result
  }

let spf_server = SPF.server SPF.Dns_cache

let config = Config.default

let unbox_spf = function
  | `Error e -> failwith (sprintf "error: %s" e)
  | `Response r -> r

let canonicalize a =
  let e = String.length a - 1 in
  let a = if a.[0] = '<' && a.[e] = '>' then String.sub a 1 (e-1) else a in
  let a = if a.[0] = '"' && a.[e] = '"' then String.sub a 1 (e-1) else a in
  let e = String.length a - 1 in
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

let milter_reject ctx comment =
  Milter.setreply ctx "550" (Some "5.7.1") (Some comment);
  Milter.Reject

let milter_tempfail ctx comment =
  Milter.setreply ctx "451" (Some "4.7.1") (Some comment);
  Milter.Tempfail

let spf_check_helo ctx priv =
  let addr = priv.addr in
  let helo = some (priv.helo) in
  let spf_res = unbox_spf (SPF.check_helo spf_server addr helo) in
  let milter_res = match SPF.result spf_res with
  | SPF.Fail c ->
      milter_reject ctx (SPF.smtp_comment c)
  | SPF.Temperror ->
      if config.Config.fail_on_helo_temperror then
        milter_tempfail ctx (SPF.header_comment spf_res)
      else
        Milter.Continue
  | _ ->
      Milter.Continue in
  spf_res, milter_res

let spf_check_from ctx priv from =
  let addr = priv.addr in
  let helo = some (priv.helo) in
  let spf_res = unbox_spf (SPF.check_from spf_server addr helo from) in
  let milter_res = match SPF.result spf_res with
  | SPF.Fail c -> milter_reject ctx (SPF.smtp_comment c)
  | SPF.Temperror -> milter_tempfail ctx (SPF.header_comment spf_res)
  | _ -> Milter.Continue in
  spf_res, milter_res

let spf_check ctx priv from =
  let spf_res, milter_res = spf_check_helo ctx priv in
  match milter_res with
  | Milter.Continue -> spf_check_from ctx priv from
  | other -> spf_res, milter_res

let milter_add_header ctx header =
  let sep = String.index header ':' in
  let field = String.sub header 0 sep in
  let value = String.sub header (sep + 2) (String.length header - sep - 2) in
  Milter.addheader ctx field value

let with_priv_data z ctx f =
  match Milter.getpriv ctx with
  | None -> z
  | Some p -> let p', r = f p in Milter.setpriv ctx p'; r

module FlagSet = SetOfList(struct
  type t = Milter.flag
  let compare = compare
end)

module StepSet = SetOfList(struct
  type t = Milter.step
  let compare = compare
end)

let whitelist s =
  Whitelisted s

(* Callbacks *)

let connect ctx host addr =
  let addr = default Unix.inet_addr_loopback inet_addr_of_sockaddr addr in
  let result = default No_result whitelist (Whitelist.check addr) in
  let priv =
    { addr   = addr
    ; helo   = None
    ; result = result
    } in
  Milter.setpriv ctx priv;
  Milter.Continue

let helo ctx helo =
  match helo with
  | None ->
      Milter.setreply ctx "503" (Some "5.0.0") (Some "Please say HELO");
      Milter.Reject
  | Some name ->
      with_priv_data Milter.Tempfail ctx
        (fun priv -> { priv with helo = Some name }, Milter.Continue)

let envfrom ctx from args =
  let is_auth = Milter.getsymval ctx "{auth_authen}" <> None in
  let verified = default false ((=)"OK") (Milter.getsymval ctx "{verify}") in
  with_priv_data Milter.Tempfail ctx
    (fun priv ->
      if is_auth || verified then
        let result = Whitelisted "X-Comment: authenticated client" in
        { priv with result = result }, Milter.Continue
      else
        match priv.result with
        | No_result ->
            let spf_res, milter_res = spf_check ctx priv (canonicalize from) in
            { priv with result = Spf_response spf_res }, milter_res
        | Whitelisted _ ->
            priv, Milter.Continue
        | Spf_response _ ->
            invalid_arg "envfrom") (* not possible here *)

let eom ctx =
  with_priv_data Milter.Tempfail ctx
    (fun priv ->
      (match priv.result with
      | No_result -> ()
      | Whitelisted s -> milter_add_header ctx s
      | Spf_response r -> milter_add_header ctx (SPF.received_spf r));
      priv, Milter.Continue)

let abort ctx =
  with_priv_data Milter.Continue ctx
    (fun priv ->
      { priv with result = No_result }, Milter.Continue)

let close ctx =
  maybe (fun _ -> Milter.unsetpriv ctx) (Milter.getpriv ctx);
  Milter.Continue

let negotiate ctx actions steps =
  let reqactions = [Milter.ADDHDRS] in
  if FlagSet.subset (FlagSet.of_list reqactions) (FlagSet.of_list actions) then
    let noreqsteps =
      StepSet.of_list
        [ Milter.NORCPT
        ; Milter.NOHDRS
        ; Milter.NOEOH
        ; Milter.NOBODY
        ; Milter.NOUNKNOWN
        ; Milter.NODATA
        ] in
    let steps = StepSet.of_list steps in
    let noreqsteps = StepSet.elements (StepSet.inter steps noreqsteps) in
    (Milter.Continue, reqactions, noreqsteps)
  else
    (Milter.Reject, [], [])
