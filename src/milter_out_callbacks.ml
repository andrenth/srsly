open Printf
open Milter_util
open Util

module O = Release_option

type result
  = No_result
  | Authenticated
  | Whitelisted of string
  | Spf_response of SPF.response

type priv =
  { signed_from : string option }

(* Callbacks *)

let connect ctx host addr =
  debug "connect callback: host=%s addr=%s"
    (O.default "?" host) (O.may_default "?" string_of_sockaddr addr);
  let priv = { signed_from = None } in
  Milter.setpriv ctx priv;
  Milter.Continue

let envfrom ctx from args =
  debug "envfrom callback: from=%s" from;
  if from = "<>" then
    Milter.Continue
  else
    with_priv_data Milter.Tempfail ctx
      (fun priv ->
        let srs_domain = Config.srs_domain () in
        let myhostname = Milter.getsymval ctx "j" in
        match O.choose srs_domain myhostname with
        | Some alias ->
            let srs = Milter_srs.current () in
            let signed_from = SRS.forward srs (canonicalize from) alias in
            info "SRS-signed %s to %s" from signed_from;
            { signed_from = Some signed_from }, Milter.Continue
        | None ->
            priv, Milter.Tempfail)

let eom ctx =
  debug "eom callback";
  with_priv_data Milter.Tempfail ctx
    (fun priv ->
      match priv.signed_from with
      | Some from ->
          Milter.chgfrom ctx from "";
          { signed_from = None }, Milter.Continue
      | None ->
          priv, Milter.Tempfail)

let abort ctx =
  debug "abort callback";
  with_priv_data Milter.Continue ctx
    (fun priv ->
      { signed_from = None }, Milter.Continue)

let close ctx =
  debug "close callback";
  O.may (fun () -> Milter.unsetpriv ctx) (Milter.getpriv ctx);
  Milter.Continue

let negotiate ctx actions steps =
  debug "negotiate callback";
  let reqactions = [Milter.CHGFROM] in
  if FlagSet.subset (FlagSet.of_list reqactions) (FlagSet.of_list actions) then
    let unreq_steps =
      StepSet.of_list
        [ Milter.NORCPT
        ; Milter.NOHELO
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
