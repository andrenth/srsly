open Printf
open Milter_util
open Util

type result
  = No_result
  | Authenticated
  | Whitelisted of string
  | Spf_response of SPF.response

type priv =
  { signed_from : string option }

(* Callbacks *)

let connect ctx host addr =
  let priv = { signed_from = None } in
  Milter.setpriv ctx priv;
  Milter.Continue

let envfrom ctx from args =
  if from = "<>" then
    Milter.Continue
  else
    with_priv_data Milter.Tempfail ctx
      (fun priv ->
        let srs_domain = Config.srs_domain () in
        let myhostname = Milter.getsymval ctx "j" in
        match srs_domain <|> myhostname with
        | Some alias ->
            let srs = Milter_srs.current () in
            let signed_from = SRS.forward srs (canonicalize from) alias in
            { signed_from = Some signed_from }, Milter.Continue
        | None ->
            priv, Milter.Tempfail)

let eom ctx =
  with_priv_data Milter.Tempfail ctx
    (fun priv ->
      match priv.signed_from with
      | Some from ->
          Milter.chgfrom ctx from "";
          { signed_from = None }, Milter.Continue
      | None ->
          priv, Milter.Tempfail)

let abort ctx =
  with_priv_data Milter.Continue ctx
    (fun priv ->
      { signed_from = None }, Milter.Continue)

let close ctx =
  maybe (fun _ -> Milter.unsetpriv ctx) (Milter.getpriv ctx);
  Milter.Continue

let negotiate ctx actions steps =
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
