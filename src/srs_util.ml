open Lwt
open Util

let read_srs_secrets () =
  let file = Config.srs_secret_file () in
  let read_secrets ch =
    let lines = Lwt_io.read_lines ch in
    match_lwt Lwt_stream.to_list lines with
    | [] -> fail_lwt ("read_srs_secrets: no available secrets in " ^ file)
    | s::ss -> return (s, ss) in
  Lwt_io.with_file ~mode:Lwt_io.input file read_secrets
