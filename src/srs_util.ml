open Lwt
open Util

let read_srs_secrets () =
  let file = Srslyd_config.srs_secret_file () in
  let dir = Srslyd_config.srs_secrets_directory () in
  Lwt_io.with_file ~mode:Lwt_io.input file Lwt_io.read_line >>= fun secret ->
  Lwt_stream.to_list (Lwt_unix.files_of_directory dir) >>= fun secrets ->
  return (secret, secrets)
