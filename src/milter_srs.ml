open Util

let srs = ref None

let make_srs () =
  let config = Config.milter () in
  let read_srs_secrets () = 
    let secrets = ref [] in
    let ch = open_in (Milter_config.srs_secret_file config) in
    (try while true; do secrets := input_line ch :: !secrets done
    with End_of_file -> close_in ch);
    List.rev !secrets in
  SRS.make
    (read_srs_secrets ())
    (Milter_config.srs_hash_max_age config)
    (Milter_config.srs_hash_length config)
    (Milter_config.srs_separator config)

let reload () =
  srs := Some (make_srs ())

let current () =
  match !srs with
  | None ->
      let s = make_srs () in
      srs := Some s;
      s
  | Some s ->
      s
