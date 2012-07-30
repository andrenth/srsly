open Util

let srs = ref None

let serialize_secrets ss =
  Marshal.to_string ss []

let unserialize_secrets s i =
  (Marshal.from_string s i : string * (string list))

let make_srs secrets =
  let config = Config.milter () in
  SRS.make
    secrets
    (Milter_config.srs_hash_max_age config)
    (Milter_config.srs_hash_length config)
    (Milter_config.srs_separator config)

let reload secrets =
  srs := Some (make_srs secrets)

let current () =
  match !srs with
  | None -> failwith "Milter_srs.current: no SRS"
  | Some s -> s
