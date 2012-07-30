open Util

let srs = ref None

let serialize_secrets ss =
  Marshal.to_string ss []

let unserialize_secrets s i =
  (Marshal.from_string s i : string * (string list))

let make_srs secrets =
  SRS.make
    secrets
    (Config.srs_hash_max_age ())
    (Config.srs_hash_length ())
    (Config.srs_separator ())

let reload secrets =
  srs := Some (make_srs secrets)

let current () =
  match !srs with
  | None -> failwith "Milter_srs.current: no SRS"
  | Some s -> s
