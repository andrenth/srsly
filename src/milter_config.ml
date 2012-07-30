open Release_config_types
open Util

type t =
  { debug_level      : int
  ; listen_address   : string * string
  ; srs_domain       : string option
  ; srs_secret_file  : Lwt_io.file_name
  ; srs_hash_max_age : int
  ; srs_hash_length  : int 
  ; srs_separator    : char
  }

let find conf key =
  Release_config.get_exn conf ~section:"milter" key () 

let find_opt conf key =
  Release_config.get conf ~section:"milter" key () 

let default_srs_hash_max_age = Some (`Int 8)
let default_srs_hash_length = Some (`Int 8)
let default_srs_hash_separator = Some (`Str "=")
let default_debug_level = Some (`Int 0)

let of_configuration c =
  let listen_address_in = string_value (find c "listen_address_in") in
  let listen_address_out = string_value (find c "listen_address_out") in
  let srs_domain = map_opt string_value (find_opt c "srs_domain") in
  let srs_secret_file = string_value (find c "srs_secret_file") in
  let srs_hash_max_age = int_value (find c "srs_hash_max_age") in
  let srs_hash_length = int_value (find c "srs_hash_length") in
  let srs_hash_separator = (string_value (find c "srs_hash_separator")).[0] in
  let debug_level = int_value (find c "debug_level") in
  { debug_level      = debug_level
  ; listen_address   = listen_address_in, listen_address_out
  ; srs_domain       = srs_domain
  ; srs_secret_file  = srs_secret_file
  ; srs_hash_max_age = srs_hash_max_age
  ; srs_hash_length  = srs_hash_length 
  ; srs_separator    = srs_hash_separator
  }

let listen_address_in c =
  fst c.listen_address

let listen_address_out c =
  snd c.listen_address

let srs_domain c =
  c.srs_domain

let srs_secret_file c =
  c.srs_secret_file

let srs_hash_max_age c =
  c.srs_hash_max_age

let srs_hash_length c =
  c.srs_hash_length

let srs_separator c =
  c.srs_separator

let debug_level c =
  c.debug_level
