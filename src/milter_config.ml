open Release_config_types
open Util

type t =
  { log_level        : int
  ; listen_address   : string * string
  ; srs_domain       : string option
  ; srs_secret       : string
  ; srs_hash_max_age : int
  ; srs_hash_length  : int 
  ; srs_separator    : char
  }

let get conf key =
  Release_config.get conf ~section:"milter" key () 

let get_req conf key =
  Release_config.get_exn conf ~section:"milter" key () 

let log_level = 0
let srs_hash_max_age = 8
let srs_hash_length = 8
let srs_hash_separator = '='

let of_configuration conf =
  let listen_address_in = string_value (get_req conf "listen_address_in") in
  let listen_address_out = string_value (get_req conf "listen_address_out") in
  let srs_domain = map_opt string_value (get conf "srs_domain") in
  let srs_secret = string_value (get_req conf "srs_secret") in
  let srs_hash_max_age =
    default srs_hash_max_age int_value (get conf "srs_hash_max_age") in
  let srs_hash_length =
    default srs_hash_length int_value (get conf "srs_hash_length") in
  let srs_hash_separator =
    default srs_hash_separator
      (fun s -> (string_value s).[0])
      (get conf "srs_hash_separator") in
  let log_level =
    default log_level int_value (get conf "log_level") in
  { log_level        = log_level
  ; listen_address   = listen_address_in, listen_address_out
  ; srs_domain       = srs_domain
  ; srs_secret       = srs_secret
  ; srs_hash_max_age = srs_hash_max_age
  ; srs_hash_length  = srs_hash_length 
  ; srs_separator    = srs_hash_separator
  }

let listen_address c =
  c.listen_address

let srs_domain c =
  c.srs_domain

let srs_secret c =
  c.srs_secret

let srs_hash_max_age c =
  c.srs_hash_max_age

let srs_hash_length c =
  c.srs_hash_length

let srs_separator c =
  c.srs_separator

let log_level c =
  c.log_level
