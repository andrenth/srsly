open Release_config_types
open Util

type t =
  { listen_address : string
  ; num_slaves     : int
  }

let get conf key =
  Release_config.get conf ~section:"policyd" key () 

let get_req conf key =
  Release_config.get_exn conf ~section:"policyd" key () 

let num_slaves = 2

let of_configuration conf =
  let listen_address = string_value (get_req conf "listen_address") in
  let num_slaves =
    default num_slaves int_value (get conf "num_slaves") in
  { listen_address = listen_address
  ; num_slaves     = num_slaves
  }

let listen_address c =
  c.listen_address
