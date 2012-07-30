open Release_config_types
open Util

type t =
  { listen_address : string
  ; num_slaves     : int
  }

let find conf key =
  Release_config.get_exn conf ~section:"policyd" key () 

let default_num_slaves = Some (`Int 2)

let of_configuration c =
  let listen_address = string_value (find c "listen_address") in
  let num_slaves = int_value (find c "num_slaves") in
  { listen_address = listen_address
  ; num_slaves     = num_slaves
  }

let listen_address c =
  c.listen_address

let num_slaves c =
  c.num_slaves
