open Printf

module Slave_types = struct
  type request
  type response
    = Configuration of Config.t
    | Reload_srs_secrets
end

module Slave_ops = struct
  include Slave_types

  let string_of_request _ = failwith "no requests to make a string"
  let request_of_string _ = failwith "no requests can be made from a string"

  let string_of_response = function
    | Configuration c -> sprintf "C:%s" (Config.serialize c)
    | Reload_srs_secrets -> "R"

  let response_of_string s =
    match s.[0] with
    | 'C' -> Configuration (Config.unserialize s 2)
    | 'R' -> Reload_srs_secrets
    | other -> failwith "unexpected response: '%c'" other
end

module Slave = Release_ipc.Make (Slave_ops)
