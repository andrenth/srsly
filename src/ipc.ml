open Printf

module Slave_types = struct
  type request
    = SRS_secrets_request

  type response
    = Configuration of Config.t
    | SRS_secrets of (string * string list)
end

module Slave_ops = struct
  include Slave_types

  let string_of_request = function
    | SRS_secrets_request -> "s"

  let request_of_string s =
    match s.[0] with
    | 's' -> SRS_secrets_request
    | other -> failwith (sprintf "unexpected request: '%c'" other)

  let string_of_response = function
    | Configuration c -> sprintf "C:%s" (Config.serialize c)
    | SRS_secrets ss -> sprintf "S:%s" (Milter_srs.serialize_secrets ss)

  let response_of_string s =
    match s.[0] with
    | 'C' -> Configuration (Config.unserialize s 2)
    | 'S' -> SRS_secrets (Milter_srs.unserialize_secrets s 2)
    | other -> failwith (sprintf "unexpected response: '%c'" other)
end

module Slave = Release_ipc.Make (Slave_ops)

module Control_types = struct
  type request
    = Stop
    | Reload

  type response
    = Stopped
    | Reloaded
end

module Control_ops = struct
  include Control_types

  let string_of_request = function
    | Stop -> "s"
    | Reload -> "r"

  let request_of_string = function
    | "s" -> Stop
    | "r" -> Reload
    | other -> failwith (sprintf "unexpected request: '%s'" other) 

  let string_of_response = function
    | Stopped -> "S"
    | Reloaded -> "R"

  let response_of_string = function
    | "S" -> Stopped
    | "R" -> Reloaded
    | other -> failwith (sprintf "unexpected response: '%s'" other)
end

module Control = Release_ipc.Make (Control_ops)
