open Printf

module O = Release_option

module Slave_types = struct
  type request
    = Configuration_request
    | Check_remote_sender of string
    | Count_remote_final_rcpts of string list
    | SRS_secrets_request

  type response
    = Configuration of Config.t
    | Remote_sender_check of bool
    | Remote_final_rcpts_count of (string * int) list
    | SRS_secrets of (string * string list)
end

let payload s =
  String.sub s 2 (String.length s - 2)

module Slave_ops = struct
  include Slave_types

  let string_of_request = function
    | Configuration_request -> "a"
    | Check_remote_sender s -> sprintf "b:%s" s
    | Count_remote_final_rcpts rs -> sprintf "c:%s" (Marshal.to_string rs [])
    | SRS_secrets_request -> "d"

  let request_of_string s =
    match s.[0] with
    | 'a' -> Configuration_request
    | 'b' -> Check_remote_sender (payload s)
    | 'c' -> Count_remote_final_rcpts (Marshal.from_string (payload s) 0)
    | 'd' -> SRS_secrets_request
    | other -> failwith (sprintf "unexpected request: '%c'" other)

  let string_of_response = function
    | Configuration c -> sprintf "A:%s" (Config.serialize c)
    | Remote_sender_check b -> sprintf "B:%s" (string_of_bool b)
    | Remote_final_rcpts_count rs -> sprintf "C:%s" (Marshal.to_string rs [])
    | SRS_secrets ss -> sprintf "D:%s" (Milter_srs.serialize_secrets ss)

  let response_of_string s =
    match s.[0] with
    | 'A' -> Configuration (Config.unserialize (payload s))
    | 'B' -> Remote_sender_check (bool_of_string (payload s))
    | 'C' -> Remote_final_rcpts_count (Marshal.from_string s 2)
    | 'D' -> SRS_secrets (Milter_srs.unserialize_secrets (payload s))
    | other -> failwith (sprintf "unexpected response = '%c'" other)
end

module Slave = Release_ipc.Make (Slave_ops)

module Control_types = struct
  type request
    = Reload_config of string option
    | Reload_secrets
    | Stop

  type response
    = Reloaded_config
    | Reloaded_secrets
    | Stopped
end

module Control_ops = struct
  include Control_types

  let option_of_string = function
    | "" -> None
    | s -> Some s

  let string_of_request = function
    | Reload_config file -> sprintf "c:%s" (O.default "" file)
    | Reload_secrets -> "s"
    | Stop -> "t"

  let request_of_string s =
    match s.[0] with
    | 'c' -> Reload_config (option_of_string (payload s))
    | 's' -> Reload_secrets
    | 't' -> Stop
    | other -> failwith (sprintf "unexpected request: '%c'" other)

  let string_of_response = function
    | Reloaded_config -> "C"
    | Reloaded_secrets -> "S"
    | Stopped -> "T"

  let response_of_string = function
    | "C" -> Reloaded_config
    | "S" -> Reloaded_secrets
    | "T" -> Stopped
    | other -> failwith (sprintf "unexpected response: '%s'" other)
end

module Control = Release_ipc.Make (Control_ops)
