open Lwt
open Printf
open Release_config_values
open Release_config_validations

open Log_lwt
open Util

module O = Release_util.Option

type milter_config =
  { listen_address : string
  ; debug_level    : int
  }

type spf_config =
  { enable                 : bool
  ; fail_on_helo_temperror : bool
  ; local_whitelist        : Network.t list
  ; relay_whitelist        : Network.t list
  ; result_headers         : string list
  }

type srs_config =
  { always_rewrite : bool }

type t =
  { milter : milter_config
  ; spf    : spf_config
  ; srs    : srs_config
  }

let local_addresses =
  [ "127.0.0.0/8"
  ; "::ffff:127.0.0.0/104"
  ]

let socket_string = function
  | `Str s ->
      if Str.string_match (Str.regexp "^\\(unix\\|local\\):\\(.+\\)") s 0 then
        existing_dirname (`Str (Str.matched_group 1 s))
      else if Str.string_match (Str.regexp "^inet6?:[0-9]+@\\(.+\\)") s 0 then
        try
          ignore (Unix.gethostbyname (Str.matched_group 1 s));
          `Valid
        with _ ->
          `Invalid "socket_string: invalid hostname or address"
      else
        `Invalid "socket_string: invalid socket string"
  | _ ->
      invalid_arg "socket_string: not a string"

module Milter_defaults = struct
  let listen_address = default_string "inet:8387@localhost"
  let debug_level = default_int 0
end

module SPF_defaults = struct
  let enable = default_bool true
  let fail_on_helo = default_bool true
  let local_whitelist = default_string_list local_addresses
  let relay_whitelist = default_string_list []
  let result_headers = default_string_list ["Authentication-Results"]
end

module SRS_defaults = struct
  let always_rewrite = default_bool false
end

let milter_spec =
  let module D = Milter_defaults in
  `Section ("milter",
    [ "listen_address", D.listen_address, [socket_string]
    ; "debug_level", D.debug_level, [int_in_range (0, 6)]
    ])

let spf_spec =
  let module D = SPF_defaults in
  `Section ("spf",
    [ "enable", D.enable, [bool]
    ; "fail_on_helo_temperror", D.fail_on_helo, [bool]
    ; "local_whitelist", D.local_whitelist, [string_list]
    ; "relay_whitelist", D.relay_whitelist, [string_list]
    ; "result_headers", D.result_headers,
        [list_of (string_in ["Authentication-Results"; "Received-SPF"])]
    ])

let srs_spec =
  let module D = SRS_defaults in
  `Section ("srs",
    ["always_rewrite", D.always_rewrite, [bool]])

let spec =
  [ milter_spec
  ; spf_spec
  ; srs_spec
  ]

let find_milter key conf =
  Release_config.get conf "milter" key

let find_spf key conf =
  Release_config.get conf "spf" key

let find_srs key conf =
  Release_config.get conf "srs" key

let network_list = List.map Network.of_string

let make c =
  let milter_config =
    let get = find_milter in
    { listen_address = string_value (get "listen_address" c)
    ; debug_level    = int_value (get "debug_level" c)
    } in
  let spf_config =
    let get = find_spf in
    { enable = bool_value (get "enable" c)
    ; fail_on_helo_temperror = bool_value (get "fail_on_helo_temperror" c)
    ; local_whitelist =
        network_list (string_list_value (get "local_whitelist" c))
    ; relay_whitelist =
        network_list (string_list_value (get "relay_whitelist" c))
    ; result_headers = string_list_value (get "result_headers" c)
    } in
  let srs_config =
    let get = find_srs in
    { always_rewrite = bool_value (get "always_rewrite" c) } in
  { milter = milter_config
  ; spf    = spf_config
  ; srs    = srs_config
  }

module C = Config.Make (struct
  type config = t
  let spec = spec
  let make = make
end)

let file = C.file
let load = C.load
let load_defaults = C.load_defaults
let current = C.current
let replace = C.replace

let milter_listen_address () =
  (current ()).milter.listen_address

let milter_debug_level () =
  (current ()).milter.debug_level

let spf_enable () =
  (current ()).spf.enable

let spf_fail_on_helo_temperror () =
  (current ()).spf.fail_on_helo_temperror

let spf_local_whitelist () =
  (current ()).spf.local_whitelist

let spf_relay_whitelist () =
  (current ()).spf.relay_whitelist

let spf_result_headers () =
  (current ()).spf.result_headers

let srs_always_rewrite () =
  (current ()).srs.always_rewrite
