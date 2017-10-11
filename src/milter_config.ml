open Lwt
open Printf
open Release_lwt
open Release.Config.Value
open Release.Config.Validation

open Log_lwt
open Util

module O = Release.Util.Option
module Value = Release.Config.Value

type milter_config =
  { listen_address : string
  ; debug_level    : int
  }

type spf_config =
  { spf_enable             : bool
  ; fail_on_helo_temperror : bool
  ; local_whitelist        : Network.t list
  ; relay_whitelist        : Network.t list
  ; result_headers         : string list
  }

type srs_config =
  { srs_enable     : bool
  ; always_rewrite : bool
  }

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
      let unix_re = Str.regexp "^\\(unix\\|local\\):\\(.+\\)" in
      let inet_re = Str.regexp "^inet\\(6?\\):[0-9]+@\\(.+\\)" in
      let ipv4_re = Str.regexp "^\\(?:[0-9]{1,3}\\.\\){3}[0-9]{1,3}$" in
      let hextet = "[A-Fa-f0-9]?[A-Fa-f0-9]?[A-Fa-f0-9]?[A-Fa-f0-9]?" in
      let ipv6_re = Str.regexp ("^\\(:?" ^ hextet ^ "\\)*:" ^ hextet ^ "$") in
      if Str.string_match unix_re s 0 then
        existing_dirname (`Str (Str.matched_group 1 s))
      else if Str.string_match inet_re s 0 then begin
        let domain =
          if Str.matched_group 1 s = ""
          then Unix.PF_INET
          else Unix.PF_INET6 in
        let host = Str.matched_group 2 s in
        let is_ip =
          Str.string_match ipv4_re host 0 || Str.string_match ipv6_re host 0 in
        if is_ip then begin
          try
            let sockaddr = Unix.ADDR_INET (Unix.inet_addr_of_string host, 0) in
            ignore (Unix.getnameinfo sockaddr [Unix.NI_NAMEREQD]);
            Printf.printf ">>>>>>>>>>>>>>>>>>>>>>>>>>> %s is valid\n%!" host;
            `Valid
          with Not_found ->
            `Invalid "socket_string: invalid IP address"
        end else begin
          match Unix.getaddrinfo host "0" [Unix.AI_FAMILY domain] with
          | [] -> `Invalid "socket_string: invalid name"
          | _ -> `Valid
        end
      end else
        `Invalid "socket_string: invalid socket string"
  | _ ->
      invalid_arg "socket_string: not a string"

module Milter_defaults = struct
  let listen_address = Default.string "inet:8387@localhost"
  let debug_level = Default.int 0
end

module SPF_defaults = struct
  let spf_enable = Default.bool true
  let fail_on_helo = Default.bool true
  let local_whitelist = Default.string_list local_addresses
  let relay_whitelist = Default.string_list []
  let result_headers = Default.string_list ["Authentication-Results"]
end

module SRS_defaults = struct
  let srs_enable = Default.bool true
  let always_rewrite = Default.bool false
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
    [ "enable", D.spf_enable, [bool]
    ; "fail_on_helo_temperror", D.fail_on_helo, [bool]
    ; "local_whitelist", D.local_whitelist, [string_list]
    ; "relay_whitelist", D.relay_whitelist, [string_list]
    ; "result_headers", D.result_headers,
        [list_of (string_in ["Authentication-Results"; "Received-SPF"])]
    ])

let srs_spec =
  let module D = SRS_defaults in
  `Section ("srs",
    [ "enable", D.srs_enable, [bool]
    ; "always_rewrite", D.always_rewrite, [bool]
    ])

let spec =
  [ milter_spec
  ; spf_spec
  ; srs_spec
  ]

let find_milter key conf =
  Release.Config.get conf "milter" key

let find_spf key conf =
  Release.Config.get conf "spf" key

let find_srs key conf =
  Release.Config.get conf "srs" key

let network_list = List.map Network.of_string

let make c =
  let milter_config =
    let get = find_milter in
    { listen_address = Value.to_string (get "listen_address" c)
    ; debug_level    = Value.to_int (get "debug_level" c)
    } in
  let spf_config =
    let get = find_spf in
    { spf_enable = Value.to_bool (get "enable" c)
    ; fail_on_helo_temperror = Value.to_bool (get "fail_on_helo_temperror" c)
    ; local_whitelist =
        network_list (Value.to_string_list (get "local_whitelist" c))
    ; relay_whitelist =
        network_list (Value.to_string_list (get "relay_whitelist" c))
    ; result_headers = Value.to_string_list (get "result_headers" c)
    } in
  let srs_config =
    let get = find_srs in
    { srs_enable = Value.to_bool (get "enable" c)
    ; always_rewrite = Value.to_bool (get "always_rewrite" c)
    } in
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
  (current ()).spf.spf_enable

let spf_fail_on_helo_temperror () =
  (current ()).spf.fail_on_helo_temperror

let spf_local_whitelist () =
  (current ()).spf.local_whitelist

let spf_relay_whitelist () =
  (current ()).spf.relay_whitelist

let spf_result_headers () =
  (current ()).spf.result_headers

let srs_enable () =
  (current ()).srs.srs_enable

let srs_always_rewrite () =
  (current ()).srs.always_rewrite
