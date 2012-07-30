open Printf
open Release_config_types
open Release_config_validations
open Util

type t =
  { lock_file              : Lwt_io.file_name
  ; user                   : string
  ; binary_path            : Lwt_io.file_name
  ; background             : bool
  ; log_level              : Lwt_log.level
  ; fail_on_helo_temperror : bool
  ; local_whitelist        : Network.t list
  ; relay_whitelist        : Network.t list
  ; listen_address         : string * string
  ; srs_domain             : string option
  ; srs_secret_file        : Lwt_io.file_name
  ; srs_hash_max_age       : int
  ; srs_hash_length        : int 
  ; srs_separator          : char
  ; milter_debug_level     : int
  }

let file =
  if Array.length Sys.argv > 1 then Sys.argv.(1)
  else "/etc/srslyd/srslyd.conf"

let log_levels =
  [ "debug"
  ; "info"
  ; "notice"
  ; "warning"
  ; "error"
  ; "fatal"
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

let secure_secret_file =
  [ file_with_mode 0o600
  ; file_with_owner "root"
  ; file_with_group "root"
  ; nonempty_file
  ]

let local_addresses =
  [ "127.0.0.0/8"
  ; "::ffff:127.0.0.0/104"
  ]

let default_lock_file = default_string "/var/run/srslyd/srslyd.pid"
let default_user = default_string "srslyd"
let default_binary_path = default_string "/usr/lib/srslyd"
let default_log_level = default_string "notice"
let default_fail_on_helo = default_bool true
let default_local_whitelist = default_string_list local_addresses
let default_relay_whitelist = default_string_list []
let default_background = default_bool true
let default_srs_hash_max_age = Some (`Int 8)
let default_srs_hash_length = Some (`Int 8)
let default_srs_separator = Some (`Str "=")
let default_milter_debug_level = Some (`Int 0)

let spec =
  [`Global
    [ `Optional ("lock_file", default_lock_file, [existing_dirname])
    ; `Optional ("user", default_user, [unprivileged_user])
    ; `Optional ("binary_path", default_binary_path, [existing_directory])
    ; `Optional ("log_level", default_log_level, [string_in log_levels])
    ; `Optional ("fail_on_helo_temperror", default_fail_on_helo, [bool])
    ; `Optional ("local_whitelist", default_local_whitelist, [string_list])
    ; `Optional ("relay_whitelist", default_relay_whitelist, [string_list])
    ; `Optional ("background", default_background, [bool])
    ; `Required ("listen_address_in", [socket_string])
    ; `Required ("listen_address_out", [socket_string])
    ; `Optional ("srs_domain", None, [string])
    ; `Required ("srs_secret_file", secure_secret_file)
    ; `Optional ("srs_hash_max_age", default_srs_hash_max_age, [int])
    ; `Optional ("srs_hash_length", default_srs_hash_length, [int])
    ; `Optional ("srs_separator", default_srs_separator,
                 [string_in ["+"; "-"; "="]])
    ; `Optional ("milter_debug_level", default_milter_debug_level,
                 [int_in_range (0, 6)])
    ]]

let find key conf =
  Release_config.get_exn conf key () 

let find_opt key conf =
  Release_config.get conf key () 

let log_level_of_string = function
  | "debug" -> Lwt_log.Debug
  | "info" -> Lwt_log.Info
  | "notice" -> Lwt_log.Notice
  | "warning" -> Lwt_log.Warning
  | "error" -> Lwt_log.Error
  | "fatal" -> Lwt_log.Fatal
  | _ -> invalid_arg "Config.log_level_of_string"

let whitelist_of_list = List.map Network.of_string

let make c =
  let lock_file = string_value (find "lock_file" c) in
  let user = string_value (find "user" c) in
  let binary_path = string_value (find "binary_path" c) in
  let background = bool_value (find "background" c) in
  let log_level = log_level_of_string (string_value (find "log_level" c)) in
  let fail_on_helo_temperror = bool_value (find "fail_on_helo_temperror" c) in
  let local_whitelist = 
    whitelist_of_list (string_list_value (find "local_whitelist" c)) in
  let relay_whitelist =
    whitelist_of_list (string_list_value (find "relay_whitelist" c)) in
  let listen_address_in = string_value (find "listen_address_in" c) in
  let listen_address_out = string_value (find "listen_address_out" c) in
  let srs_domain = map_opt string_value (find_opt "srs_domain" c) in
  let srs_secret_file = string_value (find "srs_secret_file" c) in
  let srs_hash_max_age = int_value (find "srs_hash_max_age" c) in
  let srs_hash_length = int_value (find "srs_hash_length" c) in
  let srs_separator = (string_value (find "srs_separator" c)).[0] in
  let milter_debug_level = int_value (find "milter_debug_level" c) in
  { lock_file              = lock_file
  ; user                   = user
  ; binary_path            = binary_path
  ; background             = background
  ; log_level              = log_level
  ; fail_on_helo_temperror = fail_on_helo_temperror
  ; local_whitelist        = local_whitelist
  ; relay_whitelist        = relay_whitelist
  ; listen_address         = listen_address_in, listen_address_out
  ; srs_domain             = srs_domain
  ; srs_secret_file        = srs_secret_file
  ; srs_hash_max_age       = srs_hash_max_age
  ; srs_hash_length        = srs_hash_length 
  ; srs_separator          = srs_separator
  ; milter_debug_level     = milter_debug_level
  }

let read () =
  match Release_config.parse file spec with
  | `Configuration conf ->
      make conf
  | `Error e ->
      fprintf stderr "%s\n%!" e;
      exit 1

let configuration = ref None

let current () =
  match !configuration with
  | None ->
      let c = read () in
      configuration := Some c;
      c
  | Some c ->
      c

let reload () =
  configuration := Some (read ())

let replace c =
  configuration := Some c

let serialize c =
  Marshal.to_string c []

let unserialize s i =
  (Marshal.from_string s i : t)

let user () =
  (current ()).user

let binary_path () =
  (current ()).binary_path

let log_level () =
  (current ()).log_level

let local_whitelist () =
  (current ()).local_whitelist

let relay_whitelist () =
  (current ()).relay_whitelist

let fail_on_helo_temperror () =
  (current ()).fail_on_helo_temperror

let background () =
  (current ()).background

let listen_address_in () =
  fst (current ()).listen_address

let listen_address_out () =
  snd (current ()).listen_address

let srs_domain () =
  (current ()).srs_domain

let srs_secret_file () =
  (current ()).srs_secret_file

let srs_hash_max_age () =
  (current ()).srs_hash_max_age

let srs_hash_length () =
  (current ()).srs_hash_length

let srs_separator () =
  (current ()).srs_separator

let milter_debug_level () =
  (current ()).milter_debug_level
