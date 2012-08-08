open Printf
open Release_config_types
open Release_config_validations
open Util

module O = Release_option

type t =
  { lock_file              : Lwt_io.file_name
  ; user                   : string
  ; milter_in_executable   : Lwt_io.file_name
  ; milter_out_executable  : Lwt_io.file_name
  ; control_socket         : Lwt_io.file_name
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
  ; srs_secret_length      : int
  ; random_device          : Lwt_io.file_name
  ; milter_debug_level     : int
  }

let default_config_file = "/etc/srsly/srslyd.conf"

let file = ref default_config_file

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

let local_addresses =
  [ "127.0.0.0/8"
  ; "::ffff:127.0.0.0/104"
  ]

let secure_secret_file =
  [ file_with_mode 0o600
  ; file_with_owner "root"
  ; file_with_group "root"
  ]

let secure_executable =
  [ file_with_mode 0o700
  ; file_with_owner "root"
  ; file_with_group "root"
  ]

module D = struct
  let lock_file = default_string "/var/run/srslyd/srslyd.pid"
  let user = default_string "srslyd"
  let control_socket = default_string "/var/run/srslyd.sock"
  let log_level = default_string "notice"
  let fail_on_helo = default_bool true
  let local_whitelist = default_string_list local_addresses
  let relay_whitelist = default_string_list []
  let background = default_bool true
  let srs_hash_max_age = Some (`Int 8)
  let srs_hash_length = Some (`Int 8)
  let srs_separator = Some (`Str "=")
  let srs_secret_length = Some (`Int 8)
  let random_device = Some (`Str "/dev/random")
  let milter_debug_level = Some (`Int 0)
end

let spec =
  [`Global
    [ `Optional ("lock_file", D.lock_file, [existing_dirname])
    ; `Optional ("user", D.user, [unprivileged_user])
    ; `Required ("milter_in_executable", secure_executable)
    ; `Required ("milter_out_executable", secure_executable)
    ; `Optional ("control_socket", D.control_socket, [existing_dirname])
    ; `Optional ("log_level", D.log_level, [string_in log_levels])
    ; `Optional ("fail_on_helo_temperror", D.fail_on_helo, [bool])
    ; `Optional ("local_whitelist", D.local_whitelist, [string_list])
    ; `Optional ("relay_whitelist", D.relay_whitelist, [string_list])
    ; `Optional ("background", D.background, [bool])
    ; `Required ("listen_address_in", [socket_string])
    ; `Required ("listen_address_out", [socket_string])
    ; `Optional ("srs_domain", None, [string])
    ; `Required ("srs_secret_file", secure_secret_file)
    ; `Optional ("srs_hash_max_age", D.srs_hash_max_age, [int])
    ; `Optional ("srs_hash_length", D.srs_hash_length, [int])
    ; `Optional ("srs_separator", D.srs_separator, [string_in ["+"; "-"; "="]])
    ; `Optional ("srs_secret_length", D.srs_secret_length, [int_greater_than 7])
    ; `Optional ("random_device", D.random_device, [character_device])
    ; `Optional ("milter_debug_level", D.milter_debug_level,
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
  let milter_in_executable = string_value (find "milter_in_executable" c) in
  let milter_out_executable = string_value (find "milter_out_executable" c) in
  let control_socket = string_value (find "control_socket" c) in
  let background = bool_value (find "background" c) in
  let log_level = log_level_of_string (string_value (find "log_level" c)) in
  let fail_on_helo_temperror = bool_value (find "fail_on_helo_temperror" c) in
  let local_whitelist = 
    whitelist_of_list (string_list_value (find "local_whitelist" c)) in
  let relay_whitelist =
    whitelist_of_list (string_list_value (find "relay_whitelist" c)) in
  let listen_address_in = string_value (find "listen_address_in" c) in
  let listen_address_out = string_value (find "listen_address_out" c) in
  let srs_domain = O.map string_value (find_opt "srs_domain" c) in
  let srs_secret_file = string_value (find "srs_secret_file" c) in
  let srs_hash_max_age = int_value (find "srs_hash_max_age" c) in
  let srs_hash_length = int_value (find "srs_hash_length" c) in
  let srs_separator = (string_value (find "srs_separator" c)).[0] in
  let srs_secret_length = int_value (find "srs_secret_length" c) in
  let random_device = string_value (find "random_device" c) in
  let milter_debug_level = int_value (find "milter_debug_level" c) in
  { lock_file              = lock_file
  ; user                   = user
  ; milter_in_executable   = milter_in_executable
  ; milter_out_executable  = milter_out_executable
  ; control_socket         = control_socket
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
  ; srs_secret_length      = srs_secret_length
  ; random_device          = random_device
  ; milter_debug_level     = milter_debug_level
  }

let read () =
  match Release_config.parse !file spec with
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

let lock_file () =
  (current ()).lock_file

let user () =
  (current ()).user

let milter_in_executable () =
  (current ()).milter_in_executable

let milter_out_executable () =
  (current ()).milter_out_executable

let control_socket () =
  (current ()).control_socket

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

let srs_secret_length () =
  (current ()).srs_secret_length

let random_device () =
  (current ()).random_device

let milter_debug_level () =
  (current ()).milter_debug_level
