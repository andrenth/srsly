open Printf
open Release_config_types
open Release_config_validations
open Util

module O = Release_option

type milter_config =
  { user              : string
  ; input_executable  : Lwt_io.file_name
  ; output_executable : Lwt_io.file_name
  ; listen_address    : string * string
  ; debug_level       : int
  }

type srs_config =
  { domain        : string option
  ; secret_file   : Lwt_io.file_name
  ; hash_max_age  : int
  ; hash_length   : int 
  ; separator     : char
  ; secret_length : int
  }

type t =
  { lock_file              : Lwt_io.file_name
  ; control_socket         : Lwt_io.file_name
  ; background             : bool
  ; log_level              : Lwt_log.level
  ; fail_on_helo_temperror : bool
  ; local_whitelist        : Network.t list
  ; relay_whitelist        : Network.t list
  ; random_device          : Lwt_io.file_name
  ; milter                 : milter_config
  ; srs                    : srs_config
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
  let control_socket = default_string "/var/run/srslyd.sock"
  let log_level = default_string "notice"
  let fail_on_helo = default_bool true
  let local_whitelist = default_string_list local_addresses
  let relay_whitelist = default_string_list []
  let background = default_bool true
  let random_device = default_string "/dev/random"
end

module MD = struct
  let user = default_string "srslyd"
  let debug_level = default_int 0
end

module SD = struct
  let hash_max_age = default_int 8
  let hash_length = default_int 8
  let separator = default_string "="
  let secret_length = default_int 8
end

let spec =
  [ `Required ("srslyd",
      [ `Optional ("lock_file", D.lock_file, [existing_dirname])
      ; `Optional ("control_socket", D.control_socket, [existing_dirname])
      ; `Optional ("log_level", D.log_level, [string_in log_levels])
      ; `Optional ("fail_on_helo_temperror", D.fail_on_helo, [bool])
      ; `Optional ("local_whitelist", D.local_whitelist, [string_list])
      ; `Optional ("relay_whitelist", D.relay_whitelist, [string_list])
      ; `Optional ("background", D.background, [bool])
      ; `Optional ("random_device", D.random_device, [character_device])
      ])
  ; `Required ("milter",
      [ `Optional ("user", MD.user, [unprivileged_user])
      ; `Required ("input_executable", secure_executable)
      ; `Required ("output_executable", secure_executable)
      ; `Required ("input_listen_address", [socket_string])
      ; `Required ("output_listen_address", [socket_string])
      ; `Optional ("debug_level", MD.debug_level, [int_in_range (0, 6)])
      ])
  ; `Required ("srs",
      [ `Optional ("domain", None, [string])
      ; `Required ("secret_file", secure_secret_file)
      ; `Optional ("hash_max_age", SD.hash_max_age, [int])
      ; `Optional ("hash_length", SD.hash_length, [int])
      ; `Optional ("separator", SD.separator, [string_in ["+"; "-"; "="]])
      ; `Optional ("secret_length", SD.secret_length, [int_greater_than 7])
      ])
  ]

let find_srslyd key conf =
  Release_config.get_exn conf ~section:"srslyd" key ()

let find_milter key conf =
  Release_config.get_exn conf ~section:"milter" key ()

let find_srs key conf =
  Release_config.get_exn conf ~section:"srs" key () 

let find_srs_opt key conf =
  Release_config.get conf ~section:"srs" key () 

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
  let lock_file = string_value (find_srslyd "lock_file" c) in
  let control_socket = string_value (find_srslyd "control_socket" c) in
  let background = bool_value (find_srslyd "background" c) in
  let log_level = log_level_of_string (string_value (find_srslyd "log_level" c)) in
  let fail_on_helo_temperror = bool_value (find_srslyd "fail_on_helo_temperror" c) in
  let local_whitelist = 
    whitelist_of_list (string_list_value (find_srslyd "local_whitelist" c)) in
  let relay_whitelist =
    whitelist_of_list (string_list_value (find_srslyd "relay_whitelist" c)) in
  let random_device = string_value (find_srslyd "random_device" c) in
  let milter_addr_in = string_value (find_milter "input_listen_address" c) in
  let milter_addr_out = string_value (find_milter "output_listen_address" c) in
  let milter_config =
    { user              = string_value (find_milter "user" c)
    ; input_executable  = string_value (find_milter "input_executable" c)
    ; output_executable = string_value (find_milter "output_executable" c)
    ; listen_address    = (milter_addr_in, milter_addr_out)
    ; debug_level       = int_value (find_milter "debug_level" c)
    } in
  let srs_config =
    { domain        = O.map string_value (find_srs_opt "domain" c)
    ; secret_file   = string_value (find_srs "secret_file" c)
    ; hash_max_age  = int_value (find_srs "hash_max_age" c)
    ; hash_length   = int_value (find_srs "hash_length" c)
    ; separator     = (string_value (find_srs "separator" c)).[0]
    ; secret_length = int_value (find_srs "secret_length" c)
    } in
  { lock_file              = lock_file
  ; control_socket         = control_socket
  ; background             = background
  ; log_level              = log_level
  ; fail_on_helo_temperror = fail_on_helo_temperror
  ; local_whitelist        = local_whitelist
  ; relay_whitelist        = relay_whitelist
  ; random_device          = random_device
  ; milter                 = milter_config
  ; srs                    = srs_config
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

let random_device () =
  (current ()).random_device

let milter_user () =
  (current ()).milter.user

let milter_input_executable () =
  (current ()).milter.input_executable

let milter_output_executable () =
  (current ()).milter.output_executable

let milter_input_listen_address () =
  fst (current ()).milter.listen_address

let milter_output_listen_address () =
  snd (current ()).milter.listen_address

let milter_debug_level () =
  (current ()).milter.debug_level

let srs_domain () =
  (current ()).srs.domain

let srs_secret_file () =
  (current ()).srs.secret_file

let srs_hash_max_age () =
  (current ()).srs.hash_max_age

let srs_hash_length () =
  (current ()).srs.hash_length

let srs_separator () =
  (current ()).srs.separator

let srs_secret_length () =
  (current ()).srs.secret_length
