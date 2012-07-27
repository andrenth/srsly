open Printf
open Release_config_types
open Release_config_validations
open Util

type slave_config
  = Policyd of Policyd_config.t
  | Milter of Milter_config.t

type t =
  { lock_file              : Lwt_io.file_name
  ; user                   : string
  ; binary_path            : Lwt_io.file_name
  ; background             : bool
  ; log_level              : Lwt_log.level
  ; fail_on_helo_temperror : bool
  ; local_whitelist        : Network.t list
  ; relay_whitelist        : Network.t list
  ; slave                  : slave_config
  }

let file =
  if Array.length Sys.argv > 1 then Sys.argv.(1)
  else "/etc/spfd/spfd.conf"

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

let spec =
  [ `Global
      [ `Optional ("lock_file",              [existing_dirname])
      ; `Optional ("user",                   [unprivileged_user])
      ; `Optional ("binary_path",            [existing_directory])
      ; `Optional ("log_level",              [string_in log_levels])
      ; `Optional ("fail_on_helo_temperror", [bool])
      ; `Optional ("local_whitelist",        [string])
      ; `Optional ("relay_whitelist",        [string])
      ; `Optional ("background",             [bool])
      ]

  ; `Optional ("policyd",
      [ `Required ("listen_address",         [socket_string])
      ; `Optional ("num_slaves",             [int])
      ])

  ; `Optional ("milter",
      [ `Required ("listen_address_in",      [socket_string])
      ; `Required ("listen_address_out",     [socket_string])
      ; `Optional ("srs_domain",             [string])
      ; `Required ("srs_secret_file",        secure_secret_file)
      ; `Optional ("srs_hash_max_age",       [int])
      ; `Optional ("srs_hash_length",        [int])
      ; `Optional ("srs_hash_separator",     [string_in ["+"; "-"; "="]])
      ; `Optional ("debug_level",            [int_in_range (0, 6)])
      ])
  ]

let find key conf =
  Release_config.get conf key () 

let log_level_of_string = function
  | "debug" -> Lwt_log.Debug
  | "info" -> Lwt_log.Info
  | "notice" -> Lwt_log.Notice
  | "warning" -> Lwt_log.Warning
  | "error" -> Lwt_log.Error
  | "fatal" -> Lwt_log.Fatal
  | _ -> invalid_arg "Config.log_level_of_string"

let whitelist_of_string s =
  List.map Network.of_string (Str.split (Str.regexp "[ \t]*,[ \t]+*") s)

let default_local_addresses =
  List.map Network.of_string
    [ "127.0.0.0/8"
    ; "::ffff:127.0.0.0/104"
    ]

let default_relay_addresses = []

let lock_file = "/var/run/spfd/spfd.pid"
let user = "spfd"
let binary_path = "/usr/lib/spfd"
let log_level = Lwt_log.Notice
let fail_on_helo_temperror = true
let local_whitelist = default_local_addresses
let relay_whitelist = default_relay_addresses

let make c =
  let lock_file = default lock_file string_value (find "lock_file" c) in
  let user = default user string_value (find "user" c) in
  let binary_path = default binary_path string_value (find "binary_path" c) in
  let background = default true bool_value (find "background" c) in
  let log_level =
    default log_level
      (fun l -> log_level_of_string (string_value l))
      (find "log_level" c) in
  let fail_on_helo_temperror =
    default fail_on_helo_temperror
      bool_value
      (find "fail_on_helo_temperror" c) in
  let local_whitelist =
    default local_whitelist
      (fun w -> whitelist_of_string (string_value w))
      (find "local_whitelist" c) in
  let relay_whitelist =
    default relay_whitelist
      (fun w -> whitelist_of_string (string_value w))
      (find "relay_whitelist" c) in
  let slave_config =
    if Release_config.has_section c "milter" then
      Milter (Milter_config.of_configuration c)
    else
      Policyd (Policyd_config.of_configuration c) in
  { lock_file              = lock_file
  ; user                   = user
  ; binary_path            = binary_path
  ; background             = background
  ; log_level              = log_level
  ; fail_on_helo_temperror = fail_on_helo_temperror
  ; local_whitelist        = local_whitelist
  ; relay_whitelist        = relay_whitelist
  ; slave                  = slave_config
  }

let validate conf =
  let milter = Release_config.has_section conf "milter" in
  let policyd = Release_config.has_section conf "policyd" in
  if (milter && policyd) || (not milter && not policyd) then begin
    fprintf stderr "choose either milter or policyd mode\n%!";
    exit 1
  end

let read () =
  match Release_config.parse file spec with
  | `Configuration conf ->
      validate conf;
      make conf
  | `Error e ->
      fprintf stderr "%s\n%!" e;
      exit 1

let milter_config conf =
  match conf.slave with
  | Milter m -> m
  | Policyd _ -> invalid_arg "milter_config"

let policyd_config conf =
  match conf.slave with
  | Policyd p -> p
  | Milter _ -> invalid_arg "policyd_config"

let configuration = ref None

let current () =
  match !configuration with
  | None ->
      let c = read () in
      configuration := Some c;
      c
  | Some c ->
      c

let milter () =
  milter_config (current ())

let policyd () =
  policyd_config (current ())

let reload () =
  configuration := Some (read ())

let replace c =
  configuration := Some c

let is_milter () =
  match (current ()).slave with
  | Milter _ -> true
  | Policyd _ -> false

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
