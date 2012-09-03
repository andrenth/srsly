open Lwt
open Printf
open Release_config_values
open Release_config_validations

open Log_lwt
open Util

module O = Release_option

type srslyd_config =
  { lock_file      : Lwt_io.file_name
  ; control_socket : Lwt_io.file_name
  ; background     : bool
  ; log_level      : Lwt_log.level
  ; random_device  : Lwt_io.file_name
  }

type milter_config =
  { user           : string
  ; executable     : Lwt_io.file_name
  ; listen_address : string
  ; debug_level    : int
  }

type proxymap_config =
  { sender_lookup_table    : string
  ; rcpt_lookup_table      : string
  ; sender_lookup_key_fmt  : string
  ; rcpt_lookup_key_fmt    : string
  ; local_sender_regexp    : Str.regexp
  ; local_rcpt_regexp      : Str.regexp
  ; query_fmt              : string
  ; query_flags            : int
  ; query_socket           : Lwt_io.file_name
  ; max_query_depth        : int
  ; result_fmt             : string
  ; result_value_separator : Str.regexp
  }

type spf_config =
  { fail_on_helo_temperror : bool
  ; local_whitelist        : Network.t list
  ; relay_whitelist        : Network.t list
  }

type srs_config =
  { secret_file       : Lwt_io.file_name
  ; secrets_directory : Lwt_io.file_name
  ; hash_max_age      : int
  ; hash_length       : int
  ; separator         : char
  ; secret_length     : int
  }

type t =
  { srslyd   : srslyd_config
  ; milter   : milter_config
  ; proxymap : proxymap_config
  ; spf      : spf_config
  ; srs      : srs_config
  }

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

let secure_secrets_directory =
  [ existing_directory
  ; file_with_mode 0o700
  ; file_with_owner "root"
  ; file_with_group "root"
  ]

let secure_executable =
  [ file_with_mode 0o700
  ; file_with_owner "root"
  ; file_with_group "root"
  ]

let postfix_table = function
  | `Str s ->
      if Str.string_match (Str.regexp "^[a-z]+:\\(.+\\)$") s 0 then
        let file = Str.matched_group 1 s in
        try
          let st = Unix.lstat file in
          if st.Unix.st_kind = Unix.S_REG then
            `Valid
          else
            `Invalid (sprintf "postfix_table: %s is not a regular file" s)
        with Unix.Unix_error (e, _, _) ->
          `Invalid (sprintf "postfix_table: %s: %s" s (Unix.error_message e))
      else
        `Invalid
          (sprintf "postfix_table: %s is not a valid postfix table" s)
  | _ ->
      `Invalid "postfix_table: not a string"

module Srslyd_defaults = struct
  let lock_file = default_string "/var/run/srslyd.pid"
  let control_socket = default_string "/var/run/srslyd.sock"
  let log_level = default_log_level Lwt_log.Notice
  let background = default_bool true
  let random_device = default_string "/dev/random"
end

module Milter_defaults = struct
  let user = default_string "srsly"
  let listen_address = default_string "inet:8387@localhost"
  let executable = default_string "/usr/lib/srsly/srsly-milter"
  let debug_level = default_int 0
end

module Proxymap_defaults = struct
  let sender_lookup_table = default_string "hash:/etc/aliases"
  let rcpt_lookup_table = default_string "hash:/etc/aliases"
  let sender_lookup_key_fmt = default_string "{a}"
  let rcpt_lookup_key_fmt = default_string "{a}"
  let local_sender_regexp = default_regexp (Str.regexp "^[a-z]+$")
  let local_rcpt_regexp = default_regexp (Str.regexp "^[a-z]+$")
  let query_fmt = default_string
    "request\000lookup\000table\000{t}\000flags\000{f}\000key\000{k}\000\000"
  let query_flags = default_int
    16448 (* DICT_FLAG_FOLD_FIX | DICT_FLAG_LOCK *)
  let query_socket = default_string "/var/spool/postfix/private/proxymap"
  let max_query_depth = default_int 100
  let result_fmt = default_string
    "status\000{s}\000value\000{v}\000\000"
  let result_value_separator = default_regexp (Str.regexp ", *")
end

module SPF_defaults = struct
  let fail_on_helo = default_bool true
  let local_whitelist = default_string_list local_addresses
  let relay_whitelist = default_string_list []
end

module SRS_defaults = struct
  let secret_file = default_string "/etc/srsly/srs_secret"
  let secrets_directory = default_string "/etc/srsly/srs_secrets.d"
  let hash_max_age = default_int 8
  let hash_length = default_int 8
  let separator = default_string "="
  let secret_length = default_int 8
end

let srslyd_spec =
  let module D = Srslyd_defaults in
  `Section ("srslyd",
    [ "lock_file", D.lock_file, [existing_dirname]
    ; "control_socket", D.control_socket, [existing_dirname]
    ; "log_level", D.log_level, [log_level]
    ; "background", D.background, [bool]
    ; "random_device", D.random_device, [character_device]
    ])

let milter_spec =
  let module D = Milter_defaults in
  `Section ("milter",
    [ "user", D.user, [unprivileged_user]
    ; "executable", D.executable, secure_executable
    ; "listen_address", D.listen_address, [socket_string]
    ; "debug_level", D.debug_level, [int_in_range (0, 6)]
    ])

let proxymap_spec =
  let module D = Proxymap_defaults in
  `Section ("proxymap",
    [ "sender_lookup_table", D.sender_lookup_table, [postfix_table]
    ; "recipient_lookup_table", D.rcpt_lookup_table, [postfix_table]
    ; "sender_lookup_key_format", D.sender_lookup_key_fmt, [string]
    ; "recipient_lookup_key_format", D.rcpt_lookup_key_fmt, [string]
    ; "local_sender_regexp", D.local_sender_regexp, [regexp]
    ; "local_recipient_regexp", D.local_rcpt_regexp, [regexp]
    ; "query_format", D.query_fmt, [string]
    ; "query_flags", D.query_flags, [int]
    ; "query_socket", D.query_socket, [unix_socket]
    ; "maximum_query_depth", D.max_query_depth, [int_greater_than 0]
    ; "result_format", D.result_fmt, [string]
    ; "result_value_separator", D.result_value_separator, [regexp]
    ])

let spf_spec =
  let module D = SPF_defaults in
  `Section ("spf",
    [ "fail_on_helo_temperror", D.fail_on_helo, [bool]
    ; "local_whitelist", D.local_whitelist, [string_list]
    ; "relay_whitelist", D.relay_whitelist, [string_list]
    ])

let srs_spec =
  let module D = SRS_defaults in
  `Section ("srs",
    [ "secret_file", D.secret_file, secure_secret_file
    ; "secrets_directory", D.secrets_directory, secure_secrets_directory
    ; "hash_max_age", D.hash_max_age, [int]
    ; "hash_length", D.hash_length, [int]
    ; "separator", D.separator, [string_in ["+"; "-"; "="]]
    ; "secret_length", D.secret_length, [int_greater_than 7]
    ])

let spec =
  [ srslyd_spec
  ; milter_spec
  ; proxymap_spec
  ; spf_spec
  ; srs_spec
  ]

let find_srslyd key conf =
  Release_config.get_exn conf ~section:"srslyd" key ()

let find_milter key conf =
  Release_config.get_exn conf ~section:"milter" key ()

let find_proxymap key conf =
  Release_config.get_exn conf ~section:"proxymap" key ()

let find_spf key conf =
  Release_config.get_exn conf ~section:"spf" key ()

let find_srs key conf =
  Release_config.get_exn conf ~section:"srs" key ()

let find_srs_opt key conf =
  Release_config.get conf ~section:"srs" key ()

let whitelist_of_list = List.map Network.of_string

let make c =
  let srslyd_config =
    let get = find_srslyd in
    { lock_file = string_value (get "lock_file" c)
    ; control_socket = string_value (get "control_socket" c)
    ; background = bool_value (get "background" c)
    ; log_level = log_level_value (get "log_level" c)
    ; random_device = string_value (get "random_device" c)
    } in
  let milter_config =
    let get = find_milter in
    { user           = string_value (get "user" c)
    ; executable     = string_value (get "executable" c)
    ; listen_address = string_value (get "listen_address" c)
    ; debug_level    = int_value (get "debug_level" c)
    } in
  let proxymap_config =
    let get = find_proxymap in
    { sender_lookup_table = string_value (get "sender_lookup_table" c)
    ; rcpt_lookup_table = string_value (get "recipient_lookup_table" c)
    ; sender_lookup_key_fmt = string_value (get "sender_lookup_key_format" c)
    ; rcpt_lookup_key_fmt = string_value (get "recipient_lookup_key_format" c)
    ; local_sender_regexp = regexp_value (get "local_sender_regexp" c)
    ; local_rcpt_regexp = regexp_value (get "local_recipient_regexp" c)
    ; query_fmt = string_value (get "query_format" c)
    ; query_flags  = int_value (get "query_flags" c)
    ; query_socket = string_value (get "query_socket" c)
    ; max_query_depth = int_value (get "maximum_query_depth" c)
    ; result_fmt = string_value (get "result_format" c)
    ; result_value_separator = regexp_value (get "result_value_separator" c)
    } in
  let spf_config =
    let get = find_spf in
    { fail_on_helo_temperror = bool_value (get "fail_on_helo_temperror" c)
    ; local_whitelist =
        whitelist_of_list (string_list_value (get "local_whitelist" c))
    ; relay_whitelist =
        whitelist_of_list (string_list_value (get "relay_whitelist" c))
    } in
  let srs_config =
    let get = find_srs in
    { secret_file       = string_value (get "secret_file" c)
    ; secrets_directory = string_value (get "secrets_directory" c)
    ; hash_max_age      = int_value (get "hash_max_age" c)
    ; hash_length       = int_value (get "hash_length" c)
    ; separator         = (string_value (get "separator" c)).[0]
    ; secret_length     = int_value (get "secret_length" c)
    } in
  { srslyd   = srslyd_config
  ; milter   = milter_config
  ; proxymap = proxymap_config
  ; spf      = spf_config
  ; srs      = srs_config
  }

let config_file = ref None
let configuration = ref None

let file () =
  !config_file

let load file =
  let err fmt =
    ksprintf (fun s -> lwt () = error "%s" s in exit 1) fmt in
  try_lwt
    lwt st = Lwt_unix.lstat file in
    if st.Lwt_unix.st_kind = Lwt_unix.S_REG then
      match_lwt Release_config.parse file spec with
      | `Configuration conf ->
          config_file := Some file;
          configuration := Some (make conf);
          return_unit
      | `Error e ->
          err "%s" e
    else
      err "%s: not a regular file" file
  with Unix.Unix_error (e, _, _) ->
    err "%s: %s" file (Unix.error_message e)

let load_defaults () =
  configuration := Some (make (Release_config.defaults spec));
  return_unit

let current () =
  match !configuration with
  | None -> fprintf stderr "no config!"; exit 1
  | Some c -> c

let replace c =
  configuration := Some c

let serialize c =
  Marshal.to_string c []

let unserialize s =
  Marshal.from_string s 0

let srslyd_lock_file () =
  (current ()).srslyd.lock_file

let srslyd_control_socket () =
  (current ()).srslyd.control_socket

let srslyd_log_level () =
  (current ()).srslyd.log_level

let srslyd_background () =
  (current ()).srslyd.background

let srslyd_random_device () =
  (current ()).srslyd.random_device

let milter_user () =
  (current ()).milter.user

let milter_executable () =
  (current ()).milter.executable

let milter_listen_address () =
  (current ()).milter.listen_address

let milter_debug_level () =
  (current ()).milter.debug_level

let proxymap_sender_lookup_table () =
  (current ()).proxymap.sender_lookup_table

let proxymap_recipient_lookup_table () =
  (current ()).proxymap.rcpt_lookup_table

let proxymap_sender_lookup_key_format () =
  (current ()).proxymap.sender_lookup_key_fmt

let proxymap_recipient_lookup_key_format () =
  (current ()).proxymap.rcpt_lookup_key_fmt

let proxymap_local_sender_regexp () =
  (current ()).proxymap.local_sender_regexp

let proxymap_local_recipient_regexp () =
  (current ()).proxymap.local_rcpt_regexp

let proxymap_query_format () =
  (current ()).proxymap.query_fmt

let proxymap_query_flags () =
  (current ()).proxymap.query_flags

let proxymap_query_socket () =
  (current ()).proxymap.query_socket

let proxymap_max_query_depth () =
  (current ()).proxymap.max_query_depth

let proxymap_result_format () =
  (current ()).proxymap.result_fmt

let proxymap_result_value_separator () =
  (current ()).proxymap.result_value_separator

let spf_fail_on_helo_temperror () =
  (current ()).spf.fail_on_helo_temperror

let spf_local_whitelist () =
  (current ()).spf.local_whitelist

let spf_relay_whitelist () =
  (current ()).spf.relay_whitelist

let srs_secret_file () =
  (current ()).srs.secret_file

let srs_secrets_directory () =
  (current ()).srs.secrets_directory

let srs_hash_max_age () =
  (current ()).srs.hash_max_age

let srs_hash_length () =
  (current ()).srs.hash_length

let srs_separator () =
  (current ()).srs.separator

let srs_secret_length () =
  (current ()).srs.secret_length
