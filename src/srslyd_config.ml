open Lwt
open Printf
open Release_lwt
open Release.Config.Value
open Release.Config.Validation

open Log_lwt
open Util

module O = Release.Util.Option
module Value = Release.Config.Value

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
  ; config_path    : Lwt_io.file_name
  }

type proxymap_config =
  { sender_lookup_table      : string
  ; rcpt_lookup_table        : string
  ; sender_lookup_key_fmt    : string
  ; rcpt_lookup_key_fmt      : string
  ; local_sender_regexp      : Str.regexp
  ; local_rcpt_regexp        : Str.regexp
  ; query_fmt                : string
  ; query_flags              : int
  ; query_socket             : Lwt_io.file_name
  ; sender_query_max_depth   : int
  ; rcpt_query_max_depth     : int
  ; sender_query_max_results : int
  ; rcpt_query_max_results   : int
  ; result_fmt               : string
  ; result_value_separator   : Str.regexp
  }

type srs_config =
  { secret_file        : Lwt_io.file_name
  ; secrets_directory  : Lwt_io.file_name
  ; hash_max_age       : int
  ; hash_length        : int
  ; separator          : char
  ; secret_length      : int
  }

type t =
  { srslyd   : srslyd_config
  ; milter   : milter_config
  ; proxymap : proxymap_config
  ; srs      : srs_config
  }

let log_level =
  string_in
    [ "debug"
    ; "info"
    ; "notice"
    ; "warning"
    ; "error"
    ; "fatal"
    ]

let log_level_value lvl =
  match Value.to_string lvl with
  | "debug" -> Lwt_log.Debug
  | "info" -> Lwt_log.Info
  | "notice" -> Lwt_log.Notice
  | "warning" -> Lwt_log.Warning
  | "error" -> Lwt_log.Error
  | "fatal" -> Lwt_log.Fatal
  | _ -> failwith "Invalid log level; shouldn't have happened"

let secure_executable =
  [ file_with_mode 0o700
  ; file_with_owner "root"
  ; file_with_group "root"
  ]

let secure_file =
  [ file_with_mode 0o600
  ; file_with_owner "root"
  ; file_with_group "root"
  ]

let secure_directory =
  [ existing_directory
  ; file_with_mode 0o700
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
  let lock_file = Default.string "/var/run/srslyd.pid"
  let control_socket = Default.string "/var/run/srslyd.sock"
  let log_level = Default.string "notice"
  let background = Default.bool true
  let random_device = Default.string "/dev/random"
end

module Milter_defaults = struct
  let user = Default.string "srsly"
  let executable = Default.string "/usr/lib/srsly/srsly-milter"
  let config_path = Default.string "/etc/srsly/conf.d"
end

module Proxymap_defaults = struct
  let sender_lookup_table = Default.string "hash:/etc/aliases"
  let rcpt_lookup_table = Default.string "hash:/etc/aliases"
  let sender_lookup_key_fmt = Default.string "{a}"
  let rcpt_lookup_key_fmt = Default.string "{a}"
  let local_sender_regexp = Default.regexp (Str.regexp "^[a-z]+$")
  let local_rcpt_regexp = Default.regexp (Str.regexp "^[a-z]+$")
  let query_fmt = Default.string
    "request\000lookup\000table\000{t}\000flags\000{f}\000key\000{k}\000\000"
  let query_flags = Default.int
    16448 (* DICT_FLAG_FOLD_FIX | DICT_FLAG_LOCK *)
  let query_socket = Default.string "/var/spool/postfix/private/proxymap"
  let sender_query_max_depth = Default.int 1
  let rcpt_query_max_depth = Default.int 20
  let sender_query_max_results = Default.int 1
  let rcpt_query_max_results = Default.int 100
  let result_fmt = Default.string
    "status\000{s}\000value\000{v}\000\000"
  let result_value_separator = Default.regexp (Str.regexp ", *")
end

module SRS_defaults = struct
  let secret_file = Default.string "/etc/srsly/srs_secret"
  let secrets_directory = Default.string "/etc/srsly/srs_secrets.d"
  let hash_max_age = Default.int 8
  let hash_length = Default.int 8
  let separator = Default.string "="
  let secret_length = Default.int 8
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
    ; "config_path", D.config_path, secure_directory
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
    ; "sender_query_max_depth", D.sender_query_max_depth, [int_greater_than 0]
    ; "recipient_query_max_depth", D.rcpt_query_max_depth, [int_greater_than 0]
    ; "sender_query_max_results", D.sender_query_max_results,
        [int_greater_than 0]
    ; "recipient_query_max_results", D.rcpt_query_max_results,
        [int_greater_than 0]
    ; "result_format", D.result_fmt, [string]
    ; "result_value_separator", D.result_value_separator, [regexp]
    ])

let srs_spec =
  let module D = SRS_defaults in
  `Section ("srs",
    [ "secret_file", D.secret_file, secure_file
    ; "secrets_directory", D.secrets_directory, secure_directory
    ; "hash_max_age", D.hash_max_age, [int]
    ; "hash_length", D.hash_length, [int]
    ; "separator", D.separator, [string_in ["+"; "-"; "="]]
    ; "secret_length", D.secret_length, [int_greater_than 7]
    ])

let spec =
  [ srslyd_spec
  ; milter_spec
  ; proxymap_spec
  ; srs_spec
  ]

let find_srslyd key conf =
  Release.Config.get conf "srslyd" key

let find_milter key conf =
  Release.Config.get conf "milter" key

let find_proxymap key conf =
  Release.Config.get conf "proxymap" key

let find_srs key conf =
  Release.Config.get conf "srs" key

let make c =
  let srslyd_config =
    let get = find_srslyd in
    { lock_file = Value.to_string (get "lock_file" c)
    ; control_socket = Value.to_string (get "control_socket" c)
    ; background = Value.to_bool (get "background" c)
    ; log_level = log_level_value (get "log_level" c)
    ; random_device = Value.to_string (get "random_device" c)
    } in
  let milter_config =
    let get = find_milter in
    { user           = Value.to_string (get "user" c)
    ; executable     = Value.to_string (get "executable" c)
    ; config_path    = Value.to_string (get "config_path" c)
    } in
  let proxymap_config =
    let get = find_proxymap in
    { sender_lookup_table = Value.to_string (get "sender_lookup_table" c)
    ; rcpt_lookup_table = Value.to_string (get "recipient_lookup_table" c)
    ; sender_lookup_key_fmt = Value.to_string (get "sender_lookup_key_format" c)
    ; rcpt_lookup_key_fmt = Value.to_string (get "recipient_lookup_key_format" c)
    ; local_sender_regexp = Value.to_regexp (get "local_sender_regexp" c)
    ; local_rcpt_regexp = Value.to_regexp (get "local_recipient_regexp" c)
    ; query_fmt = Value.to_string (get "query_format" c)
    ; query_flags  = Value.to_int (get "query_flags" c)
    ; query_socket = Value.to_string (get "query_socket" c)
    ; sender_query_max_depth = Value.to_int (get "sender_query_max_depth" c)
    ; rcpt_query_max_depth = Value.to_int (get "recipient_query_max_depth" c)
    ; sender_query_max_results = Value.to_int (get "sender_query_max_results" c)
    ; rcpt_query_max_results = Value.to_int (get "recipient_query_max_results" c)
    ; result_fmt = Value.to_string (get "result_format" c)
    ; result_value_separator = Value.to_regexp (get "result_value_separator" c)
    } in
  let srs_config =
    let get = find_srs in
    { secret_file        = Value.to_string (get "secret_file" c)
    ; secrets_directory  = Value.to_string (get "secrets_directory" c)
    ; hash_max_age       = Value.to_int (get "hash_max_age" c)
    ; hash_length        = Value.to_int (get "hash_length" c)
    ; separator          = (Value.to_string (get "separator" c)).[0]
    ; secret_length      = Value.to_int (get "secret_length" c)
    } in
  { srslyd   = srslyd_config
  ; milter   = milter_config
  ; proxymap = proxymap_config
  ; srs      = srs_config
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

let milter_config_path () =
  (current ()).milter.config_path

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

let proxymap_sender_query_max_depth () =
  (current ()).proxymap.sender_query_max_depth

let proxymap_recipient_query_max_depth () =
  (current ()).proxymap.rcpt_query_max_depth

let proxymap_sender_query_max_results () =
  (current ()).proxymap.sender_query_max_results

let proxymap_recipient_query_max_results () =
  (current ()).proxymap.rcpt_query_max_results

let proxymap_result_format () =
  (current ()).proxymap.result_fmt

let proxymap_result_value_separator () =
  (current ()).proxymap.result_value_separator

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
