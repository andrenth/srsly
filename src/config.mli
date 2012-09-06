type t

val file : unit -> Lwt_io.file_name option
val load : Lwt_io.file_name -> unit Lwt.t
val load_defaults : unit -> unit Lwt.t
val current : unit -> t
val replace : t -> unit
val serialize : t -> string
val unserialize : string -> t

val srslyd_lock_file : unit -> Lwt_io.file_name
val srslyd_control_socket : unit -> string
val srslyd_log_level : unit -> Lwt_log.level
val srslyd_background : unit -> bool
val srslyd_random_device : unit -> Lwt_io.file_name

val milter_user : unit -> string
val milter_executable : unit -> string
val milter_listen_address : unit -> string
val milter_debug_level : unit -> int

val proxymap_sender_lookup_table : unit -> string
val proxymap_recipient_lookup_table : unit -> string
val proxymap_sender_lookup_key_format : unit -> string
val proxymap_recipient_lookup_key_format : unit -> string
val proxymap_local_sender_regexp : unit -> Str.regexp
val proxymap_local_recipient_regexp : unit -> Str.regexp
val proxymap_query_format : unit -> string
val proxymap_query_flags : unit -> int
val proxymap_query_socket : unit -> Lwt_io.file_name
val proxymap_sender_query_max_depth : unit -> int
val proxymap_recipient_query_max_depth : unit -> int
val proxymap_sender_query_max_results : unit -> int
val proxymap_recipient_query_max_results : unit -> int
val proxymap_result_format : unit -> string
val proxymap_result_value_separator : unit -> Str.regexp

val spf_local_whitelist : unit -> Network.t list
val spf_relay_whitelist : unit -> Network.t list
val spf_fail_on_helo_temperror : unit -> bool
val spf_result_headers : unit -> string list

val srs_secret_file : unit -> Lwt_io.file_name
val srs_secrets_directory : unit -> Lwt_io.file_name
val srs_hash_max_age : unit -> int
val srs_hash_length : unit -> int
val srs_separator : unit -> char
val srs_secret_length : unit -> int
