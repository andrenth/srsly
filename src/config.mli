type t

val file : Lwt_io.file_name ref
val default_config_file : string
val current : unit -> t
val reload : unit -> unit
val replace : t -> unit
val serialize : t -> string
val unserialize : string -> int -> t
val lock_file : unit -> Lwt_io.file_name
val control_socket : unit -> string
val log_level : unit -> Lwt_log.level
val local_whitelist : unit -> Network.t list
val relay_whitelist : unit -> Network.t list
val fail_on_helo_temperror : unit -> bool
val background : unit -> bool
val random_device : unit -> Lwt_io.file_name

val milter_user : unit -> string
val milter_input_executable : unit -> string
val milter_output_executable : unit -> string
val milter_input_listen_address : unit -> string
val milter_output_listen_address : unit -> string
val milter_debug_level : unit -> int

val srs_domain : unit -> string option
val srs_secret_file : unit -> Lwt_io.file_name
val srs_hash_max_age : unit -> int
val srs_hash_length : unit -> int
val srs_separator : unit -> char
val srs_secret_length : unit -> int
