type t

val file : unit -> Lwt_io.file_name option
val load : Lwt_io.file_name -> unit Lwt.t
val load_defaults : unit -> unit Lwt.t
val current : unit -> t
val replace : t -> unit

val milter_listen_address : unit -> string
val milter_debug_level : unit -> int

val spf_enable : unit -> bool
val spf_local_whitelist : unit -> Network.t list
val spf_relay_whitelist : unit -> Network.t list
val spf_fail_on_helo_temperror : unit -> bool
val spf_result_headers : unit -> string list

val srs_always_rewrite : unit -> bool
