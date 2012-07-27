type t

val current : unit -> t
val milter : unit -> Milter_config.t
val policyd : unit -> Policyd_config.t
val reload : unit -> unit
val replace : t -> unit
val is_milter : unit -> bool
val serialize : t -> string
val unserialize : string -> int -> t
val user : unit -> string
val binary_path : unit -> string
val log_level : unit -> Lwt_log.level
val local_whitelist : unit -> Network.t list
val relay_whitelist : unit -> Network.t list
val fail_on_helo_temperror : unit -> bool
val background : unit -> bool
