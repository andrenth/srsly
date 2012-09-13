type ops =
  { is_remote_sender      : (string -> bool Lwt.t)
  ; choose_forward_domain : (string list -> string option Lwt.t)
  }

val init : ops -> unit
val connect : Milter.ctx -> string option -> Unix.sockaddr option -> Milter.stat
val helo : Milter.ctx -> string -> Milter.stat
val envfrom : Milter.ctx -> string -> string list -> Milter.stat
val envrcpt : Milter.ctx -> string -> string list -> Milter.stat
val eom : Milter.ctx -> Milter.stat
val abort : Milter.ctx -> Milter.stat
val close : Milter.ctx -> Milter.stat

val negotiate : Milter.ctx
             -> Milter.flag list
             -> Milter.step list
             -> Milter.stat * Milter.flag list * Milter.step list
