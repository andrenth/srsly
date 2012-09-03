type ops =
  { is_remote_sender         : (string -> bool Lwt.t)
  ; remote_final_rcpt_counts : (string list -> (string * int) list Lwt.t)
  }

val init : ops -> unit
val connect : Milter.ctx -> string option -> Unix.sockaddr option -> Milter.stat
val helo : Milter.ctx -> string option -> Milter.stat
val envfrom : Milter.ctx -> string -> string list -> Milter.stat
val envrcpt : Milter.ctx -> string -> string list -> Milter.stat
val eom : Milter.ctx -> Milter.stat
val abort : Milter.ctx -> Milter.stat
val close : Milter.ctx -> Milter.stat

val negotiate : Milter.ctx
             -> Milter.flag list
             -> Milter.step list
             -> Milter.stat * Milter.flag list * Milter.step list
