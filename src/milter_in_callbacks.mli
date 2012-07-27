open Milter_util

val connect : Milter.ctx -> string option -> Unix.sockaddr option -> Milter.stat
val helo : Milter.ctx -> string option -> Milter.stat
val envfrom : Milter.ctx -> string -> string list -> Milter.stat
val envrcpt : Milter.ctx -> string -> string list -> Milter.stat
val eom : Milter.ctx -> Milter.stat
val abort : Milter.ctx -> Milter.stat
val close : Milter.ctx -> Milter.stat

val negotiate : Milter.ctx
             -> FlagSet.elt list
             -> StepSet.elt list
             -> Milter.stat * Milter.flag list * StepSet.elt list
