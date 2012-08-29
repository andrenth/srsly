module Lwt : sig
  val debug : ('a, unit, string, unit Lwt.t) format4 -> 'a
  val notice : ('a, unit, string, unit Lwt.t) format4 -> 'a
  val info : ('a, unit, string, unit Lwt.t) format4 -> 'a
  val warning : ('a, unit, string, unit Lwt.t) format4 -> 'a
  val error : ('a, unit, string, unit Lwt.t) format4 -> 'a
end

module Preemptive : sig
  val debug : ('a, unit, string, unit) format4 -> 'a
  val notice : ('a, unit, string, unit) format4 -> 'a
  val info : ('a, unit, string, unit) format4 -> 'a
  val warning : ('a, unit, string, unit) format4 -> 'a
  val error : ('a, unit, string, unit) format4 -> 'a
end
