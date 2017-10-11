open Release_lwt

module type Spec = sig
  type config
  val spec : Release.Config.spec
  val make : Release.Config.t -> config
end

module type S = sig
  type t
  val file : unit -> Lwt_io.file_name option
  val load : Lwt_io.file_name -> unit Lwt.t
  val load_defaults : unit -> unit Lwt.t
  val current : unit -> t
  val replace : t -> unit
end

module Make (S : Spec) : S with type t = S.config
