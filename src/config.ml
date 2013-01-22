open Lwt
open Printf
open Release_config_values
open Release_config_validations

open Log_lwt
open Util

module O = Release_util.Option

module type Spec = sig
  type config
  val spec : Release_config.spec
  val make : Release_config.t -> config
end

module type S = sig
  type t
  val file : unit -> Lwt_io.file_name option
  val load : Lwt_io.file_name -> unit Lwt.t
  val load_defaults : unit -> unit Lwt.t
  val current : unit -> t
  val replace : t -> unit
end

module Make (S : Spec) : S with type t = S.config = struct
  type t = S.config

  let config_file = ref None
  let configuration = ref None

  let file () =
    !config_file

  let load file =
    let err fmt =
      ksprintf (fun s -> lwt () = error "%s" s in exit 1) fmt in
    try_lwt
      lwt st = Lwt_unix.lstat file in
      if st.Lwt_unix.st_kind = Lwt_unix.S_REG then
        match_lwt Release_config.parse file S.spec with
        | `Configuration conf ->
            config_file := Some file;
            configuration := Some (S.make conf);
            return_unit
        | `Error e ->
            err "%s" e
      else
        err "%s: not a regular file" file
    with
    | Unix.Unix_error (e, _, _) ->
        err "%s: %s" file (Unix.error_message e)
    | Network.Network_error e ->
        err "%s: %s" file e

  let load_defaults () =
    configuration := Some (S.make (Release_config.defaults S.spec));
    return_unit

  let current () =
    match !configuration with
    | None -> fprintf stderr "no config!"; exit 1
    | Some c -> c

  let replace c =
    configuration := Some c
end
