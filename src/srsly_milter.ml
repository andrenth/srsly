open Lwt
open Printf
open Util

let filter =
  { Milter.name      = "srsly-milter"
  ; Milter.version   = Milter.version_code
  ; Milter.flags     = [Milter.ADDHDRS; Milter.DELRCPT; Milter.ADDRCPT]
  ; Milter.connect   = Some Milter_callbacks.connect
  ; Milter.helo      = Some Milter_callbacks.helo
  ; Milter.envfrom   = Some Milter_callbacks.envfrom
  ; Milter.envrcpt   = Some Milter_callbacks.envrcpt
  ; Milter.header    = None
  ; Milter.eoh       = None
  ; Milter.body      = None
  ; Milter.eom       = Some Milter_callbacks.eom
  ; Milter.abort     = Some Milter_callbacks.abort
  ; Milter.close     = Some Milter_callbacks.close
  ; Milter.unknown   = None
  ; Milter.data      = None
  ; Milter.negotiate = Some Milter_callbacks.negotiate
  }

let () =
  if Array.length Sys.argv > 1 then
    Config.file := Sys.argv.(1);
  let main = Milter_util.main filter (Config.milter_listen_address ()) in
  set_log_level (Config.log_level ());
  Release.me ~syslog:true ~user:(Config.milter_user ()) ~main:main ()
