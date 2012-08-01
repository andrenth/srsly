open Lwt
open Printf
open Util

let filter =
  { Milter.name      = "spf_milter_in"
  ; Milter.version   = Milter.version_code
  ; Milter.flags     = [Milter.ADDHDRS; Milter.DELRCPT; Milter.ADDRCPT]
  ; Milter.connect   = Some Milter_in_callbacks.connect
  ; Milter.helo      = Some Milter_in_callbacks.helo
  ; Milter.envfrom   = Some Milter_in_callbacks.envfrom
  ; Milter.envrcpt   = Some Milter_in_callbacks.envrcpt
  ; Milter.header    = None
  ; Milter.eoh       = None
  ; Milter.body      = None
  ; Milter.eom       = Some Milter_in_callbacks.eom
  ; Milter.abort     = Some Milter_in_callbacks.abort
  ; Milter.close     = Some Milter_in_callbacks.close
  ; Milter.unknown   = None
  ; Milter.data      = None
  ; Milter.negotiate = Some Milter_in_callbacks.negotiate
  }

let () =
  if Array.length Sys.argv > 1 then
    Config.file := Sys.argv.(1);
  let main = Milter_util.main filter (Config.listen_address_in ()) in
  set_log_level (Config.log_level ());
  Release.me ~syslog:true ~user:(Config.user ()) ~main:main ()
