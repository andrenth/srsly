open Lwt
open Printf

let set_log_level level =
  Lwt_log.Section.set_level Lwt_log.Section.main level

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

let main fd =
  lwt () = Lwt_log.notice "starting up" in
  Milter.setdbg (Milter_config.debug_level (Config.milter ()));
  Milter.setconn (Milter_config.listen_address_in (Config.milter ()));
  Milter.register filter;
  Milter.main ();
  return ()

let () =
  set_log_level (Config.log_level ());
  Release.me ~syslog:false ~user:(Config.user ()) ~main:main ()
