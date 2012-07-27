open Lwt
open Printf

let set_log_level level =
  Lwt_log.Section.set_level Lwt_log.Section.main level

let filter =
  { Milter.name      = "spf_milter_out"
  ; Milter.version   = Milter.version_code
  ; Milter.flags     = [Milter.ADDHDRS]
  ; Milter.connect   = Some Milter_out_callbacks.connect
  ; Milter.helo      = None
  ; Milter.envfrom   = Some Milter_out_callbacks.envfrom
  ; Milter.envrcpt   = None
  ; Milter.header    = None
  ; Milter.eoh       = None
  ; Milter.body      = None
  ; Milter.eom       = Some Milter_out_callbacks.eom
  ; Milter.abort     = Some Milter_out_callbacks.abort
  ; Milter.close     = Some Milter_out_callbacks.close
  ; Milter.unknown   = None
  ; Milter.data      = None
  ; Milter.negotiate = Some Milter_out_callbacks.negotiate
  }

let main fd =
  lwt () = Lwt_log.notice "starting up" in
  Milter.setdbg (Milter_config.debug_level (Config.milter ()));
  Milter.setconn (Milter_config.listen_address_out (Config.milter ()));
  Milter.register filter;
  Milter.main ();
  return ()

let () =
  set_log_level (Config.log_level ());
  Release.me ~syslog:false ~user:(Config.user ()) ~main:main ()
