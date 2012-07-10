open Lwt
open Printf

(* TODO configuration file *)

let context_mtx = Lwt_mutex.create ()

let set_log_level level =
  Lwt_log.Section.set_level Lwt_log.Section.main level

let filter =
  { Milter.name      = "spfd"
  ; Milter.version   = Milter.version_code
  ; Milter.flags     = [Milter.ADDHDRS]
  ; Milter.connect   = Some Milter_callbacks.connect
  ; Milter.helo      = Some Milter_callbacks.helo
  ; Milter.envfrom   = Some Milter_callbacks.envfrom
  ; Milter.envrcpt   = None
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

let config = Config.default

let main fd =
  lwt () = Lwt_log.notice "starting up" in
  Milter.setconn config.Config.listen_address;
  Milter.register filter;
  Milter.main ();
  return ()

let () =
  (* TODO let config = read_config_file "/etc/spfd.conf" in *)
  set_log_level config.Config.log_level;
  Release.me ~syslog:false ~user:config.Config.user ~main:main ()
