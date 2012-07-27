open Lwt
open Printf
open Util

let filter =
  { Milter.name      = "spf_milter_out"
  ; Milter.version   = Milter.version_code
  ; Milter.flags     = [Milter.CHGFROM]
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

let () =
  let main =
    Milter_util.main
      filter
      (Milter_config.listen_address_out (Config.milter ())) in
  set_log_level (Config.log_level ());
  Release.me ~syslog:false ~user:(Config.user ()) ~main:main ()
