open Lwt
open Printf
open Util
open Milter_util

let flags =
  [ Milter.ADDHDRS
  ; Milter.DELRCPT
  ; Milter.ADDRCPT
  ; Milter.CHGFROM
  ]

let filter =
  { Milter.name      = "srsly-milter"
  ; Milter.version   = Milter.version_code
  ; Milter.flags     = flags
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

let main fd =
  lwt () = Lwt_log.notice "starting up" in
  ignore (Lwt_unix.on_signal Sys.sighup (handle_sighup fd));
  ignore (Lwt_unix.on_signal Sys.sigusr1 (handle_sigusr1 fd));
  lwt () = read_srs_secrets fd in
  Milter_callbacks.init (proxymap_is_remote_addr fd);
  Milter.setdbg (Config.milter_debug_level ());
  Milter.setconn (Config.milter_listen_address ());
  Milter.register filter;
  let milter_t = Lwt_preemptive.detach Milter.main () in
  Lwt.join [milter_t]

let () =
  if Array.length Sys.argv > 1 then
    Config.file := Sys.argv.(1);
  set_log_level (Config.log_level ());
  Release.me ~syslog:true ~user:(Config.milter_user ()) ~main:main ()
