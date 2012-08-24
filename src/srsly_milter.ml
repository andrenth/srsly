open Ipc.Slave_types
open Lwt
open Printf
open Util

let ipc_error = function
  | `EOF -> Lwt_log.error "EOF on IPC socket" >> exit 1
  | `Timeout -> Lwt_log.error "timeout on IPC socket" >> exit 1

let read_configuration fd =
  let handle_ipc = function
    | `Response (Configuration c) ->
        lwt () = Lwt_log.notice "received a new configuration; replacing" in
        Config.replace c;
        set_log_level (Config.log_level ());
        Milter.setdbg (Config.milter_debug_level ());
        return ()
    | `EOF | `Timeout as e ->
        ipc_error e
    | _ ->
        fail_lwt "unexpected response while waiting for configuration" in
  Ipc.Slave.make_request fd Configuration_request handle_ipc

let read_srs_secrets fd =
  let handle_ipc = function
    | `Response (SRS_secrets ss) ->
        lwt () = Lwt_log.notice "received new SRS secrets; reloading" in
        Milter_srs.reload ss;
        return ()
    | `EOF | `Timeout as e ->
        ipc_error e
    | _ ->
        fail_lwt "unexpected response while waiting for SRS secrets" in
  Ipc.Slave.make_request fd SRS_secrets_request handle_ipc

let proxymap_is_remote_addr fd =
  let handle_ipc = function
    | `Response (Proxymap_response r) ->
        lwt () = Lwt_log.debug_f "received proxymap response: %b" r in
        return r
    | `EOF | `Timeout as e ->
        ipc_error e
    | _ ->
        fail_lwt "unexpected response while waiting for proxymap response" in
  (fun addr -> Ipc.Slave.make_request fd (Proxymap_query addr) handle_ipc)

let handle_sighup fd _ =
  Lwt.async
    (fun () ->
      lwt () = Lwt_log.info "got SIGHUP, asking for configuration" in
      read_configuration fd)

let handle_sigusr1 fd _ =
  Lwt.async
    (fun () ->
      lwt () = Lwt_log.info "got SIGUSR1, asking for SRS secrets" in
      read_srs_secrets fd)

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
  Lwt_preemptive.detach Milter.main ()

let () =
  if Array.length Sys.argv > 1 then
    Config.file := Some Sys.argv.(1);
  set_log_level (Config.log_level ());
  Release.me ~syslog:true ~user:(Config.milter_user ()) ~main:main ()
