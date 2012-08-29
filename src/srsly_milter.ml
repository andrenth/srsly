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

let join_counts = function
  | [] ->
      "none"
  | (d, c)::rest ->
      List.fold_left
        (fun acc (d, c) -> sprintf "%s, %s:%d" acc d c)
        (sprintf "%s:%d" d c)
        rest

let proxymap_is_remote_sender fd =
  let handle_ipc = function
    | `Response (Remote_sender_check b) ->
        let s = if b then "remote" else "local" in
        lwt () = Lwt_log.debug_f "received proxymap response: sender is %s" s in
        return b
    | `EOF | `Timeout as e ->
        ipc_error e
    | _ ->
        fail_lwt "unexpected response while waiting for remote sender check" in
  (fun sender ->
    Ipc.Slave.make_request fd (Check_remote_sender sender) handle_ipc)

let proxymap_count_remote_final_rcpts fd =
  let handle_ipc = function
    | `Response (Remote_final_rcpts_count c) ->
        lwt () =
          Lwt_log.debug_f "received proxymap final destination counts: %s"
            (join_counts c) in
        return c
    | `EOF | `Timeout as e ->
        ipc_error e
    | _ ->
        fail_lwt "unexpected response while waiting for final rcpt counts" in
  (fun rcpts ->
    Ipc.Slave.make_request fd (Count_remote_final_rcpts rcpts) handle_ipc)

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
  let module C = Milter_callbacks in
  let callback_ops =
    { C.count_remote_final_rcpts = proxymap_count_remote_final_rcpts fd
    ; C.is_remote_sender         = proxymap_is_remote_sender fd
    } in
  C.init callback_ops;
  Milter.setdbg (Config.milter_debug_level ());
  Milter.setconn (Config.milter_listen_address ());
  Milter.register filter;
  Lwt_preemptive.detach Milter.main ()

let () =
  let config_t =
    if Array.length Sys.argv > 1 then
      Config.load Sys.argv.(1)
    else
      Config.load_defaults () in
  Lwt_main.run config_t;
  set_log_level (Config.log_level ());
  Release.me ~syslog:true ~user:(Config.milter_user ()) ~main:main ()
