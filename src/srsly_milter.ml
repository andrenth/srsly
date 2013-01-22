open Lwt
open Printf

open Ipc.Slave_types
open Log_lwt
open Util

module S_conf = Srslyd_config
module M_conf = Milter_config

let instance_name = ref ""

let ipc_error = function
  | `EOF -> error "EOF on IPC socket" >> exit 1
  | `Timeout -> error "timeout on IPC socket" >> exit 1

let read_configuration fd =
  let handle_ipc = function
    | `Response (Configuration (sc, mc)) ->
        lwt () = notice "received a new configuration; replacing" in
        S_conf.replace sc;
        M_conf.replace mc;
        set_log_level (S_conf.srslyd_log_level ());
        Milter.setdbg (M_conf.milter_debug_level ());
        return ()
    | `EOF | `Timeout as e ->
        ipc_error e
    | _ ->
        fail_lwt "unexpected response while waiting for configuration" in
  Ipc.Slave.make_request fd (Configuration_request !instance_name) handle_ipc

let read_srs_secrets fd =
  let handle_ipc = function
    | `Response (SRS_secrets ss) ->
        lwt () =
          notice "instance %s received new SRS secrets; reloading"
            !instance_name in
        Milter_srs.reload ss;
        return_unit
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
        lwt () = debug "received proxymap response: sender is %s" s in
        return b
    | `EOF | `Timeout as e ->
        ipc_error e
    | _ ->
        fail_lwt "unexpected response while waiting for remote sender check" in
  (fun sender ->
    Ipc.Slave.make_request fd (Check_remote_sender sender) handle_ipc)

let proxymap_choose_forward_domain fd =
  let handle_ipc = function
    | `Response (SRS_forward_domain d) ->
        lwt () = debug "received proxymap SRS forward domain: %s"
          (match d with None -> "none" | Some x -> x) in
        return d
    | `EOF | `Timeout as e ->
        ipc_error e
    | _ ->
        fail_lwt "unexpected response while waiting for SRS forward domain" in
  (fun rcpts ->
    Ipc.Slave.make_request fd (Choose_srs_forward_domain rcpts) handle_ipc)

let handle_sighup fd _ =
  Lwt.async
    (fun () ->
      lwt () = info "got SIGHUP, asking for configuration" in
      read_configuration fd)

let handle_sigusr1 fd _ =
  Lwt.async
    (fun () ->
      lwt () = info "got SIGUSR1, asking for SRS secrets" in
      read_srs_secrets fd)

let flags =
  [ Milter.ADDHDRS
  ; Milter.DELRCPT
  ; Milter.ADDRCPT
  ; Milter.CHGFROM
  ]

let filter =
  { Milter.empty with
    Milter.name      = "srsly-milter (" ^ !instance_name ^ ")"
  ; Milter.flags     = flags
  ; Milter.connect   = Some Milter_callbacks.connect
  ; Milter.helo      = Some Milter_callbacks.helo
  ; Milter.envfrom   = Some Milter_callbacks.envfrom
  ; Milter.envrcpt   = Some Milter_callbacks.envrcpt
  ; Milter.eom       = Some Milter_callbacks.eom
  ; Milter.abort     = Some Milter_callbacks.abort
  ; Milter.close     = Some Milter_callbacks.close
  ; Milter.negotiate = Some Milter_callbacks.negotiate
  }

let main fd =
  lwt () = notice "running instance %s" !instance_name in
  ignore (Lwt_unix.on_signal Sys.sighup (handle_sighup fd));
  ignore (Lwt_unix.on_signal Sys.sigusr1 (handle_sigusr1 fd));
  lwt () = read_srs_secrets fd in
  let module C = Milter_callbacks in
  let callback_ops =
    { C.choose_forward_domain = proxymap_choose_forward_domain fd
    ; C.is_remote_sender      = proxymap_is_remote_sender fd
    } in
  C.init callback_ops;
  Milter.setdbg (M_conf.milter_debug_level ());
  Milter.setconn (M_conf.milter_listen_address ());
  Milter.register filter;
  Lwt_preemptive.detach Milter.main ()

let set_instance_name config_path =
  let module F = Filename in
  if F.check_suffix config_path ".conf" then
    instance_name := F.chop_suffix (F.basename config_path) ".conf"
  else begin
    ignore_result (error "invalid configuration file name: %s" config_path);
    exit 1
  end

let () =
  let config_t =
    if Array.length Sys.argv = 3 then begin
      let srslyd_config = Sys.argv.(1) in
      let milter_config = Sys.argv.(2) in
      lwt () = S_conf.load srslyd_config in
      lwt () = M_conf.load milter_config in
      set_instance_name milter_config;
      return_unit
    end else
      lwt () = notice "setting configuration parameters to default values" in
      lwt () = S_conf.load_defaults () in
      lwt () = M_conf.load_defaults () in
      return_unit in
  Lwt_main.run config_t;
  set_log_level (S_conf.srslyd_log_level ());
  Release.me
    ~syslog:(S_conf.srslyd_background ())
    ~user:(S_conf.milter_user ())
    ~main:main ()
