open Lwt
open Printf
open Release_lwt

open Ipc.Slave_types
open Log_lwt
open Util

module S_conf = Srslyd_config
module M_conf = Milter_config

let instance_name = ref ""

(* In theory these locks shouldn't be necessary since even though libmilter
 * is multi-threaded, OCaml will not run pthreads concurrently. *)
let ipc_lock = Lwt_mutex.create ()

let ipc_error = function
  | `EOF -> error "EOF on IPC socket" >>= fun () -> exit 1
  | `Timeout -> error "timeout on IPC socket" >>= fun () -> exit 1

let read_configuration fd =
  Lwt_mutex.with_lock ipc_lock
    (fun () ->
      let handle_ipc = function
        | `Response (Configuration (sc, mc)) ->
            notice "received a new configuration; replacing" >>= fun () ->
            S_conf.replace sc;
            M_conf.replace mc;
            set_log_level (S_conf.srslyd_log_level ());
            Milter.setdbg (M_conf.milter_debug_level ());
            return ()
        | `EOF | `Timeout as e ->
            ipc_error e
        | _ ->
            fail_lwt "unexpected response while waiting for configuration" in
      let req = Configuration_request !instance_name in
      Ipc.Slave.Client.make_request fd req handle_ipc)

let read_srs_secrets fd =
  Lwt_mutex.with_lock ipc_lock
    (fun () ->
      let handle_ipc = function
        | `Response (SRS_secrets ss) ->
            notice "instance %s received new SRS secrets; reloading"
              !instance_name >>= fun () ->
            Milter_srs.reload ss;
            return_unit
        | `EOF | `Timeout as e ->
            ipc_error e
        | _ ->
            fail_lwt "unexpected response while waiting for SRS secrets" in
      Ipc.Slave.Client.make_request fd SRS_secrets_request handle_ipc)

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
        debug "received proxymap response: sender is %s" s >>= fun () ->
        return b
    | `EOF | `Timeout as e ->
        ipc_error e
    | _ ->
        fail_lwt "unexpected response while waiting for remote sender check" in
  (fun sender ->
    let req = Check_remote_sender sender in
    Lwt_mutex.with_lock ipc_lock
      (fun () -> Ipc.Slave.Client.make_request fd req handle_ipc))

let proxymap_choose_forward_domain fd =
  let handle_ipc = function
    | `Response (SRS_forward_domain d) ->
        debug "received proxymap SRS forward domain: %s"
          (match d with None -> "none" | Some x -> x) >>= fun () ->
        return d
    | `EOF | `Timeout as e ->
        ipc_error e
    | _ ->
        fail_lwt "unexpected response while waiting for SRS forward domain" in
  (fun rcpts ->
    let req = Choose_srs_forward_domain rcpts in
    Lwt_mutex.with_lock ipc_lock
      (fun () -> Ipc.Slave.Client.make_request fd req handle_ipc))

let handle_sighup fd _ =
  Lwt.async
    (fun () ->
      notice "got SIGHUP, asking for configuration" >>= fun () ->
      read_configuration fd)

let handle_sigusr1 fd _ =
  Lwt.async
    (fun () ->
      notice "got SIGUSR1, asking for SRS secrets" >>= fun () ->
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
  notice "running instance %s" !instance_name >>= fun () ->
  ignore (Lwt_unix.on_signal Sys.sighup (handle_sighup fd));
  ignore (Lwt_unix.on_signal Sys.sigusr1 (handle_sigusr1 fd));
  read_srs_secrets fd >>= fun () ->
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
      S_conf.load srslyd_config >>= fun () ->
      M_conf.load milter_config >>= fun () ->
      set_instance_name milter_config;
      return_unit
    end else
      notice "setting configuration parameters to default values" >>= fun () ->
      S_conf.load_defaults () >>= fun () ->
      M_conf.load_defaults () >>= fun () ->
      return_unit in
  Lwt_main.run config_t;
  set_log_level (S_conf.srslyd_log_level ());
  Release.me
    ~logger:Future.Logger.syslog
    ~user:(S_conf.milter_user ())
    ~main:main ()
