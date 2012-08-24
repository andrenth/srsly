open Lwt
open Printf
open Ipc.Control_types
open Ipc.Slave_types

module O = Release_option

let slave_connections = ref (fun () -> [])

let sighup = 1
let sigusr1 = 10
let sigterm = 15

let signal_slaves signum =
  Lwt_list.iter_p
    (fun (pid, _) ->
      Unix.kill pid signum;
      return ())
    (!slave_connections ())

let no_config_warning () =
  Lwt_log.warning
    ("I was started with no configuration file; " ^
     "use the `srsly reload` command to give me one")

let handle_sighup _ =
  ignore_result (Lwt_log.info "got SIGHUP, reloading configuration");
  Lwt.async
    (fun () ->
      lwt () = O.either no_config_warning Config.load (Config.file ()) in
      signal_slaves sighup)

let rec handle_sigusr1 _ =
  Lwt.async
    (fun () ->
      lwt () = Lwt_log.info "got SIGUSR1, reloading SRS secrets" in
      signal_slaves sigusr1)

let slave_ipc_handler fd =
  lwt () = Lwt_log.debug "received IPC request from slave" in
  let handler = function
    | Configuration_request ->
        lwt () = Lwt_log.info "sending configuration to slave" in
        return (Configuration (Config.current ()))
    | Proxymap_query addr ->
        lwt () = Lwt_log.debug_f "proxymap query request for %s" addr in
        lwt res = Proxymap.is_remote addr in
        return (Proxymap_response res)
    | SRS_secrets_request ->
        lwt () = Lwt_log.info "sending SRS secrets to slave" in
        return (SRS_secrets (Srs_util.read_srs_secrets ())) in
  Ipc.Slave.handle_request fd handler

let control_connection_handler fd =
  let handler = function
    | Reload_config file ->
        let reload file =
          lwt () = Lwt_log.notice_f "reloading configuration at %s" file in
          lwt () = Config.load file in
          signal_slaves sighup in
        let config_file = O.choose file (Config.file ()) in
        lwt () = O.either no_config_warning reload config_file in
        return Reloaded_config
    | Reload_secrets ->
        lwt () = Lwt_log.notice "reloading SRS secrets" in
        lwt () = signal_slaves sigusr1 in
        return Reloaded_secrets
    | Stop ->
        lwt () = Lwt_log.notice "received stop command" in
        (* Suicide. Release will catch SIGTERM and kill the slaves *)
        Unix.kill (Unix.getpid ()) sigterm;
        lwt () = Lwt_unix.sleep 1.0 in (* give the signal handler time to run *)
        raise_lwt (Failure "I'm already dead!") in
  Ipc.Control.handle_request ~eof_warning:false ~timeout:5. fd handler

let main get_conns =
  slave_connections := get_conns;
  ignore (Lwt_unix.on_signal Sys.sighup handle_sighup);
  ignore (Lwt_unix.on_signal Sys.sigusr1 handle_sigusr1);
  return ()

let () =
  ignore_result (Lwt_log.notice "starting up");
  let config_file =
    if Array.length Sys.argv > 1 then Some Sys.argv.(1)
    else None in
  let config_t =
    O.either Config.load_defaults Config.load config_file in
  Lwt_main.run config_t;
  let slave = (Config.milter_executable (), slave_ipc_handler) in
  Release.master_slave
    ~background:(Config.background ())
    ~lock_file:(Config.lock_file ())
    ~control:(Config.control_socket (), control_connection_handler)
    ~main:main
    ~slave:slave
    ()
