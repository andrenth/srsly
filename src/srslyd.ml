open Lwt
open Printf

open Ipc.Control_types
open Ipc.Slave_types
open Log_lwt
open Util

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
  warning "no configuration file given; try `srsly reload` to set one"

let handle_sighup _ =
  ignore_result (info "got SIGHUP, reloading configuration");
  Lwt.async
    (fun () ->
      lwt () = O.either no_config_warning Config.load (Config.file ()) in
      signal_slaves sighup)

let rec handle_sigusr1 _ =
  Lwt.async
    (fun () ->
      lwt () = info "got SIGUSR1, reloading SRS secrets" in
      signal_slaves sigusr1)

let slave_ipc_handler fd =
  lwt () = debug "received IPC request from slave" in
  let handler = function
    | Configuration_request ->
        lwt () = info "sending configuration to slave" in
        return (Configuration (Config.current ()))
    | Check_remote_sender s ->
        lwt () = debug "proxymap remote sender check for '%s'" s in
        lwt r = Proxymap.is_remote_sender s in
        return (Remote_sender_check r)
    | Count_remote_final_rcpts rcpts ->
        lwt () =
          debug "proxymap final destination count request for %s"
            (join_strings ", " rcpts) in
        lwt dests = Proxymap.count_remote_final_rcpts rcpts in
        return (Remote_final_rcpts_count dests)
    | SRS_secrets_request ->
        lwt () = info "sending SRS secrets to slave" in
        lwt secrets = Srs_util.read_srs_secrets () in
        return (SRS_secrets secrets) in
  Ipc.Slave.handle_request fd handler

let control_connection_handler fd =
  let handler = function
    | Reload_config file ->
        let reload file =
          lwt () = notice "reloading configuration at %s" file in
          lwt () = Config.load file in
          signal_slaves sighup in
        let config_file = O.choose file (Config.file ()) in
        lwt () = O.either no_config_warning reload config_file in
        return Reloaded_config
    | Reload_secrets ->
        lwt () = notice "reloading SRS secrets" in
        lwt () = signal_slaves sigusr1 in
        return Reloaded_secrets
    | Stop ->
        lwt () = notice "received stop command" in
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
  ignore_result (notice "starting up");
  let config_file =
    if Array.length Sys.argv > 1 then Some Sys.argv.(1)
    else None in
  let config_t =
    O.either Config.load_defaults Config.load config_file in
  Lwt_main.run config_t;
  set_log_level (Config.srslyd_log_level ());
  let slave = (Config.milter_executable (), slave_ipc_handler) in
  let background = Config.srslyd_background () in
  Release.master_slave
    ~syslog:background
    ~background:background
    ~lock_file:(Config.srslyd_lock_file ())
    ~control:(Config.srslyd_control_socket (), control_connection_handler)
    ~main:main
    ~slave:slave
    ()
