open Lwt
open Printf

open Ipc.Control_types
open Ipc.Slave_types
open Log_lwt
open Util

module O = Release_util.Option
module C = Srslyd_config

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

let warn_no_config () =
  warning "no configuration file given; try `srsly reload` to set one"

let reload_config file =
  lwt () = C.load file in
  set_log_level (C.srslyd_log_level ());
  return_unit

let handle_sighup _ =
  ignore_result (info "got SIGHUP, reloading configuration");
  Lwt.async
    (fun () ->
      lwt () = O.either warn_no_config reload_config (C.file ()) in
      signal_slaves sighup)

let rec handle_sigusr1 _ =
  Lwt.async
    (fun () ->
      lwt () = info "got SIGUSR1, reloading SRS secrets" in
      signal_slaves sigusr1)

let instance_config_file instance =
  sprintf "%s/%s.conf" (C.milter_config_path ()) instance

let slave_ipc_handler fd =
  lwt () = debug "received IPC request from slave" in
  let handler = function
    | Configuration_request instance ->
        lwt () = info "sending configuration to instance %s" instance in
        lwt () = Milter_config.load (instance_config_file instance) in
        return (Configuration (C.current (), Milter_config.current ()))
    | Check_remote_sender s ->
        lwt () = debug "proxymap remote sender check for '%s'" s in
        lwt r = Proxymap.is_remote_sender s in
        return (Remote_sender_check r)
    | Choose_srs_forward_domain rcpts ->
        lwt () = debug "proxymap final destination count request" in
        lwt fwd = Proxymap.choose_forward_domain rcpts in
        return (SRS_forward_domain fwd)
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
          lwt () = reload_config file in
          signal_slaves sighup in
        let config_file = O.choose file (C.file ()) in
        lwt () = O.either warn_no_config reload config_file in
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

let filter_dir f dir =
  let entries = Lwt_unix.files_of_directory dir in
  Lwt_stream.fold
    (fun x acc -> if f x then (dir ^ "/" ^ x)::acc else acc)
    entries
    []

let milter_config_files () =
  let dir = C.milter_config_path () in
  lwt files =
    filter_dir
      (fun e -> e.[0] <> '.' && Filename.check_suffix e ".conf")
      dir in
  if files = [] then
    lwt () = error "no milter configuration file(s) found" in
    exit 1
  else
    return files

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
    lwt () = O.either C.load_defaults C.load config_file in
    lwt cfgs = milter_config_files () in
    lwt () = notice "number of instances: %d" (List.length cfgs) in
    return cfgs in
  let milter_configs = Lwt_main.run config_t in
  set_log_level (C.srslyd_log_level ());
  let exec = C.milter_executable () in
  let slaves =
    List.map
    (fun milter_config ->
      let argv =
        O.either
          (fun () -> [|exec; milter_config|])
          (fun srslyd_config -> [|exec; srslyd_config; milter_config|])
          config_file in
      ((exec, argv), slave_ipc_handler, 1))
    milter_configs in
  let background = C.srslyd_background () in
  Release.master_slaves
    ~syslog:background
    ~background:background
    ~lock_file:(C.srslyd_lock_file ())
    ~control:(C.srslyd_control_socket (), control_connection_handler)
    ~main:main
    ~slaves:slaves
    ()
