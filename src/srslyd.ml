open Lwt
open Printf
open Ipc.Control_types
open Ipc.Slave_types

module O = Release_option

let slave_connections = ref (fun () -> [])

let signal_slaves signum =
  Lwt_list.iter_p
    (fun (pid, _) ->
      printf "! %d - %d\n%!" pid signum;
      Unix.kill pid signum;
      return ())
    (!slave_connections ())

let handle_sighup _ =
  printf ">>>>> master hup\n%!";
  ignore_result (Lwt_log.info "got SIGHUP, reloading configuration");
  Config.reload ();
  ignore_result (signal_slaves 1)

let rec handle_sigusr1 _ =
  printf ">>>>> master usr1\n%!";
  ignore_result (Lwt_log.info "got SIGUSR1, reloading SRS secrets");
  ignore_result (signal_slaves 10)

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
    | Stop ->
        lwt () = Lwt_log.notice "received stop command" in
        (* Suicide. Release will catch SIGTERM and kill the slaves *)
        Unix.kill (Unix.getpid ()) 15;
        lwt () = Lwt_unix.sleep 1.0 in (* give the signal handler time to run *)
        raise_lwt (Failure "I'm already dead!")
    | Reload ->
        Config.reload ();
        lwt () = signal_slaves Sys.sighup in
        return Reloaded in
  Ipc.Control.handle_request ~eof_warning:false ~timeout:5. fd handler

let main get_conns =
  slave_connections := get_conns;
  ignore (Lwt_unix.on_signal Sys.sighup handle_sighup);
  ignore (Lwt_unix.on_signal Sys.sigusr1 handle_sigusr1);
  return ()

let () =
  ignore_result (Lwt_log.notice "starting up");
  if Array.length Sys.argv > 1 then
    Config.file := Some Sys.argv.(1);
  let slave = (Config.milter_executable (), slave_ipc_handler) in
  Release.master_slave
    ~background:(Config.background ())
    ~lock_file:(Config.lock_file ())
    ~control:(Config.control_socket (), control_connection_handler)
    ~main:main
    ~slave:slave
    ()
