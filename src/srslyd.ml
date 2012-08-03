open Lwt
open Printf
open Ipc.Control_types
open Ipc.Slave_types

let slave_connections = ref (fun () -> [])

let send_to_slaves msg =
  Lwt_list.iter_p
    (fun fd -> Ipc.Slave.write_response fd msg)
    (!slave_connections ())

let reload_config () =
  Config.reload ();
  send_to_slaves (Configuration (Config.current ()))

let reload_srs_secrets () =
  send_to_slaves (SRS_secrets (Srs_util.read_srs_secrets ()))

let handle_sighup _ =
  ignore_result (
    Lwt_log.info "got SIGHUP, reloading configuration" >> reload_config ()
  )

let handle_sigusr1 _ =
  ignore_result (
    Lwt_log.info "got SIGUSR1, reloading SRS secrets" >> reload_srs_secrets ()
  )

let slave_ipc_handler fd =
  lwt () = Lwt_log.debug "received IPC request from slave" in
  let handler = function
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
        lwt () = reload_config () in
        return Reloaded in
  Ipc.Control.handle_request ~eof_warning:false ~timeout:5. fd handler

let main get_conns =
  ignore (Lwt_unix.on_signal Sys.sighup handle_sighup);
  slave_connections := get_conns;
  return ()

let () =
  ignore_result (Lwt_log.notice "starting up");
  if Array.length Sys.argv > 1 then
    Config.file := Sys.argv.(1);
  let slaves =
    [ Config.milter_in_executable (), slave_ipc_handler, 1
    ; Config.milter_out_executable (), slave_ipc_handler, 1 ] in
  Release.master_slaves
    ~background:(Config.background ())
    ~lock_file:(Config.lock_file ())
    ~control:(Config.control_socket (), control_connection_handler)
    ~syslog:true
    ~main:main
    ~slaves:slaves
    ()
