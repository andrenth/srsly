open Lwt
open Printf
open Ipc.Control_types
open Ipc.Slave_types

let milters = ["srsly_in.native"; "srsly_out.native"]

let slave_connections = ref (fun () -> [])

let read_srs_secrets () = 
  let secret = ref "" in
  let secrets = ref [] in
  let ch = open_in (Config.srs_secret_file ()) in
  let first = ref true in
  (try
    while true do
      if !first then begin
        secret := input_line ch;
        first := false
      end else
        secrets := input_line ch :: !secrets
    done
  with End_of_file ->
    close_in ch);
  !secret, List.rev !secrets

let send_to_slaves msg =
  Lwt_list.iter_p
    (fun fd -> Ipc.Slave.write_response fd msg)
    (!slave_connections ())

let reload_config () =
  Config.reload ();
  send_to_slaves (Configuration (Config.current ()))

let reload_srs_secrets () =
  send_to_slaves (SRS_secrets (read_srs_secrets ()))

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
        return (SRS_secrets (read_srs_secrets ())) in
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
    List.map
      (fun slave ->
        let path = sprintf "%s/%s" (Config.binary_path ()) slave in
        (path, slave_ipc_handler, 1))
      milters in
  Release.master_slaves
    ~background:(Config.background ())
    ~lock_file:(Config.lock_file ())
    ~control:(Config.control_socket (), control_connection_handler)
    ~syslog:true
    ~main:main
    ~slaves:slaves
    ()
