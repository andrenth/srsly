open Lwt
open Printf
open Ipc.Slave_types

let milters = ["srsly_milter_in.native"; "srsly_milter_out.native"]

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

let reload_config () =
  Config.reload ();
  let send_configuration fd =
    Ipc.Slave.write_response fd (Configuration (Config.current ())) in
  Lwt_list.iter_p send_configuration (!slave_connections ())

let reload_srs_secrets () =
  let send_secrets fd =
    Ipc.Slave.write_response fd (SRS_secrets (read_srs_secrets ())) in
  Lwt_list.iter_p send_secrets (!slave_connections ())

let handle_sigterm _ =
  ignore_result (Lwt_log.notice "got SIGTERM, exiting");
  exit 0

let handle_sighup _ =
  ignore_result (Lwt_log.notice "got SIGHUP" >> reload_config ())

let handle_sigusr1 _ =
  ignore_result (Lwt_log.notice "got SIGUSR1" >> reload_srs_secrets ())

let slave_ipc_handler fd =
  let handler = function
    | SRS_secrets_request -> return (SRS_secrets (read_srs_secrets ())) in
  Ipc.Slave.handle_request fd handler

let lock_file = "/tmp/srslyd.pid"
let num_slaves = 4
let listen_socket = "/tmp/srsly.socket" 

let main get_conns =
  ignore (Lwt_unix.on_signal Sys.sigterm handle_sigterm);
  ignore (Lwt_unix.on_signal Sys.sighup handle_sighup);
  slave_connections := get_conns;
  return ()

let () =
  let slaves =
    List.map
      (fun slave ->
        let path = sprintf "%s/%s" (Config.binary_path ()) slave in
        (path, slave_ipc_handler, 1))
      milters in
  Release.master_slaves
    ~background:(Config.background ())
    ~syslog:true
    ~lock_file:lock_file
    ~main:main
    ~slaves:slaves
    ()
