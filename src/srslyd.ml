open Lwt
open Printf
open Ipc.Slave_types

let slave_ipc_handler fd = return ()

let policyd_slaves = ["srsrly_policyd.native"]
let milter_slaves = ["srsrly_milter_in.native"; "srsrly_milter_out.native"]

let slave_connections = ref (fun () -> [])

let reload_config () =
  Config.reload ();
  (* At this point the slaves have released privileges and can't read
   * the configuration file by themselves, so we use IPC. *)
  let send_configuration_to_slave fd =
    Ipc.Slave.write_response fd (Configuration (Config.current ())) in
  Lwt_list.iter_p send_configuration_to_slave (!slave_connections ())

let reload_srs_secrets () =
  let send_srs_reload_to_slave fd =
    Ipc.Slave.write_response fd Reload_srs_secrets in
  Lwt_list.iter_p send_srs_reload_to_slave (!slave_connections ())

let handle_sigterm _ =
  ignore_result (Lwt_log.notice "got SIGTERM, exiting");
  exit 0

let handle_sighup _ =
  ignore_result (Lwt_log.notice "got SIGHUP" >> reload_config ())

let handle_sigusr1 _ =
  ignore_result (Lwt_log.notice "got SIGUSR1" >> reload_srs_secrets ())

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
      (if Config.is_milter () then milter_slaves else policyd_slaves) in
  Release.master_slaves
    ~background:(Config.background ())
    ~syslog:false
    ~lock_file:lock_file
    ~main:main
    ~slaves:slaves
    ()
