open Lwt
open Printf
open Ipc.Slave_types

let slave_ipc_handler fd = return ()

let policyd_slaves = ["spf_policyd.native"]
let milter_slaves = ["spf_milter_in.native"; "spf_milter_out.native"]

let slave_connections = ref (fun () -> [])

let reload_config () =
  Config.reload ();
  (* At this point the slaves have released privileges and can't read
   * the configuration file by themselves, so we use IPC. *)
  let send_configuration_to_slave fd =
    Ipc.Slave.write_response fd (Configuration (Config.current ())) in
  Lwt_list.iter_p send_configuration_to_slave (!slave_connections ())

let handle_sigterm _ =
  ignore_result (Lwt_log.notice "got sigterm, exiting");
  exit 0

let handle_sighup _ =
  ignore_result (Lwt_log.notice "got sighup" >> reload_config ())

let lock_file = "/tmp/spfd.pid"
let num_slaves = 4
let listen_socket = "/tmp/spf.socket" 

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
