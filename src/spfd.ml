open Lwt
open Printf

(* XXX configuration file *)
let config = Config.milter_in_default

let policyd_slaves = ["spf_policyd.native"]
let milter_slaves = ["spf_milter_in.native"; "spf_milter_out.native"]

let spf_ipc_handler fd = return ()

let slaves =
  let bins = match config with
  | Config.Milter_in _ | Config.Milter_out _ -> milter_slaves
  | Config.Policyd _ -> policyd_slaves in
  List.map
    (fun bin ->
      let exec =  sprintf "%s/_build/src/%s" (Unix.getcwd ()) bin in
      (exec, spf_ipc_handler, 1))
    bins

let lock_file = "/tmp/spfd.pid"
let num_slaves = 4
let listen_socket = "/tmp/spf.socket" 

let () =
  Release.master_slaves
    ~background:false
    ~syslog:false
    ~lock_file:lock_file
    ~slaves:slaves
    ()
