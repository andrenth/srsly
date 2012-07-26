open Lwt
open Printf

let config = Config.configuration

let policyd_slaves = ["spf_policyd.native"]
let milter_slaves = ["spf_milter_in.native"; "spf_milter_out.native"]

let spf_ipc_handler fd = return ()

let slaves =
  List.map
    (fun bin ->
      let exec =  sprintf "%s/%s" (Config.binary_path config) bin in
      (exec, spf_ipc_handler, 1))
    (if Config.is_milter config then milter_slaves else policyd_slaves)

let lock_file = "/tmp/spfd.pid"
let num_slaves = 4
let listen_socket = "/tmp/spf.socket" 

let () =
  Release.master_slaves
    ~background:(Config.background config)
    ~syslog:false
    ~lock_file:lock_file
    ~slaves:slaves
    ()
