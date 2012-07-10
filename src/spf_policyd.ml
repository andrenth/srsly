open Lwt
open Printf

(* TODO configuration file *)

type config =
  { user           : string
  ; listen_address : Lwt_unix.sockaddr
  ; log_level      : Lwt_log.level
  }

let config =
  { user           = "andre"
  ; listen_address = Lwt_unix.ADDR_UNIX "/tmp/spf.socket"
  ; log_level      = Lwt_log.Debug
  }

let set_log_level level =
  Lwt_log.Section.set_level Lwt_log.Section.main level

let handle_sigterm _ =
  let log_t =
    Lwt_log.notice "got sigterm" in
  let cleanup_t =
    match config.listen_address with
    | Lwt_unix.ADDR_UNIX path -> Lwt_unix.unlink path
    | _ -> return () in
  Lwt_main.run (log_t >> cleanup_t);
  exit 0

let spf_handler fd =
  let spf_server = SPF.server SPF.Dns_cache in
  match_lwt Postfix.parse_attrs fd with
  | None ->
      return ()
  | Some attrs ->
      lwt action = Policy.handle_attrs spf_server attrs in
      let reply = sprintf "action=%s\n\n" action in
      Release_io.write fd (Release_buffer.of_string reply)

let main fd =
  ignore (Lwt_unix.on_signal Sys.sigterm handle_sigterm);
  Release_socket.accept_loop
    ~timeout:30.0 (* DNS lookup may be slow *)
    Lwt_unix.SOCK_STREAM
    config.listen_address
    spf_handler

let () =
  (* TODO let config = read_config_file "/etc/spfd.conf" in *)
  set_log_level config.log_level;
  Release.me ~syslog:false ~user:config.user ~main:main ()
