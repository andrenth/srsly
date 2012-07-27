open Lwt
open Printf

let set_log_level level =
  Lwt_log.Section.set_level Lwt_log.Section.main level

let handle_sigterm _ =
  let log_t =
    Lwt_log.notice "got sigterm" in
  let cleanup_t =
    let addr = Policyd_config.listen_address (Config.policyd ()) in
    let re = Str.regexp "^unix:\\(.+\\)$" in
    if Str.string_match re addr 0 then
      Lwt_unix.unlink (Str.matched_group 1 addr)
    else
      return () in
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

let parse_sockaddr s =
  let re = Str.regexp "^\\(inet|inet6|unix\\):\\(.+\\)$" in
  if Str.string_match re s 0 then
    let family = Str.matched_group 1 s in
    let addr = Str.matched_group 2 s in
    if family = "unix" then
      Lwt_unix.ADDR_UNIX addr
    else
      match Str.split (Str.regexp "@") addr with
      | [port; ip] ->
          Lwt_unix.ADDR_INET (Unix.inet_addr_of_string ip, int_of_string port)
      | _ ->
          invalid_arg (sprintf "parse_sockaddr: %s" s)
  else
    invalid_arg (sprintf "parse_sockaddr: %s" s)

let main fd =
  ignore (Lwt_unix.on_signal Sys.sigterm handle_sigterm);
  Release_socket.accept_loop
    ~timeout:30.0 (* DNS lookup may be slow *)
    Lwt_unix.SOCK_STREAM
    (parse_sockaddr (Policyd_config.listen_address (Config.policyd ())))
    spf_handler

let () =
  set_log_level (Config.log_level ());
  Release.me ~syslog:false ~user:(Config.user ()) ~main:main ()
