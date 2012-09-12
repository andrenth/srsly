let listen_address () =
  let sock = Config.milter_listen_address () in
  if Str.string_match (Str.regexp "^\\(unix\\|local\\):\\(.+\\)") s 0 then
    let path = Str.matched_group 2 s in
    Lwt_unix.ADDR_UNIX path
  else if Str.string_match (Str.regexp "^inet6?:\\([0-9]+\\)@\\(.+\\)") s 0 then
    let port = int_of_string (Str.matched_group 1 s) in
    let ip = Unix.inet_addr_of_string (Str.matched_group 2 s) in
    Lwt_unix.ADDR_INET (ip, port)

let proxy fd =
  lwt () = debug "received connection" in
  let milter_fd = choose_connection () in


let main fd =
  lwt () = notice "starting up" in
  let addr = listen_address () in
  Release_socket.accept_loop Lwt_unix.SOCK_STREAM addr proxy

let () =
  let config_t =
    if Array.length Sys.argv > 1 then
      Config.load Sys.argv.(1)
    else
      Config.load_defaults () in
  Lwt_main.run config_t;
  set_log_level (Config.srslyd_log_level ());
  Release.me
    ~syslog:(Config.srslyd_background ())
    ~user:(Config.milter_user ())
    ~main:main ()
