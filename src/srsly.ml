open Lwt
open Printf
open Ipc.Control_types
open Util

module O = Release_option

let srslyd = "/usr/lib/srsly/srslyd"

let control f =
  let fd = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let sock_addr = Lwt_unix.ADDR_UNIX (Config.control_socket ()) in
  lwt () =
    try_lwt
      Lwt_unix.connect fd sock_addr
    with Unix.Unix_error (e, _, _) ->
      err "cannot connect to control socket: %s" (Unix.error_message e) in
  finalize (fun () -> f fd) (fun () -> Lwt_unix.close fd)

let start (argc, argv) =
  let argv = Array.init argc (fun i -> if i = 0 then srslyd else argv.(i)) in
  Unix.execv srslyd argv

let stop () =
  control (fun fd -> Ipc.Control.write_request fd Stop)

let reload file =
  let handler = function
    | `EOF -> err "could not reload srslyd: EOF on control socket"
    | `Timeout -> err "could not reload srslyd: timeout on control socket"
    | `Response Reloaded_config -> return ()
    | `Response _ -> err "unexpected response on control socket" in
  control (fun fd -> Ipc.Control.make_request fd (Reload_config file) handler)

let restart argcv =
  lwt () = stop () in
  start argcv

let read_old_secrets () =
  try
    let st = Unix.lstat (Config.srs_secret_file ()) in
    if st.Unix.st_size > 0 then
      let old, older = Srs_util.read_srs_secrets () in
      old::older
    else
      []
  with _ ->
    []

let random_init () =
  let random_dev = Config.random_device () in
  let ch = open_in random_dev in
  let seed = input_binary_int ch in
  close_in ch;
  Random.init seed

let make_secret () =
  let ascii_min = 0x21 in
  let ascii_max = 0x7e in
  let len = Config.srs_secret_length () in
  let secret = String.create len in
  random_init ();
  for i = 0 to len - 1 do
    secret.[i] <- char_of_int (ascii_min + Random.int (ascii_max - ascii_min))
  done;
  secret

let new_secret () =
  Lwt_io.printl (make_secret ())

let add_secret () =
  let secret = make_secret () in
  let old_secrets = read_old_secrets () in
  let ch = open_out (Config.srs_secret_file ()) in
  List.iter (fprintf ch "%s\n") (secret::old_secrets);
  close_out ch;
  let handler = function
    | `EOF -> err "could not reload SRS secrets: EOF on control socket"
    | `Timeout -> err "could not reload SRS secrets: timeout on control socket"
    | `Response Reloaded_secrets -> return ()
    | `Response _ -> err "unexpected response on control socket" in
  control (fun fd -> Ipc.Control.make_request fd Reload_secrets handler)

let usage rc =
  warn "usage: srsly <command> [/path/to/config/file]";
  warn "  available commands: start, stop, reload, restart, add-secret";
  warn "  if no configuration file is given, default values will be used";
  exit rc

let srslyd_args cmd_argc cmd_argv =
  let argc = cmd_argc - 1 in
  let argv = [| srslyd |] in
  if argc = 1 then
    (argc, argv)
  else
    (argc, Array.append argv (Array.sub cmd_argv 2 (cmd_argc - 2)))

let main argc argv =
  if argc = 1 then usage 1;
  let config_file =
    if argc = 2 then begin
      warn "no configuration file given; using default values";
      None
    end else
      Some argv.(2) in
  lwt () = O.either Config.load_defaults Config.load config_file in
  match argv.(1) with
  | "start" -> start (srslyd_args argc argv)
  | "stop" ->  stop ()
  | "reload" -> reload config_file
  | "restart" -> restart (srslyd_args argc argv)
  | "new-secret" -> new_secret ()
  | "add-secret" -> add_secret ()
  | "help" -> usage 0
  | _ -> usage 1

let () =
  if Unix.getuid () <> 0 then err "srsly must be run as root";
  Lwt_main.run (main (Array.length Sys.argv) Sys.argv)
