open Lwt
open Printf
open Ipc.Control_types
open Util

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

let reload () =
  let handler = function
    | `EOF -> err "could not reload srslyd: EOF on control socket"
    | `Timeout -> err "could not reload srslyd: timeout on control socket"
    | `Response Reloaded -> return ()
    | `Response _ -> err "unexpected response on control socket" in
  control (fun fd -> Ipc.Control.make_request fd Reload handler)

let restart argcv =
  lwt () = stop () in
  start argcv

let usage rc =
  warn "usage: srsly <command> [/path/to/config/file]";
  warn "  available commands: start, stop, reload, restart";
  warn "  default configuration file: %s" Config.default_config_file;
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
  if argc = 3 then
    Config.file := argv.(2);
  match argv.(1) with
  | "start" -> start (srslyd_args argc argv)
  | "stop" ->  stop ()
  | "reload" -> reload ()
  | "restart" -> restart (srslyd_args argc argv)
  | "help" -> usage 0
  | _ -> usage 1

let () =
  if Unix.getuid () <> 0 then err "srsly must be run as root";
  Lwt_main.run (main (Array.length Sys.argv) Sys.argv)
