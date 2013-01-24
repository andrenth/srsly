open Lwt
open Printf

open Ipc.Control_types
open Log_lwt
open Util

module O = Release_util.Option
module C = Srslyd_config

let srslyd = "/usr/lib/srsly/srslyd"

let err fmt =
  ksprintf (fun s -> lwt () = error "%s" s in exit 1) fmt

let control f =
  let fd = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let sock_addr = Lwt_unix.ADDR_UNIX (C.srslyd_control_socket ()) in
  lwt () =
    try_lwt
      Lwt_unix.connect fd sock_addr
    with Unix.Unix_error (e, _, _) ->
      err "cannot connect to control socket: %s" (Unix.error_message e) in
  finalize
    (fun () -> f fd)
    (fun () -> Lwt_unix.close fd)

let start config =
  Unix.execv srslyd [| srslyd; config |]

let stop () =
  control (fun fd -> Ipc.Control.Client.write_request fd Stop)

let reload file =
  let config = Some file in
  let handler = function
    | `EOF -> err "could not reload srslyd: EOF on control socket"
    | `Timeout -> err "could not reload srslyd: timeout on control socket"
    | `Response Reloaded_config -> return ()
    | `Response _ -> err "unexpected response on control socket" in
  control
    (fun fd ->
      Ipc.Control.Client.make_request fd (Reload_config config) handler)

let restart config =
  lwt () = stop () in
  start config

let read_old_secret () =
  try_lwt
    let file = C.srs_secret_file () in
    Lwt_io.with_file ~mode:Lwt_io.input file Lwt_io.read_line
  with Unix.Unix_error (e, _, _) ->
    lwt () = error "cannot read SRS secret: %s" (Unix.error_message e) in
    exit 1

let random_init () =
  let random_dev = C.srslyd_random_device () in
  let ch = open_in random_dev in
  let seed = input_binary_int ch in
  close_in ch;
  Random.init seed

let make_secret () =
  let ascii_min = 0x21 in
  let ascii_max = 0x7e in
  let len = C.srs_secret_length () in
  let secret = String.create len in
  random_init ();
  for i = 0 to len - 1 do
    secret.[i] <- char_of_int (ascii_min + Random.int (ascii_max - ascii_min))
  done;
  secret

let new_secret () =
  Lwt_io.printl (make_secret ())

let replace_secret () =
  let file = C.srs_secret_file () in
  let dir = C.srs_secrets_directory () in
  let old_file = dir ^"/"^ sprintf "%d.%.0f" (Unix.getpid ()) (Unix.time ()) in
  let output = Lwt_io.output in
  let write_secret secret ch =
    Lwt_io.write_line ch secret in
  lwt old_secret = read_old_secret () in
  lwt () = Lwt_io.with_file ~mode:output old_file (write_secret old_secret) in
  let secret = make_secret () in
  lwt () = Lwt_io.with_file ~mode:output file (write_secret secret) in
  let handler = function
    | `EOF -> err "could not reload SRS secrets: EOF on control socket"
    | `Timeout -> err "could not reload SRS secrets: timeout on control socket"
    | `Response Reloaded_secrets -> return ()
    | `Response _ -> err "unexpected response on control socket" in
  control (fun fd -> Ipc.Control.Client.make_request fd Reload_secrets handler)

let seconds_of_days n =
  float_of_int n *. 24.0 *. 60.0 *. 60.0

let map_stream f s =
  lwt u = Lwt_stream.fold (fun x u -> lwt () = u in f x) s return_unit in u

let expire () =
  let dir = C.srs_secrets_directory () in
  let max_age = seconds_of_days (C.srs_hash_max_age ()) in
  let secrets = Lwt_unix.files_of_directory dir in
  let remove file =
    if file <> "." && file <> ".." then
      let path = dir ^ "/" ^ file in
      try_lwt
        lwt st = Lwt_unix.lstat path in
        let mtime = st.Lwt_unix.st_mtime in
        if Unix.time () -. mtime > max_age then
          lwt () = info "removing expired secret in %s" path in
          Lwt_unix.unlink path
        else
          debug "secret in %s still not expired" path
      with Unix.Unix_error (e, _, _) ->
        error "expire: %s: %s" path (Unix.error_message e)
    else
      return_unit in
  map_stream remove secrets

let usage rc =
  let warn = Lwt_io.eprintl in
  warn "usage: srsly <command> [/path/to/config/file]" >>
  warn "  commands: start, stop, reload, restart," >>
  warn "            new-secret, replace-secret, expire" >>
  warn "  if no configuration file is given, default values will be used" >>
  exit rc

let main argc argv =
  lwt () = if argc = 1 then usage 1 else return_unit in
  let config = if argc = 2 then "/etc/srsly/srslyd.conf" else argv.(2) in
  match argv.(1) with
  | "help" ->
      usage 0
  | _ ->
      lwt () = C.load config in
      set_log_level (C.srslyd_log_level ());
      match argv.(1) with
      | "start" -> start config
      | "stop" ->  stop ()
      | "reload" -> reload config
      | "restart" -> restart config
      | "new-secret" -> new_secret ()
      | "replace-secret" -> replace_secret ()
      | "expire" -> expire ()
      | _ -> usage 1

let () =
  if Unix.getuid () <> 0 then begin
    fprintf stderr "srsly must be run as root";
    exit 1
  end;
  Lwt_main.run (main (Array.length Sys.argv) Sys.argv)
