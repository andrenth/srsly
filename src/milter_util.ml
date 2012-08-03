open Lwt
open Printf
open Ipc.Slave_types
open Util

module O = Release_option

module SetOfList = struct
  module type S = sig
    include Set.S
    val of_list : elt list -> t
  end

  module Make (E : Set.OrderedType) : S with type elt = E.t = struct
    include Set.Make(E)
    let of_list =
      List.fold_left (fun s e -> add e s) empty
  end
end

module FlagSet = SetOfList.Make(struct
  type t = Milter.flag
  let compare = compare
end)

module StepSet = SetOfList.Make(struct
  type t = Milter.step
  let compare = compare
end)

let syslog = ref None

let open_log () =
  let myname = Filename.basename Sys.argv.(0) in
  let log = Syslog.openlog ~facility:`LOG_DAEMON ~flags:[`LOG_PID] myname in
  syslog := Some log

let log level fmt =
  ksprintf (fun s -> Syslog.syslog (O.some !syslog) level (s^"\n")) fmt

let close_log () =
  O.may Syslog.closelog !syslog

let setup_syslog () =
  open_log ();
  Lwt_main.at_exit (fun () -> close_log (); return ())

let debug fmt = log `LOG_DEBUG fmt
let notice fmt = log `LOG_NOTICE fmt
let info fmt = log `LOG_INFO fmt
let warning fmt = log `LOG_WARNING fmt
let error fmt = log `LOG_ERR fmt

let with_priv_data z ctx f =
  match Milter.getpriv ctx with
  | None -> z
  | Some p -> let p', r = f p in Milter.setpriv ctx p'; r

let canonicalize a =
  let e = String.length a - 1 in
  let a = if a.[0] = '<' && a.[e] = '>' then String.sub a 1 (e-1) else a in
  let a = if a.[0] = '"' && a.[e] = '"' then String.sub a 1 (e-1) else a in
  let e = String.length a - 1 in
  try
    let t = String.rindex a '@' in
    let u = String.sub a 0 (t) in
    let d = String.sub a (t+1) (e-t) in
    let u = if u.[0] = '"' && u.[t-1] = '"' then String.sub u 1 (t-2) else u in
    try
      let v = String.rindex u ':' in
      let u = String.sub u (v+1) (String.length u - v - 1) in
      u ^ "@" ^ d
    with Not_found ->
      u ^ "@" ^ d
  with Not_found ->
    a

let handle_ipc_response = function
  | Configuration c ->
      notice "received a new configuration; replacing";
      Config.replace c;
      set_log_level (Config.log_level ());
      Milter.setdbg (Config.milter_debug_level ())
  | SRS_secrets ss ->
      notice "received new SRS secrets; reloading";
      Milter_srs.reload ss

let handle_ipc = function
  | `Response r -> return (handle_ipc_response r)
  | `EOF -> Lwt_log.error "EOF on IPC socket" >> exit 1
  | `Timeout -> Lwt_log.error "timeout on IPC socket"

let read_srs_secrets fd =
  Ipc.Slave.make_request fd SRS_secrets_request handle_ipc

let rec ipc_reader fd =
  lwt r = Ipc.Slave.read_response fd in
  lwt () = Lwt_log.debug "received IPC response from master" in
  lwt () = handle_ipc r in
  ipc_reader fd

let main filter listen_addr fd =
  lwt () = Lwt_log.notice "starting up" in
  setup_syslog ();
  lwt () = read_srs_secrets fd in
  let ipc_read_t = ipc_reader fd in
  Milter.setdbg (Config.milter_debug_level ());
  Milter.setconn listen_addr;
  Milter.register filter;
  Milter.main ();
  Lwt.join [ipc_read_t]
