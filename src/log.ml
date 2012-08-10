open Lwt
open Printf

module O = Release_option

let syslog = ref None

let open_log () =
  let myname = Filename.basename Sys.argv.(0) in
  let log = Syslog.openlog ~facility:`LOG_DAEMON ~flags:[`LOG_PID] myname in
  syslog := Some log

let log level fmt =
  ksprintf (fun s -> Syslog.syslog (O.some !syslog) level (s^"\n")) fmt

let close_log () =
  O.may Syslog.closelog !syslog

let setup () =
  open_log ();
  Lwt_main.at_exit (fun () -> close_log (); return ())

let debug fmt = log `LOG_DEBUG fmt
let notice fmt = log `LOG_NOTICE fmt
let info fmt = log `LOG_INFO fmt
let warning fmt = log `LOG_WARNING fmt
let error fmt = log `LOG_ERR fmt
