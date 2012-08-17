open Printf

let log f fmt =
  ksprintf (fun s -> Lwt_preemptive.run_in_main (fun () -> f s)) fmt

let debug fmt = log Lwt_log.debug fmt
let notice fmt = log Lwt_log.notice fmt
let info fmt = log Lwt_log.info fmt
let warning fmt = log Lwt_log.warning fmt
let error fmt = log Lwt_log.error fmt
