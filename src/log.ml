open Printf

module Lwt = struct
  let debug fmt = Lwt_log.debug_f fmt
  let notice fmt = Lwt_log.notice_f fmt
  let info fmt = Lwt_log.info_f fmt
  let warning fmt = Lwt_log.warning_f fmt
  let error fmt = Lwt_log.error_f fmt
end

module Preemptive = struct
  let log f fmt =
    ksprintf (fun s -> Lwt_preemptive.run_in_main (fun () -> f s)) fmt

  let debug fmt = log Lwt_log.debug fmt
  let notice fmt = log Lwt_log.notice fmt
  let info fmt = log Lwt_log.info fmt
  let warning fmt = log Lwt_log.warning fmt
  let error fmt = log Lwt_log.error fmt
end
