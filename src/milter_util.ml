open Lwt
open Printf
open Ipc.Slave_types
open Util

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

let with_priv_data z ctx f =
  match Milter.getpriv ctx with
  | None -> z
  | Some p -> let p', r = f p in Milter.setpriv ctx p'; r

let log f fmt =
  ksprintf (fun s -> ignore_result (f s)) fmt

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
      Config.replace c;
      set_log_level (Config.log_level ());
      Milter.setdbg (Config.milter_debug_level ())
  | SRS_secrets ss ->
      Milter_srs.reload ss

let handle_ipc = function
  | `Response r -> return (handle_ipc_response r)
  | `EOF -> Lwt_log.error "EOF on IPC socket" >> exit 1
  | `Timeout -> Lwt_log.error "timeout on IPC socket"

let read_srs_secrets fd =
  Ipc.Slave.make_request fd SRS_secrets_request handle_ipc

let rec ipc_reader fd =
  lwt r = Ipc.Slave.read_response fd in
  lwt () = handle_ipc r in
  ipc_reader fd

let main filter listen_addr fd =
  lwt () = Lwt_log.notice "starting up" in
  lwt () = read_srs_secrets fd in
  let ipc_read_t = ipc_reader fd in
  Milter.setdbg (Config.milter_debug_level ());
  Milter.setconn listen_addr;
  Milter.register filter;
  Milter.main ();
  Lwt.join [ipc_read_t]

let debug fmt = log Lwt_log.debug fmt
let notice fmt = log Lwt_log.notice fmt
let info fmt = log Lwt_log.info fmt
let warning fmt = log Lwt_log.warning fmt
let error fmt = log Lwt_log.error fmt
