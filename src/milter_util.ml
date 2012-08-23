open Lwt
open Printf
open Ipc.Slave_types
open Log
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

let ipc_error = function
  | `EOF -> Lwt_log.error "EOF on IPC socket" >> exit 1
  | `Timeout -> Lwt_log.error "timeout on IPC socket" >> exit 1

let read_configuration fd =
  let handle_ipc = function
    | `Response (Configuration c) ->
        lwt () = Lwt_log.notice "received a new configuration; replacing" in
        Config.replace c;
        set_log_level (Config.log_level ());
        Milter.setdbg (Config.milter_debug_level ());
        return ()
    | `EOF | `Timeout as e ->
        ipc_error e
    | _ ->
        fail_lwt "unexpected response while waiting for configuration" in
  Ipc.Slave.make_request fd Configuration_request handle_ipc

let read_srs_secrets fd =
  let handle_ipc = function
    | `Response (SRS_secrets ss) ->
        lwt () = Lwt_log.notice "received new SRS secrets; reloading" in
        Milter_srs.reload ss;
        return ()
    | `EOF | `Timeout as e ->
        ipc_error e
    | _ ->
        fail_lwt "unexpected response while waiting for SRS secrets" in
  Ipc.Slave.make_request fd SRS_secrets_request handle_ipc

let proxymap_is_remote_addr fd =
  let handle_ipc = function
    | `Response (Proxymap_response r) ->
        lwt () = Lwt_log.debug_f "received proxymap response: %b" r in
        return r
    | `EOF | `Timeout as e ->
        ipc_error e
    | _ ->
        fail_lwt "unexpected response while waiting for proxymap response" in
  (fun addr -> Ipc.Slave.make_request fd (Proxymap_query addr) handle_ipc)

let handle_sighup fd _ =
  Lwt.async
    (fun () ->
      lwt () = Lwt_log.info "got SIGHUP, asking for configuration" in
      read_configuration fd)

let handle_sigusr1 fd _ =
  Lwt.async
    (fun () ->
      lwt () = Lwt_log.info "got SIGUSR1, asking for SRS secrets" in
      read_srs_secrets fd)
