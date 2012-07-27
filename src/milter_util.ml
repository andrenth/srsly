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
  ksprintf (fun s -> ignore (f s)) fmt

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

let make_srs () =
  let config = Config.milter () in
  let read_srs_secrets () = 
    let secrets = ref [] in
    let ch = open_in (Milter_config.srs_secret_file config) in
    (try while true; do secrets := input_line ch :: !secrets done
    with End_of_file -> close_in ch);
    List.rev !secrets in
  SRS.make
    (read_srs_secrets ())
    (Milter_config.srs_hash_max_age config)
    (Milter_config.srs_hash_length config)
    (Milter_config.srs_separator config)

let handle_ipc_response = function
  | Configuration c ->
      Config.replace c;
      set_log_level (Config.log_level ());
        Sys.argv.(0) (Milter_config.debug_level (Config.milter ()));
      Milter.setdbg (Milter_config.debug_level (Config.milter ()))

let rec ipc_reader fd =
  lwt () = match_lwt Ipc.Slave.read_response fd with
  | `Response r -> return (handle_ipc_response r)
  | `EOF -> Lwt_log.error "EOF on IPC socket" >> exit 1
  | `Timeout -> Lwt_log.error "timeout on IPC socket" in
  ipc_reader fd

let main filter listen_addr fd =
  lwt () = Lwt_log.notice "starting up" in
  let ipc_read_t = ipc_reader fd in
  Milter.setdbg (Milter_config.debug_level (Config.milter ()));
  Milter.setconn listen_addr;
  Milter.register filter;
  Milter.main ();
  Lwt.join [ipc_read_t]

let debug fmt = log Lwt_log.debug fmt
let notice fmt = log Lwt_log.notice fmt
let info fmt = log Lwt_log.info fmt
let warning fmt = log Lwt_log.warning fmt
let error fmt = log Lwt_log.error fmt
