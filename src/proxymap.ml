open Lwt
open Printf

open Log.Lwt
open Util

let replace_formats =
  List.fold_left (fun s (fmt, rep) -> Str.global_replace (Str.regexp fmt) rep s)

let build_request fmt table flags key =
  replace_formats fmt
    [ ("{t}", table)
    ; ("{f}", string_of_int flags)
    ; ("{k}", key)
    ]

let make_request socket req =
  let fd = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  lwt () = debug "connecting to proxymap socket" in
  try_lwt
    lwt () = Lwt_unix.connect fd (Unix.ADDR_UNIX socket) in
    lwt () = Release_io.write fd (Release_buffer.of_string req) in
    let buf = Release_buffer.create 1024 in
    lwt n = Release_io.read_once fd buf 0 (Release_buffer.size buf) in
    lwt () = Lwt_unix.close fd in
    return (Release_buffer.to_string (Release_buffer.sub buf 0 n))
  with
  | Unix.Unix_error (e, _, _) ->
      lwt () = Lwt_unix.close fd in
      let err = Unix.error_message e in
      raise_lwt (Failure (sprintf "Proxymap.make_request: connect: %s" err))
  | e ->
      lwt () = Lwt_unix.close fd in
      let bt = Printexc.to_string e in
      raise_lwt (Failure (sprintf "Proxymap.make_request: %s" bt))

type status
  = Ok of string list
  | Key_not_found
  | Try_again
  | Invalid_parameter
  | Table_not_approved
  | Unknown of int

let status_of_code c values =
  match c with
  | 0 -> Ok values
  | 1 -> Key_not_found
  | 2 -> Try_again
  | 3 -> Invalid_parameter
  | 4 -> Table_not_approved
  | x -> Unknown c

let string_of_status = function
  | Ok _ -> "operation succeeded"
  | Key_not_found -> "requested key not found"
  | Try_again -> "try lookup again later"
  | Invalid_parameter -> "invalid request parameter"
  | Table_not_approved -> "table not approved for proxying"
  | Unknown c -> sprintf "unknown proxymap return status: %d" c

(* This parser doesn't take advantage of the null-byte separator of the
 * postfix query format because there's no guarantee it's not going to
 * change in the future. *)
let parse_result res fmt sep =
  let results = Hashtbl.create 2 in
  let sep_re = Str.regexp sep in
  let set_result k v =
    if Hashtbl.mem results k then
      failwith (sprintf "Proxymap.parse_result: key {%s} already set" k)
    else
      Hashtbl.replace results k v in
  let rec scan i j =
    if i < String.length res && j < String.length fmt then
      match res.[i], fmt.[j] with
      | _, '{' -> get_key i (j+1)
      | a, b when a = b -> scan (i+1) (j+1)
      | a, b -> failwith (sprintf "Proxymap.parse_result: got %c, want %c" b a)
    else
      () (* done *)
  and get_key i j =
    let close = String.index_from fmt j '}' in
    let key = String.sub fmt j (close-j) in
    get_value i (close+1) key (* 1-char lookahead *)
  and get_value i j key =
    if j < String.length fmt then
      let val_end = String.index_from res i fmt.[j] in
      let value = String.sub res i (val_end-i) in
      set_result key value;
      scan val_end j
    else
      let value = String.sub res i (String.length res - i) in
      set_result key value in
  scan 0 0;
  try
    let code = int_of_string (Hashtbl.find results "s") in
    let value = Hashtbl.find results "v" in
    status_of_code code (Str.split sep_re value)
  with Not_found ->
    failwith (sprintf "parse_result: missing keys")

module ResultSet = Set.Make(struct
  type t = string
  let compare = Pervasives.compare
end)

let query key table =
  let query_fmt = Config.proxymap_query_format () in
  let flags = Config.proxymap_query_flags () in
  let socket = Config.proxymap_query_socket () in
  let res_fmt = Config.proxymap_result_format () in
  let sep = Config.proxymap_result_value_separator () in
  let max_depth = Config.proxymap_max_query_depth () in
  let make_query key table =
    let req = build_request query_fmt table flags key in
    lwt raw_res = make_request socket req in
    return (parse_result raw_res res_fmt sep) in
  let rec resolve keys depth results =
    match keys with
    | [] ->
        lwt () = debug "no more keys, returning" in
        return results
    | key::rest ->
        lwt () = debug "keys: %s; depth %d" (join_strings ", " keys) depth in
        if depth < max_depth then begin
          lwt () = debug "querying key %s" key in
          match_lwt make_query key table with
          | Ok values ->
              lwt () =
                debug "redirects for %s: %s" key (join_strings ", " values) in
              resolve values (depth + 1) results
          | Key_not_found ->
              lwt () = debug "no redirects found for %s" key in
              resolve rest depth (ResultSet.add key results)
          | other ->
              let e = string_of_status other in
              lwt () = error "proxymap query error in table %s: %s" table e in
              resolve rest depth results
        end else begin
          lwt () =
            error "proxymap query maximum depth reached in table %s" table in
          resolve rest depth results
        end in
  lwt res = resolve [key] 0 ResultSet.empty in
  return (ResultSet.elements res)

let (=~) s re =
  try
    ignore (Str.search_forward re s 0);
    true
  with Not_found ->
    false

let proxymap_key fmt addr =
  try
    let at = String.index addr '@' in
    let user = String.sub addr 0 at in
    let domain = String.sub addr (at+1) (String.length addr - at - 1) in
    replace_formats fmt
      [ ("{u}", user)
      ; ("{d}", domain)
      ; ("{a}", addr)
      ]
  with Not_found ->
    replace_formats fmt [("{a}", addr)]

let is_remote_sender sender =
  let table = Config.proxymap_sender_lookup_table () in
  let re = Config.proxymap_local_sender_regexp () in
  let fmt = Config.proxymap_sender_lookup_key_format () in
  let key = proxymap_key fmt sender in
  lwt () = debug "is_remote_sender: querying for '%s'" key in
  (* The sender should translate to a single address, so it would probably
   * be safe to return just the first element of the result, but who knows
   * whatever crazy postfix configurations exist out there. *)
  lwt res = query key table in
  return (List.exists (fun s -> not (s =~ re)) res)

module type COUNT_MAP = sig
  include Map.S
  val incr_by : int -> key -> int t -> int t
end

module IncrMap = struct
  module Make (Ord : Map.OrderedType) : COUNT_MAP with type key = Ord.t =
    struct
      include Map.Make(Ord)

      let incr_by x k m =
        try
          let c = find k m in
          add k (c + x) m
        with Not_found ->
          add k x m
    end
end

module RcptMap = IncrMap.Make(struct
  type t = string
  let compare = compare
end)

(* Like List.filter but count the number of results to avoid
 * List.length later. *)
let filter_remote addrs =
  let re = Config.proxymap_local_recipient_regexp () in
  List.fold_left
    (fun (rem, n) addr ->
      if addr =~ re then (rem, n)
      else (addr::rem, n+1))
    ([], 0) addrs

(* Returns the list of pairs consisting of the original recipient which
 * translate to remote addresses and the number of remote addresses it
 * was translated to. This number will be used as a weight in the random
 * selection of the SRS forward domain. *)
let count_remote_final_rcpts orig_rcpts =
  let table = Config.proxymap_recipient_lookup_table () in
  let fmt = Config.proxymap_recipient_lookup_key_format () in
  lwt counts =
    Lwt_list.fold_left_s
      (fun acc rcpt ->
        let key = proxymap_key fmt rcpt in
        lwt () = debug "count_remote_final_rcpts: querying for '%s'" key in
        lwt addrs = query key table in
        let remote, n = filter_remote addrs in
        match n with
        | 0 -> return acc
        | _ -> return (RcptMap.incr_by n rcpt acc))
      RcptMap.empty
      orig_rcpts in
  return (RcptMap.bindings counts)
