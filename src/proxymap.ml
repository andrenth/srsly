open Lwt
open Printf
open Release_lwt

open Log_lwt
open Util

module O = Release.Util.Option
module B = Release.Buffer
module C = Srslyd_config

let replace_formats =
  List.fold_left (fun s (fmt, rep) -> Str.global_replace (Str.regexp fmt) rep s)

let build_request fmt table flags key =
  replace_formats fmt
    [ ("{t}", table)
    ; ("{f}", string_of_int flags)
    ; ("{k}", key)
    ]

let alloc_read fd =
  let rec read buf =
    let siz = B.size buf in
    let len = B.length buf in
    Release.IO.read_once fd buf len (siz - len) >>= fun n ->
    if len + n = siz then begin
      let buf' = B.create (siz * 2) in
      B.add_buffer buf' buf;
      read buf'
    end else
      return buf in
  read (B.create 8192)

let make_request socket req =
  let fd = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Lwt.catch
    (fun () ->
      Lwt_unix.connect fd (Unix.ADDR_UNIX socket) >>= fun () ->
      Release.IO.write fd (B.of_string req) >>= fun () ->
      alloc_read fd >>= fun buf ->
      Lwt_unix.close fd >>= fun () ->
      let n = B.length buf in
      return (B.to_string (B.sub buf 0 n)))
    (function
    | Unix.Unix_error (e, _, _) ->
        Lwt_unix.close fd >>= fun () ->
        let err = Unix.error_message e in
        fail_lwt (sprintf "Proxymap.make_request: connect: %s" err)
    | e ->
        Lwt_unix.close fd >>= fun () ->
        let bt = Printexc.to_string e in
        fail_lwt (sprintf "Proxymap.make_request: %s" bt))

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
    status_of_code code (Str.split sep value)
  with Not_found ->
    failwith (sprintf "parse_result: missing keys")

let query key table =
  let query_fmt = C.proxymap_query_format () in
  let flags = C.proxymap_query_flags () in
  let socket = C.proxymap_query_socket () in
  let res_fmt = C.proxymap_result_format () in
  let sep = C.proxymap_result_value_separator () in
  let req = build_request query_fmt table flags key in
  make_request socket req >>= fun raw_res ->
  return (parse_result raw_res res_fmt sep)

(*
 * Query the given table looking for a recipient key. Results are recursively
 * queried depth-first and recursion stops when a key is not found, or when
 * either the recursion depth limit or the maximum results limit is reached.
 * If the recursion depth limit is reached in a recursion branch, other
 * branches are still queried. The maximum results limit is global and stops
 * recursion completely.
 *
 * The function given in the first parameter is called for each leaf result,
 * that is, those for which a query returns "not found". This function takes
 * the original query key, the leaf result, the current limit for the
 * maximum number of results, and an accumulator value. It is expected to
 * return an updated limit and a new accumulator.
 *)
let query_with f key table max_depth max_results acc =
  let rec resolve keys max_depth max_res acc =
    match keys with
    | [] ->
        debug "no more keys, returning" >>= fun () ->
        return (max_res, acc)
    | k::rest ->
        if max_res = 0 then
          error "too many results querying for %s in %s" key table >>= fun () ->
          return (0, acc)
        else if max_depth < 0 then
          error "maximum depth reached in %s for %s" table key >>= fun () ->
          resolve rest max_depth max_res acc
        else
          debug "querying key %s" k >>= fun () ->
          query k table >>= function
          | Ok values ->
              resolve values (max_depth - 1) max_res acc >>= fun (max, acc) ->
              resolve rest max_depth max acc
          | Key_not_found ->
              debug "no redirects found for %s" k >>= fun () ->
              f key k max_res acc >>= fun (max, acc) ->
              resolve rest max_depth max acc
          | other ->
              let e = string_of_status other in
              error "error querying %s for %s: %s" table key e >>= fun () ->
              resolve rest max_depth max_res acc in
  resolve [key] max_depth max_results acc

let (=~) s re =
  try
    ignore (Str.search_forward re s 0);
    true
  with Not_found ->
    false

let proxymap_key fmt addr =
  try
    let at = String.rindex addr '@' in
    let user = String.sub addr 0 at in
    let domain = String.sub addr (at+1) (String.length addr - at - 1) in
    replace_formats fmt
      [ ("{u}", user)
      ; ("{d}", domain)
      ; ("{a}", addr)
      ]
  with Not_found ->
    replace_formats fmt [("{a}", addr)]

(*
 * Queries for `sender` in the appropriate lookup table and checks if the
 * result is a remote address.
 *)
let is_remote_sender sender =
  let table = C.proxymap_sender_lookup_table () in
  let local_re = C.proxymap_local_sender_regexp () in
  let fmt = C.proxymap_sender_lookup_key_format () in
  let key = proxymap_key fmt sender in
  let max_depth = C.proxymap_sender_query_max_depth () in
  let max_res = C.proxymap_sender_query_max_results () in
  debug "is_remote_sender: querying for %s" key >>= fun () ->
  let is_remote _ sender max res =
    if not (sender =~ local_re) then
      return (max-1, true)
    else
      return (max-1, res) in
  query_with is_remote key table max_depth max_res false >>= fun (_, res) ->
  return res

module RcptSet = Set.Make(struct
  type t = string
  let compare = Pervasives.compare
end)

module RcptMap = Map.Make(struct
  type t = string
  let compare = Pervasives.compare
end)

(* Chooses a random key from a map, weighting the choice by its value. *)
let weighted_sample m =
  let module M = struct exception Sample of string end in
  let tot = float_of_int (RcptMap.fold (fun _ c s -> s + c) m 0) in
  let r = tot *. (1.0 -. Random.float 1.0) in
  try
    ignore
      (RcptMap.fold
        (fun k c r ->
          let s = r -. float_of_int c in
          if s < 0.0 then raise (M.Sample k)
          else s)
        m
        r);
    assert false (* not reached *)
  with M.Sample k ->
    k

let map_incr k m =
  try
    let v = RcptMap.find k m in
    RcptMap.add k (v+1) m
  with Not_found ->
    RcptMap.add k 1 m

(*
 * This function is given as a parameter to `query_with` when called from
 * the `choose_forward_domain` function, below. It adds a final destination
 * address to the recipients set and increments the count of remote
 * destinations for the given original recipient if the corresponding final
 * destination is a remote address. The maximum number of results is
 * decremented if appropriate.
 *)
let visit_rcpt local_re orig_rcpt final_rcpt max_res (rcpts, counts) =
  if max_res > 0 then begin
    let max, rcpts =
      if not (RcptSet.mem final_rcpt rcpts) then
        max_res - 1, RcptSet.add final_rcpt rcpts
      else
        max_res, rcpts in
    begin
      if not (final_rcpt =~ local_re) then
        debug "%s is remote" final_rcpt >>= fun () ->
        return (map_incr orig_rcpt counts)
      else
        debug "%s is local" final_rcpt >>= fun () ->
        return counts
    end >>= fun counts ->
    return (max, (rcpts, counts))
  end else
    return (max_res, (rcpts, counts))

(*
 * We choose a random domain to be used as the SRS forward domain.
 * This is done because there's no way to associate each of the original
 * recipients to the final addresses after virtual alias translation
 * is done.
 *)
let choose_forward_domain orig_rcpts =
  let table = C.proxymap_recipient_lookup_table () in
  let re = C.proxymap_local_recipient_regexp () in
  let fmt = C.proxymap_recipient_lookup_key_format () in
  let max_depth = C.proxymap_recipient_query_max_depth () in
  let num_rcpts = List.length orig_rcpts in
  let max_res =
    num_rcpts * C.proxymap_recipient_query_max_results () in
  Lwt_list.fold_left_s
    (fun (max_res, rcpts, counts) rcpt ->
      let key = proxymap_key fmt rcpt in
      debug "choose_forward_domain: querying for %s" key >>= fun () ->
      query_with
        (visit_rcpt re)
        key
        table
        max_depth
        max_res
        (rcpts, counts) >>= fun (max_res', (rcpts', counts')) ->
      return (max_res', rcpts', counts'))
    (max_res, RcptSet.empty, RcptMap.empty)
    orig_rcpts
  >>= fun (_, _, counts) ->
  if counts <> RcptMap.empty then
    return (Some (weighted_sample counts))
  else
    return None
