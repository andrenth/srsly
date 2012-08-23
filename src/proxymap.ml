open Lwt
open Printf

let build_request fmt table flags key =
  let i = ref 1 in
  let escape s =
    let c = Str.matched_group 1 s in
    incr i;
    sprintf "%c" (char_of_int (int_of_string c)) in
  let s = Str.global_substitute (Str.regexp "\\\\\\([0-9]+\\)") escape fmt in
  let s = Str.global_replace (Str.regexp "\\{t\\}") table s in
  let s = Str.global_replace (Str.regexp "\\{f\\}") (string_of_int flags) s in
  let s = Str.global_replace (Str.regexp "\\{k\\}") key s in
  s

let make_request socket req =
  let fd = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  lwt () = Lwt_log.debug "connecting to proxymap socket" in
  try_lwt
    lwt () = Lwt_unix.connect fd (Unix.ADDR_UNIX socket) in
    lwt () = Release_io.write fd (Release_buffer.of_string req) in
    let buf = Release_buffer.create 1024 in
    lwt n = Release_io.read_once fd buf 0 (Release_buffer.size buf) in
    lwt () = Lwt_unix.close fd in
    lwt () = Lwt_log.debug_f "<%s>" (Release_buffer.to_string
    (Release_buffer.sub buf 0 n)) in
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

let status_message = function
  | "0" -> "operation succeeded"
  | "1" -> "requested key not found"
  | "2" -> "try lookup again later"
  | "3" -> "invalid request parameter"
  | "4" -> "table not approved for proxying"
  | s -> "unknown status: " ^ s

type result =
  { status : string
  ; value  : string list
  }

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
    let status = Hashtbl.find results "s" in
    let value = Hashtbl.find results "v" in
    { status = status; value = (Str.split sep_re value) }
  with Not_found ->
    failwith (sprintf "parse_result: missing keys")

let query key =
  let query_fmt = Config.proxymap_query_format () in
  let tables = Config.proxymap_lookup_tables () in
  let flags = Config.proxymap_query_flags () in
  let socket = Config.proxymap_query_socket () in
  let rec run_query = function
    | [] ->
        return None
    | t::ts ->
        let req = build_request query_fmt t flags key in
        lwt raw_res = make_request socket req in
        let res_fmt = Config.proxymap_result_format () in
        let sep = Config.proxymap_result_value_separator () in
        let res = parse_result raw_res res_fmt sep in
        if res.status = "0" then
          return (Some res.value)
        else begin
          lwt () =
            if res.status <> "1" then 
              let err = status_message res.status in
              Lwt_log.warning_f "Proxymap.query: %s" err
            else
              return () in
          run_query ts
        end in
  run_query tables

let (=~) s re =
  try
    ignore (Str.search_forward re s 0);
    true
  with Not_found ->
    false

let at_re = Str.regexp "@"

let is_remote user =
  let re = Config.proxymap_local_user_regexp () in
  let rec remote u =
    if u =~ re then
      return false
    else
      match_lwt query u with
      | None -> return true
      | Some aliases -> Lwt_list.exists_p remote aliases in
  remote user
