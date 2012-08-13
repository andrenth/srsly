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
  let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.connect fd (Unix.ADDR_UNIX socket);
  let rec writen off rem =
    let n = Unix.write fd req off rem in
    let diff = rem - n in
    if diff > 0 then
      writen n diff in
  writen 0 (String.length req);
  let buf = String.create 1024 in
  let n = Unix.read fd buf 0 (String.length buf) in
  Unix.close fd;
  String.sub buf 0 n

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
      failwith (sprintf "parse_result: key {%s} already set" k)
    else
      Hashtbl.replace results k v in
  let rec scan i j =
    if i < String.length res && j < String.length fmt then
      match res.[i], fmt.[j] with
      | _, '{' -> get_key i (j+1)
      | a, b when a = b -> scan (i+1) (j+1)
      | a, b -> failwith (sprintf "parse_result: expected '%c', found '%c'" b a)
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
        None
    | t::ts ->
        let req = build_request query_fmt t flags key in
        let raw_res = make_request socket req in
        let res_fmt = Config.proxymap_result_format () in
        let sep = Config.proxymap_result_value_separator () in
        let res = parse_result raw_res res_fmt sep in
        if res.status = "0" then
          Some res.value
        else begin
          if res.status <> "1" then 
            Log.warning "Proxymap.query: %s" (status_message res.status);
          run_query ts
        end in
  run_query tables
