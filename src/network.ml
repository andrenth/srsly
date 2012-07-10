open Printf

type t = Unix.inet_addr * int

let of_string s =
  match Str.split (Str.regexp "\\/") s with
  | [prefix; len] ->
      let prefix' = Unix.inet_addr_of_string prefix in
      let len = int_of_string len in
      let maxlen = match String.length ((Obj.magic prefix') : string) with
      | 4 -> 32
      | 16 -> 128
      | _ -> invalid_arg "Network.of_string" in
      if len < 0 || len > maxlen then
        invalid_arg "Network.of_string";
      (Unix.inet_addr_of_string prefix, len)
  | _ -> invalid_arg "Network.of_string"

let ipv4_mask = Uint32.of_int (-1)
let ipv6_mask = Uint128.of_int (-1)

let ipv4_of_string s =
  let ip = ref Uint32.zero in
  for i = 0 to 3 do
    let b = Uint32.of_int (int_of_char s.[i]) in
    ip := Uint32.logor !ip (Uint32.shift_left b (32 - (i + 1) * 8))
  done;
  !ip

let ipv6_of_string s =
  let ip = ref Uint128.zero in
  for i = 0 to 16 do
    let b = Uint128.of_int (int_of_char s.[i]) in
    ip := Uint128.logor !ip (Uint128.shift_left b (128 - (i + 1) * 8))
  done;
  !ip

let includes ip net =
  let prefix, len = net in
  let ip' : string = Obj.magic ip in
  let prefix' : string = Obj.magic prefix in
  match String.length prefix', String.length ip' with
  | 4, 4 ->
      let mask = Uint32.shift_left ipv4_mask (32 - len) in
      let prefix'' = ipv4_of_string prefix' in
      let ip'' = ipv4_of_string ip' in
      Uint32.logand prefix'' mask = Uint32.logand ip'' mask
  | 16, 16 ->
      let mask = Uint128.shift_left ipv6_mask (128 - len) in
      let prefix'' = ipv6_of_string prefix' in
      let ip'' = ipv6_of_string ip' in
      Uint128.logand prefix'' mask = Uint128.logand ip'' mask
  | _ ->
      false
