open Util

type result = string option

let check_networks networks msg addr =
  let rec check = function
    | [] ->
        None
    | net::rest ->
        if Network.includes addr net then
          Some ("X-Comment: " ^ msg)
        else
          check rest in
  check networks

(* XXX *)
let config = Config.milter_in_default

let check_local addr =
  check_networks
    (Config.local_whitelist config)
    "SPF not applicable to localhost connection - skipped check"    
    addr

let check_relay addr =
  check_networks
    (Config.relay_whitelist config)
    "SPF skipped for whitelisted relay"
    addr

let check addr =
  check_local addr <|> check_relay addr
