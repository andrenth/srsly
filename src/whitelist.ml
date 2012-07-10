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

let config = Config.default

let check_local addr =
  check_networks
    config.Config.local_addresses
    "SPF not applicable to localhost connection - skipped check"    
    addr

let check_relay addr =
  check_networks
    config.Config.relay_addresses
    "SPF skipped for whitelisted relay"
    addr

let check addr =
  check_local addr <|> check_relay addr
