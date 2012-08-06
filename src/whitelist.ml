open Util

module O = Release_option

let check_networks networks msg addr =
  let rec check = function
    | [] ->
        None
    | net::rest ->
        if Network.includes addr net then
          Some ("X-Comment:", msg)
        else
          check rest in
  check networks

let check_local addr =
  check_networks
    (Config.local_whitelist ())
    "SPF not applicable to localhost connection - skipped check"    
    addr

let check_relay addr =
  check_networks
    (Config.relay_whitelist ())
    "SPF skipped for whitelisted relay"
    addr

let check addr =
  O.choose (check_local addr) (check_relay addr)
