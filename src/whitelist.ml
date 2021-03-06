open Util
open Release_lwt

module O = Release.Util.Option

let check_networks networks msg addr =
  let rec check = function
    | [] ->
        None
    | net::rest ->
        if Network.includes addr net then
          Some ("X-Comment", msg)
        else
          check rest in
  check networks

let check_local addr =
  check_networks
    (Milter_config.spf_local_whitelist ())
    "SPF not applicable to localhost connection - skipped check"
    addr

let check_relay addr =
  check_networks
    (Milter_config.spf_relay_whitelist ())
    "SPF skipped for whitelisted relay"
    addr

let check addr =
  O.choose (check_local addr) (check_relay addr)
