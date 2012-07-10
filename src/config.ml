type t =
  { user                   : string
  ; listen_address         : string
  ; log_level              : Lwt_log.level
  ; local_addresses        : Network.t list
  ; relay_addresses        : Network.t list
  ; fail_on_helo_temperror : bool
  }

let default_local_addresses =
  List.map Network.of_string
    [ "127.0.0.0/8"
    ; "::ffff:127.0.0.0/104"
    ]

let default_relay_addresses =
  List.map Network.of_string
    []

let default =
  { user                   = "andre"
  ; listen_address         = "inet:9999@127.0.0.1"
  ; log_level              = Lwt_log.Debug
  ; local_addresses        = default_local_addresses
  ; relay_addresses        = default_relay_addresses
  ; fail_on_helo_temperror = true
  }
