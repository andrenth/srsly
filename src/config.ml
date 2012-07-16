type policyd =
  { policyd_user                   : string
  ; policyd_listen_address         : string
  ; policyd_log_level              : Lwt_log.level
  ; policyd_local_whitelist        : Network.t list
  ; policyd_relay_whitelist        : Network.t list
  ; policyd_fail_on_helo_temperror : bool
  }

type milter =
  { milter_user                     : string
  ; milter_listen_address           : string
  ; milter_log_level                : Lwt_log.level
  ; milter_local_whitelist          : Network.t list
  ; milter_relay_whitelist          : Network.t list
  ; milter_reject_on_helo_temperror : bool
  ; milter_srs_domain               : string option
  ; milter_srs_secret               : string
  ; milter_srs_hash_max_age         : int
  ; milter_srs_hash_length          : int 
  ; milter_srs_separator            : SRS.separator
  }

type t
  = Milter_in of milter
  | Milter_out of milter
  | Policyd of policyd

let default_local_addresses =
  List.map Network.of_string
    [ "127.0.0.0/8"
    ; "::ffff:127.0.0.0/104"
    ]

let default_relay_addresses =
  List.map Network.of_string
    []

let user = function
  | Milter_in m | Milter_out m -> m.milter_user
  | Policyd p -> p.policyd_user

let listen_address = function
  | Milter_in m | Milter_out m -> m.milter_listen_address
  | Policyd p -> p.policyd_listen_address

let log_level = function
  | Milter_in m | Milter_out m -> m.milter_log_level
  | Policyd p -> p.policyd_log_level

let local_whitelist = function
  | Milter_in m | Milter_out m -> m.milter_local_whitelist
  | Policyd p -> p.policyd_local_whitelist

let relay_whitelist = function
  | Milter_in m | Milter_out m -> m.milter_relay_whitelist
  | Policyd p -> p.policyd_relay_whitelist

let fail_on_helo_temperror = function
  | Milter_in m | Milter_out m -> m.milter_reject_on_helo_temperror
  | Policyd p -> p.policyd_fail_on_helo_temperror

let srs_domain = function
  | Milter_in m | Milter_out m -> m.milter_srs_domain
  | Policyd p -> invalid_arg "Config.srs_domain: policyd mode has no SRS support"

let srs_secret = function
  | Milter_in m | Milter_out m -> m.milter_srs_secret
  | Policyd p -> invalid_arg "Config.srs_secret: policyd mode has no SRS support"

let srs_hash_max_age = function
  | Milter_in m | Milter_out m -> m.milter_srs_hash_max_age
  | Policyd p -> invalid_arg "Config.srs_hash_max_age: policyd mode has no SRS support"

let srs_hash_length = function
  | Milter_in m | Milter_out m -> m.milter_srs_hash_length
  | Policyd p -> invalid_arg "Config.srs_hash_length: policyd mode has no SRS support"

let srs_separator = function
  | Milter_in m | Milter_out m -> m.milter_srs_separator
  | Policyd p -> invalid_arg "Config.srs_separator: policyd mode has no SRS support"

let milter_in_default = Milter_in
  { milter_user                     = "andre"
  ; milter_listen_address           = "inet:9999@127.0.0.1"
  ; milter_log_level                = Lwt_log.Debug
  ; milter_local_whitelist          = default_local_addresses
  ; milter_relay_whitelist          = default_relay_addresses
  ; milter_reject_on_helo_temperror = true
  ; milter_srs_domain               = None
  ; milter_srs_secret               = "secret"
  ; milter_srs_hash_max_age         = 8
  ; milter_srs_hash_length          = 8
  ; milter_srs_separator            = SRS.Plus
  }

let milter_out_default = Milter_out
  { milter_user                     = "andre"
  ; milter_listen_address           = "inet:9998@127.0.0.1"
  ; milter_log_level                = Lwt_log.Debug
  ; milter_local_whitelist          = default_local_addresses
  ; milter_relay_whitelist          = default_relay_addresses
  ; milter_reject_on_helo_temperror = true
  ; milter_srs_domain               = None
  ; milter_srs_secret               = "secret"
  ; milter_srs_hash_max_age         = 8
  ; milter_srs_hash_length          = 8
  ; milter_srs_separator            = SRS.Plus
  }

let policyd_default = Policyd
  { policyd_user                   = "andre"
  ; policyd_listen_address         = "unix:/tmp/spf.socket"
  ; policyd_log_level              = Lwt_log.Debug
  ; policyd_local_whitelist        = default_local_addresses
  ; policyd_relay_whitelist        = default_relay_addresses
  ; policyd_fail_on_helo_temperror = true
  }
