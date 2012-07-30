open Release_config_types

type t

val of_configuration : Release_config.t -> t
val listen_address : t -> string
val num_slaves : t -> int
val default_num_slaves : value option
