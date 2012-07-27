type t

val of_configuration : Release_config.t -> t
val listen_address : t -> string
