open Release_config_types

type t

val of_configuration : Release_config.t -> t
val listen_address_in : t -> string
val listen_address_out : t -> string
val srs_domain : t -> string option
val srs_secret_file : t -> Lwt_io.file_name
val srs_hash_max_age : t -> int
val srs_hash_length : t -> int
val srs_separator : t -> char
val debug_level : t -> int

val default_srs_hash_max_age : value option
val default_srs_hash_length : value option
val default_srs_hash_separator : value option
val default_debug_level : value option
