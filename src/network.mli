type t

exception Network_error of string

val of_string : string -> t
val includes : Unix.inet_addr -> t -> bool
