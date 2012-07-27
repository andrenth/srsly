type t

val of_string : string -> t
val includes : Unix.inet_addr -> t -> bool
