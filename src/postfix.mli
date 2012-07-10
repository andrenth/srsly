type attrs

val instance : attrs -> string
val client_address : attrs -> string
val helo_name : attrs -> string
val sender : attrs -> string

val parse_attrs : Lwt_unix.file_descr -> attrs option Lwt.t
