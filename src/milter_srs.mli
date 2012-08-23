val current : unit -> SRS.t
val reload : string * string list -> unit
val serialize_secrets : string * string list -> string
val unserialize_secrets : string -> string * string list
