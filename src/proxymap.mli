val is_remote_sender : string -> bool Lwt.t
val count_remote_final_rcpts : string list -> (string * int) list Lwt.t
