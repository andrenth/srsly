open Printf

let some = function
  | None -> invalid_arg "some"
  | Some x -> x

let maybe f = function
  | None -> ()
  | Some x -> f x

let either g f = function
  | None -> g ()
  | Some x -> f x

let default z f = function
  | None -> z
  | Some x -> f x

let inet_addr_of_sockaddr = function
  | Unix.ADDR_INET (a, _) -> a
  | Unix.ADDR_UNIX s -> invalid_arg ("inet_addr_of_sockaddr "^s)

let (<?>) opt z =
  match opt with
  | None -> z
  | Some x -> x

let (<|>) opt1 opt2 =
  match opt1 with
  | None -> opt2
  | x -> x

let map_opt f = function
  | None -> None
  | Some x -> Some (f x)

let applyn f x n =
  let rec apply z = function
    | 0 -> z
    | k -> apply (f z) (k - 1) in
  apply x n

let set_log_level level =
  Lwt_log.Section.set_level Lwt_log.Section.main level
