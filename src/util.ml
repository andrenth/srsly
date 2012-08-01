open Lwt
open Printf

let inet_addr_of_sockaddr = function
  | Unix.ADDR_INET (a, _) -> a
  | Unix.ADDR_UNIX s -> invalid_arg ("inet_addr_of_sockaddr "^s)

let string_of_sockaddr = function
  | Unix.ADDR_INET (a, _) -> Unix.string_of_inet_addr a
  | Unix.ADDR_UNIX s -> s

let applyn f x n =
  let rec apply z = function
    | 0 -> z
    | k -> apply (f z) (k - 1) in
  apply x n

let set_log_level level =
  Lwt_log.Section.set_level Lwt_log.Section.main level

let warn fmt =
  ksprintf print_endline fmt

let err fmt =
  ksprintf (fun s -> prerr_endline s; exit 1) fmt
