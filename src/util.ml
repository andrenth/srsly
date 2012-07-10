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

module type SETOFLIST = sig
  include Set.S
  val of_list : elt list -> t
end

module SetOfList (E : Set.OrderedType) : SETOFLIST with type elt = E.t = struct
  include Set.Make(E)
  let of_list =
    List.fold_left (fun s e -> add e s) empty
end
