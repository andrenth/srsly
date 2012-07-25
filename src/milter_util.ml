open Printf

module SetOfList = struct
  module type S = sig
    include Set.S
    val of_list : elt list -> t
  end

  module Make (E : Set.OrderedType) : S with type elt = E.t = struct
    include Set.Make(E)
    let of_list =
      List.fold_left (fun s e -> add e s) empty
  end
end

module FlagSet = SetOfList.Make(struct
  type t = Milter.flag
  let compare = compare
end)

module StepSet = SetOfList.Make(struct
  type t = Milter.step
  let compare = compare
end)

let with_priv_data z ctx f =
  match Milter.getpriv ctx with
  | None -> z
  | Some p -> let p', r = f p in Milter.setpriv ctx p'; r

let log f fmt =
  ksprintf (fun s -> ignore (f s)) fmt

let debug fmt = log Lwt_log.debug fmt
let notice fmt = log Lwt_log.notice fmt
let info fmt = log Lwt_log.info fmt
let warning fmt = log Lwt_log.warning fmt
let error fmt = log Lwt_log.error fmt
