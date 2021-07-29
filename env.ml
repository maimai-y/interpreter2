type 'a t = 'a list

let empty = []

(* exceptions *)
exception UnboundVariable

(* offset : string -> xs -> int *)
let offset x xs =
  let rec loop xs n = match xs with
      [] -> raise UnboundVariable
    | first :: rest -> if x = first then n else loop rest (n + 1)
  in
  loop xs 0

let get xs vs x = List.nth vs (offset x xs)
