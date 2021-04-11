type ('a, 'b) t = ('a * 'b) list

let empty = []

let extend env x v = (x, v) :: env

let rec get env x = match env with
    [] -> raise Not_found
  | (y, v) :: rest ->
      if x = y then v else get rest x
