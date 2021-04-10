(* op_t : ２項演算子の型 *)
type op_t = Plus | Minus | Times | Divide

(* ２項演算子を文字列にする関数 *)
(* op_to_string : op_t -> string *)
let op_to_string op = match op with
    Plus -> " + "
  | Minus -> " - "
  | Times -> " * "
  | Divide -> " / "

(* Syntax.t : プログラムを表す型 *)
type t = Number of int
       | Op of t * op_t * t

(* プログラムを文字列にする関数 *)
(* Syntax.to_string : Syntax.t -> string *)
let rec to_string exp = match exp with
    Number (n) -> string_of_int n
  | Op (arg1, op, arg2) ->
	"(" ^ to_string arg1
	    ^ op_to_string op
	    ^ to_string arg2 ^ ")"

(* プログラムをプリントする関数 *)
(* Syntax.print : Syntax.t -> unit *)
let print exp =
  let str = to_string exp in
  print_string str
