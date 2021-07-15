(* op_t : ２項演算子の型 *)
type op_t = Plus | Minus | Times | Divide | Equal | NotEqual | Less | LessEqual

(* ２項演算子を文字列にする関数 *)
(* op_to_string : op_t -> string *)
let op_to_string op = match op with
    Plus -> " + "
  | Minus -> " - "
  | Times -> " * "
  | Divide -> " / "
  | Equal -> " = "
  | NotEqual -> " <> "
  | Less -> " < "
  | LessEqual -> " <= "

(* Syntax.t : プログラムを表す型 *)
type t = Number of int
       | Bool of bool
       | Var of string
       | Op of t * op_t * t
       | If of t * t * t
       | Let of string * t * t
       | Letrec of string * string * t * t
       | Fun of string * t
       | App of t * t
       | Try of t * t
       | Shift of string * t
       | Control of string * t
       | Shift0 of string * t
       | Control0 of string * t
       | Angle_bracket of t

(* プログラムを文字列にする関数 *)
(* Syntax.to_string : Syntax.t -> string *)
let rec to_string exp = match exp with
    Number (n) -> string_of_int n
  | Bool (b) -> if b then "true" else "false"
  | Var (x) -> x
  | Op (arg1, op, arg2) ->
        "(" ^ to_string arg1
            ^ op_to_string op
            ^ to_string arg2 ^ ")"
  | If (p, t, e) ->
        "(if " ^ to_string p ^
        " then " ^ to_string t ^
        " else " ^ to_string e ^ ")"
  | Let (x, t1, t2) ->
        "(let " ^ x ^ " = " ^ to_string t1 ^
        " in " ^ to_string t2 ^ ")"
  | Letrec (f, x, t1, t2) ->
        "(let rec " ^ f ^ " " ^ x ^ " = " ^ to_string t1 ^
        " in " ^ to_string t2 ^ ")"
  | Fun (x, t) ->
        "(fun " ^ x ^ " -> " ^ to_string t ^ ")"
  | App (t1, t2) ->
        "(" ^ to_string t1 ^ " " ^ to_string t2 ^ ")"
  | Try (t1, t2) ->
        "(try " ^ to_string t1 ^ " with _ -> " ^ to_string t2 ^ ")"
  | Shift (x, t) ->
        "(shift " ^ x ^ " -> " ^ to_string t ^ ")"
  | Control (x, t) ->
        "(control " ^ x ^ " -> " ^ to_string t ^ ")"
  | Shift0 (x, t) ->
        "(shift0 " ^ x ^ " -> " ^ to_string t ^ ")"
  | Control0 (x, t) ->
        "(control0 " ^ x ^ " -> " ^ to_string t ^ ")"
  | Angle_bracket (t) ->
        "< " ^ to_string t ^ ">0"

(* プログラムをプリントする関数 *)
(* Syntax.print : Syntax.t -> unit *)
let print exp =
  let str = to_string exp in
  print_string str
