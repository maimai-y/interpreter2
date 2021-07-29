type trail = Idt | K of cont

and cont = fst_cont list

and fst_cont = COp1 of (Syntax.t * string list * t list * Syntax.op_t)
             | COp2 of (t * Syntax.op_t)
             | CIf of (Syntax.t * Syntax.t * string list * t list)
             | CLet of (string * Syntax.t * string list * t list)
             | CApp1 of (Syntax.t * string list * t list)
             | CApp2 of t
             | CCons of cont

(* Value.t : プログラムの実行結果を表す型 *)
and t = VNumber of int
       | VBool of bool
       | VClosure of (string * Syntax.t * string list * t list)
       | VClosureR of (string * string * Syntax.t * string list * t list)
       | VContSS0 of (cont * trail)
       | VContCC0 of (cont * trail)
       | VError of string

(* プログラムの実行結果を文字列にする関数 *)
(* Value.to_string : Value.t -> string *)
let rec to_string value = match value with
    VNumber (n) -> string_of_int n
  | VBool (b) -> if b then "true" else "false"
  | VClosure (x, t', xs', vs') -> "<fun>"
  | VClosureR (f, x, t1', xs', vs') -> "<funR>"
  | VContSS0 (c, t) -> "<ContSS0>"
  | VContCC0 (c, t) -> "<ContCC0>"
  | VError (s) -> "Error: " ^ s

(* プログラムの実行結果をプリントする関数 *)
(* Value.print : Value.t -> unit *)
let print exp =
  let str = to_string exp in
  print_string str
