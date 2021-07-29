type trail = Idt | K of cont * stack

and cont = frame list

and frame = COp1 of (Syntax.t * string list * Syntax.op_t)
          | COp2 of (Syntax.op_t)
          | CIf of (Syntax.t * Syntax.t * string list)
          | CLet of (string * Syntax.t * string list)
          | CApp1 of (Syntax.t * string list)
          | CApp2
          | CCons of cont

and stack = v list

and v = VNumber of int
      | VBool of bool
      | VClosure of (string * Syntax.t * string list * v list)
      | VClosureR of (string * string * Syntax.t * string list * v list)
      | VContSS0 of (cont * stack * trail)
      | VContCC0 of (cont * stack * trail)
      | VError of string
      | VEnvVS of v list
      | VCons of stack

(* プログラムの実行結果を文字列にする関数 *)
(* Value.to_string : Value.t -> string *)
let rec to_string value = match value with
    VNumber (n) -> string_of_int n
  | VBool (b) -> if b then "true" else "false"
  | VClosure (x, t', xs', vs') -> "<fun>"
  | VClosureR (f, x, t1', xs', vs') -> "<funR>"
  | VContSS0 (c, s, t) -> "<ContSS0>"
  | VContCC0 (c, s, t) -> "<ContCC0>"
  | VError (s) -> "Error: " ^ s
  | VEnvVS (lst) -> "<VEnvVS>"
  | VCons (s) -> "<vcons>"

(* プログラムの実行結果をプリントする関数 *)
(* Value.print : Value.t -> unit *)
let print exp =
  let str = to_string exp in
  print_string str
