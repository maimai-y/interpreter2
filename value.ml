type trail = Idt | K of (t -> trail -> (cont * trail) list -> t)

(* and cont = CInit
 *          | CValue of (v)
 *          | COp of (v, op) *)

and cont = Cont of (t -> trail -> (cont * trail) list -> t)

(* Value.t : プログラムの実行結果を表す型 *)
and t = VNumber of int
       | VBool of bool
       | VClosure of (string * Syntax.t * (string, t) Env.t)
       | VClosureR of (string * string * Syntax.t * (string, t) Env.t)
       | VContSS0 of (cont * trail)
       | VContCC0 of (cont * trail)
       | VError of string

(* プログラムの実行結果を文字列にする関数 *)
(* Value.to_string : Value.t -> string *)
let rec to_string value = match value with
    VNumber (n) -> string_of_int n
  | VBool (b) -> if b then "true" else "false"
  | VClosure (x, t', env') -> "<fun>"
  | VClosureR (f, x, t1', env') -> "<funR>"
  | VContSS0 (c, t) -> "<ContSS0>"
  | VContCC0 (c, t) -> "<ContCC0>"
  | VError (s) -> "Error: " ^ s

(* プログラムの実行結果をプリントする関数 *)
(* Value.print : Value.t -> unit *)
let print exp =
  let str = to_string exp in
  print_string str
