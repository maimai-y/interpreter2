(* Value.t : プログラムの実行結果を表す型 *)
type t = VNumber of int
       | VBool of bool
       | VClosure of (t -> (t -> t) -> t)
       | VClosureR of (t -> t -> (t -> t) -> t)
       | VError of string
       | VC of (t -> (t -> t) -> t)
       | VHV of t
       | VH of (t -> (t -> t) -> t) * ((t -> (t -> t) -> t) -> (t -> t) -> t)

(* プログラムの実行結果を文字列にする関数 *)
(* Value.to_string : Value.t -> string *)
let rec to_string value = match value with
    VNumber (n) -> string_of_int n
  | VBool (b) -> if b then "true" else "false"
  | VClosure (f) -> "<fun>"
  | VClosureR (f) -> "<fun>"
  | VError (s) -> "Error: " ^ s
  | VC (f) -> "<fun>"
  | VH (_) -> "VH"
  | VHV (v) -> to_string v

(* プログラムの実行結果をプリントする関数 *)
(* Value.print : Value.t -> unit *)
let print exp =
  let str = to_string exp in
  print_string str
