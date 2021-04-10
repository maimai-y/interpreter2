(* Value.t : プログラムの実行結果を表す型 *)
type t = VNumber of int
       | VError of string

(* 値をプログラムに埋め込む関数 *)
(* Value.to_syntax : Value.t -> Syntax.t  *)
let to_syntax v = match v with
    VNumber (n) -> Syntax.Number (n)
  | VError (s) -> failwith "Error value can't be converted to syntax."

(* プログラムの実行結果を文字列にする関数 *)
(* Value.to_string : Value.t -> string *)
let rec to_string value = match value with
    VNumber (n) -> string_of_int n
  | VError (s) -> "Error: " ^ s

(* プログラムの実行結果をプリントする関数 *)
(* Value.print : Value.t -> unit *)
let print exp =
  let str = to_string exp in
  print_string str
