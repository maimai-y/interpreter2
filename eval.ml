open Syntax

(* 実際の計算をする関数 *)
(* Eval.f : Syntax.t -> int *)
let rec f expr = match expr with
    Number (n) -> n
  | Op (arg1, Plus, arg2) ->
      f arg1 + f arg2
  | Op (arg1, Minus, arg2) ->
      f arg1 - f arg2
  | Op (arg1, Times, arg2) ->
      f arg1 * f arg2
