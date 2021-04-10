open Syntax
open Value

(* 実際の計算をする関数 *)
(* Eval.f : Syntax.t -> (Value.type -> Value.type) -> Value.type *)
let rec f expr cont = match expr with
    Number (n) -> cont (VNumber (n))
  | Op (arg1, Plus, arg2) ->
      f arg1 (fun v1 ->
      f arg2 (fun v2 ->
      begin match (v1, v2) with
          (VNumber (n1), VNumber (n2)) -> cont (VNumber (n1 + n2))
        | (VError (s), _) -> VError (s)
        | (_, VError (s)) -> VError (s)
      end))
  | Op (arg1, Minus, arg2) ->
      f arg1 (fun v1 ->
      f arg2 (fun v2 ->
      begin match (v1, v2) with
          (VNumber (n1), VNumber (n2)) -> cont (VNumber (n1 - n2))
        | (VError(s), _) -> VError(s)
        | (_, VError(s)) -> VError(s)
      end))
  | Op (arg1, Times, arg2) ->
      f arg1 (fun v1 ->
      f arg2 (fun v2 ->
      begin match (v1, v2) with
          (VNumber (n1), VNumber (n2)) -> cont (VNumber (n1 * n2))
        | (VError(s), _) -> VError(s)
        | (_, VError(s)) -> VError(s)
      end))
  | Op (arg1, Divide, arg2) ->
      f arg1 (fun v1 ->
      f arg2 (fun v2 ->
      begin match (v1, v2) with
          (VNumber (n1), VNumber (n2)) ->
            if v2 = VNumber(0) then VError("Division by zero")
            else cont (VNumber (n1 / n2))
        | (VError(s), _) -> VError(s)
        | (_, VError(s)) -> VError(s)
      end))
