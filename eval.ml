open Syntax
open Value

(* 実際の計算をする関数 *)
(* Eval.f : Syntax.t -> Value.type *)
let rec f expr = match expr with
    Number (n) -> VNumber(n)
  | Op (arg1, Plus, arg2) ->
      let v1 = f arg1 in
      let v2 = f arg2 in
      begin match (v1, v2) with
          (VNumber (n1), VNumber (n2)) -> VNumber (n1 + n2)
        | (VError(s), _) -> VError(s)
        | (_, VError(s)) -> VError(s)
      end
  | Op (arg1, Minus, arg2) ->
      let v1 = f arg1 in
      let v2 = f arg2 in
      begin match (v1, v2) with
          (VNumber (n1), VNumber (n2)) -> VNumber (n1 - n2)
        | (VError(s), _) -> VError(s)
        | (_, VError(s)) -> VError(s)
      end
  | Op (arg1, Times, arg2) ->
      let v1 = f arg1 in
      let v2 = f arg2 in
      begin match (v1, v2) with
          (VNumber (n1), VNumber (n2)) -> VNumber (n1 * n2)
        | (VError(s), _) -> VError(s)
        | (_, VError(s)) -> VError(s)
      end
  | Op (arg1, Divide, arg2) ->
      let v1 = f arg1 in
      let v2 = f arg2 in
      begin match (v1, v2) with
          (VNumber (n1), VNumber (n2)) ->
            if v2 = VNumber(0) then VError("Division by zero")
            else VNumber (n1 / n2)
        | (VError(s), _) -> VError(s)
        | (_, VError(s)) -> VError(s)
      end
