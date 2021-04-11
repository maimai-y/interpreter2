open Syntax
open Value

(* 実際の計算をする関数 *)
(* Eval.g2 : Syntax.t -> (string, Value.t) Env.t -> (Value.type -> Value.type) -> Value.t *)
let rec g2 expr env cont = match expr with
    Number (n) -> cont (VNumber (n))
  | Bool (b) -> cont (VBool (b))
  | Var (x) ->
      begin try
        cont (Env.get env x)
      with Not_found -> VError ("Unbound variable: " ^ x) end
  | Op (arg1, op, arg2) ->
      g2 arg1 env (fun v1 ->
      g2 arg2 env (fun v2 ->
      begin match (v1, v2) with
          (VNumber (n1), VNumber (n2)) ->
            begin match op with
                Plus      -> cont (VNumber (n1 + n2))
              | Minus     -> cont (VNumber (n1 - n2))
              | Times     -> cont (VNumber (n1 * n2))
              | Divide    -> if n2 = 0 then VError "Division by zero"
                             else cont (VNumber (n1 / n2))
              | Equal     -> cont (VBool (n1 = n2))
              | NotEqual  -> cont (VBool (n1 <> n2))
              | Less      -> cont (VBool (n1 < n2))
              | LessEqual -> cont (VBool (n1 <= n2))
            end
        | (VError (s), _) -> VError (s)
        | (_, VError (s)) -> VError (s)
        | (_, _) -> VError ("Bad arguments to" ^ op_to_string op ^ ": " ^
                            Value.to_string v1 ^ ", " ^
                            Value.to_string v2)
      end))
  | If (p, t, e) ->
      g2 p env (fun v ->
      begin match v with
          VBool (true) -> g2 t env cont
        | VBool (false) -> g2 e env cont
        | VError (s) -> VError (s)
        | _ -> VError ("Bad predicate for if: " ^
                       Value.to_string v)
      end)
  | Let (x, t1, t2) ->
      g2 t1 env (fun v1 ->
      let new_env = Env.extend env x v1 in
      g2 t2 new_env cont)
  | Letrec (f, x, t1, t2) ->
      let v1 = VClosureR (f, x, t1, env) in
      let new_env = Env.extend env f v1 in
      g2 t2 new_env cont
  | Fun (x, t) ->
      cont (VClosure (x, t, env))
  | App (t1, t2) ->
      g2 t1 env (fun v1 ->
      g2 t2 env (fun v2 ->
      begin match v1 with
          VClosure (x, t, e) -> g2 t (Env.extend e x v2) cont
        | VClosureR (f, x, t, e) ->
            g2 t (Env.extend (Env.extend e x v2) f v1) cont
        | VError (s) -> VError (s)
        | _ -> VError ("Not a function: " ^
                       Value.to_string v1)
      end))
  | Try (t1, t2) ->
      let v1 = g2 t1 env (fun x -> x) in
      begin match v1 with
          VError (s) -> g2 t2 env cont
        | _ -> cont v1
      end
  | Shift (x, t) -> VError "Shift not supported yet."
  | Reset (t) -> VError "Reset not supported yet."

(* Eval.f : Syntax.t -> (string, Value.t) Env.t -> Value.t *)
let f expr env = g2 expr env
