open Syntax
open Value

let id = fun v -> VHV (v)

let rec hr_stop x cont' = match x with
    VH (c, f) -> hr_stop (f c id) cont'
  | VHV (v) -> cont' v
  | _ -> VError ("Bad arguments to hr_stop")

let rec hs_stop x cont' = match x with
    VH (c, f) -> hs_stop (f c id) cont'
  | VHV (v) -> cont' v
  | _ -> VError ("Bad arguments to hs_stop")

let rec hr_prop x cont' = match x with
    VH (c, f) -> f c cont'
  | VHV (v) -> cont' v
  | _ -> VError ("Bad arguments to hr_prop")

let rec hs_prop x cont' = match x with
    VH (c, f) -> VH ((fun x -> fun cont'' -> hs_prop (c x cont') cont''), f)
  | VHV (v) -> cont' v
  | _ -> VError ("Bad arguments to hs_prop")

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
      let new_env = Env.extend env f 
        (VClosureR (fun v1 -> fun v2 -> fun c -> g2 t1 (Env.extend (Env.extend env x v2) f v1) c)) in
      g2 t2 new_env cont
      (* let rec を使った別解 *)
      (* let rec vclo = VClosure (fun v -> fun c -> g2 t1 (Env.extend (Env.extend env x v) f vclo) c) in
      let new_env = Env.extend env f vclo in
      g2 t2 new_env cont *)
  | Fun (x, t) ->
      cont (VClosure (fun v2 -> fun c -> g2 t (Env.extend env x v2) c))
  | App (t1, t2) ->
      g2 t1 env (fun v1 ->
      g2 t2 env (fun v2 ->
      begin match v1 with
          VClosure (f) -> f v2 cont
        | VClosureR (f) -> f v1 v2 cont
        | VError (s) -> VError (s)
        | VC (f) -> f v2 cont
        | _ -> VError ("Not a function: " ^
                       Value.to_string v1)
      end))
  | Try (t1, t2) ->
      let v1 = g2 t1 env (fun x -> x) in
      begin match v1 with
          VError (s) -> g2 t2 env cont
        | _ -> cont v1
      end

  | S (k, e) ->
      let c = fun x -> fun cont' -> hs_stop (cont x) cont' in
      let f = fun c -> fun cont' -> g2 e (Env.extend env k (VC c)) cont' in
      VH (c, f)

  | Angle_bracket (e) -> hr_stop (g2 e env id) cont

  | F (k, e) ->
      let c = fun x -> fun cont' -> hs_prop (cont x) cont' in
      let f = fun c -> fun cont' -> g2 e (Env.extend env k (VC c)) cont' in
      VH (c, f)

  | Angle_bracket0 (e) -> hr_prop (g2 e env id) cont

(* Eval.f : Syntax.t -> (string, Value.t) Env.t -> Value.t *)
let f expr env = g2 expr env
