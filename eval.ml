open Syntax
open Value

let idk v trl = match trl with
    Idt -> v
  | K (k) -> k v Idt

let rec cons : (t -> trail -> t) -> trail -> trail = fun k -> fun trl ->
  match trl with
      Idt -> K (k)
    | K (k') -> K (fun v -> fun t' -> k v (cons k' t'))

let atsign : trail -> trail -> trail = fun trl1 -> fun trl2 ->
  match trl1 with
      Idt -> trl2
    | K (k') -> cons k' trl2
 
(* 実際の計算をする関数 *)
(* Eval.g2 : Syntax.t -> (string, Value.t) Env.t -> (Value.type -> Value.type) -> Value.t *)
let rec g2 expr env cont trl = match expr with
    Number (n) -> cont (VNumber (n)) trl
  | Bool (b) -> cont (VBool (b)) trl
  | Var (x) ->
      begin try
        cont (Env.get env x) trl
      with Not_found -> VError ("Unbound variable: " ^ x) end
  | Op (arg1, op, arg2) ->
      g2 arg1 env (fun v1 -> fun trl1 ->
      g2 arg2 env (fun v2 -> fun trl2 ->
      begin match (v1, v2) with
          (VNumber (n1), VNumber (n2)) ->
            begin match op with
                Plus      -> cont (VNumber (n1 + n2)) trl2
              | Minus     -> cont (VNumber (n1 - n2)) trl2
              | Times     -> cont (VNumber (n1 * n2)) trl2
              | Divide    -> if n2 = 0 then VError "Division by zero"
                             else cont (VNumber (n1 / n2)) trl2
              | Equal     -> cont (VBool (n1 = n2)) trl2
              | NotEqual  -> cont (VBool (n1 <> n2)) trl2
              | Less      -> cont (VBool (n1 < n2)) trl2
              | LessEqual -> cont (VBool (n1 <= n2)) trl2
            end
        | (VError (s), _) -> VError (s)
        | (_, VError (s)) -> VError (s)
        | (_, _) -> VError ("Bad arguments to" ^ op_to_string op ^ ": " ^
                            Value.to_string v1 ^ ", " ^
                            Value.to_string v2)
      end) trl1 ) trl
  | If (p, t, e) ->
      g2 p env (fun v -> fun trl' ->
      begin match v with
          VBool (true) -> g2 t env cont trl'
        | VBool (false) -> g2 e env cont trl'
        | VError (s) -> VError (s)
        | _ -> VError ("Bad predicate for if: " ^
                       Value.to_string v)
      end) trl
  | Let (x, t1, t2) ->
      g2 t1 env (fun v1 -> fun trl' ->
      let new_env = Env.extend env x v1 in
      g2 t2 new_env cont trl') trl
  
  | Letrec (f, x, t1, t2) ->
      let new_env = Env.extend env f 
        (VClosureR (fun v1 -> fun v2 -> fun c -> fun trl' -> g2 t1 (Env.extend (Env.extend env x v2) f v1) c trl')) in
      g2 t2 new_env cont trl
  | Fun (x, t) ->
        cont (VClosure (fun v2 -> fun c -> fun trl' -> g2 t (Env.extend env x v2) c trl')) trl
  | App (t1, t2) ->
      g2 t1 env (fun v1 -> fun trl1 ->
      g2 t2 env (fun v2 -> fun trl2 ->
      begin match v1 with
          VClosure (f) -> f v2 cont trl2
        | VClosureR (f) -> f v1 v2 cont trl2
        | VError (s) -> VError (s)
        | _ -> VError ("Not a function: " ^
                        Value.to_string v1)
      end) trl1) trl
  | Try (t1, t2) ->
      let v1 = g2 t1 env idk Idt in
      begin match v1 with
          VError (s) -> g2 t2 env cont trl
        | _ -> cont v1 trl
      end

  | S (k, e) ->
      let c = fun v -> fun cont' -> fun trl' -> cont' (cont v trl) trl' in
      let new_env = Env.extend env k (VClosure c) in
      g2 e new_env idk Idt

  | Angle_bracket (e) -> cont (g2 e env idk Idt) trl

  | F (k, e) ->
      let c = fun v -> fun cont' -> fun trl' -> cont v (atsign trl (cons cont' trl')) in
      let new_env = Env.extend env k (VClosure c) in
      g2 e new_env idk Idt

  | Angle_bracket0 (e) -> VError ("Angle_bracket0は未実装")
