open Syntax
open Value

let idk v trl mc = match trl with
    Idt -> begin match mc with
              [] -> v
            | (Cont (cont), t) :: m -> cont v t m end      
  | K (k) -> k v Idt mc

let rec cons (Cont (k)) trl =
  match trl with
      Idt -> K (k)
    | K (k') -> K (fun v -> fun t' -> k v (cons (Cont (k')) t'))

let atsign trl1 trl2 =
  match trl1 with
      Idt -> trl2
    | K (k) -> cons (Cont (k)) trl2

(* 実際の計算をする関数 *)
(* Eval.g2 : Syntax.t -> (string, Value.t) Env.t -> (Value.type -> Value.type) -> Value.t *)
let rec g2 expr env conta trl mc = match conta with Cont (cont) -> match expr with
    Number (n) -> cont (VNumber (n)) trl mc
  | Bool (b) -> cont (VBool (b)) trl mc
  | Var (x) ->
      begin try
        cont (Env.get env x) trl mc
      with Not_found -> VError ("Unbound variable: " ^ x) end
  | Op (arg1, op, arg2) ->
      g2 arg1 env (Cont (fun v1 -> fun trl1 -> fun mc1 ->
      g2 arg2 env (Cont (fun v2 -> fun trl2 -> fun mc2 ->
      begin match (v1, v2) with
          (VNumber (n1), VNumber (n2)) ->
            begin match op with
                Plus      -> cont (VNumber (n1 + n2)) trl2 mc2
              | Minus     -> cont (VNumber (n1 - n2)) trl2 mc2
              | Times     -> cont (VNumber (n1 * n2)) trl2 mc2
              | Divide    -> if n2 = 0 then VError ("Division by zero")
                             else cont (VNumber (n1 / n2)) trl2 mc2
              | Equal     -> cont (VBool (n1 = n2)) trl2 mc2
              | NotEqual  -> cont (VBool (n1 <> n2)) trl2 mc2
              | Less      -> cont (VBool (n1 < n2)) trl2 mc2
              | LessEqual -> cont (VBool (n1 <= n2)) trl2 mc2
            end
        | (VError (s), _) -> VError (s)
        | (_, VError (s)) -> VError (s)
        | (_, _) -> VError ("Bad arguments to" ^ op_to_string op ^ ": " ^
                            Value.to_string v1 ^ ", " ^
                            Value.to_string v2)
      end)) trl1 mc1)) trl mc
  | If (p, t, e) ->
      g2 p env (Cont (fun v -> fun trl' -> fun mc' ->
      begin match v with
          VBool (true) -> g2 t env conta trl' mc'
        | VBool (false) -> g2 e env conta trl' mc'
        | VError (s) -> VError (s)
        | _ -> VError ("Bad predicate for if: " ^
                       Value.to_string v)
      end)) trl mc
  | Let (x, t1, t2) ->
      g2 t1 env (Cont (fun v1 -> fun trl' -> fun mc' ->
      let new_env = Env.extend env x v1 in
      g2 t2 new_env conta trl' mc')) trl mc
  
      
  | Letrec (f, x, t1, t2) ->
      let new_env = Env.extend env f 
        (VClosureR (fun v1 -> fun v2 -> fun c -> fun trl' -> fun mc' -> 
          g2 t1 (Env.extend (Env.extend env x v2) f v1) c trl' mc')) in
      g2 t2 new_env conta trl mc
  | Fun (x, t) ->
        cont (VClosure (fun v2 -> fun c -> fun trl' -> fun mc' -> g2 t (Env.extend env x v2) c trl' mc')) trl mc
  | App (t1, t2) ->
      g2 t1 env (Cont (fun v1 -> fun trl1 -> fun mc1 ->
      g2 t2 env (Cont (fun v2 -> fun trl2 -> fun mc2 ->
      begin match v1 with
          VClosure (f) -> f v2 conta trl2 mc2
        | VClosureR (f) -> f v1 v2 conta trl2 mc2
        | VError (s) -> VError (s)
        | _ -> VError ("Not a function: " ^
                        Value.to_string v1)
      end)) trl1 mc1)) trl mc
  | Try (t1, t2) ->
      let v1 = g2 t1 env (Cont (idk)) Idt [] in
      begin match v1 with
          VError (s) -> g2 t2 env conta trl mc
        | _ -> cont v1 trl mc
      end

  | Shift (k, e) ->
      let c = fun v -> fun conta' -> fun trl' -> fun mc' -> cont v trl ((conta', trl') :: mc') in
      let new_env = Env.extend env k (VClosure c) in
      g2 e new_env (Cont (idk)) Idt mc
  | Control (k, e) ->
        let c = fun v -> fun conta' -> fun trl' -> fun mc' ->
                  cont v (atsign trl (cons conta' trl')) mc' in
        let new_env = Env.extend env k (VClosure c) in
        g2 e new_env (Cont (idk)) Idt mc
  | Shift0 (k, e) ->
      begin match mc with 
      (cont0, t0) :: m0 ->
        let c = fun v -> fun conta' -> fun trl' -> fun mc' -> cont v trl ((conta', trl') :: mc') in
        let new_env = Env.extend env k (VClosure c) in
        g2 e new_env cont0 t0 m0
      end
  | Control0 (k, e) ->
      begin match mc with (cont0, t0) :: m0 ->
        let c = fun v -> fun conta' -> fun trl' -> fun mc' -> cont v (atsign trl (cons conta' trl')) mc' in
        let new_env = Env.extend env k (VClosure c) in
        g2 e new_env cont0 t0 m0
      end

  | Angle_bracket (e) -> g2 e env (Cont (idk)) Idt ((conta, trl) :: mc)
