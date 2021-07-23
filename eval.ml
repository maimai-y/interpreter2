open Syntax
open Value


let rec cons k trl =
  match trl with
      Idt -> K (k)
    | K (k') -> K (fun v -> fun t' -> k v (cons k' t'))

let atsign trl1 trl2 =
  match trl1 with
      Idt -> trl2
    | K (k) -> cons k trl2

(* 実際の計算をする関数 *)
(* Eval.g2 : Syntax.t -> (string, Value.t) Env.t -> (Value.type -> Value.type) -> Value.t *)
let rec g2 expr env cont trl mc = match expr with
    Number (n) -> applyToCont cont (VNumber (n)) trl mc
  | Bool (b) -> applyToCont cont (VBool (b)) trl mc
  | Var (x) ->
      begin try
        applyToCont cont (Env.get env x) trl mc
      with Not_found -> VError ("Unbound variable: " ^ x) end
  | Op (e1, op, e2) ->
      g2 e1 env (COp1 (e2, env, op, cont)) trl mc
  | If (p, t, e) ->
      g2 p env (CIf (t, e, env, cont)) trl mc
  | Let (x, t1, t2) ->
      g2 t1 env (CLet (x, t2, cont, env)) trl mc
  | Letrec (f, x, t1, t2) ->
      let new_env = Env.extend env f (VClosureR (f, x, t1, env)) in
        g2 t2 new_env cont trl mc
  | Fun (x, t) ->
      applyToCont cont (VClosure (x, t, env)) trl mc
  | App (t1, t2) ->
      g2 t1 env (CApp1 (t2, env, cont)) trl mc
  | Try (t1, t2) ->
      let v1 = g2 t1 env C0 Idt [] in
      begin match v1 with
          VError (s) -> g2 t2 env cont trl mc
        | _ -> applyToCont cont v1 trl mc
      end
(*
  | Shift (k, e) ->
      let new_env = Env.extend env k (VContSS0 (conta, trl)) in
      g2 e new_env (Cont (idk)) Idt mc
  | Control (k, e) ->
      let new_env = Env.extend env k (VContCC0 (conta, trl)) in
      g2 e new_env (Cont (idk)) Idt mc
  | Shift0 (k, e) ->
      begin match mc with
          [] -> VError ("short of mc")
        | (cont0, t0) :: m0 ->
            let new_env = Env.extend env k (VContSS0 (conta, trl)) in
            g2 e new_env cont0 t0 m0
      end
  | Control0 (k, e) ->
      begin match mc with
          [] -> VError ("short of mc")
        | (cont0, t0) :: m0 ->
            let new_env = Env.extend env k (VContCC0 (conta, trl)) in
            g2 e new_env cont0 t0 m0
      end

  | Angle_bracket (e) -> g2 e env (Cont (idk)) Idt ((conta, trl) :: mc)
*)
  and applyToCont cont = fun v trl mc -> match cont with
      C0 -> 
        begin match trl with
            Idt -> begin match mc with
                    [] -> v
                  | (cont, t) :: m -> applyToCont cont v t m end      
          | K (k) -> k v Idt mc
        end
    | COp1 (e2, env', op, cont') -> g2 e2 env' (COp2 (v, op, cont')) trl mc
    | COp2 (v1, op, cont') ->
        begin match (v1, v) with
            (VNumber (n1), VNumber (n2)) ->
              begin match op with
                  Plus      -> applyToCont cont' (VNumber (n1 + n2)) trl mc
                | Minus     -> applyToCont cont' (VNumber (n1 - n2)) trl mc
                | Times     -> applyToCont cont' (VNumber (n1 * n2)) trl mc
                | Divide    -> if n2 = 0 then VError ("Division by zero")
                               else applyToCont cont' (VNumber (n1 / n2)) trl mc
                | Equal     -> applyToCont cont' (VBool (n1 = n2)) trl mc
                | NotEqual  -> applyToCont cont' (VBool (n1 <> n2)) trl mc
                | Less      -> applyToCont cont' (VBool (n1 < n2)) trl mc
                | LessEqual -> applyToCont cont' (VBool (n1 <= n2)) trl mc
              end
          | (VError (s), _) -> VError (s)
          | (_, VError (s)) -> VError (s)
          | (_, _) -> VError ("Bad arguments to" ^ op_to_string op ^ ": " ^
                              Value.to_string v1 ^ ", " ^
                              Value.to_string v)
        end
    | CIf (t, e, env', cont') ->
        begin match v with
            VBool (true) -> g2 t env' cont' trl mc
          | VBool (false) -> g2 e env' cont' trl mc
          | VError (s) -> VError (s)
          | _ -> VError ("Bad predicate for if: " ^
                        Value.to_string v)
        end
    | CLet (x, e2, cont', env') ->
        let new_env = Env.extend env' x v in
        g2 e2 new_env cont' trl mc
    | CApp1 (e2, env', cont') -> g2 e2 env' (CApp2 (v, cont')) trl mc
    | CApp2 (v1, cont') -> 
        begin match v1 with
            VClosure (x, e, env') -> g2 e (Env.extend env' x v) cont' trl mc
          | VClosureR (f, x, e1, env') -> g2 e1 (Env.extend (Env.extend env' x v) f v1) cont' trl mc
          (*
          | VContSS0 (cont', trl') -> cont' v2 trl' ((conta, trl2) :: mc2)
          | VContCC0 (cont', trl') -> cont' v2 (atsign trl' (cons conta trl2)) mc2

          | VError (s) -> VError (s) *)
          | _ -> VError ("Not a function: " ^
                          Value.to_string v1)
                          
        end
