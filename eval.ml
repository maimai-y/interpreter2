open Syntax
open Value


let rec cons k s trl =
  match trl with
      Idt -> K (k, s)
    | K (k', s') -> K (CCons k :: k', VCons s :: s')

let atsign trl1 trl2 =
  match trl1 with
      Idt -> trl2
    | K (k, s) -> cons k s trl2

(* 実際の計算をする関数 *)
(* Eval.g2 : Syntax.t -> string list * t list -> Value.cont -> Value.trail -> (Value.cont * Value.trail) list -> Value.t *)
let rec g2 expr xs vs cont stack trl mc = match expr with
    Number (n) -> applyToCont cont stack (VNumber (n)) trl mc
  | Bool (b) -> applyToCont cont stack (VBool (b)) trl mc
  | Var (x) ->
      begin try
        applyToCont cont stack (Env.get xs vs x) trl mc
      with Env.UnboundVariable -> VError ("Unbound variable: " ^ x) end
  | Op (e1, op, e2) ->
      g2 e1 xs vs (COp1 (e2, xs, op) :: cont) ((VEnvVS vs) :: stack) trl mc
  | If (p, t, e) ->
      g2 p xs vs (CIf (t, e, xs) :: cont) ((VEnvVS vs) :: stack) trl mc
  | Let (x, t1, t2) ->
      g2 t1 xs vs (CLet (x, t2, xs) :: cont) ((VEnvVS vs) :: stack) trl mc
  | Letrec (f, x, t1, t2) ->
      let new_xs = f :: xs in
      let new_vs = (VClosureR (f, x, t1, xs, vs)) :: vs in
        g2 t2 new_xs new_vs cont stack trl mc
  | Fun (x, t) ->
      applyToCont cont stack (VClosure (x, t, xs, vs)) trl mc
  | App (t1, t2) ->
      g2 t1 xs vs (CApp1 (t2, xs) :: cont) (VEnvVS vs :: stack) trl mc
  | Try (t1, t2) ->
      let v1 = g2 t1 xs vs [] [] Idt [] in
      begin match v1 with
          VError (s) -> g2 t2 xs vs cont stack trl mc
        | _ -> applyToCont cont stack v1 trl mc
      end
  | Shift (k, e) ->
      let new_xs = k :: xs in
      let new_vs = VContSS0 (cont, stack, trl) :: vs in
      g2 e new_xs new_vs [] [] Idt mc
  | Control (k, e) ->
      let new_xs = k :: xs in
      let new_vs = VContCC0 (cont, stack, trl) :: vs in
      g2 e new_xs new_vs [] [] Idt mc
  | Shift0 (k, e) ->
      begin match mc with
          [] -> VError ("short of mc")
        | (cont0, stack0, t0) :: m0 ->
            let new_xs = k :: xs in
            let new_vs = VContSS0 (cont, stack, trl) :: vs in
            g2 e new_xs new_vs cont0 stack0 t0 m0
      end
  | Control0 (k, e) ->
      begin match mc with
          [] -> VError ("short of mc")
        | (cont0, stack0, t0) :: m0 ->
            let new_xs = k :: xs in
            let new_vs = VContCC0 (cont, stack, trl) :: vs in
            g2 e new_xs new_vs cont0 stack0 t0 m0
      end
  | Angle_bracket (e) -> g2 e xs vs [] [] Idt ((cont, stack, trl) :: mc)

and applyToCont cont stack = fun v trl mc -> match (cont, stack) with
      ([], []) -> 
        begin match trl with
            Idt -> begin match mc with
                    [] -> v
                  | (cont, stack, t) :: m -> applyToCont cont stack v t m end      
          | K (k, s) -> applyToCont k s v Idt mc
        end
    | (cont_fst :: cont_rest, stack_fst :: stack_rest) -> begin match (cont_fst, stack_fst) with
        (COp1 (e2, xs', op), VEnvVS (vs')) -> g2 e2 xs' vs' (COp2 (op) :: cont_rest) (v :: stack_rest) trl mc
      | (COp2 (op), v1) ->
          begin match (v1, v) with
              (VNumber (n1), VNumber (n2)) ->
              begin match op with
                  Plus      -> applyToCont cont_rest stack_rest (VNumber (n1 + n2)) trl mc
                  | Minus     -> applyToCont cont_rest stack_rest (VNumber (n1 - n2)) trl mc
                  | Times     -> applyToCont cont_rest stack_rest (VNumber (n1 * n2)) trl mc
                  | Divide    -> if n2 = 0 then VError ("Division by zero")
                              else applyToCont cont_rest stack_rest (VNumber (n1 / n2)) trl mc
                  | Equal     -> applyToCont cont_rest stack_rest (VBool (n1 = n2)) trl mc
                  | NotEqual  -> applyToCont cont_rest stack_rest (VBool (n1 <> n2)) trl mc
                  | Less      -> applyToCont cont_rest stack_rest (VBool (n1 < n2)) trl mc
                  | LessEqual -> applyToCont cont_rest stack_rest (VBool (n1 <= n2)) trl mc
              end
          | (VError (s), _) -> VError (s)
          | (_, VError (s)) -> VError (s)
          | (_, _) -> VError ("Bad arguments to" ^ op_to_string op ^ ": " ^
                              Value.to_string v1 ^ ", " ^
                              Value.to_string v)
          end
      | (CIf (t, e, xs'), VEnvVS vs') ->
          begin match v with
            VBool (true) -> g2 t xs' vs' cont_rest stack_rest trl mc
          | VBool (false) -> g2 e xs' vs' cont_rest stack_rest trl mc
          | VError (s) -> VError (s)
          | _ -> VError ("Bad predicate for if: " ^
                          Value.to_string v)
          end
      | (CLet (x, e2, xs'), VEnvVS vs') ->
          g2 e2 (x :: xs') (v :: vs') cont_rest stack_rest trl mc
      | (CApp1 (e2, xs'), VEnvVS vs') -> g2 e2 xs' vs' (CApp2 :: cont_rest) (v :: stack_rest) trl mc
      | (CApp2, v1) -> 
          begin match v1 with
            VClosure (x, e, xs', vs') -> g2 e (x :: xs') (v :: vs') cont_rest stack_rest trl mc
          | VClosureR (f, x, e1, xs', vs') -> g2 e1 (f :: x :: xs') (v1 :: v :: vs') cont_rest stack_rest trl mc
          | VContSS0 (cont'', stack'', trl') -> applyToCont cont'' stack'' v trl' ((cont_rest, stack_rest, trl) :: mc)
          | VContCC0 (cont'', stack'', trl') -> applyToCont cont'' stack'' v (atsign trl' (cons cont_rest stack_rest trl)) mc
          | VError (s) -> VError (s)
          | _ -> VError ("Not a function: " ^
                          Value.to_string v1)
          end
      | (CCons k, VCons s) -> applyToCont k s v (cons cont_rest stack_rest trl) mc
      | (_, _) -> failwith "Should not happen1"
      end
    | (_, _) -> failwith "Should not happen2"
