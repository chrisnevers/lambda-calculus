open Ast

exception EvalError of string
let error msg = raise (EvalError msg)

module Env = Map.Make(String)

let ppEnv e = Env.iter (fun k v -> print_string @@ k ^ " " ^ ppExp v ^ ", ") e

let rec isValue = function
| Num _ | Bool _ | Abs _ | Str _ | Unit _ -> true
| Pair (l, r) when isValue l && isValue r -> true
| Inl v when isValue v -> true
| Inr v when isValue v -> true
| List _ -> true
| _ -> false

let rec eval env = function
| Nil -> List []
| m when isValue m -> env, m
| Var id -> begin match Env.find_opt id env with
  | Some e -> env, e
  | None -> error @@ id ^ " not bound."
  end
| App (Abs (Var x, m), n) when isValue n ->
  let env', n' = eval env n in
  let env' = Env.add x n' env' in
  eval env' m
| App (m, n) ->
  let env', n' = eval env n in
  let env', m' = eval env' m in
  eval env' @@ App (m', n')
| Abs (x, m) ->
  let env', m' = eval env m in
  env', Abs (x, m')
| Pair (m, n) ->
  let env', m' = eval env m in
  let env', n' = eval env' n in
  env', Pair (m', n')
| Let (Var id, m, n) ->
  let env', m' = eval env m in
  let env' = Env.add id m' env' in
  eval env' n
| Inl e ->
  let env', e' = eval env e in
  env', Inl e
| Inr e ->
  let env', e' = eval env e in
  env', Inr e
| Fix (Abs (Var x, m)) ->
  let env' = Env.add x (Fix (Abs (Var x, m))) env in
  eval env' m
| Case (c, l, r) ->
  let _, c' = eval env c in
  begin match c' with
  | Inl e -> eval env @@ App (l, e)
  | Inr e -> eval env @@ App (r, e)
  end
| If (c, t, e) ->
  let _, c' = eval env c in
  begin match c' with
  | Bool true  -> eval env t
  | Bool false -> eval env e
  end
| Unop (op, e) ->
  let _, e' = eval env e in
  begin match op, e' with
  | Fst, Pair (l, _) -> env, l
  | Snd, Pair (_, r) -> env, r
  | Hd, List [] -> error "Cannot perform hd empty list"
  | Tl, List [] -> error "Cannot perform tl empty list"
  | Hd, List (h::_) -> env, h
  | Tl, List (_::t) -> env, List t
  | Print, e -> print_endline @@ ppExp e; env, e
  end
| Binop (op, l, r) ->
  let _, l = eval env l in
  let _, r = eval env r in
  begin
    match op, l, r with
    | Add, Num l, Num r -> env, Num (l + r)
    | Sub, Num l, Num r -> env, Num (l - r)
    | Mul, Num l, Num r -> env, Num (l * r)
    | Div, Num l, Num r -> env, Num (l / r)
    | Equal, l, r -> env, Bool (l = r)
    | Cons, h, Nil -> env, List [h]
    | Cons, h, List t -> env, List (h :: t)
  end
| Err msg -> error msg
