open Ast

module Env = Map.Make(String)

let rec isValue = function
| Num _ | Bool _ | Abs _ | Str _ | Unit _ -> true
| Pair (l, r) when isValue l && isValue r -> true
| Inl v when isValue v -> true
| Inr v when isValue v -> true
| _ -> false

let rec eval env = function
| m when isValue m -> m
| Var id -> Env.find id env
| App (Abs (x, m), n) when isValue n ->
  let env' = Env.add x (eval env n) env in
  eval env' m
| App (m, n) -> eval env @@ App (eval env m, eval env n)
| Abs (x, m) -> Abs (x, eval env m)
| Pair (m, n) -> Pair (eval env m, eval env n)
| Let (id, m, n) ->
  let env' = Env.add id (eval env m) env in
  eval env' n
| Inl e -> Inl (eval env e)
| Inr e -> Inr (eval env e)
| Fix (Abs (x, m)) ->
  let env' = Env.add x (Fix (Abs (x, m))) env in
  eval env' m
| Match (c, l, r) -> begin match eval env c with
  | Inl e -> eval env @@ App (l, e)
  | Inr e -> eval env @@ App (r, e)
  end
| If (c, t, e) -> begin match eval env c with
  | Bool true -> eval env t
  | Bool false -> eval env e
  end
| Unop (op, e) -> begin match op, eval env e with
  | Fst, Pair (l, _) -> l
  | Snd, Pair (_, r) -> r
  | Print, e -> print_endline @@ ppExp e; e
  end
| Binop (op, l, r) -> begin
  match op, eval env l, eval env r with
  | Add, Num l, Num r -> Num (l + r)
  | Sub, Num l, Num r -> Num (l - r)
  | Mul, Num l, Num r -> Num (l * r)
  | Div, Num l, Num r -> Num (l / r)
  | Equal, Num l, Num r -> Bool (l = r)
  | Cons, h, Inr Unit -> List [h]
  | Cons, h, List t -> List (h :: t)
end
