open Ast

let rec isValue = function
| Num _ | Bool _ | Abs _ -> true
| Inl v when isValue v -> true
| Inr v when isValue v -> true
| _ -> false

let rec subst old rep = function
| Num n -> Num n
| Bool b -> Bool b
| App (m, n) -> App (subst old rep m, subst old rep n)
| Abs (id, m) -> Abs (id, subst old rep m)
| Var id when id = old -> rep
| Var id -> Var id
| Binop (o, l, r) -> Binop (o, subst old rep l, subst old rep r)
| Inl e -> Inl (subst old rep e)
| Inr e -> Inr (subst old rep e)
| Match (c, l, r) -> Match (subst old rep c, subst old rep l, subst old rep r)
| If (c, t, e) -> If (subst old rep c, subst old rep t, subst old rep e)
| Fix e -> Fix e

let rec eval = function
| m when isValue m -> m
| App (Abs (x, m), n) when isValue n -> eval @@ subst x (eval n) m
| App (m, n) -> eval @@ App (eval m, eval n)
| Abs (x, m) -> Abs (x, eval m)
| Pair (m, n) -> Pair (eval m, eval n)
| Inl e -> Inl (eval e)
| Inr e -> Inr (eval e)
| Fix (Abs (x, m)) ->  eval @@ subst x (Fix (Abs (x, m))) m
| Match (c, l, r) -> begin match eval c with
  | Inl e -> eval @@ App (l, e)
  | Inr e -> eval @@ App (r, e)
  end
| If (c, t, e) -> begin match eval c with
  | Bool true -> eval t
  | Bool false -> eval e
  end
| Unop (op, e) -> begin match op, eval e with
  | Fst, Pair (l, _) -> l
  | Snd, Pair (_, r) -> r
  end
| Binop (op, l, r) ->
  match op, eval l, eval r with
  | Add, Num l, Num r -> Num (l + r)
  | Sub, Num l, Num r -> Num (l - r)
  | Mul, Num l, Num r -> Num (l * r)
  | Div, Num l, Num r -> Num (l / r)
  | Equal, Num l, Num r -> Bool (l = r)
