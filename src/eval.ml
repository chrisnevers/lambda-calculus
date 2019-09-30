open Ast

let rec isValue = function
| Num _ | Bool _ | Abs _ -> true
| _ -> false

let rec subst old rep = function
| Num n -> Num n
| Bool b -> Bool b
| App (m, n) -> App (subst old rep m, subst old rep n)
| Abs (id, m) -> Abs (id, subst old rep m)
| Var id when id = old -> rep
| Var id -> Var id
| Binop (o, l, r) -> Binop (o, subst old rep l, subst old rep r)

let rec eval = function
| m when isValue m -> m
| App (Abs (x, m), n) when isValue n -> eval @@ subst x (eval n) m
| App (m, n) -> eval @@ App (eval m, eval n)
| Abs (x, m) -> Abs (x, eval m)
| If (c, t, e) -> begin match eval c with
  | Bool true -> eval t
  | Bool false -> eval e
  end
| Binop (op, l, r) ->
  match op, eval l, eval r with
  | Add, Num l, Num r -> Num (l + r)
  | Sub, Num l, Num r -> Num (l + r)
