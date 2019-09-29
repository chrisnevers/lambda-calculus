open Ast

let rec isValue = function
  | Num _ | Bool _ | Var _ -> true
  | Abs (_, n) when isValue n -> true
  | _ -> false

let rec subst old rep = function
| Num n -> Num n
| Bool b -> Bool b
| App (m, n) -> App (subst old rep m, subst old rep n)
| Abs (id, m) -> Abs (id, subst old rep m)
| Var id when id = old -> rep
| Var id -> Var id

let rec eval = function
| m when isValue m -> m
| App (Abs (x, m), n) -> subst x (eval n) m
| App (m, n) -> App (eval m, eval n)
| Abs (x, m) -> Abs (x, eval m)
