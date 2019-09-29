type exp =
| Var of string
| Abs of string * exp
| App of exp * exp
| Num of int
| Bool of bool

let rec ppExp = function
| Var id -> id
| Abs (id, e) -> "λ" ^ id ^ "." ^ ppExp e
| App (e, e') -> ppExp e ^ " " ^ ppExp e'
| Num i -> string_of_int i
| Bool true -> "True"
| Bool false -> "False"

type ty =
| TyInt
| TyBool
| TyFn of ty * ty
| TyVar of int

let rec ppTy = function
| TyInt -> "Int"
| TyBool -> "Bool"
| TyFn (t, t') -> ppTy t ^ " -> " ^ ppTy t'
| TyVar i -> "'" ^ Char.escaped @@ Char.chr (97 + i)
