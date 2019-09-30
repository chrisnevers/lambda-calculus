type op =
| Add
| Sub

let ppOp = function
| Add -> "+"
| Sub -> "-"

type exp =
| Var of string
| Abs of string * exp
| App of exp * exp
| Num of int
| Bool of bool
| Binop of op * exp * exp
| If of exp * exp * exp

let rec ppExp = function
| Var id -> id
| Abs (id, e) -> "(λ" ^ id ^ "." ^ ppExp e ^ ")"
| App (e, e') -> "[" ^ ppExp e ^ " " ^ ppExp e' ^ "]"
| Num i -> string_of_int i
| Bool true -> "True"
| Bool false -> "False"
| Binop (o, l, r) -> ppExp l ^ " " ^ ppOp o ^ " " ^ ppExp r
| If (c, t, e) -> "if " ^ ppExp c ^ " " ^ ppExp t ^ " " ^ ppExp e

type ty =
| TyInt
| TyBool
| TyFn of ty * ty
| TyVar of string

let rec ppTy = function
| TyInt -> "Int"
| TyBool -> "Bool"
| TyFn (t, t') -> ppTy t ^ " ⇒ " ^ ppTy t'
| TyVar id -> "'" ^ id

let getNum = function
| Num n -> n

let isIntToIntOp = function
| Add | Sub -> true
| _ -> false
