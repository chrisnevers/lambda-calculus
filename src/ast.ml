type op =
| Add
| Sub
| Fst
| Snd

let ppOp = function
| Add -> "+"
| Sub -> "-"
| Fst -> "fst"
| Snd -> "snd"

type exp =
| Var of string
| Abs of string * exp
| App of exp * exp
| Num of int
| Bool of bool
| Binop of op * exp * exp
| Unop of op * exp
| If of exp * exp * exp
| Pair of exp * exp
| Inl of exp
| Inr of exp
| Match of exp * exp * exp

let rec ppExp = function
| Var id -> id
| Abs (id, e) -> "(λ" ^ id ^ "." ^ ppExp e ^ ")"
| App (e, e') -> "[" ^ ppExp e ^ " " ^ ppExp e' ^ "]"
| Num i -> string_of_int i
| Bool true -> "True"
| Bool false -> "False"
| Binop (o, l, r) -> ppExp l ^ " " ^ ppOp o ^ " " ^ ppExp r
| Unop (o, e) -> ppOp o ^ " " ^ ppExp e
| If (c, t, e) -> "if " ^ ppExp c ^ " " ^ ppExp t ^ " " ^ ppExp e
| Pair (l, r) -> "(" ^ ppExp l ^ ", " ^ ppExp r ^ ")"
| Inl e -> "inl " ^ ppExp e
| Inr e -> "inr " ^ ppExp e
| Match (c, l, r) -> "match " ^ ppExp c ^ " | " ^ ppExp l ^ " | " ^ ppExp r

type ty =
| TyInt
| TyBool
| TyFn of ty * ty
| TyVar of string
| TyProd of ty * ty
| TySum of ty * ty

let rec ppTy = function
| TyInt -> "Int"
| TyBool -> "Bool"
| TyFn (t, t') -> ppTy t ^ " ⇒ " ^ ppTy t'
| TyVar id -> "'" ^ id
| TyProd (l, r) -> "(" ^ ppTy l ^ " × " ^ ppTy r ^ ")"
| TySum (l, r) -> "(" ^ ppTy l ^ " + " ^ ppTy r ^ ")"

let getNum = function
| Num n -> n

let isIntToIntOp = function
| Add | Sub -> true
| _ -> false
