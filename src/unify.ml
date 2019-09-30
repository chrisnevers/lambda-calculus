open Constraint
open Ast

exception UnificationError of string
let error msg = raise @@ UnificationError ("Typecheck Error: " ^ msg)

type s = S of string * ty

(* Solution pretty printers *)
let ppS = function S (l, r) -> l ^ " : " ^ ppTy r

let printSubs s =
  print_endline @@ "Solution: [" ^ String.concat ", "
  (List.map ppS s) ^ "]"

let rec lookupTy id = function
| S (l, ty) :: _ when l = id -> ty
| h :: tl -> lookupTy id tl
| [] -> error ("Unbound variable: " ^ id)

let rec substTy replaceThis withThis = function
| TyVar id when id = replaceThis -> withThis
| TyInt -> TyInt
| TyBool -> TyBool
| TyVar id -> TyVar id
| TyFn (l, r) ->
  TyFn (substTy replaceThis withThis l, substTy replaceThis withThis r)
| TyProd (l, r) ->
  TyProd (substTy replaceThis withThis l, substTy replaceThis withThis r)
| TySum (l, r) ->
  TySum (substTy replaceThis withThis l, substTy replaceThis withThis r)

(*
  Goes through the constraints and replaces occurences of the
  TyVar [replaceThis] with [withThis]
*)
let rec replaceCons replaceThis withThis c =
  let _rec = replaceCons replaceThis withThis in
  let _sub = substTy replaceThis withThis in
  match c with
  | [] -> []
  | Eq (TyVar l, r) :: tl when l = replaceThis ->
    Eq (withThis, _sub r) :: _rec tl
  | Eq (l, r) :: tl -> Eq (_sub l, _sub r) :: _rec tl

(*
  Goes through the solutions and replaces occurences of the
  TyVar [replaceThis] with [withThis]
*)
let rec replaceSol replaceThis withThis s =
  let _rec = replaceSol replaceThis withThis in
  let _sub = substTy replaceThis withThis in
  match s with
  | [] -> []
  | S (id, r) :: tl when id = replaceThis -> S (id, withThis) :: _rec tl
  | S (id, r) :: tl -> S (id, _sub r) :: _rec tl

(*
  U : [C] -> [S] -> [S]
  [C] - Set of constraints
  [S] - Set of solutions (A = T) from variables to types
*)
let rec u c s = match c with
(* No constraints to solve *)
| [] -> s
(*
  Preserve type variables that are equated to each other.
  This is to pretty print sum types, when we only discover one of it's types.
 *)
| Eq (TyVar a, TyVar b) :: c when a = b -> u c (s @ [S (a, TyVar b)])
(* Useless constraint, no info added *)
| Eq (t, t') :: c when t = t' -> u c s
(* Two functions/products must have same param and return type *)
| Eq (TyFn (t1, t2), TyFn (t3, t4)) :: c
| Eq (TyProd (t1, t2), TyProd (t3, t4)) :: c
| Eq (TySum (t1, t2), TySum (t3, t4)) :: c ->
  u (Eq (t1, t3) :: Eq (t2, t4) :: c) s
(*
  e.g. TyVar x ≡ Int
  Once we discover the type of a type variable, update the constraint and
  solution list with the new value, Add a record of the mapping to the solutions
*)
| Eq (TyVar a, t) :: c | Eq (t, TyVar a) :: c ->
  u (replaceCons a t c) (replaceSol a t s @ [S (a, t)])
| h :: _ -> error ("could not unify " ^ ppC h)
