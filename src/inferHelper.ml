open Ast


exception UnificationError of string
let uError msg = raise @@ UnificationError ("Typecheck Error: " ^ msg)

type s = S of string * ty

(* Solution pretty printers *)
let ppS = function S (l, r) -> l ^ " : " ^ ppTy r

let printSubs s =
  print_endline @@ "Solution: [" ^ String.concat ", "
  (List.map ppS s) ^ "]"

(* Lookup type associate with type var in list of subs *)
let rec lookupTy id = function
| S (l, ty) :: _ when l = id -> ty
| h :: tl -> lookupTy id tl
| [] -> TyVar id

(* Get the type associated with a type var in the list of subs *)
let rec getType subs = function
| TyVar id -> lookupTy id subs
| TyFn (l, r) -> TyFn (getType subs l, getType subs r)
| TyProd (l, r) -> TyProd (getType subs l, getType subs r)
| TySum (l, r) -> TySum (getType subs l, getType subs r)
| ow -> ow

(* Replaces all occurences of [replaceThis] with [withThis] in type *)
let rec substTy replaceThis withThis = function
| TyVar id when id = replaceThis -> withThis
| TyInt -> TyInt
| TyStr -> TyStr
| TyBool -> TyBool
| TyVar id -> TyVar id
| TyFn (l, r) ->
  TyFn (substTy replaceThis withThis l, substTy replaceThis withThis r)
| TyProd (l, r) ->
  TyProd (substTy replaceThis withThis l, substTy replaceThis withThis r)
| TySum (l, r) ->
  TySum (substTy replaceThis withThis l, substTy replaceThis withThis r)
| TyForAll (id, t) -> TyForAll (id, substTy replaceThis withThis t)

exception ConstraintError of string
let cError msg = raise @@ ConstraintError ("Typecheck Error: " ^ msg)

(* Constraint type *)
type c = Eq of ty * ty

(* Constraint pretty printers *)
let ppC = function Eq (t, t') -> ppTy t ^ " ≡ " ^ ppTy t'

let printCons c =
  print_endline @@ "Constraints: [" ^ String.concat ", "
  (List.map ppC c) ^ "]"

(* Lookup type associated with ty in constraints *)
let rec lookup ty = function
| Eq (l, r) :: tl when l = ty -> r
| h :: tl -> lookup ty tl
| [] -> cError ("cannot find type in Γ: " ^ ppTy ty)

(* Return a list of the free variables in a type *)
let rec freeVars env = function
| TyVar id when Hashtbl.mem env id -> []
| TyVar id -> id :: []
| TyForAll (id, t) -> Hashtbl.add env id (TyVar id);
                      freeVars env t
| TyFn (l, r) -> freeVars env l @ freeVars env r
| TyProd (l, r)-> freeVars env l @ freeVars env r
| TySum (l, r) -> freeVars env l @ freeVars env r
| _ -> []

(* Generalization function that makes [let] expressiosn polymorphic *)
let rec p t = function
| [] -> t
| a :: x when List.mem a (freeVars (Hashtbl.create 10) t) ->
  p (TyForAll (a, t)) x
| a :: x -> p t x

let rec monoTy ty subTy =
  match ty, subTy with
  | TyForAll (id, TyForAll (id2, dt)), TyProd (l, TyProd (l2, r)) ->
    monoTy (substTy id2 l (substTy id l2 dt)) r
  | TyForAll (id, TyForAll (id2, dt)), TyProd (l, r) ->
    substTy id2 l (substTy id r dt)
  | TyForAll (id, dt), subTy -> substTy id subTy dt
  | _ -> ty
