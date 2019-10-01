open Ast
open Gensym

exception ConstraintError of string
let error msg = raise @@ ConstraintError ("Typecheck Error: " ^ msg)

(* Constraint type *)
type c = Eq of ty * ty

(* Constraint pretty printers *)
let ppC = function Eq (t, t') -> ppTy t ^ " → " ^ ppTy t'

let printCons c =
  print_endline @@ "Constraints: [" ^ String.concat ", "
  (List.map ppC c) ^ "]"

(* Lookup type associated with ty *)
let rec lookup ty = function
| Eq (l, r) :: tl when l = ty -> r
| h :: tl -> lookup ty tl
| [] -> error ("cannot find type in Γ: " ^ ppTy ty)

(* Generate type constraints *)
let rec cg gamma = function
| Abs (id, m) ->
  let freshVar = Gensym.gen_str "a" in
  let freshTy = TyVar freshVar in
  let gamma' = Eq (TyVar id, freshTy) :: gamma in
  let mTy, mCon, mVar = cg gamma' m in
  TyFn (freshTy, mTy), mCon, mVar @ [freshVar]
| App (m, n) ->
  let t1, c1, a1 = cg gamma m in
  let d, c2, a2  = cg gamma n in
  let r = Gensym.gen_str "a" in
  TyVar r, c1 @ c2 @ [Eq (t1, TyFn (d, TyVar r))], a1 @ a2 @ [r]
| Num _ -> TyInt, [], []
| Bool _ -> TyBool, [], []
| Var id -> lookup (TyVar id) gamma, [], []
| Binop (op, l, r) when isIntToIntOp op ->
  let t1, c1, a1 = cg gamma l in
  let t2, c2, a2 = cg gamma r in
  TyInt, c1 @ c2 @ [Eq (t1, TyInt); Eq (t2, TyInt)], a1 @ a2
| Binop (op, l, r) when isIntToBoolOp op ->
  let t1, c1, a1 = cg gamma l in
  let t2, c2, a2 = cg gamma r in
  TyBool, c1 @ c2 @ [Eq (t1, TyInt); Eq (t2, TyInt)], a1 @ a2
| Unop (Fst, e) ->
  let t1, c1, a1 = cg gamma e in
  let a = Gensym.gen_str "a" in
  let b = Gensym.gen_str "b" in
  TyVar a, c1 @ [Eq (t1, TyProd (TyVar a, TyVar b))], a1 @ [a; b]
| Unop (Snd, e) ->
  let t1, c1, a1 = cg gamma e in
  let a = Gensym.gen_str "a" in
  let b = Gensym.gen_str "b" in
  TyVar b, c1 @ [Eq (t1, TyProd (TyVar a, TyVar b))], a1 @ [a; b]
| If (c, t, e) ->
  let t1, c1, a1 = cg gamma c in
  let t2, c2, a2 = cg gamma t in
  let t3, c3, a3 = cg gamma e in
  let r = Gensym.gen_str "a" in
  TyVar r, c1 @ c2 @ c3 @ [Eq(t1, TyBool); Eq (t2, t3); Eq (t2, TyVar r); Eq (t2, TyVar r)], a1 @ a2 @ [r]
| Pair (l, r) ->
  let t1, c1, a1 = cg gamma l in
  let t2, c2, a2 = cg gamma r in
  TyProd (t1, t2), c1 @ c2, a1 @ a2
| Inl e ->
  let t1, c1, a1 = cg gamma e in
  let a = Gensym.gen_str "a" in
  let b = Gensym.gen_str "b" in
  TySum (TyVar a, TyVar b), c1 @ [Eq (t1, TyVar a); Eq (TyVar b, TyVar b)], a1 @ [a; b]
| Inr e ->
  let t1, c1, a1 = cg gamma e in
  let a = Gensym.gen_str "a" in
  let b = Gensym.gen_str "b" in
  TySum (TyVar a, TyVar b), c1 @ [Eq (t1, TyVar b); Eq (TyVar a, TyVar a)], a1 @ [a; b]
| Match (c, l, r) ->
  let t1, c1, a1 = cg gamma c in
  let t2, c2, a2 = cg gamma l in
  let t3, c3, a3 = cg gamma r in
  let v = Gensym.gen_str "s" in
  let v1 = Gensym.gen_str "a" in
  let v2 = Gensym.gen_str "b" in
  TyVar v,
  c1 @ c2 @ c3 @ [Eq (t1, TySum (TyVar v1, TyVar v2)); Eq (t2, TyFn (TyVar v1, TyVar v)); Eq (t3, TyFn (TyVar v2, TyVar v))],
  a1 @ a2 @ a3 @ [v; v1; v2]
| Fix e ->
  let t1, c1, a1 = cg gamma e in
  let a = Gensym.gen_str "b" in
  TyVar a, c1 @ [Eq (t1, TyFn (TyVar a, TyVar a))], a1 @ [a]
