open Ast


(*                                    *)
(*     Type definitions               *)
(*                                    *)

type con = {
  name  : string;
  arity : int;
  span  : int
}

type pat =
| PVar  of string
| PLit  of exp
| PList of pat list
| PPair of pat * pat

type matchRule = pat * exp

type matchRules = matchRule list

type termDesc =
| Pos of con * termDesc list
| Neg of con list

type context = (con * termDesc list) list

type staticMatch = Yes | No | Maybe

type access =
| Obj of exp
| Nth of int * access
| Fst of access
| Snd of access

type decision =
| Failure
| Success of exp
| IfEq of access * exp * decision * decision
| LetPat of string * access * decision

(*                                    *)
(*     Pretty printers                *)
(*                                    *)

let ppCon con =
  "{ name: " ^ con.name ^ ", arity: " ^ string_of_int con.arity ^
  ", span: " ^ string_of_int con.span ^ " }"

let rec ppPat = function
  | PVar id -> "PVar " ^ id
  | PLit e -> "PLit " ^ ppExp e
  | PList ps -> "PList [" ^ String.concat ", " (List.map ppPat ps) ^ "]"
  | PPair (l, r) -> "PPair (" ^ ppPat l ^ ", " ^ ppPat r ^ ")"

let ppMatchRule = function
  | p, e -> ppPat p ^ " -> " ^ ppExp e

let ppMatchRules ms = String.concat " | " (List.map ppMatchRule ms)

let rec ppAccess = function
| Obj e -> ppExp e
| Nth (i, e) -> "Nth (" ^ string_of_int i ^ ", " ^ ppAccess e ^ ")"
| Fst e -> "Fst " ^ ppAccess e
| Snd e -> "Snd " ^ ppAccess e

let rec ppDecision = function
| Failure -> "Failure"
| Success e -> "Success " ^ ppExp e
| IfEq (a, c, t, e) -> "IfEq(" ^ ppAccess a ^ ", " ^ ppExp c ^ ", " ^
  ppDecision t ^ ", " ^ ppDecision e ^ ")"
| LetPat (id, a, d) -> "LetPat(" ^ id ^ ", " ^ ppAccess a ^ ", " ^
  ppDecision d ^ ")"

(*                                    *)
(*     Helper Functions               *)
(*                                    *)

let addNeg (Neg nonset, con) = Neg (con :: nonset)

let augment = function
| [], desc -> []
| (con, args) :: rs, desc -> (con, desc :: args) :: rs

(* let norm = function *)
(* | (con, args) :: rs -> augment (rs, Pos (con, List.rev args)) *)

let rec buildDesc = function
| [], desc, [] -> desc
| (con, args) :: rs, desc, (_,_,dargs) :: work ->
  buildDesc (rs, Pos (con, List.rev args @ (desc :: dargs)), work)

let arityOf con = con.arity

let tabulate f n =
  let rec aux = function
  | s when s = n -> []
  | s -> f s :: aux (s + 1)
  in
  aux 0

let consEqual = function
| con, conId when con.name = conId -> true
| _ -> false

let staticMatch = function
| con, Pos (con2, _) when con.name = con2.name -> Yes
| con, Pos _ -> No
| con, Neg cs when List.exists (fun c -> consEqual (c, con.name)) cs -> No
| con, Neg cs when con.span = List.length cs + 1 -> Yes
| _ -> Maybe

let matcher matchObj allRules =
  (* Failure *)
  let rec fail = function
  | desc, [] -> Failure
  | desc, (p, rhs) :: rules -> match' (p, matchObj, desc, [], [], rhs, rules)
  (* Succeed *)
  and succeed = function
  | ctx, [], rhs, rules -> Success rhs
  | ctx, work :: works, rhs, rules -> begin
    match work with
    | [], [], [] -> succeed (ctx, works, rhs, rules)
    | p :: ps, o :: os, d :: ds ->
      match' (p, o, d, ctx, (ps, os, ds) :: works, rhs, rules)
    end
  and match' = function
  | PVar "_", obj, desc, ctx, work, rhs, rules ->
    succeed (augment (ctx, desc), work, rhs, rules)
  | PVar id, obj, desc, ctx, work, rhs, rules ->
    LetPat (id, obj, succeed (augment (ctx, desc), work, rhs, rules))
  | PLit l, Obj ol, desc, ctx, work, rhs, rules when l = ol ->
    succeed (augment (ctx, desc), work, rhs, rules)
  | PLit l, Obj ol, desc, ctx, work, rhs, rules when l != ol ->
    fail (desc, rules)
  | PLit l, obj, desc, ctx, work, rhs, rules ->
    let s = succeed (augment (ctx, desc), work, rhs, rules) in
    let f = fail (desc, rules) in
    IfEq (obj, l, s, f)
  | PPair (l, r), Obj (Pair (l2, r2)), desc, ctx, work, rhs, rules ->
    succeed (augment (ctx, desc), ([l; r], [Obj l2; Obj r2], [Neg []; Neg []]) :: work, rhs, rules)
  | PPair (l, r), obj, desc, ctx, work, rhs, rules ->
    succeed (augment (ctx, desc), ([l; r], [Fst obj; Snd obj], [Neg []; Neg []]) :: work, rhs, rules)
  in
  (* Run it *)
  fail (Neg [], allRules)

let isLit = function
| Num _ | Str _ | Bool _ -> true
| _ -> false

let rec expToPat = function
| e when isLit e -> PLit e
| Var id -> PVar id
| Pair (l, r) -> PPair (expToPat l, expToPat r)

let rec getRules = function
| [] -> []
| Rule (l, e) :: tl when isLit l -> (PLit l, e) :: getRules tl
| Rule (Var id, e) :: tl -> (PVar id, e) :: getRules tl
| Rule (Pair (l, r), e) :: tl ->
  (PPair (expToPat l, expToPat r), e) :: getRules tl

let rec accessToExp = function
| Obj e -> e
| Nth (i, e) -> Binop (Nth, Num i, accessToExp e)
| Fst e -> Unop (Fst, accessToExp e)
| Snd e -> Unop (Snd, accessToExp e)

let rec extractExp = function
| Failure  -> Err "No pattern matches"
| Success e -> e
| IfEq (a, c, t, e) ->
  If (Binop (Equal, accessToExp a, c), extractExp t, extractExp e)
| LetPat (id, a, d) -> Let (Var id, accessToExp a, extractExp d)

let rec pm = function
  | Match (e, rules) ->
    let matchRules = getRules rules in
    print_endline @@ "Got rules";
    let decision = matcher (Obj e) matchRules in
    print_endline @@ ppDecision decision;
    extractExp decision
  | Let (id, e, b) -> Let (id, pm e, pm b)
  | ow -> ow
