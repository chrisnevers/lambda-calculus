open Ast

type con = {
  name: string;
  arity: int;
  span: int
}

let ppCon con =
"{ name: " ^ con.name ^ ", arity: " ^ string_of_int con.arity ^
", span: " ^ string_of_int con.span ^ " }"

type pat =
  | PVar of string
  | PLit of exp
  | PCon of con * pat list

let rec ppPat = function
  | PVar id -> "PVar " ^ id
  | PLit e -> "PLit " ^ ppExp e
  | PCon (c, p) -> "PCon (" ^ ppCon c ^ ", " ^ String.concat ", " (List.map ppPat p) ^ ")"

type mrule = pat * exp

let ppMRule = function
  | p, e -> ppPat p ^ " -> " ^ ppExp e

type _match = mrule list

let ppMatch ms = String.concat " | " (List.map ppMRule ms)

let exTrue = PCon ({ name="True"; arity=0; span=2 }, [])
let exFalse = PCon ({ name="False"; arity=0; span=2 }, [])

let tup args = PCon ({ name=""; span=1; arity=List.length args }, args)
let pair (x, y) = tup [x;y]

let nil = PCon ({ name="Nil"; arity=0; span= 2}, [])
let cons (a, b) = PCon ({ name="Cons"; arity=2; span=2 }, [a; b])

type termd =
  | Pos of con * termd list
  | Neg of con list

let addNeg (Neg nonset, con) = Neg(con :: nonset)

type context = (con * termd list) list

let augment = function
  | [], dsc -> []
  | (con, args):: rs, dsc -> (con, dsc :: args) :: rs

let norm = function
  | (con, args) :: rs -> augment (rs, Pos(con, List.rev args))

let rec buildDsc = function
  | [], dsc, [] -> dsc
  | (con, args) :: rs, dsc, (_, _, dargs) :: wrk ->
    buildDsc (rs, Pos (con, List.rev args @ (dsc :: dargs)), wrk)

let arityOf = function
  | { arity= n; _ } -> n

let tabulate f n =
  let rec aux = function
    | s when s = n -> []
    | s -> f s :: aux (s + 1)
  in
  aux 0

type staticmatch =
  | Yes
  | No
  | Maybe

let consEqual = function
  | { name= c; _}, c1 when c = c1 -> true
  | _ -> false

let staticmatch = function
  | { name=c1; _ }, Pos ({ name=c2;_ }, _) when c1 = c2 -> Yes
  | { name=c1; _ }, Pos ({ name=c2;_ }, _) -> No
  | { name=c1; _ }, Neg cs when List.exists (fun c -> consEqual (c, c1)) cs -> No
  | { name=c1; span=s; _ }, Neg cs when s = List.length cs + 1 -> Yes
  | _ -> Maybe

type access = Obj of exp | Sel of int * access

let rec ppAccess = function
| Obj e -> ppExp e
| Sel (i, a) -> "Sel (" ^ string_of_int i ^ ", " ^ ppAccess a ^ ")"

exception Error of string
let error msg = raise (Error msg)

type decision =
  | Failure
  | Success of exp
  | IfEq of access * exp * decision * decision

let rec ppDecision = function
| Failure -> "Failure"
| Success e -> "Success " ^ ppExp e
| IfEq (a, c, t, e) -> "IfEq(" ^ ppAccess a ^ ", " ^ ppExp c ^ ", " ^
  ppDecision t ^ ", " ^ ppDecision e ^ ")"

let matcher matchObj allMRules =
  print_endline @@ "Match Obj: " ^ ppAccess matchObj;
  print_endline @@ "Match Rules: " ^ ppMatch allMRules;
  let rec fail = function
    | dsc, [] -> print_endline "failure"; Failure
    | dsc, (p1, rhs1) :: rs -> print_endline "failure2";
      _match (p1, matchObj, dsc, [], [], rhs1, rs)
  and succeed = function
    | ctx, [], rhs, rules -> print_endline "succeed1"; Success rhs
    | ctx, work1 :: ws, rhs, rules -> begin
      print_endline "succeed2";
      match work1 with
      | [], [], [] -> succeed (norm ctx, ws, rhs, rules)
      | p1::ps, o1::os, d1 ::ds ->
        _match (p1, o1, d1, ctx, ((ps, os, ds)::ws), rhs, rules)
      end
  and _match = function
    | PVar _, obj, dsc, ctx, work, rhs, rules -> print_endline "var"; succeed (augment(ctx, dsc), work, rhs, rules)
    | PLit l, Obj l2, dsc, ctx, work, rhs, rules ->
      print_endline "lit";
      if l = l2 then succeed (augment(ctx, dsc), work, rhs, rules)
      else IfEq ( Obj l2,
                  l,
                  succeed (augment(ctx, dsc), work, rhs, rules),
                  fail (dsc, rules)
                )
    | PLit l, obj, dsc, ctx, work, rhs, rules ->
      IfEq ( obj,
                  l,
                  succeed (augment(ctx, dsc), work, rhs, rules),
                  fail (dsc, rules)
                )
    | PCon(pcon, pargs), obj, dsc, ctx, work, rhs, rules ->
      print_endline @@ "cons: " ^ string_of_int (arityOf pcon);
      let args f = tabulate f (arityOf pcon) in
      let getdargs = function
        | Neg _ -> args (fun _ -> Neg [])
        | Pos (con, dargs) -> dargs
      in
      let getToArgs () = print_endline "getToArgs"; args (fun i -> Sel (i + 1, obj)) in
      let succeed' = function
        | () -> print_endline "succeed'"; succeed ((pcon, []):: ctx, (pargs, getToArgs (), getdargs dsc):: work, rhs, rules)
      in
      let fail' newdsc =
        print_endline "fail'";
        fail (buildDsc (ctx, newdsc, work), rules)
      in
      begin match staticmatch (pcon, dsc) with
      | Yes -> print_endline "yes"; succeed' ()
      | No -> print_endline "no"; fail' dsc
      | Maybe ->  print_endline "maybe"; IfEq (obj, Str pcon.name, succeed' (), fail' (addNeg (dsc, pcon)))
      end
    | ow, obj, dsc, ctx, work, rhs, rules -> error @@ ppPat ow
  in
  fail (Neg [], allMRules)


let isLit = function
| Num _ | Str _ | Bool _ -> true
| _ -> false

let rec getCon = function
| Binop (Cons, l, r) -> PCon ({ name="Cons"; arity=2; span=2; }, [getCon l; getCon r])
| Nil -> PCon ({ name="Nil"; arity=0; span=2 }, [])
| Var id -> PVar id
| e when isLit e -> PLit e

let rec getRules = function
| [] -> []
| Rule (l, e) :: tl when isLit l -> (PLit l, e) :: getRules tl
| Rule (Var id, e) :: tl -> (PVar id, e) :: getRules tl
| Rule (Binop (Cons, l, r), e) :: tl ->
  let lCon = getCon l in
  print_endline @@ "LCon: " ^ ppPat lCon;
  let rCon = getCon r in
  print_endline @@ "RCon: " ^ ppPat rCon;
  (PCon ({ name="Cons"; arity=2; span=2; }, [lCon; rCon]), e) :: getRules tl

let rec getAccess = function
| Obj e -> e
| Sel (i, e) -> Binop (Nth, Num i, getAccess e)

let rec extractExp = function
| Failure  -> Err "No pattern matches"
| Success e -> e
| IfEq (a, c, t, e) ->
  If (Binop (ConEq, getAccess a, c), extractExp t, extractExp e)

let rec pm = function
  | Match (e, rules) ->
    let matchRules = getRules rules in
    print_endline @@ "Got rules";
    let decision = matcher (Obj e) matchRules in
    print_endline @@ ppDecision decision;
    extractExp decision
  | Let (id, e, b) -> Let (id, pm e, pm b)
  | ow -> ow
