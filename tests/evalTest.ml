open OUnit
open Parse
open Eval
open Ast

let evalNum () =
  let ast = eval (Hashtbl.create 10) @@ parseString "5" in
  assert_equal ast (Num 5)

let evalBool () =
  let ast = eval (Hashtbl.create 10) @@ parseString "True" in
  assert_equal ast (Bool true)

let evalAbs () =
  let ast = eval (Hashtbl.create 10) @@ parseString "\\ x . x" in
  assert_equal ast (Abs ("x", Var "x"))

let evalApp () =
  let ast = eval (Hashtbl.create 10) @@ parseString "(\\ x . x) 5" in
  assert_equal ast (Num 5)

let evalLet () =
  let ast = eval (Hashtbl.create 10) @@ parseString "let x = (\\ x . x) in let y = (\\ y . y) in x y" in
  assert_equal ast (Abs ("y", Var "y"))

let evalAdd () =
  let ast = eval (Hashtbl.create 10) @@ parseString "let x = \\ x . x + 5 in x 4" in
  assert_equal ast (Num 9)

let evalSwap () =
  let ast = eval (Hashtbl.create 10) @@ parseString "let swap = \\ p . (fst (snd p), fst p) in swap (1, 2, 3)" in
  assert_equal ast (Pair (Num 2, Num 1))

let evalList () =
  let ast = eval (Hashtbl.create 10) @@ parseString "[1]" in
  assert_equal ast (List [Num 1])

let evalNil () =
  let ast = eval (Hashtbl.create 10) @@ parseString "[]" in
  assert_equal ast Nil

let suite =
  "Tests" >:::
  [
    "evalNum"   >:: evalNum;
    "evalBool"  >:: evalBool;
    "evalAbs"   >:: evalAbs;
    "evalApp"   >:: evalApp;
    "evalLet"   >:: evalLet;
    "evalAdd"   >:: evalAdd;
    "evalSwap"  >:: evalSwap;
    "evalList"  >:: evalList;
    "evalNil"   >:: evalNil;
  ]

let evalTests () = run_test_tt_main suite

