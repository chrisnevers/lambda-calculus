open OUnit
open Parse
open Eval
open Ast

let evalNum () =
  let ast = eval @@ parseString "5" in
  assert_equal ast (Num 5)

let evalBool () =
  let ast = eval @@ parseString "True" in
  assert_equal ast (Bool true)

let evalAbs () =
  let ast = eval @@ parseString "\\ x . x" in
  assert_equal ast (Abs ("x", Var "x"))

let evalApp () =
  let ast = eval @@ parseString "(\\ x . x) 5" in
  assert_equal ast (Num 5)

let evalLet () =
  let ast = eval @@ parseString "let x = (\\ x . x) in let y = (\\ y . y) in x y" in
  assert_equal ast (Abs ("y", Var "y"))

let evalAdd () =
  let ast = eval @@ parseString "let x = \\ x . x + 5 in x 4" in
  assert_equal ast (Num 9)

let suite =
  "Tests" >:::
  [
    "evalNum"   >:: evalNum;
    "evalBool"  >:: evalBool;
    "evalAbs"   >:: evalAbs;
    "evalApp"   >:: evalApp;
    "evalLet"   >:: evalLet;
    "evalAdd"   >:: evalAdd;
  ]

let evalTests () = run_test_tt_main suite

