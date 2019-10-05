open OUnit
open Parse
open Eval
open Ast

let evalNum () =
  let ast = eval Env.empty @@ parseString "5" in
  assert_equal ast (Num 5)

let evalBool () =
  let ast = eval Env.empty @@ parseString "True" in
  assert_equal ast (Bool true)

let evalAbs () =
  let ast = eval Env.empty @@ parseString "\\ x . x" in
  assert_equal ast (Abs ("x", Var "x"))

let evalApp () =
  let ast = eval Env.empty @@ parseString "(\\ x . x) 5" in
  assert_equal ast (Num 5)

let evalLet () =
  let ast = eval Env.empty @@ parseString "let x = (\\ x . x) in let y = (\\ y . y) in x y" in
  assert_equal ast (Abs ("y", Var "y"))

let evalAdd () =
  let ast = eval Env.empty @@ parseString "let x = \\ x . x + 5 in x 4" in
  assert_equal ast (Num 9)

let evalSwap () =
  let ast = eval Env.empty @@ parseString "let swap = \\ p . (fst (snd p), fst p) in swap (1, 2, 3)" in
  assert_equal ast (Pair (Num 2, Num 1))

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
  ]

let evalTests () = run_test_tt_main suite

