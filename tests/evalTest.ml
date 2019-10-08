open OUnit
open Parse
open Eval
open Ast

let evalNum () =
  let _, ast = eval Env.empty @@ parseString "5" in
  assert_equal ast (Num 5)

let evalBool () =
  let _, ast = eval Env.empty @@ parseString "True" in
  assert_equal ast (Bool true)

let evalAbs () =
  let _, ast = eval Env.empty @@ parseString "\\ x . x" in
  assert_equal ast (Abs (Var "x", Var "x"))

let evalApp () =
  let _, ast = eval Env.empty @@ parseString "(\\ x . x) 5" in
  assert_equal ast (Num 5)

let evalLet () =
  let _, ast = eval Env.empty @@ parseString "let x = (\\ x . x) in let y = (\\ y . y) in x y" in
  assert_equal ast (Abs (Var "y", Var "y"))

let evalAdd () =
  let _, ast = eval Env.empty @@ parseString "let x = \\ x . x + 5 in x 4" in
  assert_equal ast (Num 9)

let evalSwap () =
  let _, ast = eval Env.empty @@ parseString "let swap = \\ p . (fst (snd p), fst p) in swap (1, 2, 3)" in
  assert_equal ast (Pair (Num 2, Num 1))

let evalList () =
  let _, ast = eval Env.empty @@ parseString "[1]" in
  assert_equal ast (List [Num 1])

let evalNil () =
  let _, ast = eval Env.empty @@ parseString "[]" in
  assert_equal ast (List [])

let evalHd () =
  let _, ast = eval Env.empty @@ parseString "hd [1]" in
  assert_equal ast (Num 1)

let evalTl () =
  let _, ast = eval Env.empty @@ parseString "tl [1]" in
  assert_equal ast (List [])

let evalIfThen () =
  let _, ast = eval Env.empty @@ parseString "if True then 1 else 0" in
  assert_equal ast (Num 1)

let evalIfElse () =
  let _, ast = eval Env.empty @@ parseString "if False then 1 else 0" in
  assert_equal ast (Num 0)

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
    "evalIfThen">:: evalIfThen;
    "evalIfElse">:: evalIfElse;
    "evalHd"    >:: evalHd;
    "evalTl"    >:: evalTl;
  ]

let evalTests () = run_test_tt_main suite

