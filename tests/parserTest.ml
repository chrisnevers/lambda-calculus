open OUnit
open Parse
open Lexer
open Ast

let parseIdUnicode () =
  let ast = parseString "Σ" in
  assert_equal ast (Var "Σ")

let parseIdAscii () =
  let ast = parseString "hello!?" in
  assert_equal ast (Var "hello!?")

let parseAbsUnicode () =
  let ast = parseString "λ x . x y" in
  let expected = Abs (Var "x", App (Var "x", Var "y")) in
  assert_equal ast expected

let parseAbsAscii () =
  let ast = parseString "\\ x . x y" in
  let expected = Abs (Var "x", App (Var "x", Var "y")) in
  assert_equal ast expected

let parseApp () =
  let ast = parseString "x y 5" in
  let expected = App (App (Var "x", Var "y"), Num 5) in
  assert_equal ast expected

let parseLet () =
  let ast = parseString "let x = \\ x . x in let y = \\ y . y in x y" in
  let app = App (Var "x", Var "y") in
  let absX = Abs (Var "x", Var "x") in
  let absY = Abs (Var "y", Var "y") in
  let expected = Let (Var "x", absX, Let (Var "y", absY, App (Var "x", Var "y"))) in
  assert_equal ast expected

let parseNum () =
  let ast = parseString "345" in
  assert_equal ast (Num 345)

let parseBool () =
  let ast = parseString "True" in
  assert_equal ast (Bool true);
  let ast = parseString "False" in
  assert_equal ast (Bool false)

let parseList () =
  let ast = parseString "[1, 2]" in
  assert_equal ast (Binop (Cons, Num 1, Binop (Cons, Num 2, Nil)))

let parseNil () =
  let ast = parseString "[]" in
  assert_equal ast Nil

let suite =
  "Tests" >:::
  [
    "parseIdUnicode" >:: parseIdUnicode;
    "parseIdAscii" >:: parseIdAscii;
    "parseAbsUnicode" >:: parseAbsUnicode;
    "parseAbsAscii" >:: parseAbsAscii;
    "parseApp" >:: parseApp;
    "parseLet" >:: parseLet;
    "parseNum" >:: parseNum;
    "parseBool" >:: parseBool;
    "parseList" >:: parseList;
    "parseNil"  >:: parseNil;
  ]

let parserTests () = run_test_tt_main suite

