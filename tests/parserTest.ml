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
  let expected = Abs ("x", App (Var "x", Var "y")) in
  assert_equal ast expected

let parseAbsAscii () =
  let ast = parseString "\\ x . x y" in
  let expected = Abs ("x", App (Var "x", Var "y")) in
  assert_equal ast expected

let parseApp () =
  let ast = parseString "x y 5" in
  let expected = App (App (Var "x", Var "y"), Num 5) in
  assert_equal ast expected

let parseLet () =
  let ast = parseString "let x = \\ x . x in let y = \\ y . y in x y" in
  let app = App (Var "x", Var "y") in
  let absX = Abs ("x", Var "x") in
  let absY = Abs ("y", Var "y") in
  let expected = Let ("x", absX, Let ("y", absY, App (Var "x", Var "y"))) in
  assert_equal ast expected

let parseNum () =
  let ast = parseString "345" in
  assert_equal ast (Num 345)

let parseBool () =
  let ast = parseString "True" in
  assert_equal ast (Bool true);
  let ast = parseString "False" in
  assert_equal ast (Bool false)

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
  ]

let parserTests () = run_test_tt_main suite

