open Lexer
open Parser
open Constraint
open Unify
open Eval
open Ast

let getType subs = function
| TyVar id -> lookupTy id subs
| ow -> ow

let rec repl () = begin try
  print_string "> ";
  let input = read_line () in
  let buffer = Lexing.from_string input in
  let ast = Parser.program token buffer in
  print_endline @@ ppExp ast;
  let ty, constraints, _ = cg [] ast in
  let subs = u constraints [] in
  let ty = getType subs ty in
  let res = eval ast in
  print_endline @@ ppExp res ^ " : " ^ ppTy ty;
  with
  | Error | LexerError -> print_endline "Syntax Error"
  | UnificationError msg -> print_endline msg
  | ConstraintError msg -> print_endline msg
  end;
  repl ()

let _ =
  try repl ()
  with End_of_file -> begin
    print_endline "";
    exit 0
  end
