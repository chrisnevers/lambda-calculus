open Lexer
open Parser
open Constraint
open Unify
open Eval
open Ast

let rec getType subs = function
| TyVar id -> lookupTy id subs
| TyFn (l, r) -> TyFn (getType subs l, getType subs r)
| TyProd (l, r) -> TyProd (getType subs l, getType subs r)
| TySum (l, r) -> TySum (getType subs l, getType subs r)
| ow -> ow

let interp buffer =
  let ast = Parser.program token buffer in
  let ty, constraints, _ = cg [] ast in
  let subs = u constraints [] in
  let ty = getType subs ty in
  let res = eval ast in
  print_endline @@ ppExp res ^ " : " ^ ppTy ty

let rec repl () = begin try
  print_string "> ";
  let input = read_line () in
  let buffer = Lexing.from_string input in
  interp buffer
  with
  | Error | LexerError -> print_endline "Syntax Error"
  | UnificationError msg -> print_endline msg
  | ConstraintError msg -> print_endline msg
  end;
  repl ()

let runFile file =
  let chan = open_in file in
  let buffer = Lexing.from_channel chan in
  interp buffer;
  close_in chan

let _ =
  if Array.length Sys.argv > 1 then
    runFile Sys.argv.(1)
  else
  try repl ()
  with End_of_file -> begin
    print_endline "";
    exit 0
  end
