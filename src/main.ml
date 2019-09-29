open Lexer
open Parser
open Ast

let rec repl () = try
  print_string "> ";
  let input = read_line () in
  let buffer = Lexing.from_string input in
  let ast = Parser.program token buffer in
  print_endline @@ ppExp ast;
  repl ()
  with
  | LexerError -> print_endline "Syntax Error"; repl ()

let _ = repl ()
