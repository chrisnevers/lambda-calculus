open Parser
open Lexer

let parseString input =
  let buffer = Lexing.from_string input in
  program token buffer
