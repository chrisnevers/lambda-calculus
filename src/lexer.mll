{
  open Parser
  exception LexerError
}

let space = [' ''\r''\t']
let comment = "-- "[^'\n']*
let num = ['0'-'9']+
let idStart = ['a'-'z''A'-'Z''\'''?''!''_']|[^'\x00'-'\x7F']
let id = idStart (idStart|['0'-'9'])*

rule token = parse
  | comment   { token lexbuf }
  | space     { token lexbuf }
  | '\n'      { Lexing.new_line lexbuf; token lexbuf }
  | "λ"       { TLAMBDA }
  | '\\'      { TLAMBDA }
  | '.'       { TDOT }
  | '('       { TLPAREN }
  | ')'       { TRPAREN }
  | '+'       { TADD }
  | '-'       { TSUB }
  | '*'       { TMUL }
  | '/'       { TDIV }
  | ','       { TCOMMA }
  | "True"    { TBOOL true }
  | "False"   { TBOOL false }
  | "if"      { TIF }
  | "else"    { TELSE }
  | "then"    { TTHEN }
  | "let"     { TLET }
  | "in"      { TIN }
  | "fst"     { TFST }
  | "snd"     { TSND }
  | "inl"     { TINL }
  | "inr"     { TINR }
  | "match"   { TMATCH }
  | "rec"     { TREC }
  | '|'       { TBAR }
  | '='       { TEQ }
  | id as id  { TID id }
  | num as n  { TNUM (int_of_string n) }
  | eof       { TEOF }
  | _         { raise LexerError }
