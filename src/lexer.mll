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
  | "()"      { TUNIT }
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
  | "print"   { TPRINT }
  | '|'       { TBAR }
  | '='       { TEQ }
  | ';'       { TSEMI }
  | '"'       { readString (Buffer.create 17) lexbuf }
  | id as id  { TID id }
  | num as n  { TNUM (int_of_string n) }
  | eof       { TEOF }
  | _         { raise LexerError }

and readString buf = parse
  | '"'       { TSTR (Buffer.contents buf) }
  | "\\/"     { Buffer.add_char buf '/'; readString buf lexbuf }
  | "\\\\"    { Buffer.add_char buf '\\'; readString buf lexbuf }
  | "\\b"     { Buffer.add_char buf '\b'; readString buf lexbuf }
  | "\\f"     { Buffer.add_char buf '\012'; readString buf lexbuf }
  | "\\n"     { Buffer.add_char buf '\n'; readString buf lexbuf }
  | "\\r"     { Buffer.add_char buf '\r'; readString buf lexbuf }
  | "\\t"     { Buffer.add_char buf '\t'; readString buf lexbuf }
  | [^ '"''\\']+ as content
    { Buffer.add_string buf content; readString buf lexbuf }
  | eof       { raise LexerError }
  | _         { raise LexerError }
