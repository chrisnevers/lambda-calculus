%{
  open Ast

  let rec curryPair = function
  | h :: [] -> h
  | h :: t -> Pair (h, curryPair t)

%}

%token<string> TID
%token<int> TNUM
%token<bool> TBOOL
%token TLAMBDA
%token TDOT
%token TADD TSUB
%token TIF TELSE TTHEN
%token TLET TEQ TIN
%token TLPAREN TRPAREN
%token TCOMMA
%token TFST TSND
%token TEOF

%left TLET TIN
%left TLAMBDA TDOT
%left TIF TTHEN TELSE
%nonassoc TCOMMA
%nonassoc TEQ
%left TADD TSUB
%nonassoc TLPAREN TID TNUM TBOOL TFST TSND
%left APP

%start<Ast.exp> program

%%

program:
  | exp TEOF  { $1 }

exp:
  | TID                   { Var $1 }
  | TNUM                  { Num $1 }
  | TBOOL                 { Bool $1 }
  | TLAMBDA TID TDOT exp  { Abs ($2, $4) }
  | TLPAREN exp TRPAREN   { $2 }
  | exp TADD exp          { Binop (Add, $1, $3) }
  | exp TSUB exp          { Binop (Sub, $1, $3) }
  | TFST exp              { Unop (Fst, $2) }
  | TSND exp              { Unop (Snd, $2) }
  | TLET TID TEQ exp TIN exp    { App (Abs ($2, $6), $4) }
  | TIF exp TTHEN exp TELSE exp { If ($2, $4, $6) }
  | exp exp %prec APP    { App ($1, $2) }
  | TLPAREN exp TCOMMA separated_list(TCOMMA, exp) TRPAREN {
      match $4 with [] -> $2 | es -> Pair ($2, curryPair es)
    }
