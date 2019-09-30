%{
  open Ast
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
%token TEOF

%left TLET TIN
%left TLAMBDA TDOT
%left TIF TTHEN TELSE
%nonassoc TEQ
%left TADD TSUB
%nonassoc TLPAREN TID TNUM TBOOL
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
  | TLET TID TEQ exp TIN exp    { App (Abs ($2, $6), $4) }
  | TIF exp TTHEN exp TELSE exp { If ($2, $4, $6) }
  | exp exp %prec APP    { App ($1, $2) }
