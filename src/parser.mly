%{
  open Ast
%}

%token<string> TID
%token<int> TNUM
%token<bool> TBOOL
%token TLAMBDA
%token TDOT
%token TLET TEQ TIN
%token TLPAREN TRPAREN
%token TEOF

%start<Ast.exp> program
%nonassoc TIN
%nonassoc TLPAREN TLAMBDA TDOT
%right TLET
%left TNUM TID TBOOL
%left TAPP
/* TODO: check below precedence levels */
/* %left  */

%%

program:
  | exp TEOF  { $1 }

exp:
  | TID                   { Var $1 }
  | TNUM                  { Num $1 }
  | TBOOL                 { Bool $1 }
  | TLAMBDA TID TDOT exp  { Abs ($2, $4) }
  | TLPAREN exp TRPAREN   { $2 }
  | exp exp %prec TAPP    { App ($1, $2) }
  | TLET TID TEQ exp TIN exp { App (Abs ($2, $6), $4) }
