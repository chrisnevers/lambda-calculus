%{
  open Ast

  let rec curryPair = function
  | h :: [] -> h
  | h :: t -> Pair (h, curryPair t)

  let letToLambda id e1 e2 =
    App (Abs (id, e2), e1)
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
%token<string> TSTR
%token TFST TSND
%token TINL TINR TMATCH TBAR
%token TMUL TDIV
%token TREC
%token TSEMI
%token TPRINT
%token TEOF

%left TLET TIN TREC
%left TMATCH TBAR
%left TSEMI
%left TLAMBDA TDOT
%left TIF TTHEN TELSE
%nonassoc TCOMMA
%nonassoc TEQ
%left TADD TSUB
%left TMUL TDIV
%nonassoc TLPAREN TID TNUM TBOOL TFST TSND TINL TINR TSTR TPRINT
%left APP

%start<Ast.exp> program

%%

program:
  | exp TEOF  { $1 }

exp:
  | TID                   { Var $1 }
  | TNUM                  { Num $1 }
  | TBOOL                 { Bool $1 }
  | TSTR                  { Str $1 }
  | TLAMBDA TID TDOT exp  { Abs ($2, $4) }
  | TLPAREN exp TRPAREN   { $2 }
  | exp TADD exp          { Binop (Add, $1, $3) }
  | exp TSUB exp          { Binop (Sub, $1, $3) }
  | exp TMUL exp          { Binop (Mul, $1, $3) }
  | exp TDIV exp          { Binop (Div, $1, $3) }
  | exp TEQ exp           { Binop (Equal, $1, $3) }
  | exp TSEMI exp         { letToLambda "_" $1 $3 }
  | TPRINT exp            { Unop (Print, $2) }
  | TFST exp              { Unop (Fst, $2) }
  | TSND exp              { Unop (Snd, $2) }
  | TINL exp              { Inl $2 }
  | TINR exp              { Inr $2 }
  | TMATCH exp TBAR exp TBAR exp { Match ($2, $4, $6) }
  | TLET TID TEQ exp TIN exp    { Let($2, $4, $6) }
  | TREC TID TEQ exp TIN exp    {
    let x = $2 in
    Let(x, Fix (Abs (x, $4)), $6)
  }
  | TIF exp TTHEN exp TELSE exp { If ($2, $4, $6) }
  | exp exp %prec APP    { App ($1, $2) }
  | TLPAREN exp TCOMMA separated_list(TCOMMA, exp) TRPAREN {
      match $4 with [] -> $2 | es -> Pair ($2, curryPair es)
    }
