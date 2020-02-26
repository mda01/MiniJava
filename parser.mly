%{
  open LMJ
%}

%token <int32> INT_CONST
%token <bool> BOOL_CONST
%token INTEGER BOOLEAN
%token <string Location.t> IDENT
%token CLASS PUBLIC STATIC VOID MAIN STRING EXTENDS RETURN
%token PLUS MINUS TIMES NOT LT AND
%token COMMA SEMICOLON
%token ASSIGN
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token THIS NEW DOT LENGTH
%token SYSO
%token IF ELSE WHILE
%token EOF


%start program

%type <LMJ.program> program

%%

program:
| m = main_class /*d = defs */EOF
   {
     let c, a, i = m in
     {
       name = c;
       defs = [];
       main_args = a;
       main = i
     }
   }

main_class:
| CLASS c = IDENT
   LBRACE
   PUBLIC STATIC VOID MAIN LPAREN STRING LBRACKET RBRACKET a = IDENT RPAREN
   LBRACE
   i = instruction
   RBRACE
   RBRACE
   { (c, a, i) }


expression:
|  e = raw_expression
   { Location.make $startpos $endpos e }
| LPAREN e = expression RPAREN
   { e }

raw_expression:
| i = INT_CONST
   { EConst (ConstInt i) }

instruction:
| LBRACE li = instruction* RBRACE { IBlock li }
| IF LPAREN e = expression RPAREN i1 = instruction ELSE i2 = instruction
{ IIf (e,i1,i2) }
| WHILE LPAREN e = expression RPAREN i = instruction
{ IWhile (e, i) }

| SYSO LPAREN e =
   expression
   RPAREN
   SEMICOLON
   { ISyso e }

