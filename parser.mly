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
| m = main_class d = defs EOF
   {
     let c, a, i = m in
     {
       name = c;
       defs = d;
       main_args = a;
       main = i
     }
   }

defs:
ll = class_declaration* {ll}


main_class:
| CLASS c = IDENT
   LBRACE
   PUBLIC STATIC VOID MAIN LPAREN STRING LBRACKET RBRACKET a = IDENT RPAREN
   LBRACE
   i = instruction
   RBRACE
   RBRACE
   { (c, a, i) }

class_declaration:
| CLASS c = IDENT id = option(preceded(EXTENDS, IDENT)) LBRACE 
var = list(pair(typi, terminated(IDENT, SEMICOLON)))
me = list(method_declaration)
RBRACE
{ clas(id, var, me) }

method_declaration:
| PUBLIC t = typi IDENT LPAREN 
varDec = option(separated_list(COMMA, pair(typi, IDENT))) 
RPAREN LBRACE
varList = list(pair(typi, terminated(IDENT, SEMICOLON)))
body = list(instruction)
RETURN e = expression SEMICOLON RBRACE
{
   formals = varDec; (* Inverser Identifiant et types*)

}

expression:
|  e = raw_expression
   { Location.make $startpos $endpos e }
| LPAREN e = expression RPAREN
   { e }


raw_expression:
| i = INT_CONST
   { EConst (ConstInt i) }
| e0 = expression AND e1 = expression
   { EBinOp (OpAnd, e0, e1) }
| e0 = expression LT e1 = expression
   { EBinOp (OpLt, e0, e1) }
| e0 = expression PLUS e1 = expression
   { EBinOp (OpAdd, e0, e1) }
| e0 = expression MINUS e1 = expression
   { EBinOp (OpSub, e0, e1) }
| e0 = expression TIMES e1 = expression
   { EBinOp (OpMul, e0, e1) }
| e0 = expression LBRACKET e1 = expression RBRACKET
   { EArrayGet (e0, e1) }
| e0 = expression DOT LENGTH { EArrayLength e0 }
| e0 = expression DOT c = IDENT LPAREN ll = separated_list(COMMA, expression) RPAREN { EMethodCall (e0, c, ll) }
| b = BOOL_CONST { EConst (ConstBool b) } 
| id = IDENT { EGetVar id }
| THIS { EThis }
| NEW i = INTEGER LBRACKET e = expression RBRACKET { EArrayAlloc e }
| NEW id = IDENT LPAREN RPAREN { EObjectAlloc id }
| NOT e = expression { EUnOp (UOpNot,e) }


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
| id = IDENT LBRACKET e = expression RBRACKET ASSIGN ee = expression SEMICOLON
   { IArraySet (id, e, ee) }
| id = IDENT ASSIGN e = expression SEMICOLON
   { ISetVar (id, e) }

typi:
| INT_CONST { TypInt }
| INT_CONST LBRACKET RBRACKET { TypIntArray}
| BOOL_CONST { TypBool }
| id = IDENT { Typ id }
