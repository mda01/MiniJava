%{
  open LMJ
%}

%token <int32> INT_CONST
%token <bool> BOOL_CONST
%token INTEGER BOOLEAN
%token <string Location.t> IDENT
%token CLASS PUBLIC STATIC VOID MAIN STRING EXTENDS RETURN
%token PLUS MINUS TIMES NOT LT GT LEQ GEQ AND OR
%token COMMA SEMICOLON
%token ASSIGN EQUAL NEQUAL
/* %token PLUSEQ MINUSEQ */
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token THIS NEW DOT LENGTH
%token SYSO
%token IF ELSE WHILE FOR
%token EOF


%start program

%type <LMJ.program> program

%left OR
%left AND 
%nonassoc LT GT LEQ GEQ EQUAL NEQUAL
%left PLUS MINUS
%left TIMES
%nonassoc NOT
%nonassoc LBRACKET
%left DOT
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
| CLASS c_id = IDENT ext = option(preceded(EXTENDS, IDENT)) LBRACE 
varList = list(pair(typi, terminated(IDENT, SEMICOLON)))
metho = list(method_declaration)
RBRACE 
{
   (* Return a list of pairs (id*typ) from menhir list *)
   let rec formatvar vars acc = match vars with
         | [] -> acc
         | head :: tail -> let typ, id = head in (id, typ)::(formatvar tail acc)
         in (c_id, {
            extends = ext;
            attributes = formatvar varList []; 
            methods = metho;
         })
}

decl_inst:
| var = pair(typi,terminated(IDENT, SEMICOLON)) met = decl_inst { 
   let varL, insL = met in (var::varL, insL) (*Extract varlist and instruction list, then append current var to the varlist*)
}
| me = list(instruction) { ([], me) } (*Initialisation of the pair of list with varlist empty*)

method_declaration:
| PUBLIC t = typi id = IDENT LPAREN 
varDec = separated_list(COMMA, pair(typi, IDENT)) 
RPAREN LBRACE
di = decl_inst
RETURN e = expression SEMICOLON RBRACE
{
   (* Return a list of pairs (id*typ) from menhir list *)
   let rec formatvar vars acc = match vars with
         | [] -> acc
         | head :: tail -> let typ, id = head in (id, typ)::(formatvar tail acc)
   in 
      let varList, insList = di in  
      (id, {
            formals = formatvar varDec [];
            result = t;
            locals = formatvar varList [];
            body = insList;
            return = e;
         })
}

expression:
|  e = raw_expression
   { Location.make $startpos $endpos e }
| LPAREN e = expression RPAREN
   { e }


raw_expression:
| i = INT_CONST
   { EConst (ConstInt i) }
| e0 = expression AND e1 = expression     { EBinOp (OpAnd, e0, e1) }
| e0 = expression OR e1 = expression      
   { 
      EUnOp(UOpNot, (Location.make $startpos $endpos (
         EBinOp (OpAnd, 
            (Location.make $startpos $endpos (EUnOp (UOpNot,e0))),
            (Location.make $startpos $endpos (EUnOp (UOpNot,e1))))
         ))) 
   }
/* Tests in ./test/custom_mini-java */
| e0 = expression EQUAL e1 = expression   { EBinOp (OpEq, e0, e1)}
| e0 = expression NEQUAL e1 = expression  { EUnOp (UOpNot, (Location.make $startpos $endpos (EBinOp (OpEq, e0, e1)))) }
/* Tests in ./test/custom_mini-java */
/* Operations >, <, >= and <= */
| e0 = expression LT e1 = expression   { EBinOp (OpLt, e0, e1) }
| e0 = expression GT e1 = expression   { EBinOp (OpGt, e0, e1) }
| e0 = expression LEQ e1 = expression  { EUnOp (UOpNot, (Location.make $startpos $endpos (EBinOp (OpGt, e0, e1)))) }
| e0 = expression GEQ e1 = expression  { EUnOp (UOpNot, (Location.make $startpos $endpos (EBinOp (OpLt, e0, e1)))) }
   
/* Math Operations */
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
| NEW INTEGER LBRACKET e = expression RBRACKET { EArrayAlloc e }
| NEW id = IDENT LPAREN RPAREN { EObjectAlloc id }
| NOT e = expression { EUnOp (UOpNot,e) }

instruction:
| LBRACE li = instruction* RBRACE { IBlock li }
| IF LPAREN e = expression RPAREN i1 = instruction ELSE i2 = instruction
   { IIf (e,i1,i2) }
| WHILE LPAREN e = expression RPAREN i = instruction
   { IWhile (e, i) }
/* Tests in ./test/custom_mini-java */
/* for(i=0; i < 6; i+1)  | Syntax : for(id (ident) = val_init (int_expr) ; stop_cond (bool_expr) ; val_update(int_expr) )  */
| FOR LPAREN id = IDENT ASSIGN 
  init = expression SEMICOLON
  e = expression SEMICOLON
  up = expression RPAREN 
  ins = instruction
   { IFor (id, init, e, up, ins) }
| SYSO LPAREN e = expression RPAREN SEMICOLON
   { ISyso e }
| id = IDENT LBRACKET e = expression RBRACKET ASSIGN ee = expression SEMICOLON
   { IArraySet (id, e, ee) }
| id = IDENT ASSIGN e = expression SEMICOLON
   { ISetVar (id, e) }
/* | id = IDENT PLUSEQ nb = INT_CONST
   { ISetVarPlus (id, nb) }
| id = IDENT MINUSEQ nb = INT_CONST
   { ISetVarMinus (id, nb) } */

typi:
| INTEGER { TypInt }
| INTEGER LBRACKET RBRACKET { TypIntArray}
| BOOLEAN { TypBool }
| id = IDENT { Typ id }
