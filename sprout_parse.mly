/* ocamlyacc parser for bean */
%{
open Sprout_ast
%}

%token <bool>   BOOL_CONST
%token <int>    INT_CONST
%token <string> IDENT
%token CONST TDKEY PROC END
%token BOOL INT
%token WRITE READ
%token ASSIGN
%token LPAREN RPAREN LCURLY RCURLY
%token EQ NEQ LT GT LTE GTE
%token AND OR NOT
%token PLUS MINUS MUL DIV
%token SEMICOLON COLON COMMA
%token EOF VAL REF

%nonassoc EQ NEQ LT GT LTE GTE AND OR
%left PLUS MINUS
%left MUL
%nonassoc UMINUS NOT

%type <Sprout_ast.program> program

%start program
%%

program:
  tdefs procs { { tdefs = List.rev $1 ; procs = List.rev $2 } }

procs :
  | procs proc { $2 :: $1 }
  | { [] }

proc :
  PROC header decls stmts END { {header = $2 ;
                                 decls  = List.rev $3 ;
                                 stmts  = List.rev $4
                                 } }
header :
  IDENT LPAREN params RPAREN { { id = $1 ; params = $3} }

params :
  | params param { $2 :: $1 }
  | { [] }

/*not happy with this implementation*/
param :
  | indicator typespec IDENT COMMA { {indicator = $1;
                                      typespec  = $2;
                                      id        = $3} }
  | indicator typespec IDENT { {indicator = $1;
                                typespec  = $2;
                                id        = $3} }

indicator :
  | VAL { Val }
  | REF { Ref }

tdefs :
  | tdefs tdef {$2 :: $1}
  | { [] }

tdef :
  | TDKEY typespec IDENT { (TDKey, $2, $3) }

typespec:
  | beantype                 { Type $1 }
  | IDENT                    { Ident $1 }
  | LCURLY opts field RCURLY { Fields { opts = $2 ; field = $3 } }

opts:
  | opts opt {$2 :: $1}
  | { [] }

opt:
  | IDENT COLON typespec COMMA { ($1, $3) }

field:
  | IDENT COLON typespec { ($1, $3) }

decl :
  | beantype IDENT SEMICOLON { Decl ($2, $1) }

decls :
  | decls decl { $2 :: $1 }
  | { [] }

beantype :
  | BOOL { Bool }
  | INT  { Int }

/* Builds stmts in reverse order */
stmts:
  | stmts stmt { $2 :: $1 }
  | { [] }

stmt :
  stmt_body SEMICOLON { $1 }

stmt_body:
  | READ lvalue { Read $2 }
  | WRITE expr { Write $2 }
  | lvalue ASSIGN rvalue { Assign ($1, $3) }

rvalue :
  | expr { Rexpr $1 }

lvalue:
  | IDENT { LId $1 }

const:
  | BOOL_CONST { Ebool  $1 }
  | INT_CONST  { Eint  $1 }

expr:
  | const      { Econst $1 }
  | lvalue     { Elval $1 }
  /* Binary operators */
  | expr PLUS  expr { Ebinop ($1, Op_add, $3) }
  | expr MINUS expr { Ebinop ($1, Op_sub, $3) }
  | expr MUL   expr { Ebinop ($1, Op_mul, $3) }
  | expr DIV   expr { Ebinop ($1, Op_div, $3) }
  | expr EQ    expr { Ebinop ($1, Op_eq,  $3) }
  | expr NEQ   expr { Ebinop ($1, Op_neq, $3) }
  | expr LT    expr { Ebinop ($1, Op_lt,  $3) }
  | expr LTE   expr { Ebinop ($1, Op_lte, $3) }
  | expr GT    expr { Ebinop ($1, Op_gt,  $3) }
  | expr GTE   expr { Ebinop ($1, Op_gte, $3) }
  | expr AND   expr { Ebinop ($1, Op_and, $3) }
  | expr OR    expr { Ebinop ($1, Op_or,  $3) }
  /* Unary operators */
  | NOT   expr              { Eunop (Op_not,   $2) }
  | MINUS expr %prec UMINUS { Eunop (Op_minus, $2) }
  /* parentheses */
  | LPAREN expr RPAREN { Enested (L_paren, $2, R_paren) }
