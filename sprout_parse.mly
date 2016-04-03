/* ocamlyacc parser for bean */
%{
open Sprout_ast
%}

%token <bool> BOOL_CONST
%token <int> INT_CONST
%token <string> IDENT
%token CONST TDKEY
%token BOOL INT
%token WRITE READ
%token ASSIGN
%token LPAREN RPAREN LCURLY RCURLY
%token EQ NEQ LT GT LTE GTE
%token AND OR NOT
%token PLUS MINUS MUL DIV
%token SEMICOLON COLON COMMA
%token EOF

%nonassoc EQ NEQ LT GT LTE GTE AND OR
%left PLUS MINUS
%left MUL
%nonassoc UMINUS NOT

%type <Sprout_ast.program> program

%start program
%%

program:
  tdefs decls stmts { { tdefs = List.rev $1 ;
                        decls = List.rev $2 ;
                        stmts = List.rev $3 } }

tdef :
  | TDKEY typespec IDENT { (TDKey, $2, $3) }

typespec:
  | BOOL                     { Bool }
  | INT                      { Int }
  | IDENT                    { Ident $1 }
  | LCURLY opts field RCURLY { Fields { opts = $2 ; field = $3 } }

opts:
  | opts opt {$2 :: $1}
  | { [] }

opt:
  | IDENT COLON typespec COMMA { ($1, $3) }

field:
  | IDENT COLON typespec { ($1, $3) }

tdefs :
  | tdefs tdef {$2 :: $1}
  | { [] }

decl :
  | beantype IDENT SEMICOLON { Decl ($2, $1) }

decls :
  | decls decl { $2 :: $1 }
  | { [] }

beantype :
  | BOOL { TBool }
  | INT  { TInt }

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
