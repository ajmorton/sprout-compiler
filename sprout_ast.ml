
(* TODO -- fix ifte --> remove alts *)
(* Specification of an AST for bean *)
type ident = string

(* TODO unify beantype and typespec usage *)
type beantype =
  | Bool
  | Int

type lparen = L_paren
type rparen = R_paren

type const =
  | Ebool of bool
  | Eint  of int

type lvalue =
  | LId of ident
  | LField of (lvalue * ident)

type binop =
  | Op_add | Op_sub | Op_mul | Op_div
  | Op_eq  | Op_neq | Op_and | Op_or
  | Op_lt  | Op_gt  | Op_lte | Op_gte

type unop =
  | Op_minus
  | Op_not

type expr =
  | Elval   of lvalue
  | Econst  of const
  | Ebinop  of (expr * binop * expr)
  | Eunop   of (unop * expr)
  | Enested of (lparen * expr * rparen)




(* Will need to AST elements with additional data.  *)
type rvalue =
  | Rexpr of expr
  | Inits of inits
and
  inits = (ident * rvalue) list

type decl = Decl of (ident * beantype)


type stmt =
  | Assign of (lvalue * rvalue)
  | Read   of lvalue
  | Write  of expr
  | Ift    of ift
  | Ifte   of ifte
  | Do     of dowhile
and ift = {
  expr  : expr ;
  stmts : stmt list
}
and ifte = {
  expr2  : expr ;
  stmts2 : stmt list ;
  alts   : stmt list ;
}
and dowhile = {
  expr3 : expr;
  stmts3 : stmt list;
}


type fielddefs = {
  (* optional additional field definitions *)
  opts  : (ident * typespec) list;
  field : (ident * typespec)
}
and typespec =
  | Type   of beantype
  | Ident  of ident
  | Fields of fielddefs

type tdkey = TDKey
type tdef = (tdkey * typespec * ident)

type indicator =
  | Ref
  | Val

type param = {
  indicator : indicator ;
  typespec  : typespec ;
  id        : ident
}

type header = {
  id     : ident ;
  params : param list
}

type proc = {
  header : header ;
  decls  : decl list ;
  stmts  : stmt list
}

type program = {
  tdefs : tdef list ;
  procs : proc list
}

type t = program
