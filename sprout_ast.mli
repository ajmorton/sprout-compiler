(* Specification of an AST for bean *)
type ident = string

(* Keep aliases intact for pretty printing. *)
(* TODO unify beantype and typespec usage *)
type beantype =
  | TBool
  | TInt

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
  | Elval of lvalue
  | Econst of const
  | Ebinop of (expr * binop * expr)
  | Eunop of (unop * expr)
  | Enested of (lparen * expr * rparen)



(* Will need to AST elements with additional data.  *)
type rvalue =
  | Rexpr of expr

type decl = Decl of (ident * beantype)

type stmt =
  | Assign of (lvalue * rvalue)
  | Read of lvalue
  | Write of expr


(* TODO add semicolon into decl and stmt *)
type semicolon = Semicolon

type fielddefs = {
  (* optional additional field definitions *)
  opts  : (ident * typespec) list;
  field : (ident * typespec)
}
and typespec =
  | Bool
  | Int
  | Ident of ident
  | Fields of fielddefs

type tdkey = TDKey
type tdef = (tdkey * typespec * ident)

type program = {
  tdefs : tdef list ;
  decls : decl list ;
  stmts : stmt list
}

type t = program
