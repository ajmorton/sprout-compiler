(* Specification of an AST for bean *)
type ident = string

(* Keep aliases intact for pretty printing. *)
type beantype =
  | Bool
  | Int

type typedef = (ident * beantype)

type lparen = L_paren
type rparen = R_paren

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
    | Ebool of bool
    | Eint of int
    | Ebinop of (expr * binop * expr)
    | Eunop of (unop * expr)
    | Nested of (lparen * expr * rparen)


(* Will need to AST elements with additional data.  *)
type rvalue =
  | Rexpr of expr

type decl = (ident * beantype)

type stmt =
  | Assign of (lvalue * rvalue)
  | Read of lvalue
  | Write of expr

type program = {
  decls : typedef list ;
  stmts : stmt list
}

type t = program
