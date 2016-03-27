open Sprout_ast
open Format

let rec print n string =
  match (n > 0) with
    | true  -> printf "    ";
               print (n-1) string
    | false -> printf string

let rec print_decls decls =
  match decls with
    | [] -> printf "\n"; ()
    | (ident, Bool)::tail -> printf "bool %s;\n" ident;
                             print_decls tail
    | (ident, Int)::tail  -> printf "int %s;\n" ident;
                             print_decls tail


let rec print_lvalue lvalue =
  match lvalue with
    | LId    ident           -> printf "%s" ident
    | LField (lvalue, ident) -> printf "(";
                                print_lvalue lvalue;
                                printf ", %s)" ident

let print_binop binop =
  match binop with
    | Op_add -> printf " + "
    | Op_sub -> printf " - "
    | Op_mul -> printf " * "
    | Op_eq  -> printf " == "
    | Op_lt  -> printf " < "

let print_unop unop =
  match unop with
    | Op_minus -> printf "-"

let rec print_expr expr =
  match expr with
    | Ebool rval -> printf "%b" rval
    | Eint rval -> printf "%d" rval
    | Elval rval -> print_lvalue rval
    | Ebinop (expr1, binop, expr2) ->  print_expr expr1;
                                       print_binop binop;
                                       print_expr expr2
    | Eunop (unop, expr1) -> print_unop unop;
                             print_expr expr1


let print_rvalue rvalue =
  match rvalue with
    | Rexpr expr -> print_expr expr

let print_assign (lvalue, rvalue) =
  print_lvalue lvalue;
  printf " := ";
  print_rvalue rvalue

let rec print_stmts stmts =
  match stmts with
    | []                  -> printf "\n"; ()
    | (Read stmt)::tail   -> printf "read ";
                             print_lvalue stmt;
                             printf "\n";
                             print_stmts tail
    | (Assign stmt)::tail -> print_assign stmt;
                             printf "\n";
                             print_stmts tail
    | (Write stmt)::tail  -> printf "write ";
                             print_expr stmt;
                             printf "\n";
                             print_stmts tail


let print_program fmt {decls ; stmts} =
  print_decls decls;
  print_stmts stmts;

(*
let print_program fmt prog =
  printf "progStarted\n";
  match prog with
    | {decls ; stmts} -> printf "prog = {decls;stmts}\n";
                         printf "print decls\n";
                         print_decls decls;
                         printf "printf stmts\n";
                         print_stmts stmts
    | _ -> printf "other"
*)