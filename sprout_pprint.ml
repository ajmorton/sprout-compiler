open Sprout_ast
open Format

let rec print n str =
  match (n > 0) with
    | true  -> printf "    ";
               print (n-1) str
    | false -> printf str

let print_decl decl =
  match decl with
  | (id, TBool) -> printf "bool %s;\n" id
  | (id, TInt)  -> printf "int %s;\n"  id

let rec print_decls decls =
  match decls with
    | [] -> printf "\n"; ()
    | (Decl decl)::tail -> print_decl decl;
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
    | Op_div -> printf " / "
    | Op_eq  -> printf " = "
    | Op_neq -> printf " != "
    | Op_lt  -> printf " < "
    | Op_lte -> printf " <= "
    | Op_gt  -> printf " > "
    | Op_gte -> printf " >= "
    | Op_and -> printf " and "
    | Op_or  -> printf " or "

let print_unop unop =
  match unop with
    | Op_minus -> printf "-"
    | Op_not   -> printf "not "

let print_const const =
  match const with
    | Ebool rval -> printf "%b" rval
    | Eint rval -> printf "%d" rval

let rec print_expr expr =
  match expr with
    | Econst const                 -> print_const const
    | Elval rval                   -> print_lvalue rval
    | Ebinop (expr1, binop, expr2) -> print_expr expr1;
                                      print_binop binop;
                                      print_expr expr2
    | Eunop (unop, expr1)          -> print_unop unop;
                                      print_expr expr1
    | Enested (l, expr1, r)        -> printf "(";
                                      print_expr expr1;
                                      printf ")"


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
                             printf ";\n";
                             print_stmts tail
    | (Assign stmt)::tail -> print_assign stmt;
                             printf ";\n";
                             print_stmts tail
    | (Write stmt)::tail  -> printf "write ";
                             print_expr stmt;
                             printf ";\n";
                             print_stmts tail





let rec print_spec spec =
  match spec with
    | Bool     -> printf "bool"
    | Int      -> printf "int"
    | Ident id -> printf "%s" id
    | Fields f -> print_fielddefs f

and print_field (id, spec) =
  printf "%s : " id;
  print_spec spec

and print_fielddefs {opts; field} =
  printf      "{"   ;
  print_opts  opts  ;
  print_field field ;
  printf      "}"

and print_opt (id, spec) =
  printf "%s : " id;
  print_spec spec;
  printf ", "

and print_opts opts =
  match opts with
    | [] -> ()
    | o::tail -> print_opt o;
                 print_opts tail

let print_tdef tdef =
  match tdef with
    | (TDKey, spec, id) -> printf "typedef ";
                           print_spec spec;
                           printf " %s\n" id

let rec print_tdefs tdefs =
  match tdefs with
    | [] -> ()
    | t::tail ->  print_tdef t;
                  print_tdefs tail

let print_program fmt {tdefs ; decls ; stmts} =
  print_tdefs tdefs;
  print_decls decls;
  print_stmts stmts
