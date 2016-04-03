open Sprout_ast
open Format

let rec print n str =
  match (n > 0) with
    | true  -> printf "    ";
               print (n-1) str
    | false -> printf str; ()

let print_type t =
  match t with
  | Bool -> printf "bool"
  | Int  -> printf "int"

let print_decl n (id, t) =
(* TODO why does this only work with string as arg? *)
  print n "";
  print_type t ;
  printf " %s" id;
  printf ";\n"

let rec print_decls n decls =
  match decls with
    | [] -> printf "\n"; ()
    | (Decl decl)::tail -> print_decl n decl;
                           print_decls n tail

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


let rec print_stmts n stmts =
  match stmts with
    | []                  -> ()
    | (Read stmt)::tail   -> print n "read ";
                             print_lvalue stmt;
                             printf ";\n";
                             print_stmts n tail
    | (Assign stmt)::tail -> print n "";
                             print_assign stmt;
                             printf ";\n";
                             print_stmts n tail
    | (Write stmt)::tail  -> print n "";
                             printf "write ";
                             print_expr stmt;
                             printf ";\n";
                             print_stmts n tail
    | (Ift  ift)::tail    -> print_ifthen n ift ;
                             print_stmts n tail
    | (Ifte ifte)::tail   -> print_ifthenelse n ifte ;
                             print_stmts n tail
    | (Do dowhile)::tail  -> print_do n dowhile ;
                             print_stmts n tail
and print_ifthen n { expr ; stmts } =
  print n "if ";
  print_expr expr;
  printf " then\n";
  print_stmts (n+1) stmts;
  print n "fi\n"
and print_ifthenelse n { expr2 ; stmts2 ; alt } =
  print n "if ";
  print_expr expr2;
  printf " then\n";
  print_stmts (n+1) stmts2;
  print n "else\n" ;
  print_stmts (n+1) alt ;
  print n "fi\n"
and print_do n { expr3 ; stmts3 } =
  print n "while ";
  print_expr expr3;
  printf " do\n";
  print_stmts (n+1) stmts3;
  print n "od\n"

let rec print_spec spec =
  match spec with
    | Type   t -> print_type t
    | Ident  id   -> printf "%s" id
    | Fields f    -> print_fielddefs f

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
    | [] -> printf "\n"; ()
    | t::tail ->  print_tdef t;
                  print_tdefs tail

let print_indicator indicator =
  match indicator with
    | Val -> printf "val "
    | Ref -> printf "ref "

let print_param {indicator ; typespec ; id} =
  print_indicator indicator ;
  print_spec typespec ;
  printf " %s" id

let rec print_params params =
  match params with
    | [p]     -> print_param p ; ()
    | p::tail -> print_param p ;
                 printf ", " ;
                 print_params tail
    | []     -> ()

let print_header { id ; params } =
  printf "%s" id ;
  printf "(" ;
  print_params params ;
  printf ")\n"


let print_proc { header ; decls ; stmts } =
  printf "proc " ;
  print_header header ;
  print_decls 1 decls ;
  print_stmts 1 stmts ;
  printf "end\n\n"

let rec print_procs procs =
  match procs with
    | [] -> printf "\n" ; ()
    | p::tail -> print_proc p;
                 print_procs tail

let print_program fmt {tdefs ; procs} =
  printf "\n\n";
  print_tdefs tdefs;
  print_procs procs
