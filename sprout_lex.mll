{
open Sprout_parse
}

let digit   = ['0' - '9']
let alpha   = ['a' - 'z' 'A' - 'Z']
let special = ['_']
let alnum   = alpha | digit | special
let digits  = digit+
let ident   = alpha alnum*

rule token = parse
    [' ' '\t']    { token lexbuf }     (* skip blanks *)
  | '\n'          { Lexing.new_line lexbuf ; token lexbuf }
  | '-'?['0'-'9']+ as lxm { INT_CONST(int_of_string lxm) }
  (* keywords *)
  | "proc"    { PROC }
  | "end"     { END }
  | "bool"    { BOOL }
  | "int"     { INT }
  | "true"    { BOOL_CONST true }
  | "false"   { BOOL_CONST false }
  | "read"    { READ }
  | "write"   { WRITE }
  | ":="      { ASSIGN }
  | "and"     { AND }
  | "or"      { OR }
  | "not"     { NOT }
  | "typedef" { TDKEY }
  | "ref"     { REF }
  | "val"     { VAL }
  | "if"      { IF }
  | "then"    { THEN }
  | "else"    { ELSE }
  | "fi"      { FI }
  | "do"      { DO }
  | "while"   { WHILE }
  | "od"      { OD }
  (* symbols *)
  | '('  { LPAREN }
  | ')'  { RPAREN }
  | '{'  { LCURLY }
  | '}'  { RCURLY }
  | '='  { EQ }
  | "!=" { NEQ}
  | '<'  { LT }
  | "<=" { LTE }
  | '>'  { GT }
  | ">=" { GTE }
  | '+'  { PLUS }
  | '-'  { MINUS }
  | '*'  { MUL }
  | '/'  { DIV }
  | ';'  { SEMICOLON }
  | ':'  { COLON }
  | ','  { COMMA }
  (* other *)
  | ident as lxm { IDENT lxm }
  | eof { EOF }
