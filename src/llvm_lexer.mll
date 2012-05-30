

{

  open Llvm_parser

  let suffix1 s = String.sub s 1 (String.length s - 1)

}

let number = ['0'-'9']+
let i = 'i'(number)+

let first_char_ident = ['a'-'z' 'A'-'Z' '$' '.' '_']
let other_char_ident = ['a'-'z' 'A'-'Z' '$' '.' '_' '0'-'9']
let global_ident = '@'(first_char_ident)(other_char_ident)*
let local_ident = '%'(first_char_ident)(other_char_ident)*

let blank = [' ' '\t']+

let newline = '\n'

rule token = parse
  | blank+ {token lexbuf}
  | newline {EOL}
  | number as n { NUMBER (int_of_string n) }
  | global_ident as g {GLOBAL (suffix1 g)}
  | local_ident as g {LOCAL (suffix1 g)}
  | '(' {LPAREN}
  | ')' {RPAREN}
  | '{' {LCURLY}
  | '}' {RCURLY}
  | '[' {LSQUARE}
  | ']' {RSQUARE}
  | '*' {STAR}
  | '=' {EQ}
  | ',' {COMMA}

  (* Things we need to throw away. TODO: complete list *)
  | ';' {comment lexbuf}
  | "target" {target lexbuf}
  | "nounwind" {token lexbuf}
  | "nsw" {token lexbuf}
  | "nuw" {token lexbuf}

  (* keywords *)
  | "define" {DEFINE}

  (*types*)
  | i as n {I (int_of_string (suffix1 n))}
  | "void" {VOID}
  | "label" {TLABEL}
  (* We are interested neither in floats nor in metadata *)

  (* operations *)
  | "alloca" {ALLOCA}
  | "add" {ADD}
  | "load" {LOAD}
  | "store" {STORE}
  | "align" {ALIGN}
  | "br" {BR}
  | "ret" {RET}
  | "phi" {PHI}



and comment = parse
  | newline {EOL}
  | "<label>:" number as n {LABEL n}
  | _ {comment lexbuf}

and target = parse
  | newline {EOL}
  | _ {target lexbuf}
