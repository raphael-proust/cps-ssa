 (* {{{ LICENSE                                                              *
  * vi: set fdm=marker fdl=0:                                                *
  *                                                                          *
  * Copyright (c) 2012 Raphaël Proust <raphlalou@gmail.com>                  *
  * Copyright (c) 2012 INRIA - Raphaël Proust <raphlalou@gmail.com>          *
  * Copyright (c) 2012 ENS - Raphaël Proust <raphlalou@gmail.com>            *
  *                                                                          *
  * Permission to use, copy, modify, and distribute this software for any    *
  * purpose with or without fee is hereby granted, provided that the above   *
  * copyright notice and this permission notice appear in all copies.        *
  *                                                                          *
  * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES *
  * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF         *
  * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  *
  * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   *
  * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN    *
  * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  *
  * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.           *
  * }}}                                                                      *)

{

  open Llvm_parser

}

let ws = [' ' '\t']
let eol = ('\n' | '\r' | "\r\n" "\n\r")
let digit = ['0'-'9']
let upletter = ['A'-'Z']
let lowletter = ['a'-'z']
let letter = upletter | lowletter
let alphanum = digit | letter
let ident_fst = alphanum | ['$' '.' '_']
let ident_nxt = alphanum | ['$' '.' '_']


rule token = parse
  (* seps and stuff *)
  | ws+ { token lexbuf }
  | eol { EOL }
  | eof { EOF }
  | '=' { EQ }
  | ',' { COMMA }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LCURLY }
  | '}' { RCURLY }

  (* keywords *) (* semi-auto generated *)
  | "define" { DEFINE }
  | "private" { PRIVATE }
  | "linker_private" { LINKER_PRIVATE }
  | "linker_private_weak" { LINKER_PRIVATE_WEAK }
  | "linker_private_weak_def_auto" { LINKER_PRIVATE_WEAK_DEF_AUTO }
  | "internal" { INTERNAL }
  | "available_externally" { AVAILABLE_EXTERNALLY }
  | "linkonce" { LINKONCE }
  | "weak" { WEAK }
  | "common" { COMMON }
  | "appending" { APPENDING }
  | "extern_weak" { EXTERN_WEAK }
  | "linkonce_odr" { LINKONCE_ODR }
  | "weak_odr" { WEAK_ODR }
  | "external" { EXTERNAL }
  | "dllimport" { DLLIMPORT }
  | "dllexport" { DLLEXPORT }
  | "default" { DEFAULT }
  | "hidden" { HIDDEN }
  | "protected" { PROTECTED }
  | "ccc" { CCC }
  | "fastcc" { FASTCC }
  | "coldcc" { COLDCC }
  | "cc" { CC }
  | "zeroext" { ZEROEXT }
  | "signext" { SIGNEXT }
  | "inreg" { INREG }
  | "byval" { BYVAL }
  | "sret" { SRET }
  | "noalias" { NOALIAS }
  | "nocapture" { NOCAPTURE }
  | "nest" { NEST }
  | "address_safety" { ADDRESS_SAFETY }
  | "alignstack" { ALIGNSTACK }
  | "alwaysinline" { ALWAYSINLINE }
  | "nonlazybind" { NONLAZYBIND }
  | "inlinehint" { INLINEHINT }
  | "naked" { NAKED }
  | "noimplicitfloat" { NOIMPLICITFLOAT }
  | "noinline" { NOINLINE }
  | "noredzone" { NOREDZONE }
  | "noreturn" { NORETURN }
  | "nounwind" { NOUNWIND }
  | "optsize" { OPTSIZE }
  | "readnone" { READNONE }
  | "readonly" { READONLY }
  | "returns_twice" { RETURNS_TWICE }
  | "ssp" { SSP }
  | "sspreq" { SSPREQ }
  | "uwtable" { UWTABLE }
  | "align" { ALIGN }
  | "gc" { GC }

  (* identifier *)
  | '@' (ident_fst ident_nxt* ) as i { GLOBAL i }
  | '@' (digit+               ) as i { GLOBAL i }
  | '%' (ident_fst ident_nxt* ) as i { LOCAL  i }
  | '%' (digit+               ) as i { LOCAL  i }

  (* constants *)
  | ( '-'? digit+ ) as d { INTEGER (int_of_string d) }
  | ( '-'? digit* '.' digit+ ) as d { FLOAT (float_of_string d) }
  | ( '-'? digit ('.' digit+)? 'e' ('+'|'-') digit+ ) as d { FLOAT (float_of_string d) }
  | "true" { BOOL true }
  | "false" { BOOL false }
  | "null" { NULL }

  (* types *)
  | "void" { TVOID }
  | "half" { THALF }
  | "float" { TFLOAT }
  | "double" { TDOUBLE }
  | "x86_fp80" { TX86_FP80 }
  | "fp128" { TFP128 }
  | "ppc_fp128" { TPPC_FP128 }
  | "label" { TLABEL }
  | "metadata" { TMETADATA }
  | "x86_mmx" { TX86_MMX }
  | 'i' digit+ as i { TI (int_of_string i) }

  (* instrs *)
  | "add" { ADD }
  | "fadd" { FADD }
  | "sub" { SUB }
  | "fsub" { FSUB }
  | "mul" { MUL }
  | "fmul" { FMUL }
  | "udiv" { UDIV }
  | "sdiv" { SDIV }
  | "fdiv" { FDIV }
  | "urem" { UREM }
  | "srem" { SREM }
  | "frem" { FREM }
  | "shl" { SHL }
  | "lshr" { LSHR }
  | "ashr" { ASHR }
  | "and" { AND }
  | "or" { OR }
  | "xor" { XOR }
  | "icmp" { ICMP }
  | "fcmp" { FCMP }
  | "phi" { PHI }
  | "call" { CALL }
  | "trunc" { TRUNC }
  | "zext" { ZEXT }
  | "sext" { SEXT }
  | "fptrunc" { FPTRUNC }
  | "fpext" { FPEXT }
  | "uitofp" { UITOFP }
  | "sitofp" { SITOFP }
  | "fptoui" { FPTOUI }
  | "fptosi" { FPTOSI }
  | "inttoptr" { INTTOPTR }
  | "ptrtoint" { PTRTOINT }
  | "bitcast" { BITCAST }
  | "select" { SELECT }
  | "va_arg" { VAARG }
  | "ret" { RET }
  | "br" { BR }
  | "switch" { SWITCH }
  | "indirectbr" { INDIRECTBR }
  | "invoke" { INVOKE }
  | "resume" { RESUME }
  | "unreachable" { UNREACHABLE }
  | "alloca" { ALLOCA }
  | "load" { LOAD }
  | "store" { STORE }
  | "cmpxchg" { ATOMICCMPXCHG }
  | "atomicrmw" { ATOMICRMW }
  | "fence" { FENCE }
  | "getelementptr" { GETELEMENTPTR }
  | "extractelement" { EXTRACTELEMENT }
  | "insertelement" { INSERTELEMENT }
  | "shufflevector" { SHUFFLEVECTOR }
  | "extractvalue" { EXTRACTVALUE }
  | "insertvalue" { INSERTVALUE }
  | "landingpad" { LANDINGPAD }


