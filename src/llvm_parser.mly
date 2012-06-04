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

%{

  open LLVM (* in productions *)

%}

(*TODO: vectors and vector types *)
(*TODO: don't throw things away *)

%token DEFINE
%token<string> GLOBAL LOCAL
%token LPAREN RPAREN LCURLY RCURLY EQ COMMA EOL EOF

%token<string> STRING
%token<int> INTEGER
%token<float> FLOAT
%token<bool> BOOL
%token NULL

%token PRIVATE LINKER_PRIVATE LINKER_PRIVATE_WEAK LINKER_PRIVATE_WEAK_DEF_AUTO INTERNAL AVAILABLE_EXTERNALLY LINKONCE WEAK COMMON APPENDING EXTERN_WEAK LINKONCE_ODR WEAK_ODR EXTERNAL DLLIMPORT DLLEXPORT
%token DEFAULT HIDDEN PROTECTED
%token CCC FASTCC COLDCC CC
%token ZEROEXT SIGNEXT INREG BYVAL SRET NOALIAS NOCAPTURE NEST
%token ADDRESS_SAFETY ALIGNSTACK ALWAYSINLINE NONLAZYBIND INLINEHINT NAKED NOIMPLICITFLOAT NOINLINE NOREDZONE NORETURN NOUNWIND OPTSIZE READNONE READONLY RETURNS_TWICE SSP SSPREQ UWTABLE
%token ALIGN
%token GC
%token ADD FADD SUB FSUB MUL FMUL UDIV SDIV FDIV UREM SREM FREM SHL LSHR ASHR AND OR XOR ICMP FCMP PHI CALL TRUNC ZEXT SEXT FPTRUNC FPEXT UITOFP SITOFP FPTOUI FPTOSI INTTOPTR PTRTOINT BITCAST SELECT VAARG RET BR SWITCH INDIRECTBR INVOKE RESUME UNREACHABLE ALLOCA LOAD STORE ATOMICCMPXCHG ATOMICRMW FENCE GETELEMENTPTR EXTRACTELEMENT INSERTELEMENT SHUFFLEVECTOR EXTRACTVALUE INSERTVALUE LANDINGPAD
%token<int> TI
%token TVOID THALF TFLOAT TDOUBLE TX86_FP80 TFP128 TPPC_FP128 TLABEL TMETADATA TX86_MMX


%start<LLVM.prog> program

%%

program:
  | procedure EOF { [ ] }


procedure:
  | DEFINE linkage? visibility? cconv?
    ret_typ = ret_type name = global
    LPAREN args = separated_list(COMMA,arg) RPAREN
    list(fn_attr) align? gc?
    LCURLY
    instrs = procedure_body
    RCURLY { {ret_typ; name; args; instrs;} }

linkage:
  | PRIVATE                      { LINKAGE_Private }
  | LINKER_PRIVATE               { LINKAGE_Linker_private }
  | LINKER_PRIVATE_WEAK          { LINKAGE_Linker_private_weak }
  | LINKER_PRIVATE_WEAK_DEF_AUTO { LINKAGE_Linker_private_weak_def_auto }
  | INTERNAL                     { LINKAGE_Internal }
  | AVAILABLE_EXTERNALLY         { LINKAGE_Available_externally }
  | LINKONCE                     { LINKAGE_Linkonce }
  | WEAK                         { LINKAGE_Weak }
  | COMMON                       { LINKAGE_Common }
  | APPENDING                    { LINKAGE_Appending }
  | EXTERN_WEAK                  { LINKAGE_Extern_weak }
  | LINKONCE_ODR                 { LINKAGE_Linkonce_odr }
  | WEAK_ODR                     { LINKAGE_Weak_odr }
  | EXTERNAL                     { LINKAGE_External }
  | DLLIMPORT                    { LINKAGE_Dllimport }
  | DLLEXPORT                    { LINKAGE_Dllexport }

visibility:
  | DEFAULT   { VISIBILITY_Default }
  | HIDDEN    { VISIBILITY_Hidden }
  | PROTECTED { VISIBILITY_Protected }

cconv:
  | CCC          { CC_Ccc }
  | FASTCC       { CC_Fastcc }
  | COLDCC       { CC_Coldcc }
  | CC n=INTEGER { CC_Cc n }

ret_type:
  | list(typ_attr) t = typ { t }

typ:
  | n = TI     { TYPE_I n }
  | TVOID      { TYPE_Tvoid }
  | THALF      { TYPE_Thalf }
  | TFLOAT     { TYPE_Tfloat }
  | TDOUBLE    { TYPE_Tdouble }
  | TX86_FP80  { TYPE_Tx86_fp80 }
  | TFP128     { TYPE_Tfp128 }
  | TPPC_FP128 { TYPE_Tppc_fp128 }
  | TLABEL     { TYPE_Tlabel }
  | TMETADATA  { TYPE_Tmetadata }
  | TX86_MMX   { TYPE_Tx86_mmx }

typ_attr:
  | ZEROEXT   { TYPEATTR_Zeroext }
  | SIGNEXT   { TYPEATTR_Signext }
  | INREG     { TYPEATTR_Inreg }
  | BYVAL     { TYPEATTR_Byval }
  | SRET      { TYPEATTR_Sret }
  | NOALIAS   { TYPEATTR_Noalias }
  | NOCAPTURE { TYPEATTR_Nocapture }
  | NEST      { TYPEATTR_Nest }

arg:
  | t = typ list(typ_attr) i = local { (t, i) }

fn_attr:
  | ADDRESS_SAFETY                      { FNATTR_Address_safety }
  | ALIGNSTACK LPAREN p = power2 RPAREN { FNATTR_Alignstack p }
  | ALWAYSINLINE                        { FNATTR_Alwaysinline }
  | NONLAZYBIND                         { FNATTR_Nonlazybind }
  | INLINEHINT                          { FNATTR_Inlinehint }
  | NAKED                               { FNATTR_Naked }
  | NOIMPLICITFLOAT                     { FNATTR_Noimplicitfloat }
  | NOINLINE                            { FNATTR_Noinline }
  | NOREDZONE                           { FNATTR_Noredzone }
  | NORETURN                            { FNATTR_Noreturn }
  | NOUNWIND                            { FNATTR_Nounwind }
  | OPTSIZE                             { FNATTR_Optsize }
  | READNONE                            { FNATTR_Readnone }
  | READONLY                            { FNATTR_Readonly }
  | RETURNS_TWICE                       { FNATTR_Returns_twice }
  | SSP                                 { FNATTR_Ssp }
  | SSPREQ                              { FNATTR_Sspreq }
  | UWTABLE                             { FNATTR_Uwtable }

power2:
  | n = INTEGER { assert (List.mem n [0;1;2;4;8;16;32;64]); n }

align:
  | ALIGN power2 { }

gc:
  | GC STRING { }

procedure_body: (*TODO*)
  | i = list(instr) { i }

instr:
  | i = ident EQ ADD t = typ o1 = operand COMMA o2 = operand
    { INSTR_Add (i, t, o1, o2) }
  | FADD           { INSTR_FAdd }
  | SUB            { INSTR_Sub }
  | FSUB           { INSTR_FSub }
  | MUL            { INSTR_Mul }
  | FMUL           { INSTR_FMul }
  | UDIV           { INSTR_UDiv }
  | SDIV           { INSTR_SDiv }
  | FDIV           { INSTR_FDiv }
  | UREM           { INSTR_URem }
  | SREM           { INSTR_SRem }
  | FREM           { INSTR_FRem }
  | SHL            { INSTR_Shl }
  | LSHR           { INSTR_LShr }
  | ASHR           { INSTR_AShr }
  | AND            { INSTR_And }
  | OR             { INSTR_Or }
  | XOR            { INSTR_Xor }
  | ICMP           { INSTR_ICmp }
  | FCMP           { INSTR_FCmp }
  | PHI            { INSTR_PHI }
  | CALL           { INSTR_Call }
  | TRUNC          { INSTR_Trunc }
  | ZEXT           { INSTR_ZExt }
  | SEXT           { INSTR_SExt }
  | FPTRUNC        { INSTR_FPTrunc }
  | FPEXT          { INSTR_FPExt }
  | UITOFP         { INSTR_UIToFP }
  | SITOFP         { INSTR_SIToFP }
  | FPTOUI         { INSTR_FPToUI }
  | FPTOSI         { INSTR_FPToSI }
  | INTTOPTR       { INSTR_IntToPtr }
  | PTRTOINT       { INSTR_PtrToInt }
  | BITCAST        { INSTR_BitCast }
  | SELECT         { INSTR_Select }
  | VAARG          { INSTR_VAArg }
  | RET            { INSTR_Ret }
  | BR             { INSTR_Br }
  | SWITCH         { INSTR_Switch }
  | INDIRECTBR     { INSTR_IndirectBr }
  | INVOKE         { INSTR_Invoke }
  | RESUME         { INSTR_Resume }
  | UNREACHABLE    { INSTR_Unreachable }
  | ALLOCA         { INSTR_Alloca }
  | LOAD           { INSTR_Load }
  | STORE          { INSTR_Store }
  | ATOMICCMPXCHG  { INSTR_AtomicCmpXchg }
  | ATOMICRMW      { INSTR_AtomicRMW }
  | FENCE          { INSTR_Fence }
  | GETELEMENTPTR  { INSTR_GetElementPtr }
  | EXTRACTELEMENT { INSTR_ExtractElement }
  | INSERTELEMENT  { INSTR_InsertElement }
  | SHUFFLEVECTOR  { INSTR_ShuffleVector }
  | EXTRACTVALUE   { INSTR_ExtractValue }
  | INSERTVALUE    { INSTR_InsertValue }
  | LANDINGPAD     { INSTR_LandingPad }

operand:
  | i = INTEGER  { VALUE_Integer i }
  | f = FLOAT    { VALUE_Float f }
  | b = BOOL     { VALUE_Bool b }
  | i = ident    { VALUE_Ident i }
  |     NULL     { VALUE_Null }

ident:
  | l = global
  | l = local    { l }

local:
  | l = LOCAL { ID_Local l }

global:
  | g = GLOBAL { ID_Global g }

