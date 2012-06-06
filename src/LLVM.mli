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

type linkage =
  | LINKAGE_Private
  | LINKAGE_Linker_private
  | LINKAGE_Linker_private_weak
  | LINKAGE_Linker_private_weak_def_auto
  | LINKAGE_Internal
  | LINKAGE_Available_externally
  | LINKAGE_Linkonce
  | LINKAGE_Weak
  | LINKAGE_Common
  | LINKAGE_Appending
  | LINKAGE_Extern_weak
  | LINKAGE_Linkonce_odr
  | LINKAGE_Weak_odr
  | LINKAGE_External
  | LINKAGE_Dllimport
  | LINKAGE_Dllexport

type visibility =
  | VISIBILITY_Default
  | VISIBILITY_Hidden
  | VISIBILITY_Protected

type cconv =
  | CC_Ccc
  | CC_Fastcc
  | CC_Coldcc
  | CC_Cc of int

type typ_attr =
  | TYPEATTR_Zeroext
  | TYPEATTR_Signext
  | TYPEATTR_Inreg
  | TYPEATTR_Byval
  | TYPEATTR_Sret
  | TYPEATTR_Noalias
  | TYPEATTR_Nocapture
  | TYPEATTR_Nest

type fn_attr =
  | FNATTR_Address_safety
  | FNATTR_Alignstack of int
  | FNATTR_Alwaysinline
  | FNATTR_Nonlazybind
  | FNATTR_Inlinehint
  | FNATTR_Naked
  | FNATTR_Noimplicitfloat
  | FNATTR_Noinline
  | FNATTR_Noredzone
  | FNATTR_Noreturn
  | FNATTR_Nounwind
  | FNATTR_Optsize
  | FNATTR_Readnone
  | FNATTR_Readonly
  | FNATTR_Returns_twice
  | FNATTR_Ssp
  | FNATTR_Sspreq
  | FNATTR_Uwtable

type typ =
  | TYPE_I of int
  | TYPE_Pointer of typ
  | TYPE_Void
  | TYPE_Half
  | TYPE_Float
  | TYPE_Double
  | TYPE_X86_fp80
  | TYPE_Fp128
  | TYPE_Ppc_fp128
  | TYPE_Label
  | TYPE_Metadata
  | TYPE_X86_mmx

type cmp =
  | Cmp_Eq
  | Cmp_Ne
  | Cmp_Ugt
  | Cmp_Uge
  | Cmp_Ult
  | Cmp_Ule
  | Cmp_Sgt
  | Cmp_Sge
  | Cmp_Slt
  | Cmp_Sle

type ident =
  | ID_Global of string
  | ID_Local  of string

type tident = typ * ident

type value =
  | VALUE_Ident of ident
  | VALUE_Integer of int
  | VALUE_Float of float
  | VALUE_Bool of bool
  | VALUE_Null
  | VALUE_Void (* for 'return void' only *)

type tvalue = typ * value

type module_ = toplevelentry list

and toplevelentry =
  | TLE_Target of string
  | TLE_Datalayout of string
  | TLE_Definition of definition

and definition = {
  ret_typ: typ;
  name: ident;
  args: tident list;
  instrs: instr list;
}

and binop_assign = ident * typ * value * value

and instr =
  | INSTR_Add of binop_assign
  | INSTR_FAdd
  | INSTR_Sub of binop_assign
  | INSTR_FSub
  | INSTR_Mul of binop_assign
  | INSTR_FMul
  | INSTR_UDiv of binop_assign
  | INSTR_SDiv of binop_assign
  | INSTR_FDiv
  | INSTR_URem of binop_assign
  | INSTR_SRem of binop_assign
  | INSTR_FRem
  | INSTR_Shl of binop_assign
  | INSTR_LShr of binop_assign
  | INSTR_AShr of binop_assign
  | INSTR_And of binop_assign
  | INSTR_Or of binop_assign
  | INSTR_Xor of binop_assign
  | INSTR_ICmp of (ident * cmp * typ * value * value)
  | INSTR_FCmp
  | INSTR_PHI of (ident * typ * (value * ident) list)
  | INSTR_Call of (ident * typ * ident * (typ * value) list)
  | INSTR_Trunc
  | INSTR_ZExt
  | INSTR_SExt
  | INSTR_FPTrunc
  | INSTR_FPExt
  | INSTR_UIToFP
  | INSTR_SIToFP
  | INSTR_FPToUI
  | INSTR_FPToSI
  | INSTR_IntToPtr
  | INSTR_PtrToInt
  | INSTR_BitCast
  | INSTR_Select
  | INSTR_VAArg
  | INSTR_Ret of (typ * value)
  | INSTR_Br of (value * value * value) (*types are constant *)
  | INSTR_Br_1 of value
  | INSTR_Switch of (typ * value * value * (typ * value * value) list)
  | INSTR_IndirectBr
  | INSTR_Invoke of (typ * ident * (typ * value) list * value * value)
  | INSTR_Resume of (typ * value)
  | INSTR_Unreachable
  | INSTR_Alloca of (ident * typ)
  | INSTR_Load
  | INSTR_Store
  | INSTR_AtomicCmpXchg
  | INSTR_AtomicRMW
  | INSTR_Fence
  | INSTR_GetElementPtr
  | INSTR_ExtractElement
  | INSTR_InsertElement
  | INSTR_ShuffleVector
  | INSTR_ExtractValue
  | INSTR_InsertValue
  | INSTR_LandingPad
  | INSTR_Label of ident

