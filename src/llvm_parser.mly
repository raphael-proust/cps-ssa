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
(*TODO: floats *)
(*TODO: don't throw things away *)

%token<string> GLOBAL LOCAL
%token LPAREN RPAREN LCURLY RCURLY LTLCURLY RCURLYGT LSQUARE RSQUARE LT GT EQ COMMA EOF EOL STAR

%token<string> STRING
%token<int> INTEGER
%token<float> FLOAT
%token KW_NULL KW_UNDEF KW_TRUE KW_FALSE KW_ZEROINITIALIZER

%token<string> LABEL

%token KW_DEFINE KW_DECLARE KW_TARGET KW_DATALAYOUT KW_TRIPLE
%token KW_PRIVATE KW_LINKER_PRIVATE KW_LINKER_PRIVATE_WEAK KW_LINKER_PRIVATE_WEAK_DEF_AUTO KW_INTERNAL KW_AVAILABLE_EXTERNALLY KW_LINKONCE KW_WEAK KW_COMMON KW_APPENDING KW_EXTERN_WEAK KW_LINKONCE_ODR KW_WEAK_ODR KW_EXTERNAL KW_DLLIMPORT KW_DLLEXPORT
%token KW_DEFAULT KW_HIDDEN KW_PROTECTED
%token KW_CCC KW_FASTCC KW_COLDCC KW_CC
%token KW_UNNAMED_ADDR
%token KW_TYPE KW_X KW_OPAQUE
%token KW_GLOBAL KW_ADDRSPACE KW_CONSTANT KW_SECTION KW_THREAD_LOCAL
%token KW_ZEROEXT KW_SIGNEXT KW_INREG KW_BYVAL KW_SRET KW_NOALIAS KW_NOCAPTURE KW_NEST
%token KW_ADDRESS_SAFETY KW_ALIGNSTACK KW_ALWAYSINLINE KW_NONLAZYBIND KW_INLINEHINT KW_NAKED KW_NOIMPLICITFLOAT KW_NOINLINE KW_NOREDZONE KW_NORETURN KW_NOUNWIND KW_OPTSIZE KW_READNONE KW_READONLY KW_RETURNS_TWICE KW_SSP KW_SSPREQ KW_UWTABLE
%token KW_ALIGN
%token KW_GC
%token KW_ADD KW_FADD KW_SUB KW_FSUB KW_MUL KW_FMUL KW_UDIV KW_SDIV KW_FDIV KW_UREM KW_SREM KW_FREM KW_SHL KW_LSHR KW_ASHR KW_AND KW_OR KW_XOR KW_ICMP KW_FCMP KW_PHI KW_CALL KW_TRUNC KW_ZEXT KW_SEXT KW_FPTRUNC KW_FPEXT KW_UITOFP KW_SITOFP KW_FPTOUI KW_FPTOSI KW_INTTOPTR KW_PTRTOINT KW_BITCAST KW_SELECT KW_VAARG KW_RET KW_BR KW_SWITCH KW_INDIRECTBR KW_INVOKE KW_RESUME KW_UNREACHABLE KW_ALLOCA KW_LOAD KW_STORE KW_ATOMICCMPXCHG KW_ATOMICRMW KW_FENCE KW_GETELEMENTPTR KW_EXTRACTELEMENT KW_INSERTELEMENT KW_SHUFFLEVECTOR KW_EXTRACTVALUE KW_INSERTVALUE KW_LANDINGPAD
%token<int> I
%token KW_VOID KW_HALF KW_FLOAT KW_DOUBLE KW_X86_FP80 KW_FP128 KW_PPC_FP128 KW_LABEL KW_METADATA KW_X86_MMX
%token KW_UNWIND KW_TO
%token KW_NUW KW_NSW
%token KW_EXACT
%token KW_EQ KW_NE KW_UGT KW_UGE KW_ULT KW_ULE KW_SGT KW_SGE KW_SLT KW_SLE
%token KW_TAIL
%token KW_VOLATILE


%start<LLVM.module_> module_

%%

module_:
  | EOL* m = list(toplevelentry_eol) EOF { m }

toplevelentry_eol:
  | tle = toplevelentry EOL+ { tle }

toplevelentry:
  | d = definition                        { TLE_Definition d     }
  | d = declaration                       { TLE_Declaration d    }
  | KW_TARGET KW_DATALAYOUT EQ s = STRING { TLE_Datalayout s     }
  | KW_TARGET KW_TRIPLE EQ s = STRING     { TLE_Target s         }
  | i = ident EQ KW_TYPE t = typ          { TLE_Type_decl (i, t) }
  | g = global_decl                       { TLE_Global g         }

global_decl:
  | g_ident = ident EQ
      linkage? visibility? KW_THREAD_LOCAL? addrspace? KW_UNNAMED_ADDR?
      g_constant = global_is_constant g_typ = typ g_value = value
      comma_section? comma_align?
      { {g_ident; g_typ; g_constant = false; g_value;} }

global_is_constant:
  | KW_GLOBAL { false }
  | KW_CONSTANT { true }

addrspace:
  | KW_ADDRSPACE LPAREN n = INTEGER RPAREN { n }

comma_section:
  | COMMA KW_SECTION s = STRING { s }

definition:
  | KW_DEFINE linkage? visibility? cconv?
           df_ret_typ = ret_type df_name = global
           LPAREN df_args = separated_list(COMMA, df_arg) RPAREN
           list(fn_attr) align? gc?
           LCURLY EOL+
           df_instrs = procedure_body
           RCURLY
    { {df_ret_typ; df_name; df_args; df_instrs;} }

declaration:
  | KW_DECLARE linkage? visibility? cconv? KW_UNNAMED_ADDR?
           dc_ret_typ = ret_type dc_name = global
           LPAREN dc_args = separated_list(COMMA, dc_arg) RPAREN
           list(fn_attr) align? gc?
    { {dc_ret_typ; dc_name; dc_args;} }

linkage:
  | KW_PRIVATE                      { LINKAGE_Private                      }
  | KW_LINKER_PRIVATE               { LINKAGE_Linker_private               }
  | KW_LINKER_PRIVATE_WEAK          { LINKAGE_Linker_private_weak          }
  | KW_LINKER_PRIVATE_WEAK_DEF_AUTO { LINKAGE_Linker_private_weak_def_auto }
  | KW_INTERNAL                     { LINKAGE_Internal                     }
  | KW_AVAILABLE_EXTERNALLY         { LINKAGE_Available_externally         }
  | KW_LINKONCE                     { LINKAGE_Linkonce                     }
  | KW_WEAK                         { LINKAGE_Weak                         }
  | KW_COMMON                       { LINKAGE_Common                       }
  | KW_APPENDING                    { LINKAGE_Appending                    }
  | KW_EXTERN_WEAK                  { LINKAGE_Extern_weak                  }
  | KW_LINKONCE_ODR                 { LINKAGE_Linkonce_odr                 }
  | KW_WEAK_ODR                     { LINKAGE_Weak_odr                     }
  | KW_EXTERNAL                     { LINKAGE_External                     }
  | KW_DLLIMPORT                    { LINKAGE_Dllimport                    }
  | KW_DLLEXPORT                    { LINKAGE_Dllexport                    }

visibility:
  | KW_DEFAULT   { VISIBILITY_Default   }
  | KW_HIDDEN    { VISIBILITY_Hidden    }
  | KW_PROTECTED { VISIBILITY_Protected }

cconv:
  | KW_CCC          { CC_Ccc    }
  | KW_FASTCC       { CC_Fastcc }
  | KW_COLDCC       { CC_Coldcc }
  | KW_CC n=INTEGER { CC_Cc n   }

ret_type:
  | list(typ_attr) t = typ { t }

typ:
  | n = I                                    { TYPE_I n              }
  | KW_VOID                                  { TYPE_Void             }
  | KW_HALF                                  { TYPE_Half             }
  | KW_FLOAT                                 { TYPE_Float            }
  | KW_DOUBLE                                { TYPE_Double           }
  | KW_X86_FP80                              { TYPE_X86_fp80         }
  | KW_FP128                                 { TYPE_Fp128            }
  | KW_PPC_FP128                             { TYPE_Ppc_fp128        }
  | KW_LABEL                                 { TYPE_Label            }
  | KW_METADATA                              { TYPE_Metadata         }
  | KW_X86_MMX                               { TYPE_X86_mmx          }
  | t = typ STAR                             { TYPE_Pointer t        }
  | i = ident                                { TYPE_Ident i          }
  | LSQUARE n = INTEGER KW_X t = typ RSQUARE { TYPE_Array (n, t)     }
  | t = typ LPAREN ts = separated_list(COMMA, typ) RPAREN
                                             { TYPE_Function (t, ts) }
  | LCURLY ts = separated_list(COMMA, typ) RCURLY
                                             { TYPE_Struct ts        }
  | LTLCURLY ts = separated_list(COMMA, typ) RCURLYGT
                                             { TYPE_Packed_struct ts }
  | KW_OPAQUE                                { TYPE_Opaque           }
  | LT n = INTEGER KW_X t = typ GT           { TYPE_Vector (n, t)    }

typ_i:
  | n = I { n }

typ_attr:
  | KW_ZEROEXT   { TYPEATTR_Zeroext   }
  | KW_SIGNEXT   { TYPEATTR_Signext   }
  | KW_INREG     { TYPEATTR_Inreg     }
  | KW_BYVAL     { TYPEATTR_Byval     }
  | KW_SRET      { TYPEATTR_Sret      }
  | KW_NOALIAS   { TYPEATTR_Noalias   }
  | KW_NOCAPTURE { TYPEATTR_Nocapture }
  | KW_NEST      { TYPEATTR_Nest      }

df_arg:
  | t = typ list(typ_attr) i = ident { (t, i) }

dc_arg:
  | t = typ list(typ_attr) { t }

call_arg:
  | t = typ list(typ_attr) i = value { (t, i) }

fn_attr:
  | KW_ADDRESS_SAFETY                      { FNATTR_Address_safety  }
  | KW_ALIGNSTACK LPAREN p = power2 RPAREN { FNATTR_Alignstack p    }
  | KW_ALWAYSINLINE                        { FNATTR_Alwaysinline    }
  | KW_NONLAZYBIND                         { FNATTR_Nonlazybind     }
  | KW_INLINEHINT                          { FNATTR_Inlinehint      }
  | KW_NAKED                               { FNATTR_Naked           }
  | KW_NOIMPLICITFLOAT                     { FNATTR_Noimplicitfloat }
  | KW_NOINLINE                            { FNATTR_Noinline        }
  | KW_NOREDZONE                           { FNATTR_Noredzone       }
  | KW_NORETURN                            { FNATTR_Noreturn        }
  | KW_NOUNWIND                            { FNATTR_Nounwind        }
  | KW_OPTSIZE                             { FNATTR_Optsize         }
  | KW_READNONE                            { FNATTR_Readnone        }
  | KW_READONLY                            { FNATTR_Readonly        }
  | KW_RETURNS_TWICE                       { FNATTR_Returns_twice   }
  | KW_SSP                                 { FNATTR_Ssp             }
  | KW_SSPREQ                              { FNATTR_Sspreq          }
  | KW_UWTABLE                             { FNATTR_Uwtable         }

power2:
  | n = INTEGER { assert (List.mem n [0;1;2;4;8;16;32;64]); n }

align:
  | KW_ALIGN power2 { }

gc:
  | KW_GC STRING { }

procedure_body: (*TODO*)
  | il = list(instr_eol) { il }

instr_eol:
  | i = instr EOL+ { i }

%public binop(KW):
  | i = ident EQ KW t = typ o1 = value COMMA o2 = value
    { (i, t, o1, o2) }

%public binop1(KW,OPT1):
  | i = ident EQ KW OPT1? t = typ o1 = value COMMA o2 = value
    { (i, t, o1, o2) }

%public binop2(KW,OPT1,OPT2):
  | i = ident EQ KW OPT1? OPT2? t = typ o1 = value COMMA o2 = value
    { (i, t, o1, o2) }

%public conversion(KW):
  | i = ident EQ KW t = typ v = value KW_TO t2 = typ
    { (i, t, v, t2) }

%public icmp(KW):
  | i = ident EQ KW_ICMP KW t = typ o1 = value COMMA o2 = value
    { (i, t, o1, o2) }

instr:
  (* arith, binop *)
  | b = binop2(KW_ADD,KW_NUW,KW_NSW) { INSTR_Add  b }
  | KW_FADD (*TODO*)                 { INSTR_FAdd   }
  | b = binop2(KW_SUB,KW_NUW,KW_NSW) { INSTR_Sub  b }
  | KW_FSUB (*TODO*)                 { INSTR_FSub   }
  | b = binop2(KW_MUL,KW_NUW,KW_NSW) { INSTR_Mul  b }
  | KW_FMUL (*TODO*)                 { INSTR_FMul   }
  | b = binop1(KW_UDIV,KW_EXACT)     { INSTR_UDiv b }
  | b = binop1(KW_SDIV,KW_EXACT)     { INSTR_SDiv b }
  | KW_FDIV (*TODO*)                 { INSTR_FDiv   }
  | b = binop (KW_UREM)              { INSTR_URem b }
  | b = binop (KW_SREM)              { INSTR_SRem b }
  | KW_FREM  (*TODO*)                { INSTR_FRem   }

  (* bitwise, binop *)
  | b = binop(KW_SHL)  { INSTR_Shl  b }
  | b = binop(KW_LSHR) { INSTR_LShr b }
  | b = binop(KW_ASHR) { INSTR_AShr b }
  | b = binop(KW_AND)  { INSTR_And  b }
  | b = binop(KW_OR)   { INSTR_Or   b }
  | b = binop(KW_XOR)  { INSTR_Xor  b }

  (* comparison *)
  (*TODO: factorisation. How?*)
  | i = icmp(KW_EQ ) { let i, t, o1, o2 = i in INSTR_ICmp (i, Cmp_Eq,  t, o1, o2) }
  | i = icmp(KW_NE ) { let i, t, o1, o2 = i in INSTR_ICmp (i, Cmp_Ne,  t, o1, o2) }
  | i = icmp(KW_UGT) { let i, t, o1, o2 = i in INSTR_ICmp (i, Cmp_Ugt, t, o1, o2) }
  | i = icmp(KW_UGE) { let i, t, o1, o2 = i in INSTR_ICmp (i, Cmp_Uge, t, o1, o2) }
  | i = icmp(KW_ULT) { let i, t, o1, o2 = i in INSTR_ICmp (i, Cmp_Ult, t, o1, o2) }
  | i = icmp(KW_ULE) { let i, t, o1, o2 = i in INSTR_ICmp (i, Cmp_Ule, t, o1, o2) }
  | i = icmp(KW_SGT) { let i, t, o1, o2 = i in INSTR_ICmp (i, Cmp_Sgt, t, o1, o2) }
  | i = icmp(KW_SGE) { let i, t, o1, o2 = i in INSTR_ICmp (i, Cmp_Sge, t, o1, o2) }
  | i = icmp(KW_SLT) { let i, t, o1, o2 = i in INSTR_ICmp (i, Cmp_Slt, t, o1, o2) }
  | i = icmp(KW_SLE) { let i, t, o1, o2 = i in INSTR_ICmp (i, Cmp_Sle, t, o1, o2) }

  | KW_FCMP           { INSTR_FCmp } (*TODO*)

  (* phi *)
  | i = ident EQ KW_PHI t = typ
                     table = separated_nonempty_list(COMMA, phi_table_entry)
    { INSTR_PHI (i, t, table) }

  (* call *)
  | i = ident EQ KW_TAIL? KW_CALL cconv? list(typ_attr) t = typ
                 n = ident LPAREN a = separated_list(COMMA, call_arg) RPAREN
                 list(fn_attr)
    { INSTR_Call (i, t, n, a) }
  | KW_TAIL? KW_CALL cconv? list(typ_attr) t = typ
                 n = ident LPAREN a = separated_list(COMMA, call_arg) RPAREN
                 list(fn_attr)
    { INSTR_Call_unit (t, n, a) }

  (* conversions *)
  | c = conversion(KW_TRUNC)    { INSTR_Trunc c    }
  | c = conversion(KW_ZEXT)     { INSTR_ZExt c     }
  | c = conversion(KW_SEXT)     { INSTR_SExt c     }
  | c = conversion(KW_FPTRUNC)  { INSTR_FPTrunc c  }
  | c = conversion(KW_FPEXT)    { INSTR_FPExt c    }
  | c = conversion(KW_UITOFP)   { INSTR_UIToFP c   }
  | c = conversion(KW_SITOFP)   { INSTR_SIToFP c   }
  | c = conversion(KW_FPTOUI)   { INSTR_FPToUI c   }
  | c = conversion(KW_FPTOSI)   { INSTR_FPToSI c   }
  | c = conversion(KW_INTTOPTR) { INSTR_IntToPtr c }
  | c = conversion(KW_PTRTOINT) { INSTR_PtrToInt c }
  | c = conversion(KW_BITCAST)  { INSTR_BitCast c  }

  (* other *)
  | KW_SELECT { INSTR_Select }
  | KW_VAARG  { INSTR_VAArg  }

  (* terminator *)
  | KW_RET t = typ o = value { INSTR_Ret (t, o) }
  | KW_RET KW_VOID           { INSTR_Ret_void }
  | KW_BR t = typ_i o = value COMMA
          KW_LABEL o1 = ident COMMA KW_LABEL o2 = ident
    { assert (t = 1); INSTR_Br (o, o1, o2) }
  | KW_BR KW_LABEL o = ident       { INSTR_Br_1 o }
  | KW_SWITCH t = typ v = value COMMA
              KW_LABEL def = value
              LSQUARE table = list(switch_table_entry) RSQUARE
    { INSTR_Switch (t, v, def, table) }
  | KW_INDIRECTBR     { INSTR_IndirectBr } (*TODO *)
  | KW_INVOKE cconv? t = ret_type i = ident
              LPAREN a = separated_list(COMMA, call_arg) RPAREN
              list(fn_attr)
              KW_TO KW_LABEL l1 = ident
              KW_UNWIND KW_LABEL l2 = ident
    { INSTR_Invoke (t, i, a, l1, l2)  }
  | KW_RESUME t = typ o = value { INSTR_Resume (t, o) }
  | KW_UNREACHABLE    { INSTR_Unreachable }

  (* memory instrs, partial support *)
  | i = ident EQ KW_ALLOCA t = typ comma_align? { INSTR_Alloca (i, t) } (*TODO: support NumElements *)
  | i = ident EQ KW_LOAD KW_VOLATILE? tp = typ p = ident comma_align? (*TODO: support more options *)
    { INSTR_Load (i, tp, p) }
  | KW_STORE KW_VOLATILE? tv = typ v = value COMMA
                          ti = typ i = ident
                          comma_align? (*TODO: support atomic and non-temporal*)
    { assert (match ti with | TYPE_Pointer _ -> true | _ -> false);
      INSTR_Store (tv, v, ti, i) }
  | KW_ATOMICCMPXCHG { INSTR_AtomicCmpXchg }
  | KW_ATOMICRMW     { INSTR_AtomicRMW     }
  | KW_FENCE         { INSTR_Fence         }
  | KW_GETELEMENTPTR { INSTR_GetElementPtr }

  (* vector ops, not supported *)
  | KW_EXTRACTELEMENT { INSTR_ExtractElement }
  | KW_INSERTELEMENT  { INSTR_InsertElement  }
  | KW_SHUFFLEVECTOR  { INSTR_ShuffleVector  }

  (* aggregate ops, not supported *)
  | KW_EXTRACTVALUE { INSTR_ExtractValue }
  | KW_INSERTVALUE  { INSTR_InsertValue  }
  | KW_LANDINGPAD   { INSTR_LandingPad   }

  (* explicit labels *)
  | l = LABEL { INSTR_Label (ID_Local l) }

comma_align:
  | COMMA align { }

phi_table_entry:
  | LSQUARE v = value COMMA l = ident RSQUARE { (v, l) }
switch_table_entry:
  | t = typ o = value COMMA KW_LABEL l = ident { (t, o, l) }

value:
  | i = INTEGER        { VALUE_Integer i  }
  | f = FLOAT          { VALUE_Float f    }
  | KW_TRUE            { VALUE_Bool true  }
  | KW_FALSE           { VALUE_Bool false }
  | i = ident          { VALUE_Ident i    }
  | KW_NULL            { VALUE_Null       }
  | KW_UNDEF           { VALUE_Undef      }
  | LCURLY l = separated_list(COMMA, typ_value) RCURLY
                       { VALUE_Struct l }
  | LTLCURLY l = separated_list(COMMA, typ_value) RCURLYGT
                       { VALUE_Struct l }
  | LSQUARE l = separated_list(COMMA, typ_value) RSQUARE
                       { VALUE_Array l }
  | LT l = separated_list(COMMA, typ_value) GT
                       { VALUE_Vector l }
  | KW_ZEROINITIALIZER { VALUE_Zero_initializer }

typ_value:
  | t = typ i = value { (t,i) }

ident:
  | l = global
  | l = local    { l }

local:
  | l = LOCAL { ID_Local l }

global:
  | g = GLOBAL { ID_Global g }

