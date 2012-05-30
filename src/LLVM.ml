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

type typ =
  | Typ_I of int
  | Typ_Pointer of typ
  | Typ_Label
  | Typ_Void

type ident =
  | Id_Global of Prim.var
  | Id_Local  of Prim.var

type tident = typ * ident

type value =
  | Vvar of ident
  | Vconst of Prim.const

type tvalue = typ * value

type prog = proc list

and proc = {
  ret_typ: typ;
  name: Prim.label;
  args: tident list;
  instrs: instr list;
}

and instr =
  | Alloca of (ident * typ)
  | Add of (ident * typ * value * value)
  | Store of (tident * tvalue)
  | Load of (ident * typ * ident)
  | Icmp of (ident * icmp * typ * value * value)
  | Br_1 of (Prim.label)
  | Br of (value * Prim.label * Prim.label)
  | Ret of tvalue
  | Label of Prim.label
  | Phi of (ident * typ * (value * Prim.label) list)

and icmp =
  | Eq
  | Ne
  | Ugt
  | Uge
  | Ult
  | Ule
  | Sgt
  | Sge
  | Slt
  | Sle

