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

(* Pprint Operators and other facilities *)
module PP = struct
  include Pprint
  include Util.PP
end
open PP.Operators

let pp_phi_entry (l, e) = Prim_pp.pp_label l ^^ PP.colon ^^ Prim_pp.pp_expr e

let pp_phi (v, les) =
  Prim_pp.pp_var v ^^ PP.space ^^ PP.equals ^^ PP.space ^^
  (!^ "phi") ^^ PP.with_paren (
    PP.list ~sep:PP.comma_space pp_phi_entry les
  )

let pp_core_instr = function
  | SSA.IAssignExpr (v, e) ->
    Prim_pp.pp_var v ^^ PP.space ^^ PP.equals ^^ PP.space ^^ Prim_pp.pp_expr e
  | SSA.IAssigncall (v, l, es) ->
    Prim_pp.pp_var v ^^ PP.space ^^ PP.equals ^^ PP.space ^^
    Prim_pp.pp_label l ^^ PP.with_paren (
      PP.list ~sep:PP.comma_space Prim_pp.pp_expr es
    )
  | SSA.IMemWrite (v, m) -> !^ "store" ^^ PP.space ^^
    Prim_pp.pp_var v ^^ PP.space ^^ Prim_pp.pp_mem_w m

let pp_jump = function
  | SSA.Jgoto l -> !^ "goto" ^^ PP.space ^^ Prim_pp.pp_label l
  | SSA.Jreturn e -> !^ "return" ^^ PP.space ^^ Prim_pp.pp_expr e
  | SSA.Jreturnvoid -> !^ "return"
  | SSA.Jtail (l, es, d) ->
    !^ "tailcall" ^^ PP.space ^^ Prim_pp.pp_label l ^^ PP.with_paren (
      PP.list ~sep:PP.comma_space Prim_pp.pp_expr es
    ) ^^ PP.space ^^ (!^ "to") ^^ PP.space ^^ Prim_pp.pp_label d
  | SSA.Jcond (e, l1, l2) ->
    !^ "branch" ^^ PP.space ^^ PP.with_paren (Prim_pp.pp_expr e) ^^ PP.space ^^
    Prim_pp.pp_label l1 ^^ PP.space ^^
    Prim_pp.pp_label l2

let pp_block {SSA.b_label; b_phis; b_core_instrs; b_jump;} =
  Prim_pp.pp_label b_label ^^ (!^ ":") ^^ PP.break1 ^^ PP.level (
    PP.list ~sep:PP.break1 pp_phi b_phis ^^ PP.break1 ^^
    PP.list ~sep:PP.break1 pp_core_instr b_core_instrs ^^ PP.break1 ^^
    pp_jump b_jump) ^^ PP.break1 ^^
  PP.break1

let pp_entry_block {SSA.eb_core_instrs; eb_jump} =
  PP.list ~sep:PP.break1 pp_core_instr eb_core_instrs ^^ PP.break1 ^^
  pp_jump eb_jump ^^ PP.break1 ^^
  PP.break1

let pp_proc {SSA.p_name; p_args; p_entry_block; p_blocks;} =
  Prim_pp.pp_label p_name ^^ PP.with_paren (
    PP.list ~sep:PP.comma_space Prim_pp.pp_var p_args
  ) ^^
  PP.lbrace ^^ PP.break1 ^^
    PP.level (pp_entry_block p_entry_block ^^ PP.break1) ^^ PP.break1 ^^
    PP.list ~sep:(PP.break1 ^^ PP.break1) pp_block p_blocks ^^
    PP.break1 ^^
  PP.rbrace ^^ PP.break1

let pp_prog = PP.list ~sep:(PP.break1 ^^ PP.break1 ^^ PP.break1) pp_proc

