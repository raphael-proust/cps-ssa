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

let paren = true (* for ~paren *)

let pp_label l = !^ (SSA.string_of_label l)

let pp_phi_entry (l, e) = pp_label l ^^ PP.colon ^^ Prim_pp.pp_value e

let pp_phi (v, les) =
  Prim_pp.pp_var v ^^ PP.space ^^ PP.equals ^^ PP.space ^^
  (!^ "phi") ^^ PP.with_paren (
    PP.list ~sep:PP.comma_space pp_phi_entry les
  )

let pp_phi_br phi = pp_phi phi ^^ PP.break1

let pp_call (l, es) =
    Prim_pp.pp_var l ^^ PP.with_paren (
      PP.list ~sep:PP.comma_space Prim_pp.pp_value es
    )

let pp_assign v = Prim_pp.pp_var v ^^ PP.space ^^ PP.equals ^^ PP.space

let pp_core_instr = function
  | SSA.IAssignExpr (v, e) -> pp_assign v ^^ Prim_pp.pp_value e
  | SSA.IAssignCall (v, call) -> pp_assign v ^^ pp_call call
  | SSA.ICall call -> pp_call call
  | SSA.IAssignSelect (v, c, v1, v2) ->
    pp_assign v ^^ (!^ "select") ^^ PP.space ^^
      Prim_pp.pp_value ~paren c ^^ PP.space ^^
      Prim_pp.pp_value ~paren v1 ^^ PP.space ^^
      Prim_pp.pp_value ~paren v2
  | SSA.IMemWrite (v, m) -> !^ "store" ^^ PP.space ^^
    Prim_pp.pp_var v ^^ PP.space ^^ Prim_pp.pp_mem_w m

let pp_core_instr_br ci = pp_core_instr ci ^^ PP.break1

let pp_jump = function
  | SSA.JGoto l -> !^ "goto" ^^ PP.space ^^ pp_label l
  | SSA.JReturn e -> !^ "return" ^^ PP.space ^^ Prim_pp.pp_value ~paren e
  | SSA.JReturnVoid -> !^ "return"
  | SSA.JTail (l, es, d) ->
    !^ "tailcall" ^^ PP.space ^^ Prim_pp.pp_var l ^^ PP.with_paren (
      PP.list ~sep:PP.comma_space Prim_pp.pp_value es
    ) ^^ PP.space ^^ (!^ "to") ^^ PP.space ^^ pp_label d
  | SSA.JCond (e, l1, l2) ->
    !^ "branch" ^^ PP.space ^^ Prim_pp.pp_value ~paren e ^^ PP.space ^^
    pp_label l1 ^^ PP.space ^^
    pp_label l2

let pp_block {SSA.b_label; b_phis; b_core_instrs; b_jump;} =
  pp_label b_label ^^ (!^ ":") ^^ PP.level (PP.break0 ^^
    PP.list ~sep:PP.empty pp_phi_br b_phis ^^
    PP.list ~sep:PP.empty pp_core_instr_br b_core_instrs ^^
    pp_jump b_jump)

let pp_entry_block {SSA.eb_label; eb_core_instrs; eb_jump} =
  pp_label eb_label ^^ (!^ ":" ) ^^ PP.level (PP.break0 ^^
    PP.list ~sep:PP.empty pp_core_instr_br eb_core_instrs ^^
    pp_jump eb_jump
  )

let pp_proc {SSA.p_name; p_args; p_entry_block; p_blocks;} =
  Prim_pp.pp_var p_name ^^ PP.with_paren (
    PP.list ~sep:PP.comma_space Prim_pp.pp_var p_args
  ) ^^
  PP.lbrace ^^
    PP.level (PP.break0 ^^ pp_entry_block p_entry_block) ^^ PP.break1 ^^
    PP.list ~sep:(PP.break1)
      (fun b -> PP.level (PP.break0 ^^ pp_block b))
      p_blocks ^^ PP.break1 ^^
  PP.rbrace ^^ PP.break1

let pp_module = PP.list ~sep:(PP.break1 ^^ PP.break1 ^^ PP.break1) pp_proc

let pp_prog (main, module_) =
  !^ "main" ^^ PP.hardline ^^
  PP.level (PP.break0 ^^ pp_proc main) ^^ PP.hardline ^^
  PP.hardline ^^
  PP.level (PP.break0 ^^ pp_module module_)

