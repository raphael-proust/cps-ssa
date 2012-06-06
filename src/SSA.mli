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

(** A program is a set of procedures. Exactly one of those has to be labeled
    with [label_main].
  *)
type prog = proc list

(** A procedure has arguments and a body (constituted of blocks). *)
and proc = {
  p_args  : Prim.var list;
  p_blocks: block list; (* First block is entry block. Hence it dominates
                           non-dead blocks *)
}

(** BLocks are not represented exactly as in Kesley's paper but the two forms
    are similar enough. A block has a label, some phi-functions, some assignement
    and one jump (conditional jumps allow for control flow).
  *)
and block = {
  mutable b_order : int;
  (*   *) b_label  : Prim.label;
  (*   *) b_phis   : phi list;
  (*   *) b_core_instrs: core_instr list;
  (*   *) b_jump   : jump;
}

(** Instructions are assignements of direct expressions or function calls or
    memory writes. *)
and core_instr =
  | IAssignExpr of (Prim.var * Prim.expr)
  | IAssigncall of (Prim.var * Prim.label * Prim.expr list)
  | IMemWrite of (Prim.var * Prim.mem_w)

(** Jumps are for intra-procedure control-flow, returning to caller, tail-calls
    or conditional jumping.
  *)
and jump =
  | Jgoto of (Prim.label)
  | Jreturn of Prim.expr
  | Jreturnvoid
  | Jtail of (Prim.label * Prim.expr list)
  | Jcond of (Prim.expr * Prim.label * Prim.label)

(** SSA-magic is made of phi-functions. *)
and phi = Prim.var * (Prim.label * Prim.expr) list

(** Label for program entry point. *)
val label_main : Prim.label

(** checks that the ssa program is indeed ssa. In particular, it checks that
    each variable is assigned to, only once, there is exactly one main procedure,
    and other things.
  *)
val check_ssa : prog -> bool


module Blocks :sig

  val block: ?label:Prim.label ->
    ?phis:phi list -> ?instrs:core_instr list ->
    jump -> block

  val return: ?label:Prim.label ->
    ?phis:phi list -> ?instrs:core_instr list ->
    Prim.expr -> block
  val return_const: ?label:Prim.label ->
    ?phis:phi list -> ?instrs:core_instr list ->
    int -> block
  val return_0: ?label:Prim.label ->
    ?phis:phi list -> ?instrs:core_instr list ->
    unit -> block
  val return_var: ?label:Prim.label ->
    ?phis:phi list -> ?instrs:core_instr list ->
    Prim.var -> block

  val cond: ?label:Prim.label ->
    ?phis:phi list -> ?instrs:core_instr list ->
    Prim.expr -> Prim.label -> Prim.label -> block

  val tail: ?label:Prim.label ->
    ?phis:phi list -> ?instrs:core_instr list ->
    Prim.label -> Prim.expr list -> block

  val goto: ?label:Prim.label ->
    ?phis:phi list -> ?instrs:core_instr list ->
    Prim.label -> block

end

module Procs : sig

  val proc: ?args:Prim.var list -> block list -> proc

  val block: ?args:Prim.var list -> block -> proc

  val cond:
       ?label:Prim.label
    -> ?args:Prim.var list
    -> Prim.expr -> block -> block
    -> proc

  val cond_e:
       ?label:Prim.label
    -> ?args:Prim.var list
    -> Prim.expr -> Prim.expr -> Prim.expr
    -> proc

end
