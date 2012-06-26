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

(** Labels. Like variables but for procedure block naming. *)
type label

(** Label generation. Similar to [var]. *)
val label : string -> label

val string_of_label: label -> string

(** A generic label generator. *)
val fresh_label : unit -> label

(** [label] to and from [var] translation. Usefull for translating procedures
    into lambdas and vice versa.
  *)

val var_of_label : label -> Prim.var
val label_of_var : Prim.var -> label

(** Instructions are assignements of direct expressions or function calls or
    memory writes. *)
type core_instr =
  | IAssignExpr   of (Prim.var * Prim.value)
  | IAssignCall   of (Prim.var * (Prim.var * Prim.value list))
  | IAssignSelect of (Prim.var * Prim.value * Prim.value * Prim.value)
  | ICall         of (Prim.var * Prim.value list)
  | IMemWrite     of (Prim.var * Prim.mem_w)

(** Jumps are for intra-procedure control-flow, returning to caller, tail-calls
    or conditional jumping.
  *)
type jump =
  | JGoto       of label
  | JReturn     of Prim.value
  | JReturnVoid
  | JTail       of (Prim.var * Prim.value list * label)
  | JCond       of (Prim.value * label * label)

(** SSA-magic is made of phi-functions. *)
type phi = Prim.var * (label * Prim.value) list

(** The entry block has no phi nodes. *)
type entry_block = {
  mutable eb_order      : int;
  (*   *) eb_label      : label;
  (*   *) eb_core_instrs: core_instr list;
  (*   *) eb_jump       : jump;
}

(** BLocks are not represented exactly as in Kesley's paper but the two forms
    are similar enough. A block has a label, some phi-functions, some assignement
    and one jump (conditional jumps allow for control flow).
  *)
type block = {
  mutable b_order      : int;
  (*   *) b_label      : label;
  (*   *) b_phis       : phi list;
  (*   *) b_core_instrs: core_instr list;
  (*   *) b_jump       : jump;
}

(** A procedure has arguments and a body (constituted of blocks). *)
type proc = {
  p_name       : Prim.var;
  p_args       : Prim.var list;
  p_entry_block: entry_block;
  p_blocks     : block list;
}

(** A module is a set of procedures. *)
type module_ = proc list

(** A Program is a main procedure and a set of those. *)
type prog = proc * module_

(** [prog m l] extrract the procedure labeled [l] from the module [m] and
    returns a program. *)
val prog: module_ -> Prim.var -> prog

(** list all the labels the jump jumps to. *)
val labels_of_jump: jump -> label list

(** find a block in a list by its label *)
val block_of_label: proc -> label -> (entry_block, block) Util.E.either

(** Label for program entry point. *)
val label_main : label

(** checks that the ssa program is indeed ssa. In particular, it checks that
    each variable is assigned to only once, and other things.
  *)
val check_prog : prog -> unit

val check_module: module_ -> unit


module Entry_blocks :sig

  val entry_block: ?label:label ->
    ?instrs:core_instr list -> jump -> entry_block

  val return: ?label:label ->
    ?instrs:core_instr list -> Prim.value -> entry_block
  val return_void: ?label:label ->
    ?instrs:core_instr list -> unit -> entry_block
  val return_const: ?label:label ->
    ?instrs:core_instr list -> int -> entry_block
  val return_0: ?label:label ->
    ?instrs:core_instr list -> unit -> entry_block
  val return_var: ?label:label ->
    ?instrs:core_instr list -> Prim.var -> entry_block

  val cond: ?label:label -> ?instrs:core_instr list ->
    Prim.value -> label -> label -> entry_block

  val tail: ?label:label -> ?instrs:core_instr list ->
    Prim.var -> Prim.value list -> label -> entry_block

  val goto: ?label:label ->
    ?instrs:core_instr list -> label -> entry_block

end

module Blocks :sig

  val block: ?label:label ->
    ?phis:phi list -> ?instrs:core_instr list ->
    jump -> block

  val return: ?label:label ->
    ?phis:phi list -> ?instrs:core_instr list ->
    Prim.value -> block
  val return_void: ?label:label ->
    ?phis:phi list -> ?instrs:core_instr list ->
    unit -> block
  val return_const: ?label:label ->
    ?phis:phi list -> ?instrs:core_instr list ->
    int -> block
  val return_0: ?label:label ->
    ?phis:phi list -> ?instrs:core_instr list ->
    unit -> block
  val return_var: ?label:label ->
    ?phis:phi list -> ?instrs:core_instr list ->
    Prim.var -> block

  val cond: ?label:label ->
    ?phis:phi list -> ?instrs:core_instr list ->
    Prim.value -> label -> label -> block

  val tail: ?label:label ->
    ?phis:phi list -> ?instrs:core_instr list ->
    Prim.var -> Prim.value list -> label -> block

  val goto: ?label:label ->
    ?phis:phi list -> ?instrs:core_instr list ->
    label -> block

end

module Procs : sig

  val proc: name:Prim.var -> ?args:Prim.var list ->
    entry_block -> block list -> proc

  val entry_block: name:Prim.var -> ?args:Prim.var list -> entry_block -> proc

  val cond:
       name:Prim.var
    -> ?args:Prim.var list
    -> Prim.value -> block -> block
    -> proc

  val cond_e:
       name:Prim.var
    -> ?args:Prim.var list
    -> Prim.value -> Prim.value -> Prim.value
    -> proc

end

(**/**)
val reset_idxs: unit -> unit
