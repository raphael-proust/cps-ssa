 (*                                                                          *
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
  *                                                                          *)


type prog = proc list

and proc = {
  p_args  : Prim.var list;
  p_blocks: block list; (* First block is entry block. Hence it dominates
                           non-dead blocks *)
}

and block = {
  b_label  : Prim.label;
  b_phis   : phi list;
  b_assigns: assign list;
  b_jump   : jump;
}

and assign =
  | Aexpr of (Prim.var * Prim.expr)
  | Acall of (Prim.var * Prim.var * Prim.expr list)

and jump =
  | Jgoto of (Prim.label)
  | Jreturn of Prim.expr
  | Jtail of (Prim.var * Prim.expr list)
  | Jcond of (Prim.expr * Prim.label * Prim.label)

and phi = Prim.var * (Prim.label * Prim.expr) list

val label_main : Prim.label

val check_ssa : prog -> bool


module Blocks :sig

  val expr: ?label:Prim.label -> Prim.expr -> block
  val const: ?label:Prim.label -> int -> block
  val zero: ?label:Prim.label -> unit -> block

  val cond: ?label:Prim.label -> Prim.expr -> Prim.label -> Prim.label -> block

end

module Procs : sig

  val block: Prim.var list -> block -> proc

  val cond:
       ?label:Prim.label
    -> Prim.var list
    -> Prim.expr -> block -> block
    -> proc

  val cond_e:
       ?label:Prim.label
    -> Prim.var list
    -> Prim.expr -> Prim.expr -> Prim.expr
    -> proc

end
