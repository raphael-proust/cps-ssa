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

(* SSA terms. We only enforce SSA property dynamically. *)

let label_main = Prim.label "main"

type prog = proc list

and proc = {
  p_args  : Prim.var list;
  p_blocks: block list; (* First block is entry block. Hence it dominates
                           non-dead blocks *)
}

and block = {
  mutable b_order  : int;
  (*   *) b_label  : Prim.label;
  (*   *) b_phis   : phi list;
  (*   *) b_core_instrs: core_instr list;
  (*   *) b_jump   : jump;
}

and core_instr =
  | IAssignExpr of (Prim.var * Prim.expr)
  | IAssigncall of (Prim.var * Prim.label * Prim.expr list)
  | IMemWrite of (Prim.var * Prim.mem_w)

and jump =
  | Jgoto of (Prim.label)
  | Jreturn of Prim.expr
  | Jreturnvoid
  | Jtail of (Prim.label * Prim.expr list * Prim.label)
  | Jcond of (Prim.expr * Prim.label * Prim.label)

and phi = Prim.var * (Prim.label * Prim.expr) list

(* inefficient! but only used with a low number of blocks *)
(*TODO: memoize or build a map before use *)
let block_of_label blocks label =
  try
    List.find (fun p -> p.b_label = label) blocks
  with
  | Not_found as e ->
    Printf.eprintf "Block not found: %s" (Prim.string_of_label label);
    raise e

let check_ssa prog =

  (* no procedure should be empty *)
  List.for_all (fun p -> p.p_blocks <> []) prog &&

  (* one procedure is the "main" *)
  Util.L.exists_one (fun p -> (List.hd p.p_blocks).b_label = label_main) prog &&

  let blocks = Util.L.concat_map (fun p -> p.p_blocks) prog in

  (* no two labels are identical *)
  Util.L.unique (fun b -> Some b.b_label) blocks &&

  (* no two assignments share their rhs variable *)
  Util.L.unique
    (function
      | IAssignExpr (v, _)
      | IAssigncall (v, _, _) -> Some v
      | IMemWrite _ -> None
    )
    (Util.L.concat_map (fun b -> b.b_core_instrs) blocks)

  (* TODO: check def dominates use (requires dominator info, not necessary) *)
  (* TODO? do one pass check? *)


(* For building trivial blocks *)
module Blocks = struct

  let block ?(label = Prim.fresh_label ()) ?(phis = []) ?(instrs = []) j = {
          b_order = 0;
          b_label = label;
           b_phis = phis;
    b_core_instrs = instrs;
           b_jump = j;
  }

  let return ?label ?phis ?instrs e =
    block ?label ?phis ?instrs (Jreturn e)

  let return_const ?label ?phis ?instrs c =
    return ?label ?phis ?instrs Prim.(ONone (Vconst c))

  let return_0 ?label ?phis ?instrs () = return_const ?label ?phis ?instrs 0

  let return_var ?label ?phis ?instrs v =
    return ?label ?phis ?instrs Prim.(ONone (Vvar v))

  let cond ?label ?phis ?instrs e l1 l2 =
    block ?label ?phis ?instrs (Jcond (e, l1, l2))

  let tail ?label ?phis ?instrs l es d =
    block ?label ?phis ?instrs (Jtail (l, es, d))

  let goto ?label ?phis ?instrs l =
    block ?label ?phis ?instrs (Jgoto l)

end

(* For building simpl procs *)
module Procs = struct

  let proc ?(args = []) blocks = {
      p_args = args;
    p_blocks = blocks;
  }

  let block ?args b = proc ?args [b]

  let cond ?label ?args e b1 b2 =
    proc ?args [Blocks.cond ?label e b1.b_label b2.b_label; b1; b2;]

  let cond_e ?label ?args e e1 e2 =
    let b1 = Blocks.return e1 in
    let b2 = Blocks.return e2 in
    cond ?label ?args e b1 b2

end

(*TODO? some ast -> ssa automatic translator?*)
