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
  (*   *) b_assigns: assign list;
  (*   *) b_jump   : jump;
}

and assign =
  | Aexpr of (Prim.var * Prim.expr)
  | Acall of (Prim.var * Prim.label * Prim.expr list)

and jump =
  | Jgoto of (Prim.label)
  | Jreturn of Prim.expr
  | Jtail of (Prim.label * Prim.expr list)
  | Jcond of (Prim.expr * Prim.label * Prim.label)

and phi = Prim.var * (Prim.label * Prim.expr) list

let check_ssa prog =

  (* no procedure should be empty *)
  List.for_all (fun p -> p.p_blocks <> []) prog &&

  (* one procedure is the "main" *)
  Util.L.exists_one (fun p -> (List.hd p.p_blocks).b_label = label_main) prog &&

  let blocks = Util.L.concat_map (fun p -> p.p_blocks) prog in

  (* no two labels are identical *)
  Util.L.unique (fun b -> b.b_label) blocks &&

  (* no two assignments share their rhs variable *)
  Util.L.unique
    (function | Aexpr (v, _) | Acall (v, _, _) -> v)
    (Util.L.concat_map (fun b -> b.b_assigns) blocks)

  (* TODO: check def dominates use (requires dominator info, not necessary) *)
  (* TODO? do one pass check? *)


(* For building trivial blocks *)
module Blocks = struct

  let return ?(label = Prim.fresh_label ()) ?(phis = []) ?(assigns = []) e = {
      b_order = 0;
      b_label = label;
       b_phis = phis;
    b_assigns = assigns;
       b_jump = Jreturn e;
  }

  let return_const ?label ?phis ?assigns c =
    return ?label ?phis ?assigns (Prim.ONone (Prim.Vconst c))

  let return_0 ?label ?phis ?assigns () = return_const ?label ?phis ?assigns 0

  let return_var ?label ?phis ?assigns v =
    return ?label ?phis ?assigns Prim.(ONone (Vvar v))

  let cond ?(label = Prim.fresh_label ()) ?(phis = []) ?(assigns = []) e l1 l2 =
    {
      b_order = 0;
      b_label = label;
       b_phis = phis;
    b_assigns = assigns;
       b_jump = Jcond (e, l1, l2);
    }

  let tail ?(label = Prim.fresh_label ()) ?(phis = []) ?(assigns = []) l vs = {
      b_order = 0;
      b_label = label;
       b_phis = phis;
    b_assigns = assigns;
       b_jump = Jtail (l, vs);
  }

  let goto ?(label = Prim.fresh_label ()) ?(phis = []) ?(assigns = []) l = {
      b_order = 0;
      b_label = label;
       b_phis = phis;
    b_assigns = assigns;
       b_jump = Jgoto l;
  }

end

(* For building simpl procs *)
module Procs = struct

  let block args b = {
      p_args = args;
    p_blocks = [ b ];
  }

  let cond ?(label = Prim.fresh_label ()) args e b1 b2 = {
      p_args = args;
    p_blocks = [
      Blocks.cond ~label e b1.b_label b2.b_label;
      b1;
      b2;
    ];
  }

  let cond_e ?label args e e1 e2 =
    let b1 = Blocks.return e1 in
    let b2 = Blocks.return e2 in
    cond ?label args e b1 b2


end

(*TODO? some ast -> ssa automatic translator?*)
