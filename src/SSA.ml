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

open Util

(* labels are for jump. Type is abstracted in the interface. *)
type label = string

let label s = (("l_" ^ s) : label)
external string_of_label : label -> string = "%identity"

let label_counter = ref (-1)

let reset_idxs () =
  label_counter := (-1)

let fresh_label =
  fun () ->
    incr label_counter;
    "l__" ^ string_of_int !label_counter

(* Standard conversion for when one translates labels into closure variables. *)
let var_of_label l = Prim.var l
let label_of_var v = label (Prim.string_of_var v)

(* SSA terms. We only enforce SSA property dynamically. *)

let label_main = "@entry"


type core_instr =
  | IAssignExpr   of (Prim.var * Prim.value)
  | IAssignCall   of (Prim.var * (Prim.var * Prim.value list))
  | IAssignSelect of (Prim.var * Prim.value * Prim.value * Prim.value)
  | ICall         of (Prim.var * Prim.value list)
  | IMemWrite     of (Prim.var * Prim.mem_w)

type jump =
  | JGoto       of label
  | JReturn     of Prim.value
  | JReturnVoid
  | JTail       of (Prim.var * Prim.value list * label)
  | JCond       of (Prim.value * label * label)

type phi = Prim.var * (label * Prim.value) list

type entry_block = {
  mutable eb_order      : int;
  (*   *) eb_label      : label;
  (*   *) eb_core_instrs: core_instr list;
  (*   *) eb_jump       : jump;
}

type block = {
  mutable b_order      : int;
  (*   *) b_label      : label;
  (*   *) b_phis       : phi list;
  (*   *) b_core_instrs: core_instr list;
  (*   *) b_jump       : jump;
}

type proc = {
  p_name       : Prim.var;
  p_args       : Prim.var list;
  p_entry_block: entry_block;
  p_blocks     : block list;
}

type module_ = proc list

type prog = proc * module_

let prog m l = L.pick_one_such_as (fun p -> p.p_name = l) m

let labels_of_jump = function
  | JReturn _ | JReturnVoid | JTail _ -> []
  | JGoto l -> [l]
  | JCond (_, l1, l2) -> [l1;l2]

(*TODO? memoize or build a map before use? *)
let block_of_label proc label =
  if label = proc.p_entry_block.eb_label then
    E.Left proc.p_entry_block
  else
    try
      E.Right (List.find (fun b -> b.b_label = label) proc.p_blocks)
    with
    | Not_found as e ->
      Printf.eprintf "Block not found: %s\nAvailable blocks: %s %s\n"
        (string_of_label label)
        (string_of_label proc.p_entry_block.eb_label)
        (String.concat " "
          (List.map (fun b -> string_of_label b.b_label) proc.p_blocks)
        )
        ;
      raise e

let check_module module_ =

  assert (L.unique (fun p -> Some p.p_name) module_);

  let check_proc proc =

    let labels = List.map (fun b -> b.b_label) proc.p_blocks in
    let jumps =
      List.concat (
           labels_of_jump proc.p_entry_block.eb_jump
        :: List.map (fun b -> labels_of_jump b.b_jump) proc.p_blocks
      )
    in
    assert (
      List.for_all
        (fun l -> l <> proc.p_entry_block.eb_label && List.mem l labels)
        jumps
    );

    (* no two labels are identical *)
    assert (L.unique (fun b -> Some b.b_label) proc.p_blocks);

    (* no internal block has entry block label *)
    assert (
      List.for_all
        (fun p ->
          List.for_all
            (fun b -> b.b_label <> p.p_entry_block.eb_label)
            p.p_blocks
        )
        module_
    );

    (* no two assignments share their lhs variable *)
    assert (L.unique
      (function
        | IAssignExpr (v, _)
        | IAssignCall (v, _)
        | IAssignSelect (v, _, _, _) -> Some v
        | ICall _ | IMemWrite _ -> None
      )
      (L.concat_map (fun b -> b.b_core_instrs) proc.p_blocks)
    )
  in
  List.iter check_proc module_

  (* TODO: check def dominates use (requires dominator info, not necessary) *)


let check_prog (main,module_) = check_module (main :: module_)


(* For making entry blocks *)
module Entry_blocks = struct

  let entry_block ?label ?(instrs = []) eb_jump =
    let eb_label = O.unopt_soft fresh_label label in
    {
            eb_order = 0;
            eb_label;
      eb_core_instrs = instrs;
             eb_jump;
    }

  let return ?label ?instrs e =
    entry_block ?label ?instrs (JReturn e)
  let return_void ?label ?instrs () =
    entry_block ?label ?instrs JReturnVoid
  let return_const ?label ?instrs c =
    return ?label ?instrs (Prim.VConst c)
  let return_0 ?label ?instrs () =
    return_const ?label ?instrs 0
  let return_var ?label ?instrs v =
    return ?label ?instrs (Prim.VVar v)

  let cond ?label ?instrs e l1 l2 =
    entry_block ?label ?instrs (JCond (e, l1, l2))

  let tail ?label ?instrs l es d =
    entry_block ?label ?instrs (JTail (l, es, d))

  let goto ?label ?instrs l =
    entry_block ?label ?instrs (JGoto l)

end

(* For building trivial blocks *)
module Blocks = struct

  let block ?label ?(phis = []) ?(instrs = []) b_jump =
    let b_label = O.unopt_soft fresh_label label in
    {
            b_order = 0;
            b_label;
             b_phis = phis;
      b_core_instrs = instrs;
             b_jump;
    }

  let return ?label ?phis ?instrs e =
    block ?label ?phis ?instrs (JReturn e)
  let return_void ?label ?phis ?instrs () =
    block ?label ?phis ?instrs JReturnVoid
  let return_const ?label ?phis ?instrs c =
    return ?label ?phis ?instrs (Prim.VConst c)
  let return_0 ?label ?phis ?instrs () = return_const ?label ?phis ?instrs 0
  let return_var ?label ?phis ?instrs v =
    return ?label ?phis ?instrs (Prim.VVar v)

  let cond ?label ?phis ?instrs e l1 l2 =
    block ?label ?phis ?instrs (JCond (e, l1, l2))

  let tail ?label ?phis ?instrs l es d =
    block ?label ?phis ?instrs (JTail (l, es, d))

  let goto ?label ?phis ?instrs l =
    block ?label ?phis ?instrs (JGoto l)

end

(* For building simpl procs *)
module Procs = struct

  let proc ~name ?(args = []) p_entry_block p_blocks = {
           p_name = name;
           p_args = args;
    p_entry_block;
         p_blocks;
  }

  let entry_block ~name ?args eb = proc ~name ?args eb []

  let cond ~name ?args e b1 b2 =
    proc ~name ?args (Entry_blocks.cond e b1.b_label b2.b_label) [b1; b2;]

  let cond_e ~name ?args e e1 e2 =
    let b1 = Blocks.return e1 in
    let b2 = Blocks.return e2 in
    cond ~name ?args e b1 b2

end
