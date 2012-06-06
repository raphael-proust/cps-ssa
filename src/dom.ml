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

open Util (* provides L and O *)

(*Let's have a persistent graph. We only use labels in the functor argument
  because SSA's hypotheses lets us do it. *)
module BlockVertex = struct
  type t = SSA.block
  let compare b1 b2 = Pervasives.compare b1.SSA.b_label b2.SSA.b_label
  let hash b  = Hashtbl.hash b.SSA.b_label
  let equal b1 b2 = b1.SSA.b_label = b2.SSA.b_label
end

module G = Graph.Persistent.Digraph.ConcreteBidirectional(BlockVertex)

(* *VERY* inefficient! *)
(*TODO: memoize or build a map before use *)
let block_of_label blocks label =
  List.find (fun p -> p.SSA.b_label = label) blocks

let vertices_of_block blocks b =
  (* we get a list of jumps out of a block *)
  match b.SSA.b_jump with
  (* inter-procedural jumps are ignored in the translation *)
  | SSA.Jreturnvoid | SSA.Jreturn _ | SSA.Jtail _ -> []
  (* intra-procedural simple jump *)
  | SSA.Jgoto label -> [G.E.create b () (block_of_label blocks label)]
  (* intra-procedural conditional jump *)
  | SSA.Jcond (_, label1, label2) ->
    [G.E.create b () (block_of_label blocks label1);
     G.E.create b () (block_of_label blocks label2);
    ]

let graph_of_blocks blocks =
  (* straight-forward translation: we iterate over the blocks adding vertices
     and edges. *)
  let graph =
    List.fold_left (* list of (list of jumps | blocks) *)
      (fun g block ->
        List.fold_left (* (list of jumps | block) *)
          G.add_edge_e
          (G.add_vertex g block)
          (vertices_of_block blocks block)
      )
      G.empty
      blocks
  in
  graph

(* Once we have a graph, we build a postorder *)

module DFS_Traverse = Graph.Traverse.Dfs(G)

let mark_postorder g =
  (* This modifies the b_order field of the blocks. *)
  (*FIXME: does not work if graph is not connex: entry node is not top node.*)
  let id = ref 0 in
  let process = ref [] in
  DFS_Traverse.postfix
    (fun b ->
      b.SSA.b_order <- !id; incr id;
      process := b :: !process
    )
    g;
  !process (* We return a list of blocks in the order they should be processed
              in the dominator fixpoint research. *)


let intersect dom b1 b2 =
  (*dominators intersection: based on Cooper, Harvey, and Kennedy*)
  let rec aux b1 b2 =
    if b1.SSA.b_order = b2.SSA.b_order then begin
      assert (b1 = b2);
      b1
    end else if b1.SSA.b_order < b2.SSA.b_order then begin
      aux (O.unopt dom.(b1.SSA.b_order)) b2
    end else if b1.SSA.b_order > b2.SSA.b_order then begin
      aux b1 (O.unopt dom.(b2.SSA.b_order))
    end else begin
      assert false
    end
  in
  aux b1 b2

(* with post-order and DAG-translation, we can translate any procedure. *)

let dom_of_blocks = function
  | [] -> assert false (* this is captured by [SSA.check_ssa]. *)
  | entry::subs as blocks ->

    assert (entry.SSA.b_order = 0);

    let graph = graph_of_blocks blocks in

    (*init*)
    let dom = Array.make (G.nb_vertex graph) None in
    let process = mark_postorder graph in
    assert (entry = List.hd process);
    assert (List.for_all (G.mem_vertex graph) process);
    dom.(entry.SSA.b_order) <- Some entry;
    let changed = ref true in

    (*main loop with fixpoint detection*)
    while !changed do
      changed := false;
      List.iter
        (fun b ->
          let (new_idom, others) =
            L.pick_one_such_as
              (fun b -> dom.(b.SSA.b_order) <> None)
              (G.pred graph b)
          in
          let new_idom = ref new_idom in
          List.iter
            (fun p ->
              begin
                if dom.(b.SSA.b_order) <> None then
                  new_idom := intersect dom p !new_idom
              end
            )
            others;
          begin
            if dom.(b.SSA.b_order) <> Some !new_idom then begin
              dom.(b.SSA.b_order) <- Some !new_idom;
              changed := true
            end
          end
        )
        (List.tl process)
    done;

    let dom_tree =
      let domref = ref G.empty in
      G.iter_vertex
        (fun b ->
          domref :=
            G.add_edge_e !domref (G.E.create b () (O.unopt dom.(b.SSA.b_order)))
        )
        graph;
      !domref
    in

    dom_tree
