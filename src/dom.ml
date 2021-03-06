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

open Util (* provides L, O, and E for Lists, Options, and Either respectively *)

type node = (SSA.entry_block, SSA.block) E.either
(*Let's have a persistent graph. We only use labels in the functor argument
  because SSA's hypotheses lets us do it. *)
module BlockVertex = struct
  type t = node
  let compare b1 b2 = match (b1, b2) with
   | E.Left _  , E.Right _  -> -1
   | E.Left e1 , E.Left e2  -> assert (e1 = e2); 0
   | E.Right _ , E.Left _   -> 1
   | E.Right b1, E.Right b2 -> Pervasives.compare b1.SSA.b_label b2.SSA.b_label
  let hash b = match b with
   | E.Left e  -> Hashtbl.hash "entryblock"
   | E.Right b -> Hashtbl.hash b.SSA.b_label
  let equal b1 b2 = match (b1, b2) with
   | E.Left _  , E.Right _  -> false
   | E.Left e1 , E.Left e2  -> assert (e1 = e2); true
   | E.Right _ , E.Left _   -> false
   | E.Right b1, E.Right b2 -> b1.SSA.b_label = b2.SSA.b_label
end

module G = Graph.Persistent.Digraph.ConcreteBidirectional(BlockVertex)

let vertices_of_jump proc v0 j =
  List.map
    (fun l -> G.E.create v0 () (SSA.block_of_label proc l))
    (SSA.labels_of_jump j)

let vertices_of_block proc b =
  vertices_of_jump proc (E.Right b) b.SSA.b_jump

let vertices_of_entry_block proc eb =
  vertices_of_jump proc (E.Left eb) eb.SSA.eb_jump

let graph_of_proc proc =
  let add_edges graph edges = List.fold_left G.add_edge_e graph edges in
  (* straight-forward translation: we iterate over the blocks adding vertices
     and edges. *)
  let initial_graph =
    add_edges
      (G.add_vertex G.empty (E.Left proc.SSA.p_entry_block))
      (vertices_of_entry_block proc proc.SSA.p_entry_block)
  in
  let graph =
    List.fold_left (* list of (list of jumps | blocks) *)
      (fun g block ->
        let rblock = E.Right block in
        add_edges (G.add_vertex g rblock) (vertices_of_block proc block)
      )
      initial_graph
      proc.SSA.p_blocks
  in
  graph

(* Once we have a graph, we build a postorder *)

module DFS_Traverse = Graph.Traverse.Dfs(G)

let mark_postorder g lentry =
  (* This modifies the b_order field of the blocks. *)
  (*FIXME? when graph is not connex, only translates the entry component, is it
    FIXME? a bug or a feature? *)
  let id = ref 0 in
  let process = ref [] in
  DFS_Traverse.postfix_component
    (fun t ->
      begin match t with
      | E.Left eb -> eb.SSA.eb_order <- !id
      | E.Right b ->  b.SSA.b_order  <- !id
      end ;
      incr id; process := t :: !process;
    )
    g
    lentry
    ;
  !process (* We return a list of blocks in the order they should be processed
              in the dominator fixpoint research. *)

let order = function
  | E.Left eb -> eb.SSA.eb_order
  | E.Right b -> b.SSA.b_order

let intersect dom b1 b2 =
  (*dominators intersection: based on Cooper, Harvey, and Kennedy*)
  let rec aux b1 b2 =
    if order b1 = order b2 then begin
      assert (b1 = b2);
      b1
    end else if order b1 < order b2 then begin
      aux (O.unopt_hard dom.(order b1)) b2
    end else if order b1 > order b2 then begin
      aux b1 (O.unopt_hard dom.(order b2))
    end else begin
      assert false
    end
  in
  aux b1 b2

(* with post-order and DAG-translation, we can translate any procedure. *)

let dom_of_proc proc =

  let lentry = E.Left proc.SSA.p_entry_block in

  let graph = graph_of_proc proc in

  (*init*)
  let dom = Array.make (G.nb_vertex graph) None in
  let process = mark_postorder graph lentry in
  assert (lentry = List.hd process);
  assert (List.for_all (G.mem_vertex graph) process);
  dom.(order (lentry)) <- Some (lentry);
  let changed = ref true in

  (*main loop with fixpoint detection*)
  while !changed do
    changed := false;
    List.iter
      (fun b ->
        let (new_idom, others) =
          L.pick_one_such_as
            (fun b -> dom.(order b) <> None)
            (G.pred graph b)
        in
        let new_idom = ref new_idom in
        List.iter
          (fun p ->
            begin
              if dom.(order b) <> None then
                new_idom := intersect dom p !new_idom
            end
          )
          others;
        begin
          if dom.(order b) <> Some !new_idom then begin
            dom.(order b) <- Some !new_idom;
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
          G.add_edge_e !domref (G.E.create b () (O.unopt_hard dom.(order b)))
      )
      graph;
    !domref
  in

  (* remove dummy entry->entry edge *)
  let dom_tree = G.remove_edge dom_tree lentry lentry in

  dom_tree
