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

(* type none of your business to protect the mutable ref *)
type noyb = int ref

module IntBlockVertex = struct
  type t = int ref * SSA.block (* ref is ugly, but Graph only has iterator
                                  trasversal (no map, no fold *)
  let compare = Pervasives.compare
  let hash    = Hashtbl.hash
  let equal   = (=)
end

module Gib = Graph.Persistent.Digraph.ConcreteBidirectional(IntBlockVertex)

module BlockVertex = struct
  type t = SSA.block
  let compare = Pervasives.compare
  let hash    = Hashtbl.hash
  let equal   = (=)
end

module G = Graph.Persistent.Digraph.ConcreteBidirectional(BlockVertex)

module OrderedBlock = struct
  type t = SSA.block
  let compare = Pervasives.compare
end

module M = Map.Make(OrderedBlock)

(* *VERY* inefficient! *)
(*TODO: memoize or build a map before use *)
let block_of_label blocks label =
  List.find (fun p -> p.SSA.b_label = label) blocks

(*TODO: add an integer tag to blocks so as to allow postorder treatment *)

let vertices_of_block map_ref blocks b =
  (* we get a list of jumps out of a block *)
  let create b1 b2 =
    let n1 = (ref 0, b1) in
    let n2 = (ref 0, b2) in
    map_ref := M.add b1 n1 !map_ref;
    map_ref := M.add b2 n2 !map_ref;
    Gib.E.create (ref 0, b1) () (ref 0, b2)
  in
  match b.SSA.b_jump with
  | SSA.Jreturn _ | SSA.Jtail _ -> []
  | SSA.Jgoto label -> [create b (block_of_label blocks label)]
  | SSA.Jcond (_, label1, label2) ->
    [create b (block_of_label blocks label1);
     create b (block_of_label blocks label2);
    ]

let graph_of_blocks blocks =
  let map_ref = ref M.empty in
  let (graph, process) =
    List.fold_left (* list of (list of jumps | blocks) *)
      (fun (g, l) block ->
        let gg =
          List.fold_left (* (list of jumps | block) *)
            Gib.add_edge_e
            g
            (vertices_of_block map_ref blocks block)
        in
        let ll = block :: l in
        (gg,ll)
      )
      (Gib.empty, [])
      blocks
  in
  (graph, process, !map_ref)

(* Once we have a graph, we build a postorder *)

module DFS_Traverse = Graph.Traverse.Dfs(Gib)

let mark_postorder g =
  let id      = ref 0 in
  DFS_Traverse.postfix
    (fun (o, b) -> incr id; o := !id)
    g

let dom_of_blocks blocks =

  let (graph, process, map) = graph_of_blocks blocks in
  let process = List.tl (List.rev process) in

  (* based on Cooper, Harvey, and Kennedy *)
  (*helpers*)
    let f b = M.find b map in (*find*)
    let n ib = !(fst ib) in (*num*)
    let nf b = n (f b) in (*num find*)
    let unopt = function
      | None -> assert false
      | Some v -> v
    in

  (*init*)
    let dom = Array.make (Gib.nb_vertex graph) None in
    mark_postorder graph;
    dom.(nf (List.hd blocks)) <- Some (f (List.hd blocks));
    let changed = ref true in

  (*dominators intersection*)
    let intersect ib1 ib2 =
      let rec aux ib1 ib2 =
        if n ib1 = n ib2 then begin
          assert (ib1 = ib2);
          ib1
        end else if n ib1 < n ib2 then
          aux (unopt dom.(n ib1)) ib2
        else if n ib1 > n ib2 then
          aux ib1 (unopt dom.(n ib2))
        else
          assert false
      in
      aux ib1 ib2
    in

  (*main loop with fixpoint detection*)
    while !changed do
      changed := false;
      List.iter
        (fun b ->
          let (new_idom, others) =
            Util.L.pick_one_such_as
              (fun ib -> dom.(n ib) <> None)
              (Gib.pred graph (f b))
          in
          let new_idom = ref new_idom in
          List.iter
            (fun p ->
              begin
                if dom.(nf b) <> None then
                  new_idom := intersect p !new_idom
              end;
              begin
                if dom.(nf b) <> Some !new_idom then begin
                  dom.(nf b) <- Some !new_idom;
                  changed := true
                end
              end
            )
            others
        )
        process
    done;

    let dom_tree =
      Array.fold_left
        (fun g ib ->
          let ib = unopt ib in
          G.add_edge_e
            g
            (G.E.create (snd ib) () (snd (unopt dom.(n ib))))
        )
        G.empty
        dom
    in

    dom_tree
