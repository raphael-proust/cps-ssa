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

module BlockVertex = struct
  type t = int ref * SSA.block (* ref is ugly, but Graph only has iterator
                                  trasversal (no map, no fold *)
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
    G.E.create (ref 0, b1) () (ref 0, b2)
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
  let graph =
    List.fold_left (* list of (list of jumps | blocks) *)
      (fun g block ->
        List.fold_left (* (list of jumps | block) *)
          G.add_edge_e
          g
          (vertices_of_block map_ref blocks block)
      )
      G.empty
      blocks
  in
  (graph, !map_ref)

(* Once we have a graph, we build a postorder *)

module DFS_Traverse = Graph.Traverse.Dfs(G)

let mark_postorder g =
  let id      = ref 0 in
  DFS_Traverse.postfix
    (fun (o, b) -> incr id; o := !id)
    g

let dom_of_graph g = (* We temporarily go in imperative mode. *)
  failwith "TODO"


let dom_of_blocks blocks = failwith "TODO"
