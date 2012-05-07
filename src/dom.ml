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

module Vertex = struct
  type t = SSA.block
  let compare = Pervasives.compare
  let hash    = Hashtbl.hash
  let equal   = (=)
end

module G = Graph.Persistent.Digraph.ConcreteBidirectional(Vertex)

(* *VERY* inefficient! *)
let block_of_label_proc proc label =
  List.find (fun p -> p.SSA.b_label = label) proc.SSA.p_blocks

let block_of_label prog label =
  let rec aux = function
    | [] -> raise Not_found
    | h::t ->
      try
        block_of_label_proc h label
      with
        | Not_found -> aux t
  in
  aux prog

(*TODO? how to handle inter-procedure calls *)

let vertices_of_block prog proc b =
  match b.SSA.b_jump with
  | SSA.Jreturn _ | SSA.Jtail _ -> []
  | SSA.Jgoto label -> [G.E.create b () (block_of_label_proc proc label)]
  | SSA.Jcond (_, label1, label2) ->
    [G.E.create b () (block_of_label_proc proc label1);
     G.E.create b () (block_of_label_proc proc label2);
    ]

let graph_of_ssa prog =
  List.fold_left
    (fun g proc ->
      List.fold_left
        (fun g block ->
          List.fold_left
            G.add_edge_e
            g
            (vertices_of_block prog proc block)
        )
        g
        proc.SSA.p_blocks
    )
    G.empty
    prog

let dom_of_graph g = failwith "TODO"
