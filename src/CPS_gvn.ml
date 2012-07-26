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

(*FIXME: avoid exponential complexity *)
(*FIXME: avoid head-assoc-body "conversion" *)
(*TODO: simplify some boilerplate code *)

open Util

(*The first part deals with cfg modifications*)

let rec calls_of_m = function
  | CPS.MLet _ | CPS.MSel _ | CPS.MSeq _ | CPS.MRec _ -> []
  (* calls *)
  | CPS.MApp  (_, _, c) -> calls_of_cont c
  | CPS.MCont (v, _) -> [v]
  | CPS.MCond (_, (v1, _), (v2, _)) -> [v1; v2]

and calls_of_cont = function
  | CPS.CVar v -> [v]
  | CPS.C    _ -> [ ]

let all_calls_of_m m = List.flatten (List.map calls_of_m (CPS.subterms m))

let is_cond = function
  | CPS.MCond _ -> true
  | _ -> false (*FIXME: catch-all case*)

let rec final_term = function
  | CPS.MApp  (_, _, CPS.CVar _)
  | CPS.MCont (_, _)
  | CPS.MCond (_, _, _) as m -> m
  | CPS.MApp  (_, _, CPS.C (_, m))
  | CPS.MLet  (_, _, m)
  | CPS.MSel  (_, _, _, _, m)
  | CPS.MSeq  (_, _, m)
  | CPS.MRec  (_, m) -> final_term m

let is_deep_cond m = is_cond (final_term m)

let lambdas_of_m = function
  | CPS.MLet _ | CPS.MSel _ | CPS.MSeq _ | CPS.MApp  _ | CPS.MCont _
  | CPS.MCond _ -> []
  | CPS.MRec (ls, _) -> ls

let head (l, _     ) = l
let args (_, (a, _)) = a
let body (_, (_, m)) = m
let heads = List.map head
let argss = List.map args
let bodys = List.map body

(* Landing Lambdas *)
(* Problem: the complexity for finding cliques is too important *)

let call_graph ls =
  List.map
    (fun l -> (head l, L.inter (heads ls) (all_calls_of_m (body l)), l))
    ls

let get_trans_cliques callgraph
  : (Prim.var * Prim.var list) list -> Prim.var list list * Prim.var list
  (* --head--   ----calls----          -----cliques------   ----other---- *)
  = failwith "TODO: extract cliques in the transitive closure of the callgraph"

let named_lambda_of_name ls l =
  List.find (fun ll -> l = head ll) ls

let loop_of_clique clique ls m =
  let clique = List.map (named_lambda_of_name ls) clique in
  let clique_bar = L.minus ls clique in
  let entries =
    L.inter
      clique
      (List.map
        (named_lambda_of_name ls)
        (List.flatten
          (   all_calls_of_m m
           :: List.map (fun l -> all_calls_of_m (body l)) clique_bar)))
  in
  let conds =
    List.filter (fun l -> is_deep_cond (body l)) clique
  in
  (
  (clique, entries, conds)
  : (CPS.named_lambda list * CPS.named_lambda list * CPS.named_lambda list))

let dispatch i args ls =
  let args_value = List.map (fun v -> Prim.VVar v) args in
  let rec aux k = function
  | [] -> assert false
  | [l] -> CPS.MCont (l, args_value)
  | l::ls ->
    let d = Prim.fresh_var () in
    CPS.MRec ([d, (i :: args, aux (k+1) ls)],
              CPS.MCond (Prim.(VEq (VVar i, VConst k)),
                         (l, args_value),
                         (d, (Prim.VConst (k+1) :: args_value))))
  in
  aux 1 ls

let loop_substitute (l, e, c) args =
  assert (L.includes l e);
  assert (L.includes e c);
  match (e,c) with
  | [], _ -> (*dead code*) failwith "TODO: what to do?"
  | [_], [] -> failwith "TODO"
  | [_], [_] -> failwith "TODO"
  | [_], _::_::_ -> assert false
  | _::_::_, [] -> failwith "TODO"
  | _::_::_, _::_ ->
    let f = Prim.fresh_var () in
    let i = Prim.fresh_var () in
    let c' =
      List.map (fun (_, (args, m)) -> (Prim.fresh_var (), (args, m))) c
    in
    (f,
     (i :: args,
      CPS.MRec (l @ e @ c', dispatch i args (heads (L.minus e c @ c')))))

