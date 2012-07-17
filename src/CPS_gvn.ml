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
(*TODO: simplify some boilerplate code *)

open Util

(*The first part deals with cfg modifications*)

let rec calls_of_m = function
  (* rec call *)
  | CPS.MLet  (_, _, m) | CPS.MSel (_, _, _, _, m) | CPS.MSeq (_, _, m) ->
    calls_of_m m
  (* calls *)
  | CPS.MApp  (_, _, c) -> calls_of_cont c
  | CPS.MCont (v, _) -> [v]
  | CPS.MCond (_, (v1, _), (v2, _)) -> [v1; v2]
  (* rec def *)
  | CPS.MRec (ls, m) ->
    List.flatten (calls_of_m m :: List.map (fun (_, (_, m)) -> calls_of_m m) ls)

and calls_of_cont = function
  | CPS.CVar v -> [v]
  | CPS.C (_, m) -> calls_of_m m


let rec is_cond = function
  | CPS.MApp (_,_, CPS.CVar _) | CPS.MCont _ -> false
  | CPS.MCond _ -> true
  | CPS.MApp (_,_, CPS.C (_, m))
  | CPS.MLet (_, _, m)
  | CPS.MSel (_, _, _, _, m)
  | CPS.MRec (_, m)
  | CPS.MSeq (_, _, m) -> is_cond m

let split_edge (varlist, var, m) =
  failwith "TODO"

let rec subs pairs term =
  let value v =
    Prim.value_map ~var:(fun x -> try List.assoc x pairs with Not_found -> x) v
  in
  CPS.m_map ~value term

let is_while_loop l m =
  is_cond m && failwith "TODO"

let rec insert_landing_lambdas_l (l, (vs, m)) =
  let mcalls = calls_of_m m in
  if not (List.mem l mcalls) then
    (l, (vs, insert_landing_lambdas_m m))
  else
    if is_while_loop l m then
      failwith "TODO"
    else
      let fl = Prim.fresh_var () in
      let fvs = List.map (fun _ -> Prim.fresh_var ()) vs in
      (l,
       (vs,
        CPS.MRec ([(fl, (fvs, subs (L.zip vs fvs) m))],
                  CPS.MCont (fl, List.map (fun x -> Prim.VVar x) vs))))

and insert_landing_lambdas_m = function
  (* terminator *)
  | CPS.MCont _ | CPS.MApp  (_, _, CPS.CVar _) | CPS.MCond _
    as m -> m
  (* folding *)
  | CPS.MApp (v, vs, CPS.C (x, m)) ->
    CPS.MApp (v, vs, CPS.C (x, insert_landing_lambdas_m m))
  | CPS.MLet (x, v, m) -> CPS.MLet (x, v, insert_landing_lambdas_m m)
  | CPS.MSeq (x, w, m) -> CPS.MSeq (x, w, insert_landing_lambdas_m m)
  | CPS.MSel (x, v, v1, v2, m) ->
    CPS.MSel (x, v, v1, v2, insert_landing_lambdas_m m)
  (* folding and recursive calls *)
  | CPS.MRec (ls, m) ->
    CPS.MRec (List.map insert_landing_lambdas_l ls, insert_landing_lambdas_m m)

let numbering m =
  (* How should we do that? Add data to AST-nodes? Build a node<->rk table? *)
  failwith "TODO"
