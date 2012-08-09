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

(*NOTICE: this is a prototype, it is known not to be:
  - feature complete
  - bug free
*)

(*We use a custom representation *)
type g =
  | GAppRet  of (Prim.var * Prim.value list)
  | GAppCont  of (Prim.var * Prim.value list * Prim.var)
  | GAppBind of (Prim.var * Prim.value list * (Prim.var * g))
  | GCont of (Prim.var * Prim.value list)
  | GCond of (  Prim.value
              * (Prim.var * Prim.value list)
              * (Prim.var * Prim.value list)
             )
  | GBind of ((int * (Prim.var * Prim.value) list ) list * g)
  | GLoop of (Prim.var * Prim.var list * (Prim.var * (Prim.var list * g)) list * g * g)
  | GLambda  of ((Prim.var * (Prim.var list * g)) list * g)


let assert_value ~env value =
  let open Prim in
  let rec aux = function
    | VVar v -> assert (List.mem v env)
    | VConst _ | VNull | VUndef | VDummy _ | VZero -> ()
    | VStruct vs -> List.iter aux vs
    | VPlus  (v1, v2) -> aux v1; aux v2
    | VMult  (v1, v2) -> aux v1; aux v2
    | VMinus (v1, v2) -> aux v1; aux v2
    | VDiv   (v1, v2) -> aux v1; aux v2
    | VRem   (v1, v2) -> aux v1; aux v2
    | VGt (v1, v2) -> aux v1; aux v2
    | VGe (v1, v2) -> aux v1; aux v2
    | VLt (v1, v2) -> aux v1; aux v2
    | VLe (v1, v2) -> aux v1; aux v2
    | VEq (v1, v2) -> aux v1; aux v2
    | VNe (v1, v2) -> aux v1; aux v2
    | VAnd (v1, v2) -> aux v1; aux v2
    | VOr  (v1, v2) -> aux v1; aux v2
    | VXor (v1, v2) -> aux v1; aux v2
    | VRead v -> aux v
    | VCast v -> aux v
    | VShl  (v1, v2) -> aux v1; aux v2
    | VLShr (v1, v2) -> aux v1; aux v2
    | VAShr (v1, v2) -> aux v1; aux v2
  in
  aux value

let assert_values ~env vs = List.iter (assert_value ~env) vs

let has ~env v = assert (List.mem v env)
let hasnt ~env v = assert (not (List.mem v env))
let ext1 ~env v = hasnt ~env v; v :: env
let ext ~env vs = List.iter (hasnt ~env) vs; vs @ env

(*TODO: rewrite with better env *)
let assert_g ~env g =
  let rec aux ~env g =
    match g with
    | GAppRet (v, vs) ->
      has ~env v;
      assert_values ~env vs
    | GAppCont (v, vs, k) ->
      assert (not (k = CPS.var_return));
      has ~env v;
      assert_values ~env vs;
      has ~env k
    | GAppBind (v, vs, (x, g)) ->
      has ~env v;
      assert_values ~env vs;
      aux ~env:(ext1 ~env x) g
    | GCont (k, vs) ->
      has ~env k;
      assert_values ~env vs
    | GCond (v, (k1, vs1), (k2, vs2)) ->
      assert_value env v;
      has ~env k1; assert_values ~env vs1;
      has ~env k2; assert_values ~env vs2
    | GBind (bs, g) ->
      let (env, _) =
        List.fold_left
          (fun (env, r) (rank, bs) ->
            assert (r < rank);
            let (vars, values) = L.unzip bs in
            List.iter (assert_value ~env) values;
            (ext ~env vars, rank)
          )
          (env, -1)
          bs
      in
      aux ~env g
    | GLoop (v, vs, ls, g1, g2) ->
      (*TODO: check ls's call graph*)
      let (names, lambdas) = L.unzip ls in
      aux ~env:(ext1 ~env v) g2;
      (*DONT: add v to g1's environment environment *)
      let env = ext ~env vs in
      let env = ext ~env names in
      aux ~env g1;
      List.iter (fun (vs, g) -> aux ~env:(ext ~env vs) g) lambdas
    | GLambda (ls, g) ->
      let env =
        List.fold_left
          (fun env (v, (vs, g)) ->
            hasnt ~env v;
            (*DONT add v to g's env, (it's not under a GLoop!)*)
            aux ~env:(ext ~env vs) g;
            ext1 ~env v
          )
          env
          ls
      in
      aux ~env g
  in
  aux ~env g

let rec m_of_g g =
  let lambda (v, (vs, g)) = (v, (vs, m_of_g g)) in
  match g with
  | GAppRet (v, vs) -> CPS.MApp (v, vs, CPS.CVar CPS.var_return)
  | GAppCont (v, vs, k) -> CPS.MApp (v, vs, CPS.CVar k)
  | GAppBind (v, vs, (x, g)) -> CPS.MApp (v, vs, CPS.C (x, m_of_g g))
  | GCont (k, vs) -> CPS.MCont (k, vs)
  | GCond (v, (k1, vs1), (k2, vs2)) -> CPS.MCond (v, (k1, vs1), (k2, vs2))
  | GBind (bs, g) ->
    List.fold_right
      (fun (_, bs) term ->
        List.fold_left (fun term (x, v) -> CPS.MLet (x, v, term)) term bs
      )
      bs
      (m_of_g g)
  | GLoop (v, vs, ls, g1, g2) ->
    CPS.MRec ([v, (vs, CPS.MRec (List.map lambda ls, m_of_g g1))], m_of_g g2)
  | GLambda (ls, g) ->
    CPS.MRec (List.map lambda ls, m_of_g g)



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

