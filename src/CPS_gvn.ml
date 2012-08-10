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

  (*
  | GAppRet (v, vs)
  | GAppCont (v, vs, k)
  | GAppBind (v, vs, (x, g))
  | GCont (k, vs)
  | GCond (v, (k1, vs1), (k2, vs2))
  | GBind (bs, g)
  | GLoop (v, vs, ls, g1, g2)
  | GLambda (ls, g)
   *)


let assert_value ~env value =
  let open Prim in
  let rec aux = function
    | VVar v -> assert (Env.has ~env v)
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

let nits xs = List.map (fun x -> (x, ())) xs

let assert_g ~env g =
  let rec aux ~env g =
    match g with
    | GAppRet (v, vs) ->
      assert (Env.has ~env v);
      assert_values ~env vs
    | GAppCont (v, vs, k) ->
      assert (not (k = CPS.var_return));
      assert (Env.has env v);
      assert_values ~env vs;
      assert (Env.has ~env k)
    | GAppBind (v, vs, (x, g)) ->
      assert (Env.has env v);
      assert_values ~env vs;
      aux ~env:(Env.add1 ~env x ()) g
    | GCont (k, vs) ->
      assert (Env.has env k);
      assert_values ~env vs
    | GCond (v, (k1, vs1), (k2, vs2)) ->
      assert_value env v;
      assert (Env.has env k1); assert_values ~env vs1;
      assert (Env.has env k2); assert_values ~env vs2
    | GBind (bs, g) ->
      let (env, _) =
        List.fold_left
          (fun (env, r) (rank, bs) ->
            assert (r < rank);
            let (vars, values) = L.unzip bs in
            List.iter (assert_value ~env) values;
            (Env.add ~env (nits vars), rank)
          )
          (env, -1)
          bs
      in
      aux ~env g
    | GLoop (v, vs, ls, g1, g2) ->
      (*TODO: check ls's call graph*)
      let (names, lambdas) = L.unzip ls in
      aux ~env:(Env.add1 ~env v ()) g2;
      (*DONT: add v to g1's environment environment *)
      let env = Env.add ~env (nits vs) in
      let env = Env.add ~env (nits names) in
      aux ~env g1;
      List.iter (fun (vs, g) -> aux ~env:(Env.add ~env (nits vs)) g) lambdas
    | GLambda (ls, g) ->
      let env =
        List.fold_left
          (fun env (v, (vs, g)) ->
            assert (Env.hasnt ~env v);
            (*DONT add v to g's env, (it's not under a GLoop!)*)
            aux ~env:(Env.add ~env (nits vs)) g;
            Env.add1 ~env v ()
          )
          env
          ls
      in
      aux ~env g
  in
  aux ~env g

let rec m_of_g g =
  let lambdas ls = List.map (fun (v, (vs, g)) -> (v, (vs, m_of_g g))) ls in
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
    CPS.MRec ([v, (vs, CPS.MRec (lambdas ls, m_of_g g1))], m_of_g g2)
  | GLambda (ls, g) ->
    CPS.MRec (lambdas ls, m_of_g g)


(* Landing Lambdas *)
(* Problem: the complexity for finding cliques is too important *)

module MP = CPS.Prop (* CPS Term Properties*)
module GP = struct (* CPS_gvn Term Properties*)

  let head (l, _     ) = l
  let args (_, (a, _)) = a
  let body (_, (_, m)) = m
  let heads = List.map head
  let argss = List.map args
  let bodys = List.map body

  let rec subterms t =
    let lambdas ls = List.flatten (List.map subterms (bodys ls)) in
    match t with
    | GAppRet _ | GAppCont _ | GCont _ | GCond _ -> [t]
    | GAppBind (_, _, (_, g)) | GBind (_, g) -> t :: subterms g
    | GLoop (_, _, ls, g1, g2) -> t :: subterms g1 @ subterms g2 @ lambdas ls
    | GLambda (ls, g) -> t :: subterms g @ lambdas ls

  let rec calls = function
    | GAppRet _ | GAppBind _ | GBind _ | GLoop _ | GLambda _ -> []
    | GAppCont (_, _, k) | GCont (k, _)                      -> [k]
    | GCond (_, (k1, _), (k2, _))                            -> [k1; k2]

  let deep_calls m = List.flatten (List.map calls (subterms m))

  let is_cond = function
    | GCond _ -> true
    | GAppRet _ | GAppCont _ | GAppBind _ | GCont _ | GBind _ | GLoop _
    | GLambda _ -> false

  let is_terminator = function
    | GAppRet _ | GAppCont _ | GCont _ | GCond _ -> true
    | GAppBind _ | GBind _ | GLoop _ | GLambda _ -> false

  let rec terminator = function
    | GAppRet _ | GAppCont _ | GCont _ | GCond _ as g -> g
    | GAppBind (_, _, (_, g))
    | GBind (_, g)
    | GLoop (_, _, _, _, g) (*is this correct?*)
    | GLambda (_, g) -> terminator g

  let is_deep_cond m = is_cond (terminator m)

  let lambdas = function
    | GAppRet _ | GAppCont _ | GAppBind _ | GCont _ | GCond _ | GBind _ -> []
    | GLoop (_, _, ls, _, _) | GLambda (ls, _) -> ls
           (* is ^this^ correct? *)

end


let call_graph (ls : (Prim.var * (Prim.var list * g)) list)
  : (Prim.var * Prim.var list) list
  =
  List.map
    (fun l -> (GP.head l, L.inter (GP.heads ls) (GP.deep_calls (GP.body l))))
    ls

type clique_or_not =
  | Clique of Prim.var list
  | NotClique of Prim.var list

let get_trans_cliques (callgraph : (Prim.var * Prim.var list) list)
  : clique_or_not list
  = failwith "TODO: extract cliques in the transitive closure of the callgraph"
  (*TODO: sort elements in reverse callee-before-caller order*)
  (*TODO? use blobs of independant NotCliques together*)

let lambda_of_name ls n = List.find (fun l -> n = GP.head l) ls
let lambdas_of_names ls ns = List.map (lambda_of_name ls) ns

let rec g_of_m m =
  let lambdas ls = List.map (fun (v, (vs, m)) -> (v, (vs, g_of_m m))) ls in
  match m with
  | CPS.MApp  (v, vs, CPS.C (x, m)) -> GAppBind (v, vs, (x, g_of_m m))
  | CPS.MApp  (v, vs, CPS.CVar k) ->
    if k = CPS.var_return then
      GAppRet (v, vs)
    else
      GAppCont (v, vs, k)
  | CPS.MCont (v, vs) -> GCont (v, vs)
  | CPS.MCond (v, (k1, vs1), (k2, vs2)) -> GCond (v, (k1, vs1), (k2, vs2))
  | CPS.MLet  (x, v, m) -> strand [x,v] m
  | CPS.MRec  (ls, m) ->
    let ls = lambdas ls in
    let cg = call_graph ls in
    let cliqs = get_trans_cliques cg in
    List.fold_right
      (fun c g -> match c with
        | Clique c -> gloop (lambdas_of_names ls c) g
        | NotClique nc -> GLambda (lambdas_of_names ls nc, g)
      )
      cliqs
      (g_of_m m)
  | CPS.MSel  _ -> failwith "Unsupported MSel constructor"
  | CPS.MSeq  _ -> failwith "Unsupported MSeq constructor"

and strand bs m = match m with
  | CPS.MLet (x, v, m) -> strand ((x,v)::bs) m
  | m -> GBind ([-1, bs], g_of_m m) (* needs ranking *)

and gloop ls g =

  let dispatch d ls =
    let rec aux i = function
      | [] -> assert false
      | [l] -> GCont (GP.head l, failwith "TODO: parameters")
      | [l1;l2] ->
        GCond (Prim.(VEq (VVar d, VConst i)),
               (GP.head l1, failwith "TODO: parameters"),
               (GP.head l2, failwith "TODO: parameters")
              )
      | l :: ls ->
        let more = Prim.fresh_var () in
        GLambda (
          [more, ([], aux (succ i) ls)],
          GCond (
            Prim.(VEq (VVar d, VConst i)),
            (GP.head l, failwith "TODO: parameters"),
            (more, [])
          )
        )
    in
    aux 0 ls
  in

  let substitute_landing l ls g = failwith "TODO" in

  let landing = Prim.fresh_var () in
  let parameters = failwith "TODO!" in
  let dispatch_param = Prim.fresh_var () in
  GLoop (landing,
         dispatch_param :: parameters,
         ls,
         dispatch dispatch_param ls,
         substitute_landing landing ls g
  )

