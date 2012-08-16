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

(*NOTICE: this is a prototype, it is known *not* to be:
  - feature complete
  - bug free
*)

(*We use a custom representation *)
type g =
  | GAppCont of (Prim.var * Prim.value list * Prim.var      )
  | GAppBind of (Prim.var * Prim.value list * (Prim.var * g))
  | GCont of (Prim.var * Prim.value list)
  | GCond of (  Prim.value
              * (Prim.var * Prim.value list)
              * (Prim.var * Prim.value list)
             )
  | GBind of ((int * (Prim.var * Prim.value) list ) list * g)
  | GLoop   of (Prim.var * Prim.var list * (Prim.var * (Prim.var list * g)) list * g * g)
  | GLambda of (                           (Prim.var * (Prim.var list * g)) list     * g)

  (*
  | GAppCont (v, vs, k)
  | GAppBind (v, vs, (x, g))
  | GCont (k, vs)
  | GCond (v, (k1, vs1), (k2, vs2))
  | GBind (bs, g)
  | GLoop (v, vs, ls, g1, g2)
  | GLambda (ls, g)
   *)

module MP = CPS.Prop (* CPS Term Properties*)
module GP = struct (* CPS_gvn Term Properties*)

  let head (l, _     ) = l
  let args (_, (a, _)) = a
  let body (_, (_, m)) = m
  let heads = List.map head
  let argss = List.map args
  let bodys = List.map body

  let map_argss f ls = List.map (fun (l, (a, g)) -> (l, (f a, g  ))) ls
  let map_bodys f ls = List.map (fun (l, (a, g)) -> (l, (a,   f g))) ls

  let rec subterms t =
    let lambdas ls = List.flatten (List.map subterms (bodys ls)) in
    match t with
    | GAppCont _ | GCont _ | GCond _ -> [t]
    | GAppBind (_, _, (_, g)) | GBind (_, g) -> t :: subterms g
    | GLoop (_, _, ls, g1, g2) -> t :: subterms g1 @ subterms g2 @ lambdas ls
    | GLambda (ls, g) -> t :: subterms g @ lambdas ls

  let rec calls = function
    | GAppBind _ | GBind _ | GLoop _ | GLambda _ -> []
    | GAppCont (_, _, k) | GCont (k, _)          -> [k]
    | GCond (_, (k1, _), (k2, _))                -> [k1; k2]

  let deep_calls m = List.flatten (List.map calls (subterms m))

  let is_cond = function
    | GCond _ -> true
    | GAppCont _ | GAppBind _ | GCont _ | GBind _ | GLoop _ | GLambda _ -> false

  let is_terminator = function
    | GAppCont _ | GCont _ | GCond _             -> true
    | GAppBind _ | GBind _ | GLoop _ | GLambda _ -> false

  let rec terminator = function
    | GAppCont _ | GCont _ | GCond _ as g -> g
    | GAppBind (_, _, (_, g))
    | GBind (_, g)
    | GLoop (_, _, _, _, g) (*this is indeed the g we want to recusrse into*)
    | GLambda (_, g) -> terminator g

  let is_deep_cond m = is_cond (terminator m)

  let lambdas = function
    | GAppCont _ | GAppBind _ | GCont _ | GCond _ | GBind _ -> []
    | GLoop (_, _, ls, _, _) | GLambda (ls, _)              -> ls
           (* is ^this^ correct? *)

end

let assert_value env v = assert (Prim.closed env v)

let assert_values env vs = List.iter (assert_value env) vs

let nits xs = List.map (fun x -> (x, ())) xs

let assert_g env g =
  let rec aux env g =
    match g with
    | GAppCont (v, vs, k) ->
      assert (not (k = CPS.var_return));
      assert (Env.has env v);
      assert_values env vs;
      assert (Env.has ~env k)
    | GAppBind (v, vs, (x, g)) ->
      assert (Env.has env v);
      assert_values env vs;
      aux (Env.add1 ~env x ()) g
    | GCont (k, vs) ->
      assert (Env.has env k);
      assert_values env vs
    | GCond (v, (k1, vs1), (k2, vs2)) ->
      assert_value env v;
      assert (Env.has env k1); assert_values env vs1;
      assert (Env.has env k2); assert_values env vs2
    | GBind (bs, g) ->
      let (env, _) =
        List.fold_left
          (fun (env, r) (rank, bs) ->
            assert (r < rank);
            let (vars, values) = List.split bs in
            List.iter (assert_value env) values;
            (Env.add ~env (nits vars), rank)
          )
          (env, -1)
          bs
      in
      aux env g
    | GLoop (v, vs, ls, g1, g2) ->
      (*TODO: check ls's call graph*)
      let (names, lambdas) = List.split ls in
      (*DO NOT: add ls to g2's environment (calls should go through v) *)
      aux (Env.add1 ~env v ()) g2;
      (*DO NOT: add v to g1's environment (g1 should dispatch to ls) *)
      let env = Env.add ~env (nits names) in
      aux (Env.add env (nits vs)) g1;
      List.iter (fun (vs, g) -> aux (Env.add ~env (nits vs)) g) lambdas
    | GLambda (ls, g) ->
      let env =
        List.fold_left
          (fun env (v, (vs, g)) ->
            assert (Env.hasnt ~env v);
            (*DONT add v to g's env, (it's not under a GLoop!)*)
            aux (Env.add ~env (nits vs)) g;
            Env.add1 ~env v ()
          )
          env
          ls
      in
      aux env g
  in
  aux env g

let rec m_of_g g =
  match g with
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
    CPS.MRec ([v, (vs, CPS.MRec (GP.map_bodys m_of_g ls, m_of_g g1))],
              m_of_g g2)
  | GLambda (ls, g) ->
    CPS.MRec (GP.map_bodys m_of_g ls, m_of_g g)


(* Landing Lambdas *)
(* Problem: the complexity for finding cliques is too important *)


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

let rec unranked_g_of_m m =
  let rec aux m =
    match m with
    | CPS.MApp  (v, vs, CPS.C (x, m)) -> GAppBind (v, vs, (x, aux m))
    | CPS.MApp  (v, vs, CPS.CVar k  ) -> GAppCont (v, vs, k         )
    | CPS.MCont (v, vs) -> GCont (v, vs)
    | CPS.MCond (v, (k1, vs1), (k2, vs2)) -> GCond (v, (k1, vs1), (k2, vs2))
    | CPS.MLet  (x, v, m) -> strand [x,v] m
    | CPS.MRec  (ls, m) ->
      let ls = GP.map_bodys aux ls in
      let cg = call_graph ls in
      let cliqs = get_trans_cliques cg in
      List.fold_right
        (fun c g -> match c with
          | Clique c -> gloop (lambdas_of_names ls c) g
          | NotClique nc -> GLambda (lambdas_of_names ls nc, g)
        )
        cliqs
        (aux m)
    | CPS.MSel  _ -> failwith "Unsupported MSel constructor"
    | CPS.MSeq  _ -> failwith "Unsupported MSeq constructor"
  in
  aux m

and strand bs m = match m with
  | CPS.MLet (x, v, m) -> strand ((x,v)::bs) m
  | m -> GBind ([-1, bs], unranked_g_of_m m) (* needs ranking *)

and gloop loop_lambdas gterm =
  (*TODO: deforest *)
  (*TODO: special case when there is only one lambda (no dispatch var)*)

  (*loop_lambdas is the list of the mutually recusrive lambdas*)
  (*gterm is the term under the scope of the loop_lambdas*)

  let landing_lambda = Prim.fresh_var () in
  let number_of_args =
    List.fold_left max 0 (List.map List.length (GP.argss loop_lambdas))
  in
  let args_ = L.n (fun _ -> Prim.fresh_var ()) number_of_args in
  let dispatch_var = Prim.fresh_var () in
  let full_args = dispatch_var :: args_ in
  (*landing_lambda: the continuation variable for the landing lambda*)
  (*number_of_args: the maximum number of arguments for loop_lambdas*)
  (*args_: the variables used as arguments for the landing lambda*)
  (*dispatch_var: the variable used to dispatch over the loop_lambdas*)
  (*full_args: the dispatch variable and the other variables*)

  let dispatch =
    let branch_args lambda =
      List.map
        (fun v -> Prim.VVar v)
        (List.tl (L.take args_ (List.length (GP.args lambda) + 1)))
    in
    let rec aux i = function
      | [] -> assert false
      | [l] -> GCont (GP.head l, branch_args l)
      | [l1;l2] ->
        GCond (Prim.(VEq (VVar dispatch_var, VConst i)),
               (GP.head l1, branch_args l1),
               (GP.head l2, branch_args l2)
              )
      | l :: ls ->
        let more = Prim.fresh_var () in
        GLambda (
          [more, ([], aux (succ i) ls)],
          GCond (
            Prim.(VEq (VVar dispatch_var, VConst i)),
            (GP.head l, branch_args l),
            (more, [])
          )
        )
    in
    aux 0 loop_lambdas
  in
  (*dispatch: the dispatch term. It forwards calls to the correct loop entry*)

  let newgterm =
    (* find the index of a lambda by name (or return None) *)
    let index_or_none x ys =
      let rec aux i = function
        | [] -> None
        | y::ys -> if x = GP.head y then Some i else aux (succ i) ys
      in
      aux 0 ys
    in
    (*add necessary (null) arguments for padding *)
    let pad n xs =
      (*FIXME? there is probably an off-by-one-bug (oh BOB!) *)
      xs @ (L.nconst Prim.VNull (number_of_args - List.length xs))
    in
    (* patches an application *)
    let app v vs = match index_or_none v loop_lambdas with
      | None -> (v, vs)
      | Some i -> (landing_lambda, Prim.VConst i :: pad number_of_args vs)
    in
    let rec aux g = match g with
      | GAppCont (v, vs, k) ->
        (* k points to a lambda_p (hence it can*not* be in loop_lambdas) *)
        let (v, vs) = app v vs in GAppCont (v, vs, k)
      | GAppBind (v, vs, (x, g)) ->
        let (v, vs) = app v vs in GAppBind (v, vs, (x, aux g))
      | GCont (k, vs) -> GCont (app k vs)
      | GCond (v, (k1, vs1), (k2, vs2)) ->
        GCond (v, (app k1  vs1), (app k2  vs2))
      | GBind (bs, g) -> GBind (bs, aux g)
      | GLoop (v, vs, ls, g1, g2) ->
        GLoop (v, vs, GP.map_bodys aux ls, aux g1, aux g2)
      | GLambda (ls, g) ->
        GLambda (GP.map_bodys aux ls, aux g)
    in
    aux gterm
  in
  (*newgterm: gterm with calls to loop entries have been substituted for calls
              to the landing lambda*)

  GLoop (landing_lambda, full_args, loop_lambdas, dispatch, newgterm)

let rec vars_of_value v =
  let open Prim in
  let rec aux acc = function
    | VVar v -> v :: acc
    | VConst _ | VNull | VUndef | VDummy _ | VZero -> acc
    | VStruct vs -> List.fold_left aux acc vs
    | VRead v | VCast v -> failwith "Unsupported memops"
    | VPlus (v1, v2) | VMinus (v1, v2)
    | VMult (v1, v2) | VDiv (v1, v2) | VRem (v1, v2)
    | VGt (v1, v2) | VGe (v1, v2)
    | VLt (v1, v2) | VLe (v1, v2)
    | VEq (v1, v2) | VNe (v1, v2)
    | VAnd (v1, v2) | VOr (v1, v2) | VXor (v1, v2)
    | VShl (v1, v2) | VLShr (v1, v2) | VAShr (v1, v2)
    -> aux (aux acc v1) v2
  in
  aux [] v

let rank g =

  let rank_value env v =
    succ (List.fold_left max 0 (List.map (Env.get ~env) (vars_of_value v)))
  in

  let rec rank_g env = function
    | GAppCont _ | GCont _ | GCond _ as g -> g
    | GAppBind (v, vs, (x, g)) ->
      let rk = List.fold_left max 0 (List.map (rank_value env) vs) in
      GAppBind (v, vs, (x, rank_g (Env.add1 ~env x rk) g))
    | GBind ([-1, bs], g) -> rank_gbind env bs g
    | GBind _ -> assert false
    | GLoop (v, vs, ls, g1, g2) -> failwith "TODO"
    | GLambda (ls, g) ->
      (*all the calls the lambdas of ls are in g*)
      failwith "TODO"

  and rank_gbind env bs g =
    let rec aux env ranked nonranked = match nonranked with
      | [] -> (env, ranked)
      | _::_ ->
        let (rankable, nonrankable) =
          List.partition
            (fun (x, v) -> List.for_all (Env.has ~env) (vars_of_value v))
            nonranked
        in
        assert (not (rankable = [])); (*replaces stack-overflow*)
        let (env, ranked) =
          List.fold_left
            (fun (env, r) (x, v) ->
              let rk = rank_value env v in
              (Env.add1 ~env x rk, (rk, (x, v)) :: ranked)
            )
            (env, ranked)
            rankable
        in
        aux env ranked nonrankable
    in
    let (env, ranked) = aux env [] bs in
    GBind (L.classes ranked, rank_g env g)
  in

  rank_g Env.empty g (*TODO: allow the passing of the "proc param" in the env*)

let g_of_m m = rank (unranked_g_of_m m)

