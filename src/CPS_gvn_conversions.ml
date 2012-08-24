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

(*TODO: optimisations*)
open Util
module GP = CPS_gvn_terms

let rec m_of_g g =
  match g with
  | GP.GAppCont (v, vs, k) -> CPS.MApp (v, vs, CPS.CVar k)
  | GP.GAppBind (v, vs, (x, g)) -> CPS.MApp (v, vs, CPS.C (x, m_of_g g))
  | GP.GCont (k, vs) -> CPS.MCont (k, vs)
  | GP.GCond (v, (k1, vs1), (k2, vs2)) -> CPS.MCond (v, (k1, vs1), (k2, vs2))
  | GP.GBind (bs, g) ->
    List.fold_right
      (fun (_, bs) term ->
        List.fold_left (fun term (x, v) -> CPS.MLet (x, v, term)) term bs
      )
      bs
      (m_of_g g)
  | GP.GLoop (v, vs, ls, g1, g2) ->
    CPS.MRec ([v, (vs, CPS.MRec (GP.map_bodys m_of_g ls, m_of_g g1))],
              m_of_g g2)
  | GP.GLambda (ls, g) ->
    CPS.MRec (GP.map_bodys m_of_g ls, m_of_g g)


(* Landing Lambdas *)


let call_graph (ls : (Prim.var * (Prim.var list * GP.g)) list)
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
  (*TODO: use blobs of independant NotCliques *)

let lambda_of_name ls n = List.find (fun l -> n = GP.head l) ls
let lambdas_of_names ls ns = List.map (lambda_of_name ls) ns

let rec unranked_g_of_m m =
  let rec aux m =
    match m with
    | CPS.MApp  (v, vs, CPS.C (x, m)) -> GP.GAppBind (v, vs, (x, aux m))
    | CPS.MApp  (v, vs, CPS.CVar k  ) -> GP.GAppCont (v, vs, k         )
    | CPS.MCont (v, vs) -> GP.GCont (v, vs)
    | CPS.MCond (v, (k1, vs1), (k2, vs2)) -> GP.GCond (v, (k1, vs1), (k2, vs2))
    | CPS.MLet  (x, v, m) -> strand [x,v] m
    | CPS.MRec  (ls, m) ->
      let ls = GP.map_bodys aux ls in
      let cg = call_graph ls in
      let cliqs = get_trans_cliques cg in
      List.fold_right
        (fun c g -> match c with
          | Clique c -> gloop (lambdas_of_names ls c) g
          | NotClique nc -> GP.GLambda (lambdas_of_names ls nc, g)
        )
        cliqs
        (aux m)
    | CPS.MSel  _ -> failwith "Unsupported MSel constructor"
    | CPS.MSeq  _ -> failwith "Unsupported MSeq constructor"
  in
  aux m

and strand bs m = match m with
  | CPS.MLet (x, v, m) -> strand ((x,v)::bs) m
  | m -> GP.GBind ([-1, bs], unranked_g_of_m m) (* needs ranking *)

and gloop loop_lambdas gterm =
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
      | [l] -> GP.GCont (GP.head l, branch_args l)
      | [l1;l2] ->
        GP.GCond (Prim.(VEq (VVar dispatch_var, VConst i)),
               (GP.head l1, branch_args l1),
               (GP.head l2, branch_args l2)
              )
      | l :: ls ->
        let more = Prim.fresh_var () in
        GP.GLambda (
          [more, ([], aux (succ i) ls)],
          GP.GCond (
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
      xs @ (L.nconst Prim.VNull (number_of_args - List.length xs))
    in
    (* patches an application *)
    let app v vs = match index_or_none v loop_lambdas with
      | None -> (v, vs)
      | Some i -> (landing_lambda, Prim.VConst i :: pad number_of_args vs)
    in
    let rec aux g = match g with
      | GP.GAppCont (v, vs, k) ->
        (* k points to a lambda_p (hence it can*not* be in loop_lambdas) *)
        let (v, vs) = app v vs in GP.GAppCont (v, vs, k)
      | GP.GAppBind (v, vs, (x, g)) ->
        let (v, vs) = app v vs in GP.GAppBind (v, vs, (x, aux g))
      | GP.GCont (k, vs) -> GP.GCont (app k vs)
      | GP.GCond (v, (k1, vs1), (k2, vs2)) ->
        GP.GCond (v, (app k1  vs1), (app k2  vs2))
      | GP.GBind (bs, g) -> GP.GBind (bs, aux g)
      | GP.GLoop (v, vs, ls, g1, g2) ->
        GP.GLoop (v, vs, GP.map_bodys aux ls, aux g1, aux g2)
      | GP.GLambda (ls, g) ->
        GP.GLambda (GP.map_bodys aux ls, aux g)
    in
    aux gterm
  in
  (*newgterm: gterm with calls to loop entries have been substituted for calls
              to the landing lambda*)

  GP.GLoop (landing_lambda, full_args, loop_lambdas, dispatch, newgterm)


let rank g =

  (*TODO: clean up environments (based on scope) to improve performance*)

  let rank_value e v =
    (* the succ of the maximum of the rank of all the variables used in v *)
    succ (List.fold_left max 0 (List.map (Env.get ~e) (Prim.vars_of_value v)))
  in
  let rank_values e vs = List.map (rank_value e) vs in

  let update_callenv e k rs =
    (* update or add the rank of the k's call-site arguments*)
    Env.add1 ~e k (
      if Env.has ~e k then
        List.map2 max rs (Env.get ~e k)
      else
        rs
    )
  in

  let rec rank_g e ce = function
    (* e: (variable, rank) environment
     * ce: (function, arguments' ranks) environment
     *)
    (* App: external call, nothing to do *)
    | GP.GAppCont _ as g -> (ce, g)

    (* Continuations: update ce for superterms (return a new ce) *)
    | GP.GCont (k, vs) as g ->
      (update_callenv ce k (rank_values e vs), g)
    | GP.GCond (v, (k1, vs1), (k2, vs2)) as g ->
      let ce = update_callenv ce k1 (rank_values e vs1) in
      let ce = update_callenv ce k2 (rank_values e vs2) in
      (ce, g)

    (* Binds: fix update e for subterms *)
    | GP.GAppBind (v, vs, (x, g)) ->
      let rk = List.fold_left max 0 (rank_values e vs) in
      let (ce, g) = rank_g (Env.add1 ~e x rk) ce g in
      (ce, GP.GAppBind (v, vs, (x, g)))
    | GP.GBind (_, GP.GBind _) -> assert false
    | GP.GBind ([-1, bs], g) -> rank_gbind e ce bs g
    | GP.GBind _ -> assert false

    (* Lambdas: fix subterm and then bodies, do not send e 'up', only ce *)
    | GP.GLoop (v, vs, ls, g1, g2) ->
      let (ce, g2) = rank_g e ce g2 in
      let (ce, g1) = rank_g e ce g1 in
      let (ce, ls) =
        List.fold_left
          (fun (ce, ls) (l, (vs, g)) ->
            let (ncenv, nl) =
              let e = Env.add ~e (List.combine vs (Env.get ~e:ce l)) in
              (*TODO: don't add ls's calls to ce*)
              let (ncenv, g) = rank_g e ce g in
              (ncenv, (l, (vs, g)))
            in
            (Env.merge ncenv ce, nl :: ls)
          )
          (ce, [])
          ls
      in
      (ce, GP.GLoop (v, vs, ls, g1, g2))
    | GP.GLambda (ls, g) ->
      (*all the calls to the lambdas of ls are in g*)
      (* we start by fixing ranks in g*)
      let (ce, g) = rank_g e ce g in
      (* we then go in each of the lambdas bodies *)
      let (ce, ls) =
        List.fold_left
          (fun (ce, ls) (l, (vs, g)) ->
            let (ncenv, nl) =
              let e = Env.add ~e (List.combine vs (Env.get ~e:ce l)) in
              let (ncenv, g) = rank_g e ce g in
              (ncenv, (l, (vs, g)))
            in
            (Env.merge ncenv ce, nl :: ls)
          )
          (ce, [])
          ls
      in
      (ce, GP.GLambda (ls, g))

  and rank_gbind e ce bs g =
    let rec aux e ranked nonranked = match nonranked with
      | [] -> (e, ranked)
      | _::_ ->
        let (rankable, nonrankable) =
          List.partition
            (fun (x, v) -> List.for_all (Env.has ~e) (Prim.vars_of_value v))
            nonranked
        in
        assert (not (rankable = [])); (*replaces stack-overflow*)
        let (e, ranked) =
          List.fold_left
            (fun (e, r) (x, v) ->
              let rk = rank_value e v in
              (Env.add1 ~e x rk, (rk, (x, v)) :: ranked)
            )
            (e, ranked)
            rankable
        in
        aux e ranked nonrankable
    in
    let (e, ranked) = aux e [] bs in
    let (ce, g) = rank_g e ce g in
    (ce, GP.GBind (L.classes ranked, g))
  in

  rank_g Env.empty Env.empty g

let g_of_m m = snd (rank (unranked_g_of_m m))
