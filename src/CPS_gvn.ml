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
module GP = CPS_gvn_terms

(*NOTICE: this is a prototype, it is known *not* to be:
  - feature complete
  - bug free
*)


let trivial_bind_removal g =
  let rec aux = function
    | GP.GAppCont _
    | GP.GCont _
    | GP.GCond _
    as g -> g
    | GP.GAppBind (v, vs, (x, g)) -> GP.GAppBind (v, vs, (x, aux g))
    | GP.GLoop (v, vs, ls, g1, g2) -> GP.GLoop (v, vs, ls, aux g1, aux g2)
    | GP.GLambda (ls, g) -> GP.GLambda (ls, aux g)
    | GP.GBind (bs, g) ->
      (*FIXME: sometimes require re-ranking*)
      let (subs, _, revbs) =
        List.fold_left (* this fold is over the (rank, bindings) list *)
          (fun (subs, env, bsacc) (r, bs) ->
            let (subs, env, bs) =
              List.fold_left (* this fold is over the bindings *)
                (fun (subs, env, bs) (x,v) ->
                  let open Prim in
                  match v with
                  | VVar _ -> ((x, v) :: subs, env, bs)
                  | VRead _ -> (subs, env, (x,v) :: bs)
                  | VConst _
                  | VNull | VUndef | VDummy _ | VZero
                  | VStruct _
                  | VPlus _ | VMult _ | VMinus _ | VDiv _ | VRem _
                  | VGt _ | VGe _ | VLt _ | VLe _ | VEq _ | VNe _
                  | VAnd _ | VOr _ | VXor _
                  | VCast _
                  | VShl _ | VLShr _ | VAShr _ ->
                    try
                      let y = Env.teg ~env v in
                      ((x, VVar y) :: subs, env, bs)
                      (* because we substitute directly here, there is no need
                       * for fixpointing this function *)
                    with
                      | Not_found ->
                        (subs, Env.add1 ~env x v, (x,v) :: bs)
                )
                (subs, env, [])
                bs
            in
            (subs, env, (r, bs) :: bsacc)
          )
          ([], Env.empty, [])
          bs
      in
      GP.GBind (List.rev revbs, aux (GP.apply_subs subs g))
  in
  aux g

let merge_binds bs bbs =
  let rec aux acc l1 l2 = match (l1,l2) with
    | ([],l2) -> List.rev_append acc l2
    | (l1,[]) -> List.rev_append acc l1
    | (((r1, bs1) as rbs1) :: t1, ((r2,bs2) as rbs2) :: t2) ->
      if r1 < r2 then
        aux (rbs1 :: acc) t1 l2
      else if r2 < r1 then
        aux (rbs2 :: acc) l1 t2
      else
        aux ((r1, bs1 @ bs2) :: acc) t1 t2
  in
  aux [] bs bbs

let movable rk binds =
  try
    let (_, bs, revbinds) =
      List.fold_left (*TODO: optimisation*)
        (fun (env, bs, binds) ((rrk, bbs) as rbs) ->
          if rk < rrk then begin (*before rank*)
            assert (bs = []);
            (Env.add ~env bbs, bs, rbs :: binds)
          end else if rk = rrk then begin (*on the rank*)
            assert (bs = []);
            let (movables, nonmovables) =
              List.partition
                (fun (_, v) ->
                  List.for_all (Env.hasnt ~env) (Prim.vars_of_value v)
                )
                bbs
            in
            let binds =
              if nonmovables = [] then
                binds
              else
                (rrk, nonmovables) :: binds
            in
            (Env.empty, movables, binds)
          end else begin (*after the rank*)
            assert (env = Env.empty);
            (env, bs, rbs :: binds)
          end
        )
        (Env.empty, [], binds)
        binds
    in
    ([rk, bs], List.rev revbinds)
  with
  | Not_found -> ([], binds)

let stop vs bs =
  (* stops bindings from bs that depends on variables of vs to bubble up *)
  (* we don't need to fix-point that because only equal-rank var are imported *)
  ((*the whole of bs is suppose to be from one rank only!*)
    match bs with
    | _::[] -> ()
    | [] | _::_::_ -> assert false
  );
  T2.map1
    (List.filter (fun (_, l) -> not (l = [])))
    (List.split (
      List.map
        (fun (r, bs) ->
          let (bs, sbs) =
            List.partition
              (fun (_, v) -> L.disjoint (Prim.vars_of_value v) vs)
              bs
          in
          ((r, bs),(r,sbs))
        )
        bs
    ))

let move rk marks g =
  (* do we need to use the 'marks' argument? Scope might give us enough
   * guarantees to just bubble them up in the return value.. *)
  let put_marks marks g = match (marks, g) with
    | ([], g) -> g
    | (marks, GP.GBind (bs, g)) -> GP.GBind (merge_binds marks bs, g)
    | bsg -> GP.GBind bsg
  in

  (* we do not implement the original algorithm. What we do is simpler in that:
    * it does not need to temper with function parameters
    * it is easy to check that scope is respected
    * binding moves are across the dominator tree instead of the cfg
   *
   * Note however that it is equivalent for simple patterns. It differs for
   * join points.
   *)
  let rec up = function

    (*terminators*)
    | GP.GAppCont _ | GP.GCont _ | GP.GCond _ as g -> ([], g)
    | GP.GAppBind (v, vs, (x, g)) ->
      let (marks, g) = up g in
      let (marks, stops) = stop [x] marks in
      (marks, GP.GAppBind (v, vs, (x, put_marks stops g)))

    (* lambdas: traverse *)
    | GP.GLoop (v, vs, ls, dispatch, g) ->
      (*TODO: loop header and landing pad code*)
      GP.assert_dispatch dispatch; (*need be rm-ed for landing pad opt *)
      let (marks, g) = up g in
      (marks, GP.GLoop (v, vs, ls, dispatch, g))
    | GP.GLambda (ls, g) ->
      let (lsmarks, ls) =
        List.fold_left
          (fun (lsmarks, ls) l ->
            let (marks, g) = up (GP.body l) in
            let (marks, stops) = stop (GP.args l) marks in
            (merge_binds marks lsmarks,
             (GP.with_body l (put_marks stops g) :: ls)
            )
          )
          ([], [])
          ls
      in
      let (marks, g) = up g in
      (merge_binds marks lsmarks, GP.GLambda (ls, g))

    (*bind: serious business*)
    | GP.GBind (_, GP.GBind _) -> assert false
    | GP.GBind (bs, g) ->
      let (marks, g) = up g in
      let (marks, bs) = movable rk (merge_binds bs marks) in (* This is it! *)
      (marks, GP.GBind (bs, g))

  in

  let (marks, g) = up g in
  put_marks marks g


let drive g =
  I.fold_inc
    (fun g rk -> trivial_bind_removal (move rk [] g))
    g
    (GP.max_rank g)



