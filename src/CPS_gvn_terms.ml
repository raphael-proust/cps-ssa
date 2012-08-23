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

(*We use a custom representation *)
type lambda = Prim.var * (Prim.var list * g)
and  g =
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

let head (l, _     ) = l
let args (_, (a, _)) = a
let body (_, (_, m)) = m
let heads = List.map head
let argss = List.map args
let bodys = List.map body

let map_argss f ls = List.map (fun (l, (a, g)) -> (l, (f a, g  ))) ls
let map_bodys f ls = List.map (fun (l, (a, g)) -> (l, (a,   f g))) ls

let with_body (l, (a, _)) g = (l, (a, g))

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

let rec apply_subs subs = function
  | GAppCont (v, vs, k) -> GAppCont (v, List.map (Prim.apply_subs subs) vs, k)
  | GAppBind (v, vs, (x, g)) ->
    GAppBind (v, List.map (Prim.apply_subs subs) vs, (x, apply_subs subs g))
  | GCont (k, vs) -> GCont (k, List.map (Prim.apply_subs subs) vs)
  | GCond (v, (k1, vs1), (k2, vs2)) ->
    GCond (Prim.apply_subs subs v,
      (k1, List.map (Prim.apply_subs subs) vs1),
      (k2, List.map (Prim.apply_subs subs) vs2)
    )
  | GBind (bs, g) ->
    GBind
      (List.map
        (fun (r, bs) ->
          (r,
           List.map (fun (x, v) -> (x, Prim.apply_subs subs v)) bs
          )
        )
        bs,
       apply_subs subs g
      )
  | GLoop (v, vs, ls, g1, g2) ->
    GLoop (v, vs, map_bodys (apply_subs subs) ls,
           apply_subs subs g1,
           apply_subs subs g2
          )
  | GLambda (ls, g) ->
    GLambda (map_bodys (apply_subs subs) ls, apply_subs subs g)


let assert_value env v = assert (Prim.closed env v)

let assert_values env vs = List.iter (assert_value env) vs

let nits xs = List.map (fun x -> (x, ())) xs

let rec assert_dispatch = function
  | GAppCont _ | GAppBind _ | GBind _ | GLoop _ -> assert false
  | GCond _ | GCont _ -> ()
  | GLambda ([l], GCond _) -> assert_dispatch (body l)
  | GLambda _ -> assert false

let assert_g g =
  let rec aux env lenv g =
    match g with
    | GAppCont (v, vs, k) ->
      assert (Env.has ~env v);
      assert_values env vs;
      assert (Env.has ~env:lenv k)
    | GAppBind (v, vs, (x, g)) ->
      assert (Env.hasnt ~env:env x);
      assert (Env.has ~env:lenv v);
      assert_values env vs;
      aux (Env.add1 ~env x ()) lenv g
    | GCont (k, vs) ->
      assert (Env.has ~env:lenv k);
      assert_values env vs
    | GCond (v, (k1, vs1), (k2, vs2)) ->
      assert_value env v;
      assert (Env.has ~env:lenv k1); assert_values env vs1;
      assert (Env.has ~env:lenv k2); assert_values env vs2
    | GBind (_, GBind _) -> assert false
    | GBind (bs, g) ->
      let (env, _) =
        List.fold_left
          (fun (env, r) (rank, bs) ->
            assert (r < rank);
            let (vars, values) = List.split bs in
            List.iter (fun v -> assert (Env.hasnt ~env v)) vars;
            List.iter (assert_value env) values;
            (Env.add ~env (nits vars), rank)
          )
          (env, -1)
          bs
      in
      aux env lenv g
    | GLoop (v, vs, ls, g1, g2) ->
      assert_dispatch g1;
      (*TODO: check ls's call graph*)
      assert (Env.hasnt ~env:lenv v);
      List.iter (fun v -> assert (Env.hasnt ~env v)) vs;
      (*DO NOT: add ls to g2's environment (calls should go through v) *)
      aux env (Env.add1 ~env:lenv v ()) g2;
      (*TODO: deforest these iter*)
      List.iter
        (fun (v, (vs, g)) ->
          assert (Env.hasnt ~env:lenv v);
          List.iter (fun v -> assert (Env.hasnt ~env v)) vs;
        )
        ls;
      let (names, lambdas) = List.split ls in
      (*DO NOT: add v to g1's environment (g1 should dispatch to ls) *)
      aux (Env.t_of_list (nits vs)) (Env.t_of_list (nits names)) g1;
      let lenv = Env.add ~env (nits names) in
      List.iter (fun (vs, g) -> aux (Env.add ~env (nits vs)) lenv g) lambdas
    | GLambda (ls, g) ->
      List.iter
        (fun (v, (vs, g)) ->
          assert (Env.hasnt ~env:lenv v);
          List.iter (fun v -> assert (Env.hasnt ~env v)) vs;
          (*DONT add v to g's env, (it's not under a GLoop!)*)
          (*DONT add v to the environment to force splitting of lambdas*)
          aux (Env.add ~env (nits vs)) lenv g
        )
        ls ;
      aux env (Env.add ~env (nits (heads ls))) g
  in
  aux Env.empty (Env.one CPS.var_return ()) g

let max_rank g =
  let rec aux rk = function
    | GAppCont _ -> rk
    | GAppBind (_, _, (_, g)) -> aux rk g
    | GCont _ -> rk
    | GCond _ -> rk
    | GBind (bs, g) -> aux (List.fold_left max 0 (List.map fst bs)) g
    | GLoop (v, vs, ls, g1, g2) ->
      aux (aux (List.fold_left (fun rk l -> aux rk (body l)) rk ls) g1) g2
    | GLambda (ls, g) ->
      aux (List.fold_left (fun rk l -> aux rk (body l)) rk ls) g
  in
  aux 0 g

