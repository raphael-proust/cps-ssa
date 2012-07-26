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

type m =
  | MApp  of (Prim.var * Prim.value list * cont)
  | MCont of (Prim.var * Prim.value list)
  | MCond of (  Prim.value
              * (Prim.var * Prim.value list)
              * (Prim.var * Prim.value list)
             )
  | MLet  of (Prim.var * Prim.value * m)
  | MSel  of (Prim.var * Prim.value * Prim.value * Prim.value * m)
  | MRec  of (named_lambda list * m)
  | MSeq  of (Prim.var * Prim.mem_w * m)

(*
  | MApp  (v, vs, cont)
  | MCont (v, vs)
  | MCond (v, (k1, vs1), (k2, vs2))
  | MLet  (x, v, m)
  | MSel  (x, v, v1, v2, m)
  | MRec  (ls, m)
  | MSeq  (v, w, m)
*)

and cont =
  | CVar of Prim.var
  | C    of Prim.var * m

and proc = Prim.var list * Prim.var * m

and prog = (Prim.var * proc) list * m

and lambda = Prim.var list * m

and named_lambda = Prim.var * lambda


let rec subterms t = match t with
  | MApp  (_, _, cont) -> t :: subterms_cont cont
  | MCont (_, _) -> [t]
  | MCond (_, _, _) -> [t]
  | MLet  (_, _, m) -> t :: subterms m
  | MSel  (_, _, _, _, m) -> t :: subterms m
  | MSeq  (_, _, m) -> t :: subterms m
  | MRec  (ls, m) ->
    t :: subterms m @ List.flatten (List.map (fun (_,(_,m)) -> subterms m) ls)

and subterms_cont = function
  | CVar _ -> []
  | C    (_, m) -> subterms m

let m_map ?(var= fun x -> x) ?(value= fun v -> v) ?(mem_w= fun w -> w) m =
  let values vs = List.map value vs in
  let vars vs = List.map var vs in
  let rec aux = function
    | MApp  (v, vs, c) -> MApp (var v, values vs, aux_cont c)
    | MCont (v, vs) -> MCont (var v, values vs)
    | MCond (v, (k1, vs1), (k2, vs2)) ->
      MCond (value v, (var k1, values vs1), (var k2, values vs2))
    | MLet  (x, v, m) -> MLet (var x, value v, aux m)
    | MSel  (x, v, v1, v2, m) ->
      MSel (var x, value v, value v1, value v2, aux m)
    | MRec  (ls, m) ->
      MRec (List.map (fun (f, (vs, m)) -> (var f, (vars vs, aux m))) ls, aux m)
    | MSeq  (v, w, m) -> MSeq (var v, mem_w w, aux m)
  and aux_cont = function
    | CVar v -> CVar (var v)
    | C (x, m) -> C (var x, aux m)
  in
  aux m

(* This is for monad entry application. *)
let var_run = Prim.var "run"

(* This is the default return continuation *)
let var_return = Prim.var "return"

(* This is for sequences of side-effects. *)
let var_unit = Prim.var "()"
