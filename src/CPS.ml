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


module Prop = struct

  let rec subterms t = match t with
    | MApp  (_, _, cont)    -> t :: subterms_cont cont
    | MCont (_, _)
    | MCond (_, _, _)       -> [t]
    | MLet  (_, _, m)
    | MSel  (_, _, _, _, m)
    | MSeq  (_, _, m)       -> t :: subterms m
    | MRec  (ls, m)         ->
      t :: subterms m @ List.flatten (List.map (fun (_,(_,m)) -> subterms m) ls)

  and subterms_cont = function
    | CVar _      -> []
    | C    (_, m) -> subterms m

  let rec calls = function
    | MApp  (_, _, C _) | MLet _ | MSel _ | MSeq _ | MRec _ -> []
    | MApp  (_, _, CVar v) | MCont (v, _)                   -> [v]
    | MCond (_, (v1, _), (v2, _))                           -> [v1; v2]

  let deep_calls m = List.flatten (List.map calls (subterms m))

  let is_cond = function
    | MCond _ -> true
    | MApp  _ | MCont _ | MLet  _ | MSel  _ | MSeq  _ | MRec  _ -> false

  let is_terminator = function
    | MApp  (_, _, CVar _) | MCont _ | MCond _ -> true
    | MApp  (_, _, C _) | MLet  _ | MSel  _ | MSeq  _ | MRec  _ -> false

  let rec terminator = function
    | MApp  (_, _, CVar _) | MCont _ | MCond _ as m -> m
    | MApp  (_, _, C (_, m))
    | MLet  (_, _, m)
    | MSel  (_, _, _, _, m)
    | MSeq  (_, _, m)
    | MRec  (_, m) -> terminator m

  let is_deep_cond m = is_cond (terminator m)

  let lambdas = function
    | MLet _ | MSel _ | MSeq _ | MApp  _ | MCont _ | MCond _ -> []
    | MRec (ls, _) -> ls

  let head (l, _     ) = l
  let args (_, (a, _)) = a
  let body (_, (_, m)) = m
  let heads = List.map head
  let argss = List.map args
  let bodys = List.map body

end

(* This is for monad entry application. *)
let var_run = Prim.var "run"

(* This is the default return continuation *)
let var_return = Prim.var "return"

(* This is for sequences of side-effects. *)
let var_unit = Prim.var "()"
