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

 (** This module deals with CPS terms. Its content is inspired by "A
     Correspondence between Continuation Passing Style and Static Single
     Assignment Form" by Kesley.
   *)


(* [m] are CPS terms. *)
type m =
  (** Application. The [var]iable can only be a procedure/function call. An
      extra continuation argument is needed.
    *)
  | MApp  of (Prim.var * Prim.value list * cont)
  (** Continuation call. The [var]iable must be a continuation. *)
  | MCont of (Prim.var * Prim.value list)
  (** Conditional Branching. Both branches must use continuation [var]iables. *)
  | MCond of ( Prim.value
             * (Prim.var * Prim.value list)
             * (Prim.var * Prim.value list))
  (** Let-binding. It only bounds values. For lambdas, use [Mrec]. *)
  | MLet  of (Prim.var * Prim.value * m)
  (** Select binding. The [Msel (v, c, v1, v2, m)] is equivalent to an ML
      [let v = if c then v1 esle v2 in m]. We compile this way (and thus
      get even further from λ-calculus) because it is easier and it does not
      blur the effect of llvm optimisations. *)
  | MSel of (Prim.var * Prim.value * Prim.value * Prim.value * m)
  (** Recursive let-binding. It only binds lambdas. For values, use [Mlet]. *)
  | MRec  of ((Prim.var * lambda) list * m)
  (** Sequences are for memory side-effect operation only. *)
  | MSeq  of (Prim.var * Prim.mem_w * m)

(* [cont] are continuations. *)
and cont =
    (** Continuation Variable. For the "return" continuation only. *)
  | CVar of Prim.var
    (** Explicit Continuation. For binding the result of a computation. *)
  | C    of Prim.var * m

(* [lambda] are lambdas translated from either join point or procedures. *)
and lambda =
  (** Procedures. These are for source program functions/procedures translation.
      The last [var]iable is for the "return" continuation.
    *)
  | LProc of (Prim.var list * Prim.var * m)
  (** Intra-procedure Jumps. E.g. for join points. *)
  | LJump of (Prim.var list * m)

(** [var_run] is for the top return continuation. Fulfilling the same purpose
    as to [SSA.label_main] somehow. *)
val var_run: Prim.var

(** [var_unit] is for assignments of calls that do not return. *)
val var_unit: Prim.var
