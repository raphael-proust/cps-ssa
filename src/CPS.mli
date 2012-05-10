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


type m =
  (** Application. The [var]iable can only be a procedure/function call. An
      extra continuation argument is needed.
    *)
  | Mapp  of (Prim.var * Prim.expr list * cont)
  (** Continuation call. The [var]iable must be a continuation. *)
  (*TODO? use GADT to enforce the [var]iable to be a continuation? *)
  | Mcont of (Prim.var * Prim.expr list)
  (** Conditional Branching. Both branches must use continuation [var]iables. *)
  | Mcond of ( Prim.expr
             * (Prim.var * Prim.expr list)
             * (Prim.var * Prim.expr list))
  (** Let-binding. Note that it only bounds [expressions]. *)
  | Mlet  of (Prim.var * Prim.expr * m)
  (** Recursive let binding. It binds lambdas. *)
  | Mrec  of ((Prim.var * lambda) list * m)

and cont =
    (** Continuation Variable. *)
  | Cvar of Prim.var
    (** Explicit Continuation. *)
  | C    of Prim.var * m

and lambda =
  (** Procedures. These are for source program functions/procedures translation.
      The last [var]iable is for continuation passing.
    *)
  | Lproc of (Prim.var list * Prim.var * m)
  (** Intra-procedure Jumps. E.g. for join points. *)
  | Ljump of (Prim.var list * m)

val var_run: Prim.var
