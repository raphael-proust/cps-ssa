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

type g =
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

val assert_g: (Prim.var, unit) Env.t -> g -> unit

val m_of_g: g -> CPS.m
val g_of_m: CPS.m -> g
