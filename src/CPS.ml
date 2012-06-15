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
  | Mapp  of (Prim.var * Prim.expr list * cont)
  | Mcont of (Prim.var * Prim.expr list)
  | Mcond of ( Prim.expr
             * (Prim.var * Prim.expr list)
             * (Prim.var * Prim.expr list)
             )
  | Mlet  of (Prim.var * Prim.expr * m)
  | Mrec  of ((Prim.var * lambda) list * m)
  | Mseq  of (Prim.var * Prim.mem_w * m)

and cont =
  | Cvar of Prim.var
  | C    of Prim.var * m

and lambda =
  | Lproc of (Prim.var list * Prim.var * m)
  | Ljump of (Prim.var list * m)

(* This is for monad entry application. *)
let var_run = Prim.var "run"

let var_unit = Prim.var "()"
