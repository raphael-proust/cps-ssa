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

 (** This module does CPS term pretty printing. It is intended to be used as a
     preparation for plain-text diff(1). Thus it introduces lots of linebreaks.
   *)

 (** [pp_α x] pretty-prints [x] into a [Pprint.document] (where [x] has type
     [CPS.α]). See [CPS] module for details on different [α]. See
     [Pprint.RENDERER]s for [document] printing.
   *)

(** pretty-prints CPS terms. *)
val pp_m     : CPS.m -> Pprint.document

(** pretty-prints CPS continuations. *)
val pp_cont  : CPS.cont -> Pprint.document

(** pretty-prints CPS lambdas (either merge points or source functions). *)
val pp_proc  : CPS.proc -> Pprint.document

(** pretty-prints a collection of CPS lambdas. *)
val pp_module: (Prim.var * CPS.proc) list -> Pprint.document
