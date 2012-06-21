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

(* Pprint Operators and other facilities *)
module PP = struct
  include Pprint
  include Util.PP
end
open PP.Operators

let pp_var v = !^ (Prim.string_of_var v)

let pp_label l = !^ (Prim.string_of_label l)

let rec pp_value = function
  | Prim.Vvar v     -> pp_var v
  | Prim.Vconst c   -> !^ (string_of_int c)
  | Prim.Vnull      -> !^ "null"
  | Prim.Vundef     -> !^ "undef"
  | Prim.Vstruct vs -> PP.with_paren (PP.list ~sep:PP.comma pp_value vs)
  | Prim.Vzero      -> !^ "(0..0)"
  | Prim.Vdummy s   -> !^ ("dummy:" ^ s)
  (* Arithmetic ops *)
  | Prim.VPlus  (v1, v2) -> PP.op pp_value v1 Pprint.plus    v2
  | Prim.VMult  (v1, v2) -> PP.op pp_value v1 Pprint.star    v2
  | Prim.VMinus (v1, v2) -> PP.op pp_value v1 Pprint.minus   v2
  | Prim.VDiv   (v1, v2) -> PP.op pp_value v1 Pprint.bar     v2
  | Prim.VRem   (v1, v2) -> PP.op pp_value v1 Pprint.percent v2
  (* Comparisons *)
  | Prim.VGt (v1, v2) -> PP.op pp_value v1 (!^ ">" ) v2
  | Prim.VGe (v1, v2) -> PP.op pp_value v1 (!^ ">=") v2
  | Prim.VLt (v1, v2) -> PP.op pp_value v1 (!^ "<" ) v2
  | Prim.VLe (v1, v2) -> PP.op pp_value v1 (!^ "=<") v2
  | Prim.VEq (v1, v2) -> PP.op pp_value v1 (!^ "==") v2
  | Prim.VNe (v1, v2) -> PP.op pp_value v1 (!^ "<>") v2
  (* Boolean ops *)
  | Prim.VAnd (v1, v2) -> PP.op pp_value v1 (!^ "&&" ) v2
  | Prim.VOr  (v1, v2) -> PP.op pp_value v1 (!^ "||" ) v2
  | Prim.VXor (v1, v2) -> PP.op pp_value v1 (!^ "^^" ) v2
  (* IO *)
  | Prim.VRead v -> PP.fn pp_value (!^ "read") [v]

let pp_mem_w = function
  | Prim.MWrite v -> (!^ "<-") ^^ PP.space ^^ pp_value v
  | Prim.MAlloc -> !^ "<- alloc ()"

