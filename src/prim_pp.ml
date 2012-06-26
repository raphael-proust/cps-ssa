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

let rec pp_op ~paren op vs =
  let f op (v1, v2) = PP.op (pp_value ~paren) v1 op v2 in
  if paren then
    PP.with_paren (f op vs)
  else
    f op vs

and pp_value ?(paren = false) = function
  | Prim.VVar v     -> pp_var v
  | Prim.VConst c   -> !^ (string_of_int c)
  | Prim.VNull      -> !^ "null"
  | Prim.VUndef     -> !^ "undef"
  | Prim.VStruct vs -> PP.with_paren (PP.list ~sep:PP.comma pp_value vs)
  | Prim.VZero      -> !^ "(0..0)"
  | Prim.VDummy s   -> !^ ("dummy:" ^ s)
  (* Arithmetic ops *)
  | Prim.VPlus  vt -> pp_op ~paren Pprint.plus    vt
  | Prim.VMult  vt -> pp_op ~paren Pprint.star    vt
  | Prim.VMinus vt -> pp_op ~paren Pprint.minus   vt
  | Prim.VDiv   vt -> pp_op ~paren Pprint.bar     vt
  | Prim.VRem   vt -> pp_op ~paren Pprint.percent vt
  (* Comparisons *)
  | Prim.VGt vt -> pp_op ~paren (!^ ">" ) vt
  | Prim.VGe vt -> pp_op ~paren (!^ ">=") vt
  | Prim.VLt vt -> pp_op ~paren (!^ "<" ) vt
  | Prim.VLe vt -> pp_op ~paren (!^ "=<") vt
  | Prim.VEq vt -> pp_op ~paren (!^ "==") vt
  | Prim.VNe vt -> pp_op ~paren (!^ "<>") vt
  (* Boolean ops *)
  | Prim.VAnd vt -> pp_op ~paren (!^ "&&" ) vt
  | Prim.VOr  vt -> pp_op ~paren (!^ "||" ) vt
  | Prim.VXor vt -> pp_op ~paren (!^ "^^" ) vt
  (* IO *)
  | Prim.VRead v -> PP.fn pp_value (!^ "read") [v]
  (* Cast *)
  | Prim.VCast v -> PP.fn pp_value (!^ "cast") [v]
  (* Bitwise *)
  | Prim.VShl  vt -> pp_op ~paren (!^ "<<<") vt
  | Prim.VLShr vt -> pp_op ~paren (!^ ">>>") vt
  | Prim.VAShr vt -> pp_op ~paren (!^ ">->") vt

let pp_mem_w = function
  | Prim.MWrite v -> (!^ "<-") ^^ PP.space ^^ pp_value v
  | Prim.MAlloc -> !^ "<- alloc ()"

