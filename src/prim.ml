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

(* This module handle primitives: values that are used for several IR.
 * These parts need not translating. *)


(* IRs only uses integer constants. *)
type const = int

(* Variable type is abstracted in the interface file. *)
type var = string

(* creation (note the prefixing) and conversion. *)
external var : string -> var = "%identity"
external string_of_var : var -> string = "%identity"

(* whoohoo indexes! *)
let var_counter = ref (-1)

let fresh_var =
  fun () ->
    incr var_counter;
    "%v__" ^ string_of_int !var_counter

type value =
  | VVar   of var
  | VConst of const
  | VNull
  | VUndef
  | VDummy of string (* for things we won't translate *)
  | VZero
  | VStruct of value list
  | VPlus  of (value * value)
  | VMult  of (value * value)
  | VMinus of (value * value)
  | VDiv   of (value * value)
  | VRem   of (value * value)
  | VGt of (value * value)
  | VGe of (value * value)
  | VLt of (value * value)
  | VLe of (value * value)
  | VEq of (value * value)
  | VNe of (value * value)
  | VAnd of (value * value)
  | VOr  of (value * value)
  | VXor of (value * value)
  | VRead of value
  | VCast of value
  | VShl  of (value * value)
  | VLShr of (value * value)
  | VAShr of (value * value)

type mem_w =
  | MWrite of value
  | MAlloc

let rec closed env = function
  | VVar v -> Env.has ~env v
  | VConst _ | VNull | VUndef | VDummy _ | VZero -> true
  | VStruct vs -> List.for_all (closed env) vs
  | VRead v | VCast v -> closed env v
  | VPlus (v1, v2) | VMinus (v1, v2)
  | VMult (v1, v2) | VDiv (v1, v2) | VRem (v1, v2)
  | VGt (v1, v2) | VGe (v1, v2)
  | VLt (v1, v2) | VLe (v1, v2)
  | VEq (v1, v2) | VNe (v1, v2)
  | VAnd (v1, v2) | VOr (v1, v2) | VXor (v1, v2)
  | VShl (v1, v2) | VLShr (v1, v2) | VAShr (v1, v2)
  -> closed env v1 && closed env v2

let reset_idxs () =
  var_counter := (-1)
