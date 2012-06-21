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
let var s = ("v:" ^ s : var)
external string_of_var : var -> string = "%identity"

(* whoohoo indexes! *)
let var_counter = ref (-1)

let fresh_var =
  fun () ->
    incr var_counter;
    "v:_" ^ string_of_int !var_counter

(* labels are for jump. This is somehow specific to ssa and might get moved in
 * the corresponding module. Type is abstracted in the interface. *)
type label = string

let label s = ("l:" ^ s : label)
external string_of_label : label -> string = "%identity"

let label_counter = ref (-1)

let fresh_label =
  fun () ->
    incr label_counter;
    "l:_" ^ string_of_int !label_counter

(* Standard conversion for when one translates labels into closure variables. *)
let var_of_label l = var l
let label_of_var v = label v

type value =
  | Vvar   of var
  | Vconst of const
  | Vnull
  | Vundef
  | Vdummy of string (* for things we won't translate *)
  | Vzero
  | Vstruct of value list
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

type mem_w =
  | MWrite of value
  | MAlloc

let reset_idxs () =
  var_counter := (-1);
  label_counter := (-1)
