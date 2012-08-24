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

let rec closed e = function
  | VVar v -> Env.has ~e v
  | VConst _ | VNull | VUndef | VDummy _ | VZero -> true
  | VStruct vs -> List.for_all (closed e) vs
  | VRead v | VCast v -> closed e v
  | VPlus (v1, v2) | VMinus (v1, v2)
  | VMult (v1, v2) | VDiv (v1, v2) | VRem (v1, v2)
  | VGt (v1, v2) | VGe (v1, v2)
  | VLt (v1, v2) | VLe (v1, v2)
  | VEq (v1, v2) | VNe (v1, v2)
  | VAnd (v1, v2) | VOr (v1, v2) | VXor (v1, v2)
  | VShl (v1, v2) | VLShr (v1, v2) | VAShr (v1, v2)
  -> closed e v1 && closed e v2

let rec apply_subs subs = function
  | VVar v as value -> begin try List.assoc v subs with Not_found -> value end
  | VConst _ | VNull | VUndef | VDummy _ | VZero as v -> v
  | VStruct vs -> VStruct (List.map (apply_subs subs) vs)
  | VPlus (v1, v2) -> VPlus (apply_subs subs v1, apply_subs subs v2)
  | VMult (v1, v2) -> VMult (apply_subs subs v1, apply_subs subs v2)
  | VMinus (v1, v2) -> VMinus (apply_subs subs v1, apply_subs subs v2)
  | VDiv (v1, v2) -> VDiv (apply_subs subs v1, apply_subs subs v2)
  | VRem (v1, v2) -> VRem (apply_subs subs v1, apply_subs subs v2)
  | VGt (v1, v2) -> VGt (apply_subs subs v1, apply_subs subs v2)
  | VGe (v1, v2) -> VGe (apply_subs subs v1, apply_subs subs v2)
  | VLt (v1, v2) -> VLt (apply_subs subs v1, apply_subs subs v2)
  | VLe (v1, v2) -> VLe (apply_subs subs v1, apply_subs subs v2)
  | VEq (v1, v2) -> VEq (apply_subs subs v1, apply_subs subs v2)
  | VNe (v1, v2) -> VNe (apply_subs subs v1, apply_subs subs v2)
  | VAnd (v1, v2) -> VAnd (apply_subs subs v1, apply_subs subs v2)
  | VOr (v1, v2) -> VOr (apply_subs subs v1, apply_subs subs v2)
  | VXor (v1, v2) -> VXor (apply_subs subs v1, apply_subs subs v2)
  | VRead v -> VRead (apply_subs subs v)
  | VCast v -> VCast (apply_subs subs v)
  | VShl (v1, v2) -> VShl (apply_subs subs v1, apply_subs subs v2)
  | VLShr (v1, v2) -> VLShr (apply_subs subs v1, apply_subs subs v2)
  | VAShr (v1, v2) -> VAShr (apply_subs subs v1, apply_subs subs v2)

let rec vars_of_value v =
  let rec aux acc = function
    | VVar v -> v :: acc
    | VConst _ | VNull | VUndef | VDummy _ | VZero -> acc
    | VStruct vs -> List.fold_left aux acc vs
    | VRead v | VCast v -> failwith "Unsupported memops"
    | VPlus (v1, v2) | VMinus (v1, v2)
    | VMult (v1, v2) | VDiv (v1, v2) | VRem (v1, v2)
    | VGt (v1, v2) | VGe (v1, v2)
    | VLt (v1, v2) | VLe (v1, v2)
    | VEq (v1, v2) | VNe (v1, v2)
    | VAnd (v1, v2) | VOr (v1, v2) | VXor (v1, v2)
    | VShl (v1, v2) | VLShr (v1, v2) | VAShr (v1, v2)
    -> aux (aux acc v1) v2
  in
  aux [] v


let reset_idxs () =
  var_counter := (-1)
