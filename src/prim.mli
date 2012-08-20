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

(** Constants are only integers when working on the intermediate
    representation. If and when floats are supported, this might change.
  *)
type const = int

(** Variables. *)
type var

(** Variable Generation. Uses the argument as a base and adds unspecified
    prefixes and suffixes.
  *)
val var : string -> var

val string_of_var: var -> string

(** A generic variable generator. *)
val fresh_var : unit -> var

(** Values are either variables, constants, undefined, null, zeros, structures,
    dummies, or operations. *)
type value =
  (* var *)
  | VVar   of var
  (* integer const *)
  | VConst of const
  (* null, undef, and the like *)
  | VNull
  | VUndef
  | VDummy of string (* for things we won't translate *)
  | VZero
  (* structures (arrays, vectors, structs) *)
  | VStruct of value list
  (* arith binop *)
  | VPlus  of (value * value)
  | VMult  of (value * value)
  | VMinus of (value * value)
  | VDiv   of (value * value)
  | VRem   of (value * value)
  (* cmp *)
  | VGt of (value * value)
  | VGe of (value * value)
  | VLt of (value * value)
  | VLe of (value * value)
  | VEq of (value * value)
  | VNe of (value * value)
  (* bool binop *)
  | VAnd of (value * value)
  | VOr  of (value * value)
  | VXor of (value * value)
  (* mem read *)
  | VRead of value
  (* casting *)
  | VCast of value
  (* bitwise *)
  | VShl  of (value * value)
  | VLShr of (value * value)
  | VAShr of (value * value)

type mem_w =
  | MWrite of value
  | MAlloc

val closed: (var, 'a) Env.t -> value -> bool

val apply_subs: (var * value) list -> value -> value

(**/*)
val reset_idxs: unit -> unit
