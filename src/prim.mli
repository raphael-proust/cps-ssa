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
    representation.
  *)
type const = int

(** Variables are represented as strings. They are private so as to ease
    printing and comparison, while making it possible, in the future to check for
    uniqueness of variable names (well, not really, but sort of).
  *)
type var = private string

(** Variable Generation. Uses the argument as a base and adds unspecified
    prefixes and suffixes.
  *)
val var : string -> var

val string_of_var: var -> string

(** A generic variable generator. *)
val fresh_var : unit -> var

(** Labels. Like variables but for procedure block naming. *)
type label = private string

(** Label generation. Similar to [var]. *)
val label : string -> label

val string_of_label: label -> string

(** A generic label generator. *)
val fresh_label : unit -> label

(** [label] to and from [var] translation. Usefull for translating procedures
    into lambdas and vice versa.
  *)

val var_of_label : label -> var
val label_of_var : var -> label

(** Values are either variables, constants, undefined, null, zeros, structures,
    dummies, or operations. *)
type value =
  (* var *)
  | Vvar   of var
  (* integer const *)
  | Vconst of const
  (* null, undef, and the like *)
  | Vnull
  | Vundef
  | Vdummy of string (* for things we won't translate *)
  | Vzero
  (* structures (arrays, vectors, structs) *)
  | Vstruct of value list
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

type mem_w =
  | MWrite of value
  | MAlloc

(**/*)
val reset_idxs: unit -> unit
