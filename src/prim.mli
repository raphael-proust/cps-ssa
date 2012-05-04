 (*                                                                          *
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
  *                                                                          *)

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

(** Values are either variables or constants. *)
type value =
  | Vvar of var
  | Vconst of const

(** Expressions are operations on values (or just a value). *)
type expr =
  | ONone of value
  | OPlus  of (value * value)
  | OMult  of (value * value)
  | OMinus of (value * value)
  | ODiv   of (value * value)
  | OMax of (value * value)
  | OMin of (value * value)

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
