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

(* This too highlights differences in CPS terms. It actually just pretty prints
 * (with appropriate newline characters and what not), so that diff(1) can take
 * care of it. *)

open Util

(* Pprint Operators and other facilities *)
module PP = struct
  include Pprint
  include Util.PP
end
open PP.Operators


(*We define pp_* functions for pretty-printing. *)

let paren = true (* for ~paren *)

let pp_mcont (v, es) =
  Prim_pp.pp_var v ^^ PP.space ^^
    PP.list ~empty:PP.unit ~sep:PP.space Prim_pp.pp_value es

let rec pp_m = function
  | CPS.MApp  (v, es, k) ->
    Prim_pp.pp_var v ^^ PP.space ^^
      PP.list (* This list is never empty *)
        ~sep:PP.space
        (PP.either (Prim_pp.pp_value ~paren) pp_cont)
        (List.map (fun e -> E.Left e) es @ [E.Right k])

  | CPS.MCont cont -> pp_mcont cont

  | CPS.MCond (e, cont1, cont2) ->
    !^ "if" ^^ PP.space ^^ Prim_pp.pp_value ~paren e ^^
    PP.level (PP.break1 ^^
      PP.with_paren (pp_mcont cont1) ^^ PP.break1 ^^
      PP.with_paren (pp_mcont cont2)
    )

  | CPS.MLet  (v, e, m) ->
    !^ "let " ^^ Prim_pp.pp_var v ^^ PP.space ^^ PP.equals ^^ PP.space ^^
      Prim_pp.pp_value e ^^
    !^ " in" ^^ PP.break1 ^^
      pp_m m

  | CPS.MSel  (v, c, v1, v2, m) ->
    !^ "sel " ^^ Prim_pp.pp_var v ^^ PP.space ^^ PP.equals ^^ PP.space ^^
      Prim_pp.pp_value ~paren c ^^ PP.space ^^
      PP.with_paren (Prim_pp.pp_value v1) ^^ PP.space ^^
      PP.with_paren (Prim_pp.pp_value v2) ^^ PP.space ^^
    !^ " in" ^^ PP.break1 ^^
      pp_m m

  | CPS.MRec  (vls, m) ->
    let vl (v, (vs, m)) =
      Prim_pp.pp_var v ^^ PP.space ^^ PP.equals ^^ PP.space ^^ pp_l 'j' vs m
    in
    let _and = !^ "and " in
    !^ "let rec " ^^ PP.list ~sep:(PP.break1 ^^ _and) vl vls ^^ PP.break1 ^^
    (!^ "in") ^^ PP.break1 ^^
    pp_m m

  | CPS.MSeq (v, w, m) ->
    !^ "let " ^^ PP.unit ^^ PP.space ^^ PP.equals ^^ PP.space ^^
      Prim_pp.pp_var v ^^ PP.space ^^
      Prim_pp.pp_mem_w w ^^ !^ " in" ^^ PP.break1 ^^
      pp_m m

and pp_l c vs m =
  (* This is used for both lambdas and explicit continuations *)
  !^ "λ" ^^ PP.char c ^^ PP.space ^^
    PP.list ~empty:PP.unit ~sep:PP.space Prim_pp.pp_var vs ^^ PP.space ^^ PP.dot
  ^^ PP.with_paren (PP.level (PP.break0 ^^ pp_m m))

and pp_cont = function
  | CPS.CVar v   -> Prim_pp.pp_var v
  | CPS.C (v, m) -> PP.with_paren (pp_l 'c' [v] m)

and pp_proc (vs, v, m) = pp_l 'p' (vs @ [v]) m

let pp_var_proc (v, l) =
  Prim_pp.pp_var v ^^ PP.space ^^ PP.equals ^^ PP.level (PP.break1 ^^ pp_proc l)

let pp_module vls = PP.list ~sep:PP.break1 pp_var_proc vls

