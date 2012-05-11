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

(* Pprint Operators and other facilities *)
module PP = Pprint
open PP.Operators
let with_paren d = PP.lparen ^^ d ^^ PP.rparen
let with_paren_br d = with_paren (d ^^ PP.break1)
let comma_space = PP.comma ^^ PP.space
let list ?(empty=PP.empty) pp = function
  | [] -> empty
  | l  ->  PP.sepmap PP.break1 pp l
type ('a, 'b) either =
  | Left of 'a
  | Right of 'b
let pp_either pl pr = function
  | Left l  -> pl l
  | Right r -> pr r
let level d = PP.nest 2 (PP.break1 ^^ d)
let unit = !^ "()"


(*We define pp_* functions for pretty-printing. *)

let pp_var v = !^ (Prim.string_of_var v)

let pp_value = function
  | Prim.Vvar v   -> pp_var v
  | Prim.Vconst c -> !^ (string_of_int c)

let pp_expr e =
  let pp_op v1 op v2 = pp_value v1 ^^ op ^^ pp_value v2 in
  let pp_fn fn v1 v2 = 
    fn ^^ PP.space ^^ with_paren (pp_value v1 ^^ comma_space ^^ pp_value v2)
  in
  match e with
  (* Direct value *)
  | Prim.ONone v -> pp_value v
  (* Arithmetic ops *)
  | Prim.OPlus  (v1, v2) -> pp_op v1 PP.plus  v2
  | Prim.OMult  (v1, v2) -> pp_op v1 PP.star  v2
  | Prim.OMinus (v1, v2) -> pp_op v1 PP.minus v2
  | Prim.ODiv   (v1, v2) -> pp_op v1 PP.bar   v2
  (* Other primitive functions *)
  | Prim.OMax (v1, v2) -> pp_fn (!^ "max") v1 v2
  | Prim.OMin (v1, v2) -> pp_fn (!^ "min") v1 v2

let rec pp_m = function
  | CPS.Mapp  (v, es, k) ->
    pp_var v ^^ level (
      list (* This list is never empty *)
        (pp_either pp_expr pp_cont)
        (List.map (fun e -> Left e) es @ [Right k])
    )

  | CPS.Mcont (v, es) ->
    pp_var v ^^ level (list ~empty:unit pp_expr es)

  | CPS.Mcond (e, (v1, es1), (v2, es2)) ->
    !^ "if0" ^^ PP.break0 ^^
    with_paren (pp_expr e) ^^ level (
      with_paren (pp_var v1 ^^ level (list ~empty:unit pp_expr es1)) ^^
      PP.break1 ^^
      with_paren (pp_var v2 ^^ level (list ~empty:unit pp_expr es2))
    )

  | CPS.Mlet  (v, e, m) ->
    !^ "let" ^^ PP.space ^^ pp_var v ^^ PP.space ^^ PP.equals ^^ level (
      pp_expr e
    ) ^^ PP.break1 ^^ !^ "in" ^^ level (
      pp_m m
    )

  | CPS.Mrec  (vls, m) ->
    let vl (v, l) =
      pp_var v ^^ PP.space ^^ PP.equals ^^ level (pp_lambda l) ^^ PP.break1
    in
    !^ "letrec" ^^ with_paren (level (
      PP.sepmap PP.hardline vl vls
    )) ^^ PP.break1 ^^ !^ "in" ^^ level (
      pp_m m
    )

and pp_l c vs m =
  (* This is used for both lambdas and explicit continuations *)
  !^ "λ" ^^ PP.char c ^^ level (
    list ~empty:unit pp_var vs ^^ PP.break1 ^^ PP.dot
  ) ^^ with_paren_br (level (
    pp_m m
  ))

and pp_cont = function
  | CPS.Cvar v   -> pp_var v
  | CPS.C (v, m) -> pp_l 'c' [v] m

and pp_lambda = function
  | CPS.Lproc (vs, v, m) -> pp_l 'p' (vs @ [v]) m
  | CPS.Ljump (vs, m)    -> pp_l 'j' vs m

