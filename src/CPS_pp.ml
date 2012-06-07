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
module PP = struct
  include Pprint
  include Util.PP
end
open PP.Operators


(*We define pp_* functions for pretty-printing. *)

let pp_var v = !^ (Prim.string_of_var v)

let pp_value = function
  | Prim.Vvar v   -> pp_var v
  | Prim.Vconst c -> !^ (string_of_int c)
  | Prim.Vnull    -> !^ "null"

let pp_expr e =
  match e with
  (* Direct value *)
  | Prim.ONone v -> pp_value v
  (* Arithmetic ops *)
  | Prim.OPlus  (v1, v2) -> PP.pp_op pp_value v1 PP.plus    v2
  | Prim.OMult  (v1, v2) -> PP.pp_op pp_value v1 PP.star    v2
  | Prim.OMinus (v1, v2) -> PP.pp_op pp_value v1 PP.minus   v2
  | Prim.ODiv   (v1, v2) -> PP.pp_op pp_value v1 PP.bar     v2
  | Prim.ORem   (v1, v2) -> PP.pp_op pp_value v1 PP.percent v2
  (* Arithmetic functions *)
  | Prim.OMax (v1, v2) -> PP.pp_fn2 pp_value (!^ "max") v1 v2
  | Prim.OMin (v1, v2) -> PP.pp_fn2 pp_value (!^ "min") v1 v2
  (* Comparisons *)
  | Prim.OGt (v1, v2) -> PP.pp_op pp_value v1 (!^ ">" ) v2
  | Prim.OGe (v1, v2) -> PP.pp_op pp_value v1 (!^ ">=") v2
  | Prim.OLt (v1, v2) -> PP.pp_op pp_value v1 (!^ "<" ) v2
  | Prim.OLe (v1, v2) -> PP.pp_op pp_value v1 (!^ "=<") v2
  | Prim.OEq (v1, v2) -> PP.pp_op pp_value v1 (!^ "==") v2
  | Prim.ONe (v1, v2) -> PP.pp_op pp_value v1 (!^ "<>") v2
  (* IO *)
  | Prim.ORead v -> PP.pp_fn1 pp_value (!^ "read") v

let pp_mem_w = function
  | Prim.MWrite v -> PP.pp_fn1 pp_value (!^ "write") v
  | Prim.MAlloc -> !^ "alloc()"

let rec pp_m = function
  | CPS.Mapp  (v, es, k) ->
    pp_var v ^^ PP.level (
      PP.list (* This list is never empty *)
        (PP.pp_either pp_expr pp_cont)
        (List.map (fun e -> Util.Left e) es @ [Util.Right k])
    )

  | CPS.Mcont (v, es) ->
    pp_var v ^^ PP.level (PP.list ~empty:PP.unit pp_expr es)

  | CPS.Mcond (e, (v1, es1), (v2, es2)) ->
    !^ "if0" ^^ PP.break0 ^^
    PP.with_paren (pp_expr e) ^^ PP.level (
      PP.with_paren
        (pp_var v1 ^^ PP.level (PP.list ~empty:PP.unit pp_expr es1)) ^^
      PP.break1 ^^
      PP.with_paren (pp_var v2 ^^ PP.level (PP.list ~empty:PP.unit pp_expr es2))
    )

  | CPS.Mlet  (v, e, m) ->
    !^ "let" ^^ PP.space ^^ pp_var v ^^ PP.space ^^ PP.equals ^^ PP.level (
      pp_expr e
    ) ^^ PP.break1 ^^ !^ "in" ^^ PP.level (
      pp_m m
    )

  | CPS.Mrec  (vls, m) ->
    let vl (v, l) =
      pp_var v ^^ PP.space ^^ PP.equals ^^ PP.level (pp_lambda l) ^^ PP.break1
    in
    !^ "letrec" ^^ PP.with_paren (PP.level (
      PP.sepmap PP.hardline vl vls
    )) ^^ PP.break1 ^^ !^ "in" ^^ PP.level (
      pp_m m
    )
  | CPS.Mseq (v, w, m) ->
    !^ "let" ^^ PP.space ^^ PP.lparen ^^ PP.rparen ^^ PP.space ^^ PP.equals ^^
    PP.level (
        pp_mem_w w
      ) ^^ PP.break1 ^^ !^ "in" ^^ PP.level (
        pp_m m
      )

and pp_l c vs m =
  (* This is used for both lambdas and explicit continuations *)
  !^ "λ" ^^ PP.char c ^^ PP.level (
    PP.list ~empty:PP.unit pp_var vs ^^ PP.break1 ^^ PP.dot
  ) ^^ PP.with_paren_br (PP.level (
    pp_m m
  ))

and pp_cont = function
  | CPS.Cvar v   -> pp_var v
  | CPS.C (v, m) -> pp_l 'c' [v] m

and pp_lambda = function
  | CPS.Lproc (vs, v, m) -> pp_l 'p' (vs @ [v]) m
  | CPS.Ljump (vs, m)    -> pp_l 'j' vs m

