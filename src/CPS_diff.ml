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


(* This too highlights differences in CPS terms. It actually just pretty prints
 * (with appropriate newline characters and what not), so that diff(1) can take
 * care of it. *)

let () = Format.set_max_indent max_int
let () = Format.set_margin max_int
let () = Format.set_max_boxes max_int

let fp = Format.fprintf

let print_var f v = fp f "%s" (Prim.string_of_var v)

let print_value f = function
  | Prim.Vvar v -> fp f "%a" print_var (v:Prim.var)
  | Prim.Vconst c -> fp f "%s" (string_of_int c)

let print_expr f = function
  | Prim.ONone v -> fp f "%a" print_value v
  | Prim.OPlus  (v1, v2) -> fp f "%a + %a" print_value v1 print_value v2
  | Prim.OMult  (v1, v2) -> fp f "%a x %a" print_value v1 print_value v2
  | Prim.OMinus (v1, v2) -> fp f "%a - %a" print_value v1 print_value v2
  | Prim.ODiv   (v1, v2) -> fp f "%a / %a" print_value v1 print_value v2
  | Prim.OMax (v1, v2) -> fp f "max (%a, %a)" print_value v1 print_value v2
  | Prim.OMin (v1, v2) -> fp f "min (%a, %a)" print_value v1 print_value v2

let print_list p f l =
  let rec aux f = function
    | [] -> ()
    | [h] -> fp f "(%a)" p h
    | h::t -> fp f "(%a)@,%a" p h aux t
  in
  match l with
  | [] -> fp f "()"
  | _ -> fp f "%a" aux l

type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

let print_either pl pr f = function
  | Left l  -> fp f "%a" pl l
  | Right r -> fp f "%a" pr r

let rec print_m f = function
  | CPS.Mapp  (v, es, k) ->
    fp f "%a@[<v>@,%a@]"
      print_var v
      (print_list (print_either print_expr print_cont))
        (List.map (fun e -> Left e) es @ [Right k])
  | CPS.Mcont (v, es) ->
    fp f "%a@[<v>@,%a@]"
      print_var v
      (print_list print_expr) es
  | CPS.Mcond (e, (v1, es1), (v2, es2)) ->
    fp f "if0 (%a)@[<v>@,(%a@[<v>@,%a@]@,)@,(%a@[<v>@,%a@]@,)@]"
      print_expr e
      print_var v1 (print_list print_expr) es1
      print_var v2 (print_list print_expr) es2
  | CPS.Mlet  (v, e, m) ->
    fp f "@[<v>let %a =@[<v>@,%a@]@,in@[<v>@,%a@]@]"
      print_var v
      print_expr e
      print_m m
  | CPS.Mrec  (vls, m) ->
    let print_vl f (v, l) =
      fp f "%a =@[<v>@,%a@]" print_var v print_lambda l
    in
    fp f "letrec (@[<v>@,%a@]@,)@[<v>@,%a@]"
      (print_list print_vl) vls
      print_m m

and print_cont f = function
  | CPS.Cvar v -> fp f "%a" print_var v
  | CPS.C (v, m) -> fp f "\\c (%a).@[<v>@,%a@]" print_var v print_m m

and print_lambda f l =
  let aux c vs m =
    fp f "\\%c (@[<v>@,%a)@[<v>@,%a@]@]" c (print_list print_var) vs print_m m
  in
  match l with
  | CPS.Lproc (vs, v, m) -> aux 'p' (vs @ [v]) m
  | CPS.Ljump (vs, m) -> aux 'j' vs m
