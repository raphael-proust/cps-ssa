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

let block_of_label bs l =
  List.find (fun b -> b.SSA.b_label = l) bs

let rec block dom return bs ({SSA.b_label; b_phis; b_assigns; b_jump;} as b) =

  let args_of_label l =
    List.map
      (fun (_, p) -> List.assoc b_label p)
      ((block_of_label bs l).SSA.b_phis)
  in

  let rec aux = function
    | SSA.Aexpr (x, e)     :: l -> CPS.Mlet (x, e,  aux l)
    | SSA.Acall (x, f, es) :: l ->
      let f = Prim.var_of_label f in
      CPS.Mapp (f, es, CPS.C (x, aux l))
    | [] -> match b_jump with (*somehow ugly*)
      | SSA.Jgoto l ->
        CPS.Mcont ((Prim.var_of_label l), (args_of_label l))
      | SSA.Jreturn e ->
        CPS.Mcont (return, [e])
      | SSA.Jtail (f, es) ->
        let f = Prim.var_of_label f in
        CPS.Mapp (f, es, CPS.Cvar return)
      | SSA.Jcond (c, l1, l2) ->
        CPS.Mcond (c,
                   (Prim.var_of_label l1, (args_of_label l1)),
                   (Prim.var_of_label l2, (args_of_label l2))
                  )
  in

  match Dom.G.pred dom b with
  | [] -> aux b_assigns
  | (h::_) as l  ->
    if h = b then
      aux b_assigns
    else begin
      let l =
        List.map
          (fun b -> (*terminates bc dominator tree is a DAG*)
            let lbl = Prim.var_of_label b.SSA.b_label in
            let vs = List.map fst b.SSA.b_phis in
            let lambda = CPS.Ljump (vs, block dom return bs b) in
            (lbl, lambda)
          )
          l
      in
      CPS.Mrec (l, aux b_assigns)
    end



and proc {SSA.p_args; p_blocks;} =
  match p_blocks with
  | [] -> failwith "Can't translate empty ssa procedure into cps"
  | entry::_ ->
    let dom = Dom.dom_of_blocks p_blocks in
    let return = Prim.fresh_var () in
    CPS.Lproc (p_args, return, block dom return p_blocks entry)

and prog proclist =
  if proclist = [] then
    failwith "Can't translate empty ssa program into cps"
  else
    (* we need immediate dominatees for the translation *)
    let lambdas =
      List.map
       (fun p ->
         let lbl= Prim.var_of_label (List.hd p.SSA.p_blocks).SSA.b_label in
         (lbl, proc p)
       )
       proclist
    in
    CPS.Mrec (lambdas,
              (CPS.Mapp ((Prim.var_of_label SSA.label_main),
                         [],
                         CPS.Cvar CPS.var_run)
              )
             )
