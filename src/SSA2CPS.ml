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

(*TODO: memoize or build map.*)
let block_of_label bs l =
  List.find (fun b -> b.SSA.b_label = l) bs

let rec block dom return bs ({SSA.b_label; b_phis; b_core_instrs; b_jump;} as b) =

  let args_of_label l =
    List.map
      (fun (_, p) -> List.assoc b_label p)
      ((block_of_label bs l).SSA.b_phis)
  in

  let rec aux = function
    | SSA.IAssignExpr (x, e)     :: l -> CPS.Mlet (x, e,  aux l)
    | SSA.IAssigncall (x, f, es) :: l ->
      let f = Prim.var_of_label f in
      CPS.Mapp (f, es, CPS.C (x, aux l))
    | SSA.IMemWrite (x, e)       :: l -> CPS.Mseq (x, e,  aux l)
    | [] ->
      (* instead of returning, we translate the last bit: the jump. *)
      match b_jump with
      | SSA.Jgoto l ->
        CPS.Mcont ((Prim.var_of_label l), (args_of_label l))
      | SSA.Jreturn e ->
        CPS.Mcont (return, [e])
      | SSA.Jreturnvoid ->
        CPS.Mcont (return, [])
      | SSA.Jtail (f, es, d) ->
        let f = Prim.var_of_label f in
        let d = Prim.var_of_label d in
        CPS.Mapp (f, es, CPS.Cvar d)
      | SSA.Jcond (c, l1, l2) ->
        CPS.Mcond (c,
                   (Prim.var_of_label l1, (args_of_label l1)),
                   (Prim.var_of_label l2, (args_of_label l2))
                  )
  in

  (* We translate immediate dominatees as local lambdas *)
  match Dom.G.pred dom b with
  | [] -> aux b_core_instrs
  | l  ->
    let l =
      List.map
        (fun domed -> (*terminates bc dominator tree is a DAG*)
          let lbl = Prim.var_of_label domed.SSA.b_label in
          let vs = List.map fst domed.SSA.b_phis in
          let lambda = CPS.Ljump (vs, block dom return bs domed) in
          (lbl, lambda)
        )
        l
    in
    CPS.Mrec (l, aux b_core_instrs)

(* translate a whole procedure. *)
and proc {SSA.p_args; p_blocks;} =
  match p_blocks with
  | [] -> failwith "Can't translate empty ssa procedure into cps"
  | entry::_ ->
    let dom = Dom.dom_of_blocks p_blocks in
    (* remove phony fixpoint-seed. *)
    let dom = Dom.G.remove_edge dom entry entry in
    let return = Prim.fresh_var () in
    CPS.Lproc (p_args, return, block dom return p_blocks entry)

(* translate a whole program *)
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
    let args =
      let main_proc =
        List.find
          SSA.(fun p -> (List.hd p.p_blocks).b_label = label_main)
          proclist
      in
      List.map
        (fun v -> Prim.(ONone (Vvar v)))
        main_proc.SSA.p_args
    in
    CPS.Mrec (lambdas,
              (CPS.Mapp ((Prim.var_of_label SSA.label_main),
                         args,
                         CPS.Cvar CPS.var_run
                        )
              )
             )
