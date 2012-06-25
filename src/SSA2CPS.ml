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

open Util

let args_of_label proc orig dest =
  let right_block = SSA.block_of_label proc dest in
  match right_block with
  | E.Left _ -> assert false (* no jump to entry block *)
  | E.Right block ->
    List.map
      (fun (_, p) ->
        try
          List.assoc orig p
        with
        | Not_found -> (* default value *)
          Printf.eprintf "Label %s not found\nAvailable labels: %s\n"
            (Prim.string_of_label orig)
            (String.concat " "
              (List.map (fun (l, _) -> Prim.string_of_label l) p)
            );
          raise Not_found
      )
      block.SSA.b_phis

let core_instrs_and_jump k proc current_l cis j =
  let open CPS in
  let rec aux = function
  | SSA.IAssignExpr (v, e) :: cis -> MLet (v, e, aux cis)
  | SSA.IAssignCall (v, (l, es)) :: cis ->
      MApp (Prim.var_of_label l, es, C (v, aux cis))
  | SSA.ICall (l, es) :: cis ->
      MApp (Prim.var_of_label l, es, C (var_unit, aux cis))
  | SSA.IAssignSelect (v, c, v1, v2) :: cis ->
      MSel (v, c, v1, v2, aux cis)
  | SSA.IMemWrite (v, w) :: cis -> MSeq (v, w, aux cis)
  | [] -> match j with
    | SSA.JGoto l ->
        MCont (Prim.var_of_label l, args_of_label proc current_l l)
    | SSA.JReturn e   -> MCont (k, [e])
    | SSA.JReturnVoid -> MCont (k, [])
    | SSA.JTail (l, es, lc) ->
        MApp (Prim.var_of_label l, es, CVar (Prim.var_of_label lc))
    | SSA.JCond (e, l1, l2) ->
        MCond (e,
               (Prim.var_of_label l1, args_of_label proc current_l l1),
               (Prim.var_of_label l2, args_of_label proc current_l l2))
  in
  aux cis

let rec tr_abstract_block dom k proc current_l node core_instrs jump =
  let m = core_instrs_and_jump k proc current_l core_instrs jump in

  match Dom.G.pred dom node with
  | [] -> m
  | domeds  ->
    let l =
      List.map
        (fun domed -> (*terminates bc dominator tree is a DAG*)
          let lbl = Prim.var_of_label domed.SSA.b_label in
          let vs = List.map fst domed.SSA.b_phis in
          let lambda = CPS.LJump (vs, tr_block dom k proc domed) in
          (lbl, lambda)
        )
        (List.map
          (function
            | E.Left _ -> assert false (* no jump to entry block *)
            | E.Right l -> l
          )
          domeds
        )
    in
    (*FIXME: scoping problem! order should be: assignements, continuations, body
     * *)
    CPS.MRec (l, m)


and tr_block dom k proc block =
  tr_abstract_block dom k proc block.SSA.b_label (E.Right block)
    block.SSA.b_core_instrs
    block.SSA.b_jump

let tr_entry_block dom k proc entry_block =
  tr_abstract_block dom k proc entry_block.SSA.eb_label (E.Left entry_block)
    entry_block.SSA.eb_core_instrs
    entry_block.SSA.eb_jump

let tr_proc proc =
  let dom = Dom.dom_of_proc proc in
  let k = Prim.fresh_var () in
  let m = tr_entry_block dom k proc proc.SSA.p_entry_block in
  CPS.LProc (proc.SSA.p_args, k, m)

let tr_module module_ =
    List.map
      (fun proc -> (Prim.var_of_label proc.SSA.p_name, tr_proc proc))
      module_

let tr_prog (main, module_) =
  let open CPS in
  MRec
    (tr_module (main :: module_),
     MApp (Prim.var_of_label main.SSA.p_name,
           List.map (fun v -> Prim.VVar v) main.SSA.p_args,
           CVar var_run
          )
    )

let proc    = tr_proc
let module_ = tr_module
let prog    = tr_prog
