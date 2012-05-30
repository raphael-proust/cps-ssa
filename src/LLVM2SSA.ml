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


let is_jump = function
  | LLVM.Br_1 _
  | LLVM.Br _
  | LLVM.Ret _ -> true
  | LLVM.Phi _
  | LLVM.Alloca _
  | LLVM.Add _
  | LLVM.Store _
  | LLVM.Load _
  | LLVM.Icmp _
  | LLVM.Label _ -> false

let ident = function
  | LLVM.Id_Global v -> Prim.var ("@" ^ Prim.string_of_var v)
  | LLVM.Id_Local  v -> Prim.var ("%" ^ Prim.string_of_var v)

let var i = Prim.ONone (Prim.Vvar (ident i))

let value = function
  | LLVM.Vvar i -> Prim.Vvar (ident i)
  | LLVM.Vconst n -> Prim.Vconst n

let block_of_instrs instrs =

  let get_label   instrs = match instrs with
    | [] -> assert false
    | LLVM.Label l :: instrs -> (l, instrs)
    | _ -> assert false
  in
  let get_phis    instrs =
    let rec aux accu = function
      | [] -> assert false
      | LLVM.Phi (res, _, assocs) :: instrs ->
        let flip_val (x,y) = (y, Prim.ONone (value x)) in
        aux ((ident res, List.map flip_val assocs) :: accu) instrs
      | instrs -> (List.rev accu, instrs)
    in
    aux [] instrs
  in
  let get_assigns instrs =
    let rec aux accu = function
      | [] -> assert false
      | LLVM.Alloca (i, _) :: instrs ->
        aux (SSA.Aexpr (ident i, Prim.OAlloc) :: accu) instrs
      | LLVM.Add (i, _, v0, v1) :: instrs ->
        aux (SSA.Aexpr (ident i, Prim.OPlus (value v0, value v1)) :: accu) instrs
      | LLVM.Store ((_,i), (_, v)) :: instrs ->
        aux (SSA.Aexpr (ident i, Prim.OWrite (value v)) :: accu) instrs
      | LLVM.Load (i1, _, i2) :: instrs ->
        aux (SSA.Aexpr (ident i1, var i2) :: accu) instrs
      | LLVM.Icmp (i, icmp, _, v1, v2) :: instrs ->
        let expr = match icmp with
          | LLVM.Eq -> Prim.OEq (value v1, value v2)
          | LLVM.Ne -> Prim.ONe (value v1, value v2)
          | LLVM.Ugt
          | LLVM.Sgt -> Prim.OGt (value v1, value v2)
          | LLVM.Uge
          | LLVM.Sge -> Prim.OGe (value v1, value v2)
          | LLVM.Ult
          | LLVM.Slt -> Prim.OLt (value v1, value v2)
          | LLVM.Ule
          | LLVM.Sle -> Prim.OLe (value v1, value v2)
        in
        aux (SSA.Aexpr (ident i, expr) :: accu) instrs
      | [jump] -> assert (is_jump jump); (List.rev accu, jump)
      | _ -> assert false (*TODO: expand _ *)
    in
    aux [] instrs
  in
  let get_jump    jump = match jump with
    | LLVM.Br_1 l -> SSA.Jgoto l
    | LLVM.Br (i, l1, l2) -> SSA.Jcond (Prim.ONone (value i), l1, l2)
    | LLVM.Ret (_, i) -> SSA.Jreturn (Prim.ONone (value i))
    | _ -> assert false (*TODO expand _ *)
  in

  let label  , instrs = get_label   instrs in
  let phis   , instrs = get_phis    instrs in
  let assigns, jump   = get_assigns instrs in
  let jump            = get_jump    jump   in

  assert (instrs = []);
  SSA.Blocks.block ~label ~phis ~assigns jump

let blocks_of_instrs instrs =
  let rec aux accu_blocks accu_instrs = function
    | [] ->
      assert (accu_instrs = []);
      List.rev accu_blocks
    | i :: is ->
      if is_jump i then
        aux
          (block_of_instrs (List.rev (i :: accu_instrs))
           :: accu_blocks
          )
          []
          is
      else
        aux accu_blocks (i :: accu_instrs) is
  in
  aux [] [] instrs

let proc {LLVM.name; args; instrs} =
  {SSA.
    p_args = List.map (fun (_, a) -> ident a) args;
    p_blocks = blocks_of_instrs (LLVM.Label name :: instrs);
  }

let prog = List.map proc

