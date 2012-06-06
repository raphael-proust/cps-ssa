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

let unsupported_feature s =
  failwith ("Unsupported feature: " ^ s)


let is_terminator = function
  | LLVM.INSTR_Br _
  | LLVM.INSTR_Br_1 _
  | LLVM.INSTR_Switch _
  | LLVM.INSTR_IndirectBr
  | LLVM.INSTR_Invoke _
  | LLVM.INSTR_Resume _
  | LLVM.INSTR_Unreachable -> true
  | _ -> false (*TODO: expand*)

let ident = function
  | LLVM.ID_Global v -> Prim.var ("@" ^ v)
  | LLVM.ID_Local  v -> Prim.var ("%" ^ v)

let label l = Prim.label_of_var (ident l)

let var i = Prim.Vvar (ident i)

let var_expr i = Prim.ONone (var i)

let int_of_bool = function
  | true -> 1
  | false -> 0

let value = function
  | LLVM.VALUE_Ident i -> var i
  | LLVM.VALUE_Integer i -> Prim.Vconst i
  | LLVM.VALUE_Float _ -> unsupported_feature "VALUE_Float" (*TODO*)
  | LLVM.VALUE_Bool b -> Prim.Vconst (int_of_bool b)
  | LLVM.VALUE_Null -> Prim.Vnull

let block_of_instrs instrs =

  (*TODO: labels are hard!*)
  let get_label   instrs = failwith "TODO" in

  let get_phis    instrs =
    let rec aux accu = function
      | [] -> assert false
      | LLVM.INSTR_PHI (res, _, assocs) :: instrs ->
        let flip_val (x,y) = (label y, Prim.ONone (value x)) in
        aux ((ident res, List.map flip_val assocs) :: accu) instrs
      | instrs -> (List.rev accu, instrs)
    in
    aux [] instrs
  in

  let get_assigns instrs =
    let rec aux accu = function
      | [] -> assert false
      | LLVM.INSTR_Add (i, _, v0, v1) :: instrs ->
        aux (SSA.IAssignExpr (ident i, Prim.OPlus (value v0, value v1)) :: accu) instrs
      | LLVM.INSTR_Sub (i, _, v0, v1) :: instrs ->
        aux (SSA.IAssignExpr (ident i, Prim.OMinus (value v0, value v1)) :: accu) instrs
      | LLVM.INSTR_Mul (i, _, v0, v1) :: instrs ->
        aux (SSA.IAssignExpr (ident i, Prim.OMult (value v0, value v1)) :: accu) instrs
      | LLVM.INSTR_UDiv (i, _, v0, v1) :: instrs
      | LLVM.INSTR_SDiv (i, _, v0, v1) :: instrs ->
        aux (SSA.IAssignExpr (ident i, Prim.ODiv (value v0, value v1)) :: accu) instrs
      | LLVM.INSTR_URem (i, _, v0, v1) :: instrs
      | LLVM.INSTR_SRem (i, _, v0, v1) :: instrs ->
        aux (SSA.IAssignExpr (ident i, Prim.ORem (value v0, value v1)) :: accu) instrs
      | LLVM.INSTR_Shl (i, _, v0, v1) :: instrs -> unsupported_feature "INSTR_Shl"
      | LLVM.INSTR_LShr (i, _, v0, v1) :: instrs -> unsupported_feature "INSTR_LShr"
      | LLVM.INSTR_AShr (i, _, v0, v1) :: instrs -> unsupported_feature "INSTR_AShr"
      | LLVM.INSTR_And (i, _, v0, v1) :: instrs -> unsupported_feature "INSTR_And"
      | LLVM.INSTR_Or (i, _, v0, v1) :: instrs -> unsupported_feature "INSTR_Or"
      | LLVM.INSTR_Xor (i, _, v0, v1) :: instrs -> unsupported_feature "INSTR_Xor"
      | LLVM.INSTR_Load (i1, _, i2) :: instrs ->
        aux (SSA.IAssignExpr (ident i1, var_expr i2) :: accu) instrs
      | LLVM.INSTR_ICmp (i, icmp, _, v1, v2) :: instrs ->
        let expr = match icmp with
          | LLVM.Cmp_Eq -> Prim.OEq (value v1, value v2)
          | LLVM.Cmp_Ne -> Prim.ONe (value v1, value v2)
          | LLVM.Cmp_Ugt
          | LLVM.Cmp_Sgt -> Prim.OGt (value v1, value v2)
          | LLVM.Cmp_Uge
          | LLVM.Cmp_Sge -> Prim.OGe (value v1, value v2)
          | LLVM.Cmp_Ult
          | LLVM.Cmp_Slt -> Prim.OLt (value v1, value v2)
          | LLVM.Cmp_Ule
          | LLVM.Cmp_Sle -> Prim.OLe (value v1, value v2)
        in
        aux (SSA.IAssignExpr (ident i, expr) :: accu) instrs

      | LLVM.INSTR_Store (_, v, _, i) :: instrs ->
        aux (SSA.IMemWrite (ident i, Prim.MWrite (value v)) :: accu) instrs
      | LLVM.INSTR_Alloca (i, _) :: instrs ->
        aux (SSA.IMemWrite (ident i, Prim.MAlloc) :: accu) instrs

      | [terminator] -> assert (is_terminator terminator); (List.rev accu, terminator)
      | _ -> assert false (*TODO: expand _ *)
    in
    aux [] instrs
  in

  let get_terminator    terminator = match terminator with
    | LLVM.INSTR_Br_1 l -> SSA.Jgoto (label l)
    | LLVM.INSTR_Br (i, l1, l2) ->
      SSA.Jcond (Prim.ONone (value i), label l1, label l2)
    | LLVM.INSTR_Ret (_, i) -> SSA.Jreturn (Prim.ONone (value i))
    | LLVM.INSTR_Ret_void -> SSA.Jreturnvoid
    | _ -> assert false (*TODO expand _ *)
  in

  let label , instrs     = get_label   instrs in
  let phis  , instrs     = get_phis    instrs in
  let instrs, terminator = get_assigns instrs in
  let terminator         = get_terminator    terminator   in

  assert (instrs = []);
  SSA.Blocks.block ~label ~phis ~instrs terminator

let blocks_of_instrs instrs =
  let rec aux accu_blocks accu_instrs = function
    | [] ->
      assert (accu_instrs = []);
      List.rev accu_blocks
    | i :: is ->
      if is_terminator i then
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
    p_blocks = blocks_of_instrs (LLVM.INSTR_Label name :: instrs);
  }

let prog = List.map proc

