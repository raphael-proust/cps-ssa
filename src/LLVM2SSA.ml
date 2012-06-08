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


(*TODO: major cosmetics *)

let unsupported_feature s =
  failwith ("Unsupported feature: " ^ s)


let is_terminator i =
  let open LLVM in
  match i with
  | INSTR_Ret _
  | INSTR_Ret_void
  | INSTR_Br _
  | INSTR_Br_1 _
  | INSTR_Switch _
  | INSTR_IndirectBr
  | INSTR_Invoke _
  | INSTR_Resume _
  | INSTR_Unreachable -> true

  |INSTR_Add _ |INSTR_FAdd |INSTR_Sub _ |INSTR_FSub |INSTR_Mul _ |INSTR_FMul
  |INSTR_UDiv _ |INSTR_SDiv _ |INSTR_FDiv |INSTR_URem _ |INSTR_SRem _
  |INSTR_FRem |INSTR_Shl _ |INSTR_LShr _ |INSTR_AShr _ |INSTR_And _ |INSTR_Or _
  |INSTR_Xor _ |INSTR_ICmp _ |INSTR_FCmp |INSTR_PHI _ |INSTR_Call _
  |INSTR_Trunc _ |INSTR_ZExt _ |INSTR_SExt _ |INSTR_FPTrunc _ |INSTR_FPExt _
  |INSTR_UIToFP _ |INSTR_SIToFP _ |INSTR_FPToUI _ |INSTR_FPToSI _
  |INSTR_IntToPtr _ |INSTR_PtrToInt _ |INSTR_BitCast _ |INSTR_Select
  |INSTR_VAArg |INSTR_Alloca _ |INSTR_Load _ |INSTR_Store _ |INSTR_AtomicCmpXchg
  |INSTR_AtomicRMW |INSTR_Fence |INSTR_GetElementPtr |INSTR_ExtractElement
  |INSTR_InsertElement |INSTR_ShuffleVector |INSTR_ExtractValue
  |INSTR_InsertValue |INSTR_LandingPad |INSTR_Label _
  -> false

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

let value_expr v = Prim.ONone (value v)

let running_idx = ref 0
let update_runnig_idx i =
  assert (i = !running_idx + 1);
  incr running_idx
let get_running_idx () =
  incr running_idx;
  !running_idx

let ident_left = function
  | LLVM.ID_Global v -> Prim.var ("@" ^ v)
  | LLVM.ID_Local  v ->
    let () =
      try
        let i = int_of_string v in
        update_runnig_idx i
      with
        | Failure "int_of_string" -> ()
    in
    Prim.var ("%" ^ v)

let var_left i = Prim.Vvar (ident_left i)

let var_expr_left i = Prim.ONone (var_left i)

let get_assigns instrs =
  let open LLVM in
  let rec aux accu = function
    | []
    | (INSTR_PHI _) :: _
    | ( INSTR_Ret _ | INSTR_Ret_void | INSTR_Br _ | INSTR_Br_1 _
      | INSTR_Switch _ | INSTR_IndirectBr | INSTR_Invoke _
      | INSTR_Resume _ | INSTR_Unreachable) :: _ :: _
    | (INSTR_Label _) :: _
    -> assert false

    | INSTR_Add (i, _, v0, v1) :: instrs ->
      aux (SSA.IAssignExpr (ident_left i, Prim.OPlus (value v0, value v1)) :: accu) instrs
    | INSTR_FAdd :: instrs -> unsupported_feature "INSTR_FAdd"
    | INSTR_Sub (i, _, v0, v1) :: instrs ->
      aux (SSA.IAssignExpr (ident_left i, Prim.OMinus (value v0, value v1)) :: accu) instrs
    | INSTR_FSub :: instrs -> unsupported_feature "INSTR_FSub"
    | INSTR_Mul (i, _, v0, v1) :: instrs ->
      aux (SSA.IAssignExpr (ident_left i, Prim.OMult (value v0, value v1)) :: accu) instrs
    | INSTR_FMul :: instrs -> unsupported_feature "INSTR_FMul"
    | INSTR_UDiv (i, _, v0, v1) :: instrs
    | INSTR_SDiv (i, _, v0, v1) :: instrs ->
      aux (SSA.IAssignExpr (ident_left i, Prim.ODiv (value v0, value v1)) :: accu) instrs
    | INSTR_FDiv :: instrs -> unsupported_feature "INSTR_FDiv"
    | INSTR_URem (i, _, v0, v1) :: instrs
    | INSTR_SRem (i, _, v0, v1) :: instrs ->
      aux (SSA.IAssignExpr (ident_left i, Prim.ORem (value v0, value v1)) :: accu) instrs
    | INSTR_FRem :: instrs -> unsupported_feature "INSTR_FRem"
    | INSTR_Shl _ :: instrs -> unsupported_feature "INSTR_Shl"
    | INSTR_LShr _ :: instrs -> unsupported_feature "INSTR_LShr"
    | INSTR_AShr _ :: instrs -> unsupported_feature "INSTR_AShr"
    | INSTR_And _ :: instrs -> unsupported_feature "INSTR_And"
    | INSTR_Or _ :: instrs -> unsupported_feature "INSTR_Or"
    | INSTR_Xor _ :: instrs -> unsupported_feature "INSTR_Xor"
    | INSTR_ICmp (i, icmp, _, v1, v2) :: instrs ->
      let expr = match icmp with
        | Cmp_Eq  -> Prim.OEq (value v1, value v2)
        | Cmp_Ne  -> Prim.ONe (value v1, value v2)
        | Cmp_Ugt
        | Cmp_Sgt -> Prim.OGt (value v1, value v2)
        | Cmp_Uge
        | Cmp_Sge -> Prim.OGe (value v1, value v2)
        | Cmp_Ult
        | Cmp_Slt -> Prim.OLt (value v1, value v2)
        | Cmp_Ule
        | Cmp_Sle -> Prim.OLe (value v1, value v2)
      in
      aux (SSA.IAssignExpr (ident_left i, expr) :: accu) instrs
    | INSTR_FCmp :: instrs -> unsupported_feature "INSTR_FCmp"
    | INSTR_Call (i, _, fn, args) :: instrs ->
      let args = List.map (fun (_, v) -> value_expr v) args in
      aux (SSA.IAssigncall (ident_left i, label fn, args) :: accu) instrs
    | ( INSTR_Trunc (i, _, v, _)
      | INSTR_ZExt (i, _, v, _)
      | INSTR_SExt (i, _, v, _)
      | INSTR_FPTrunc (i, _, v, _)
      | INSTR_FPExt (i, _, v, _)
      | INSTR_UIToFP (i, _, v, _)
      | INSTR_SIToFP (i, _, v, _)
      | INSTR_FPToUI (i, _, v, _)
      | INSTR_FPToSI (i, _, v, _)
      | INSTR_IntToPtr (i, _, v, _)
      | INSTR_PtrToInt (i, _, v, _)
      | INSTR_BitCast (i, _, v, _)
      ) :: instrs ->
      aux (SSA.IAssignExpr (ident_left i, value_expr v) :: accu) instrs
    | INSTR_Select :: instrs -> unsupported_feature "INSTR_Select"
    | INSTR_VAArg :: instrs -> unsupported_feature "INSTR_VAArg"
    | INSTR_Alloca (i, _) :: instrs ->
      aux (SSA.IMemWrite (ident_left i, Prim.MAlloc) :: accu) instrs
    | INSTR_Load (i1, _, i2) :: instrs ->
      aux (SSA.IAssignExpr (ident_left i1, Prim.ORead (var i2)) :: accu) instrs
    | INSTR_Store (_, v, _, i) :: instrs ->
      aux (SSA.IMemWrite (ident i, Prim.MWrite (value v)) :: accu) instrs
    | INSTR_AtomicCmpXchg :: instrs -> unsupported_feature "INSTR_AtomicCmpXchg"
    | INSTR_AtomicRMW :: instrs -> unsupported_feature "INSTR_AtomicRMW"
    | INSTR_Fence :: instrs -> unsupported_feature "INSTR_Fence"
    | INSTR_GetElementPtr :: instrs -> unsupported_feature "INSTR_GetElementPtr"
    | INSTR_ExtractElement :: instrs -> unsupported_feature "INSTR_ExtractElement"
    | INSTR_InsertElement :: instrs -> unsupported_feature "INSTR_InsertElement"
    | INSTR_ShuffleVector :: instrs -> unsupported_feature "INSTR_ShuffleVector"
    | INSTR_ExtractValue :: instrs -> unsupported_feature "INSTR_ExtractValue"
    | INSTR_InsertValue :: instrs -> unsupported_feature "INSTR_InsertValue"
    | INSTR_LandingPad :: instrs -> unsupported_feature "INSTR_LandingPad"

    | (  INSTR_Ret _ | INSTR_Ret_void | INSTR_Br _ | INSTR_Br_1 _
       | INSTR_Switch _ | INSTR_IndirectBr | INSTR_Invoke _
       | INSTR_Resume _ | INSTR_Unreachable) as terminator :: []
    ->  (List.rev accu, terminator)
  in
  aux [] instrs

let get_terminator    terminator =
  let open LLVM in
  match terminator with
  | INSTR_Ret (_, i) -> SSA.Jreturn (value_expr i)
  | INSTR_Ret_void -> SSA.Jreturnvoid
  | INSTR_Br (i, l1, l2) ->
    SSA.Jcond (value_expr i, label l1, label l2)
  | INSTR_Br_1 i -> SSA.Jgoto (label i)
  | INSTR_Switch _ -> unsupported_feature "INSTR_Switch"
  | INSTR_IndirectBr _ -> unsupported_feature "INSTR_IndirectBr"
  | INSTR_Invoke (_, fn, args, i, _) ->
    let args = List.map (fun (_, v) -> value_expr v) args in
    SSA.Jtail (label fn, args, label i)
  | INSTR_Resume _ -> unsupported_feature "INSTR_Resume"
  | INSTR_Unreachable -> unsupported_feature "INSTR_Unreachable"

  |INSTR_Add _ |INSTR_FAdd |INSTR_Sub _ |INSTR_FSub |INSTR_Mul _ |INSTR_FMul
  |INSTR_UDiv _ |INSTR_SDiv _ |INSTR_FDiv |INSTR_URem _ |INSTR_SRem _
  |INSTR_FRem |INSTR_Shl _ |INSTR_LShr _ |INSTR_AShr _ |INSTR_And _
  |INSTR_Or _ |INSTR_Xor _ |INSTR_ICmp _ |INSTR_FCmp |INSTR_PHI _
  |INSTR_Call _ |INSTR_Trunc _ |INSTR_ZExt _ |INSTR_SExt _ |INSTR_FPTrunc _
  |INSTR_FPExt _ |INSTR_UIToFP _ |INSTR_SIToFP _ |INSTR_FPToUI _
  |INSTR_FPToSI _ |INSTR_IntToPtr _ |INSTR_PtrToInt _ |INSTR_BitCast _
  |INSTR_Select |INSTR_VAArg |INSTR_Alloca _ |INSTR_Load _ |INSTR_Store _
  |INSTR_AtomicCmpXchg |INSTR_AtomicRMW |INSTR_Fence |INSTR_GetElementPtr
  |INSTR_ExtractElement |INSTR_InsertElement |INSTR_ShuffleVector
  |INSTR_ExtractValue |INSTR_InsertValue |INSTR_LandingPad |INSTR_Label _
  -> assert false


let entry_block_of_instrs instrs =

  let instrs, terminator = get_assigns instrs in
  let terminator         = get_terminator    terminator   in

  SSA.Entry_blocks.entry_block ~instrs terminator


let block_of_instrs instrs =

  let get_label   instrs = match instrs with
    | LLVM.INSTR_Label l :: instrs ->
      (label l, instrs)
    | _ ->
      let lbl = "%" ^ string_of_int (get_running_idx ()) in
      (Prim.label_of_var (Prim.var lbl), instrs)

  in

  let get_phis    instrs =
    let rec aux accu = function
      | [] -> assert false
      | LLVM.INSTR_PHI (res, _, assocs) :: instrs ->
        let flip_val (x,y) = (label y, value_expr x) in
        aux ((ident_left res, List.map flip_val assocs) :: accu) instrs
      | instrs -> (List.rev accu, instrs)
    in
    aux [] instrs
  in

  let label , instrs     = get_label   instrs in
  let phis  , instrs     = get_phis    instrs in
  let instrs, terminator = get_assigns instrs in
  let terminator         = get_terminator    terminator   in

  SSA.Blocks.block ~label ~phis ~instrs terminator

let blocks_of_instrs instrs =
  let rec mk_entry_block accu = function
    | [] -> assert false
    | i :: is ->
      if is_terminator i then
        (entry_block_of_instrs (List.rev (i :: accu)), is)
      else
        mk_entry_block (i :: accu) is
  in
  let rec mk_blocks accu_blocks accu_instrs = function
    | [] ->
      assert (accu_instrs = []);
      List.rev accu_blocks
    | i :: is ->
      if is_terminator i then
        mk_blocks
          (block_of_instrs (List.rev (i :: accu_instrs))
           :: accu_blocks
          )
          []
          is
      else
        mk_blocks accu_blocks (i :: accu_instrs) is
  in
  let (entry_block, instrs) = mk_entry_block [] instrs in
  (entry_block, mk_blocks [] [] instrs)

let proc {LLVM.name; args; instrs} =
  let (p_entry_block, p_blocks) = blocks_of_instrs instrs in
  {SSA.
    p_name   = label name;
    p_args   = List.map (fun (_, a) -> ident a) args;
    p_entry_block;
    p_blocks;
  }

let tpl = function
  | LLVM.TLE_Target _
  | LLVM.TLE_Datalayout _ -> None
  | LLVM.TLE_Definition d -> Some (proc d)

let prog = Util.L.map_option tpl

