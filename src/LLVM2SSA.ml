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


let is_terminator i =
  let open LLVM in
  match i with
  (* Thoses are terminators *)
  | INSTR_Terminator _ -> true

  (* Those are not *)
  |INSTR_Assign _ |INSTR_PHI _ |INSTR_Call _ |INSTR_Call_unit _ |INSTR_Select _
  |INSTR_VAArg |INSTR_Mem _ |INSTR_AtomicCmpXchg
  |INSTR_AtomicRMW |INSTR_Fence |INSTR_LandingPad |INSTR_Label _
  -> false

let ident_string = function
  | LLVM.ID_Global v -> "@" ^ v
  | LLVM.ID_Local  v -> "%" ^ v

let ident i = Prim.var (ident_string i)

let label l = Prim.label (ident_string l)

let var i = Prim.VVar (ident i)

let int_of_bool = function
  | true -> 1
  | false -> 0

let rec expr e =
  let open LLVM in
  match e with
  | EXPR_Add (_, v0, v1) -> Prim.VPlus (value v0, value v1)
  | EXPR_FAdd -> unsupported_feature "EXPR_FAdd"
  | EXPR_Sub (_, v0, v1) -> Prim.VMinus (value v0, value v1)
  | EXPR_FSub -> unsupported_feature "EXPR_FSub"
  | EXPR_Mul (_, v0, v1) -> Prim.VMult (value v0, value v1)
  | EXPR_FMul -> unsupported_feature "EXPR_FMul"
  | EXPR_UDiv (_, v0, v1)
  | EXPR_SDiv (_, v0, v1) -> Prim.VDiv (value v0, value v1)
  | EXPR_FDiv -> unsupported_feature "EXPR_FDiv"
  | EXPR_URem (_, v0, v1)
  | EXPR_SRem (_, v0, v1) -> Prim.VRem (value v0, value v1)
  | EXPR_FRem -> unsupported_feature "EXPR_FRem"

  | EXPR_Shl  (_, v0, v1) -> Prim.VShl (value v0, value v1)
  | EXPR_LShr (_, v0, v1) -> Prim.VLShr (value v0, value v1)
  | EXPR_AShr (_, v0, v1) -> Prim.VAShr (value v0, value v1)

  | EXPR_And (_, v0, v1) -> Prim.VAnd (value v0, value v1)
  | EXPR_Or (_, v0, v1) -> Prim.VOr (value v0, value v1)
  | EXPR_Xor (_, v0, v1) -> Prim.VXor (value v0, value v1)

  | EXPR_ICmp (icmp, _, v1, v2) -> begin match icmp with
    | Cmp_Eq  -> Prim.VEq (value v1, value v2)
    | Cmp_Ne  -> Prim.VNe (value v1, value v2)
    | Cmp_Ugt
    | Cmp_Sgt -> Prim.VGt (value v1, value v2)
    | Cmp_Uge
    | Cmp_Sge -> Prim.VGe (value v1, value v2)
    | Cmp_Ult
    | Cmp_Slt -> Prim.VLt (value v1, value v2)
    | Cmp_Ule
    | Cmp_Sle -> Prim.VLe (value v1, value v2)
  end
  | EXPR_FCmp -> unsupported_feature "EXPR_FCmp"

  | EXPR_Trunc    (_, v, _)
  | EXPR_ZExt     (_, v, _)
  | EXPR_SExt     (_, v, _)
  | EXPR_FPTrunc  (_, v, _)
  | EXPR_FPExt    (_, v, _)
  | EXPR_UIToFP   (_, v, _)
  | EXPR_SIToFP   (_, v, _)
  | EXPR_FPToUI   (_, v, _)
  | EXPR_FPToSI   (_, v, _)
  | EXPR_IntToPtr (_, v, _)
  | EXPR_PtrToInt (_, v, _)
  | EXPR_BitCast  (_, v, _) ->
    Prim.VCast (value v)

  | EXPR_GetElementPtr _ -> Prim.VDummy "GetElementPtr"
  | EXPR_ExtractElement  -> unsupported_feature "EXPR_ExtractElement"
  | EXPR_InsertElement   -> unsupported_feature "EXPR_InsertElement"
  | EXPR_ShuffleVector   -> unsupported_feature "EXPR_ShuffleVector"
  | EXPR_ExtractValue    -> unsupported_feature "EXPR_ExtractValue"
  | EXPR_InsertValue     -> unsupported_feature "EXPR_InsertValue"


and value = function
  | LLVM.VALUE_Ident i          -> var i
  | LLVM.VALUE_Integer i        -> Prim.VConst i
  | LLVM.VALUE_Float _          -> unsupported_feature "VALUE_Float"
  | LLVM.VALUE_Bool b           -> Prim.VConst (int_of_bool b)
  | LLVM.VALUE_Null             -> Prim.VNull
  | LLVM.VALUE_Undef            -> Prim.VUndef
  | LLVM.VALUE_Vector tvs
  | LLVM.VALUE_Array tvs
  | LLVM.VALUE_Packed_struct tvs
  | LLVM.VALUE_Struct tvs       ->
    Prim.VStruct (List.map (fun (_, v) -> value v) tvs)
  | LLVM.VALUE_Zero_initializer -> Prim.VZero
  | LLVM.VALUE_Expr e           -> expr e


let running_idx = ref (-1)
let reset_running_idx () = running_idx := (-1)
let update_runnig_idx i =
  incr running_idx;
  assert (i = !running_idx)
let get_running_idx () =
  incr running_idx;
  !running_idx


(* ident_left is for automatic tracking of running anonymous variable index *)
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

let get_assigns instrs =
  let open LLVM in
  let aux_call (_, fn, args) =
    (label fn, List.map (fun (_, v) -> value v) args)
  in
  let rec aux accu = function
    (* Those forms are illegal *)
    | []
    | (INSTR_PHI _) :: _
    | (INSTR_Terminator _) :: _ :: _
    | (INSTR_Label _) :: _
    -> assert false

    (* Those are what we are looking for *)
    | INSTR_Assign (i, e) :: instrs ->
      aux (SSA.IAssignExpr (ident_left i, expr e) :: accu) instrs

    | INSTR_Call (i, call) :: instrs ->
      aux (SSA.IAssignCall (ident_left i, aux_call call) :: accu) instrs

    | INSTR_Call_unit call :: instrs ->
      aux (SSA.ICall (aux_call call) :: accu) instrs

    | INSTR_Select (i, _, v, _, v1, v2) :: instrs ->
      (* It is difficult to compile to several blocks because we are working
       * "inside" the block. Moreover that would blur the effects of the
       * optimisations. *)
      let vv  = value v  in
      let vv1 = value v1 in
      let vv2 = value v2 in
      aux (SSA.IAssignSelect (ident_left i, vv, vv1, vv2) :: accu) instrs

    | INSTR_VAArg :: _ -> unsupported_feature "INSTR_VAArg"

    | INSTR_Mem (MEM_Alloca (i, _, _)) :: instrs ->
      aux (SSA.IMemWrite (ident_left i, Prim.MAlloc) :: accu) instrs
    | INSTR_Mem (MEM_Load (i, _, v)) :: instrs ->
      aux
        (SSA.IAssignExpr (ident_left i, Prim.VRead (value v)) :: accu)
        instrs
    | INSTR_Mem (MEM_Store (_, v, _, i)) :: instrs ->
      aux (SSA.IMemWrite (ident i, Prim.MWrite (value v)) :: accu) instrs

    | INSTR_AtomicCmpXchg :: _  -> unsupported_feature "INSTR_AtomicCmpXchg"
    | INSTR_AtomicRMW :: _      -> unsupported_feature "INSTR_AtomicRMW"
    | INSTR_Fence :: _          -> unsupported_feature "INSTR_Fence"
    | INSTR_LandingPad :: _     -> unsupported_feature "INSTR_LandingPad"

    (* Those indicate the end of the instruction section and arrival of the terminator *)
    | INSTR_Terminator
      (( TERM_Ret _ | TERM_Ret_void | TERM_Br _ | TERM_Br_1 _ | TERM_Switch _
       | TERM_IndirectBr | TERM_Invoke _ | TERM_Resume _ | TERM_Unreachable
       ) as terminator) :: []
    ->  (List.rev accu, terminator)
  in
  aux [] instrs

let get_terminator terminator =
  let open LLVM in
  match terminator with
  | TERM_Ret (_, i) -> SSA.JReturn (value i)
  | TERM_Ret_void -> SSA.JReturnVoid
  | TERM_Br (i, l1, l2) ->
    SSA.JCond (value i, label l1, label l2)
  | TERM_Br_1 i -> SSA.JGoto (label i)
  | TERM_Switch _ -> unsupported_feature "TERM_Switch"
  | TERM_IndirectBr _ -> unsupported_feature "TERM_IndirectBr"
  | TERM_Invoke (_, fn, args, i, _) ->
    let args = List.map (fun (_, v) -> value v) args in
    SSA.JTail (label fn, args, label i)
  | TERM_Resume _ -> unsupported_feature "TERM_Resume"
  | TERM_Unreachable -> unsupported_feature "TERM_Unreachable"

let get_label = function
  | LLVM.INSTR_Label l :: instrs -> (label l, instrs)
  | instrs -> (Prim.label ("%" ^ string_of_int (get_running_idx ())), instrs)

let get_phis instrs =
  let rec aux accu = function
    | [] -> assert false
    | LLVM.INSTR_PHI (res, _, assocs) :: instrs ->
      let flip_val (x,y) = (label y, value x) in
      aux ((ident_left res, List.map flip_val assocs) :: accu) instrs
    | instrs -> (List.rev accu, instrs)
  in
  aux [] instrs


let entry_block_of_instrs instrs =
  let label , instrs     = get_label      instrs     in
  let instrs, terminator = get_assigns    instrs     in
  let terminator         = get_terminator terminator in
  SSA.Entry_blocks.entry_block ~label ~instrs terminator


let block_of_instrs instrs =
  let label , instrs     = get_label      instrs     in
  let phis  , instrs     = get_phis       instrs     in
  let instrs, terminator = get_assigns    instrs     in
  let terminator         = get_terminator terminator in
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
          (block_of_instrs (List.rev (i :: accu_instrs)) :: accu_blocks)
          []
          is
      else
        mk_blocks accu_blocks (i :: accu_instrs) is
  in
  let (entry_block, instrs) = mk_entry_block [] instrs in
  (entry_block, mk_blocks [] [] instrs)

let proc {LLVM.df_name; df_args; df_instrs} =
  reset_running_idx ();
  let (p_entry_block, p_blocks) = blocks_of_instrs df_instrs in
  {SSA.
    p_name = label df_name;
    p_args = List.map (fun (_, a) -> ident a) df_args;
    p_entry_block;
    p_blocks;
  }

let tpl = function
  (* ignore llvm boring stuff, get to the meat *)
  | LLVM.TLE_Target _
  | LLVM.TLE_Datalayout _
  | LLVM.TLE_Declaration _
  | LLVM.TLE_Type_decl _
  | LLVM.TLE_Global _ -> None
  | LLVM.TLE_Definition d -> Some (proc d)

let module_ m =
  Prim.reset_idxs ();
  Util.L.map_option tpl m

