
let return = Prim.var "return"

let block_of_label bs l =
  List.find (fun b -> b.SSA.b_label = l) bs

let immediate_dominatees bs b =
  (* /!\ WARNING /!\ temp version causes infinite loops on non-DAG graphs /!\ *)
  (* /!\ WARNING /!\ really! *will* cause loops in the translation! *)

  match b.SSA.b_jump with
  | SSA.Jgoto l -> [block_of_label bs l]
  | SSA.Jreturn _ | SSA.Jtail _ -> []
  | SSA.Jcond (_, l1, l2) -> [block_of_label bs l1; block_of_label bs l2]


let rec block bs ({SSA.b_label; b_phis; b_assigns; b_jump;} as b) =

  let args_of_label l =
    List.map
      (fun (_, p) -> List.assoc b_label p)
      ((block_of_label bs l).SSA.b_phis)
  in

  let rec aux = function
    | SSA.Aexpr (x, e)     :: l -> CPS.Mlet (x, e,  aux l)
    | SSA.Acall (x, f, es) :: l -> CPS.Mapp (f, es, CPS.C (x, aux l))
    | [] -> match b_jump with
      | SSA.Jgoto l ->
          CPS.Mcont ((Prim.var_of_label l), (args_of_label l))
      | SSA.Jreturn e ->
          CPS.Mcont (return, [e])
      | SSA.Jtail (v, es) ->
          CPS.Mapp (v, es, CPS.Cvar return)
      | SSA.Jcond (c, l1, l2) ->
          CPS.Mcond (c,
            (Prim.var_of_label l1, (args_of_label l1)),
            (Prim.var_of_label l2, (args_of_label l2))
          )
  in

  match immediate_dominatees bs b with
  | [] -> aux b_assigns
  | l  ->
    let l =
      List.map
        (fun b ->
          let vs = List.map fst b.SSA.b_phis in
          (Prim.var_of_label b.SSA.b_label,
           CPS.Ljump (vs, block bs b) (*terminates bc dominator tree is a DAG*)
          )
        )
        l
    in
    CPS.Mrec (l, aux b_assigns)



and proc {SSA.p_args; p_blocks;} cont =
  match p_blocks with
  | [] -> failwith "Can't translate empty ssa procedure into cps"
  | h::_ -> CPS.Lproc (p_args, cont, block p_blocks h)


