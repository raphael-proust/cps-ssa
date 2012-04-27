
let return = Prim.var "return"

let block_of_label b = failwith "TODO"
let immediate_dominatees b = failwith "TODO"

let rec block ({SSA.b_label; b_phis; b_assigns; b_jump;} as b) =

	let args_of_label l =
		List.map
			(fun (_, p) -> List.assoc b_label p)
			((block_of_label l).SSA.b_phis)
	in

	let rec aux = function
		| SSA.Aexpr (x, e)     :: l -> CPS.Mlet (x, e,  aux l)
		| SSA.Acall (x, e, es) :: l -> CPS.Mapp (e, es, CPS.C (x, aux l))
		| [] -> match b_jump with
			| SSA.Jgoto l ->
					CPS.Mcont ((Prim.var_of_label l), (args_of_label l))
			| SSA.Jreturn e ->
					CPS.Mcont (return, [e])
			| SSA.Jtail (e, es) ->
					CPS.Mapp (e, es, CPS.Cvar return)
			| SSA.Jcond (c, l1, l2) ->
					CPS.Mcond (c,
						(Prim.var_of_label l1, (args_of_label l1)),
						(Prim.var_of_label l2, (args_of_label l2))
					)
	in

	match immediate_dominatees b with
	| [] -> aux b_assigns
	| l  ->
		let l =
			List.map
				(fun b ->
					let vs = List.map fst b.SSA.b_phis in
					(Prim.var_of_label b.SSA.b_label,
					 CPS.Ljump (vs, block b) (*terminates bc dominator tree is a DAG*)
					)
				)
				l
		in
		CPS.Mrec (l, aux b_assigns)



and proc {SSA.p_args; p_blocks;} cont =
	match p_blocks with
	| [] -> failwith "Can't translate empty ssa procedure into cps"
	| h::_ -> CPS.Lproc (p_args, cont, block h)


