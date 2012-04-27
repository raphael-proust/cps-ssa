
type proc = {
	p_args  : Prim.var list;
	p_blocks: block list; (* First block is entry block. Hence it dominates
	                         non-dead blocks *)
}

and block = {
	b_label  : Prim.label;
	b_phis   : phi list;
	b_assigns: assign list;
	b_jump   : jump;
}

and assign =
	| Aexpr of (Prim.var * Prim.expr)
	| Acall of (Prim.var * Prim.expr * Prim.expr list)

and jump =
	| Jgoto of (Prim.label)
	| Jreturn of Prim.expr
	| Jtail of (Prim.expr * Prim.expr list)
	| Jcond of (Prim.expr * Prim.label * Prim.label)

and phi = Prim.var * (Prim.label * Prim.expr) list


