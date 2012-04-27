
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


(* For building trivial blocks *)
module Blocks = struct

	let expr ?(label = Prim.fresh_label ()) e = {
		b_label   = label;
		b_phis    = [];
		b_assigns = [];
		b_jump    = Jreturn e;
	}

	let const ?label c = expr ?label (Prim.ONone (Prim.Vconst c))

	let zero ?label () = const ?label 0

	let cond ?(label = Prim.fresh_label ()) e l1 l2 = {
		b_label   = label;
		b_phis    = [];
		b_assigns = [];
		b_jump    = Jcond (e, l1, l2);
	}

end

(* For building simpl procs *)
module Procs = struct

	let block args b = {
		p_args = args;
		p_blocks = [ b ];
	}

	let cond ?(label = Prim.fresh_label ()) args e b1 b2 = {
		p_args = args;
		p_blocks = [
			Blocks.cond ~label e b1.b_label b2.b_label;
			b1;
			b2;
		];
	}

	let cond_e ?(label = Prim.fresh_label ()) args e e1 e2 =
		let b1 = Blocks.expr e1 in
		let b2 = Blocks.expr e2 in
		cond ~label args e b1 b2


end

(*TODO? some ast -> ssa automatic translator?*)
