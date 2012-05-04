
type m =
	| Mapp  of (Prim.var * Prim.expr list * cont)
	| Mcont of (Prim.var * Prim.expr list)
	| Mcond of (Prim.expr * (Prim.var * Prim.expr list) * (Prim.var * Prim.expr list))
	| Mlet  of (Prim.var * Prim.expr * m)
	| Mrec  of ((Prim.var * lambda) list * m)

and cont =
	| Cvar of Prim.var
	| C    of Prim.var * m

and lambda =
	| Lproc of (Prim.var list * Prim.var * m)
	| Ljump of (Prim.var list * m)
