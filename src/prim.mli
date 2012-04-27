type const = int

type var
val var : string -> var
val string_of_var : var -> string

val fresh_var : unit -> var

type value =
	| Vvar of var
	| Vconst of const

type expr =
	| ONone of value
	| OPlus  of (value * value)
	| OMult  of (value * value)
	| OMinus of (value * value)
	| ODiv   of (value * value)
	| OMax of (value * value)
	| OMin of (value * value)

type label
val label : string -> label
val string_of_label : label -> string

val fresh_label : unit -> label

val var_of_label : label -> var
val label_of_var : var -> label
