(* This module handle primitives: values that are used for several IR.
 * These parts need not translating. *)


(* IRs only uses integer constants. *)
type const = int

(* Variable type is abstracted in the interface file. *)
type var = string

(* creation (note the prefixing) and conversion. *)
let var s = ("v:" ^ s : var)
external string_of_var : var -> string = "%identity"

(* whoohoo indexes! *)
let fresh_var =
  let c = ref 0 in
  fun () ->
    let r = "v:_" ^ string_of_int !c in
    incr c;
    r

(* things that are used in expressions. *)
type value =
  | Vvar of var
  | Vconst of const

(* expressions are operation (or not, see ONone) on values. Might be completed
 * later. It might also need to be lifted to a value lattice. *)
type expr =
  | ONone  of value
  | OPlus  of (value * value)
  | OMult  of (value * value)
  | OMinus of (value * value)
  | ODiv   of (value * value)
  | OMax   of (value * value)
  | OMin   of (value * value)
  (* etc. *)

(* labels are for jump. This is somehow specific to ssa and might get moved in
 * the corresponding module. Type is abstracted in the interface. *)
type label = string

let label s = ("l:" ^ s : label)
external string_of_label : label -> string = "%identity"

let fresh_label =
  let c = ref 0 in
  fun () ->
    let r = "l:_" ^ string_of_int !c in
    incr c;
    r

(* Standard conversion for when one translates labels into closure variables. *)
let var_of_label l = var l
let label_of_var v = label v
