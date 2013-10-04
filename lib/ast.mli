type objid = string
type typeid = string

type expr = [
  | `Bool      of bool * (typeid option ref)	(* 7.1 Constants *)
  | `Int       of int * (typeid option ref)
  | `String    of string * (typeid option ref)
  | `Ident     of objid * (typeid option ref)	(* 7.2 Identifiers *)
  | `Assign    of objid * expr * (typeid option ref)	(* 7.3 Assignment *)
  | `Dispatch  of expr * typeid option * objid
	* (expr list) * (typeid option ref)	(* 7.4 Dispatch *)
  | `Cond      of expr * expr * expr * (typeid option ref) (* 7.5 Conditionals *)
  | `Loop      of expr * expr * (typeid option ref)	    (* 7.6 Loops *)
  | `Block     of expr list * (typeid option ref)	    (* 7.7 Blocks *)
  | `Let       of objid * typeid * expr option
	* expr * (typeid option ref)				(* 7.8 Let *)
  | `Case      of expr * ((objid * typeid * expr) list)
	* (typeid option ref)			(* 7.9 Case *)
  | `New       of typeid * (typeid option ref)	(* 7.10 New *)
  | `Isvoid    of expr * (typeid option ref)	(* 7.11 Isvoid *)
  | `Plus      of expr * expr
	* (typeid option ref) (* 7.12 Arithmetic and Comparison Operations *)
  | `Minus     of expr * expr * (typeid option ref)
  | `Times     of expr * expr * (typeid option ref)
  | `Div       of expr * expr * (typeid option ref)
  | `Lt        of expr * expr * (typeid option ref)
  | `Le        of expr * expr * (typeid option ref)
  | `Eq        of expr * expr * (typeid option ref)
  | `Complmnt  of expr * (typeid option ref)
  | `Not       of expr * (typeid option ref)
  | `Paren     of expr			(* parenthesis *)
  | `InterExpr of objid * (typeid option ref) (* internal expression *)
  ]

type formal = [
    `Formal of objid * typeid
  ]

type feature = [
  | `Method of objid * formal list * typeid * expr
  | `Attr   of objid * typeid * expr option
  ]

type cls = [
    `Class of typeid * typeid * feature list
  ]

val type_of_expr : expr -> typeid option
val print_expr   : int -> expr -> unit
val print        : string -> cls list -> unit
