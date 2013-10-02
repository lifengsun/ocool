type objid = string
type typeid = string

type expr = [
  | `Bool      of bool			(* 7.1 Constants *)
  | `Int       of int
  | `String    of string
  | `Ident     of objid			(* 7.2 Identifiers *)
  | `Assign    of objid * expr		(* 7.3 Assignment *)
  | `Dispatch  of expr * typeid option * objid * (expr list) (* 7.4 Dispatch *)
  | `Cond      of expr * expr * expr	(* 7.5 Conditionals *)
  | `Loop      of expr * expr		(* 7.6 Loops *)
  | `Block     of expr list		(* 7.7 Blocks *)
  | `Let       of objid * typeid * expr option * expr	(* 7.8 Let *)
  | `Case      of expr * ((objid * typeid * expr) list)	     (* 7.9 Case *)
  | `New       of typeid		(* 7.10 New *)
  | `Isvoid    of expr			(* 7.11 Isvoid *)
  | `Plus      of expr * expr  (* 7.12 Arithmetic and Comparison Operations *)
  | `Minus     of expr * expr
  | `Times     of expr * expr
  | `Div       of expr * expr
  | `Lt        of expr * expr
  | `Le        of expr * expr
  | `Eq        of expr * expr
  | `Complmnt  of expr
  | `Not       of expr
  | `Paren     of expr			(* parenthesis *)
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
