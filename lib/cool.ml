type objid = string
type typeid = string

type expr = [
  | `Assign    of objid * expr
  | `DotMethod of expr * typeid option * objid * (expr list)
  | `Func      of objid * expr list
  | `If        of expr * expr * expr
  | `While     of expr * expr
  | `Block     of expr list
  | `Let       of (objid * typeid * expr option) list * expr
  | `Case      of expr * ((objid * typeid * expr) list)
  | `New       of typeid
  | `Isvoid    of expr
  | `Plus      of expr * expr
  | `Minus     of expr * expr
  | `Times     of expr * expr
  | `Div       of expr * expr
  | `Comm      of expr
  | `Lt        of expr * expr
  | `Le        of expr * expr
  | `Eq        of expr * expr
  | `Not       of expr
  | `PExpr     of expr
  | `Object    of objid
  | `Int       of int
  | `String    of string
  | `Bool      of bool
  ]

type formal = [
    `Formal of objid * typeid
  ]

type feature = [
  | `Method of objid * formal list * typeid * expr
  | `Attr   of objid * typeid * expr option
  ]

type cls = [
    `Class of typeid * typeid option * feature list
  ]
