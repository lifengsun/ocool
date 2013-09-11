type objid = string
type typeid = string

type expr = [
  | `DotMethod of expr * typeid option * objid * (expr list)
  | `Method    of objid * expr list
  | `If        of expr * expr * expr
  | `While     of expr * expr
  | `Block     of expr list
  | `Let       of (objid * typeid * expr) list * expr
  | `Case      of expr * (objid * typeid * expr) list
  | `New       of typeid
  | `Isvoid    of expr
  | `Plus      of expr * expr
  | `Minus     of expr * expr
  | `Prodct    of expr * expr
  | `Divide    of expr * expr
  | `Comp      of expr
  | `Less      of expr * expr
  | `LE        of expr * expr
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
  | `Method of formal list * typeid * expr
  | `Var    of objid * typeid * expr option
  ]

type cls = [
    `Class of typeid * typeid option * feature list
  ]
