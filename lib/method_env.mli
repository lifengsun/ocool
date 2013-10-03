open Ast

type t
type name       = typeid * objid
type param_type = typeid
type retn_type  = typeid
type signt      = param_type list * retn_type

val create : unit -> t
val insert : t -> name:name -> signt:signt -> unit
val find   : t -> name:name -> signt option
val iter   : t -> f:(name:name -> signt:signt -> unit) -> unit
val build  : cls list -> t
val print  : t -> unit
