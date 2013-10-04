open Ast

type t

val create : unit -> t
val enter_new : t -> unit
val exit_curr : t -> unit
val add       : t -> objid * typeid -> unit
val find      : t -> objid:objid    -> (typeid option) option
