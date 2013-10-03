open Ast

module Children : sig
  type t
  val empty    : t
  val is_empty : t -> bool
  val add      : t -> string -> t
  val iter     : t -> f:(string -> unit) -> unit
end

type t
type name = string
type node = cls * Children.t

val create : unit -> t
val find   : t -> name:name -> node option
val change : t -> name:name -> f:(node option -> node option) -> unit
val insert : t -> name:name -> node:node -> unit
val iter   : t -> f:(name:name -> node -> unit) -> unit
val build  : cls list -> t
val print  : t -> unit
