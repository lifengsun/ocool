module Children : sig
  type t
  val empty : t
  val add   : t -> string -> t
  val iter  : t -> f:(string -> unit) -> unit
end

type t
type name   = string
type parent = string
type child  = string
type node   = parent * Children.t

val create : unit -> t
val find   : t -> name:name -> node option
val insert : t -> name:name -> node:node -> unit
val iter   : t -> f:(name:name -> node -> unit) -> unit
val print  : t -> unit
