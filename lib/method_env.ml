open Core.Std
open Ast

type t = (typeid * objid, typeid list * typeid) Hashtbl.t

type name       = typeid * objid
type param_type = typeid
type retn_type  = typeid
type signt      = param_type list * retn_type

(* TODO: avoid polymorphic compare. *)
let create () = Hashtbl.create ~hashable:Hashtbl.Poly.hashable ()

let insert table ~name:(clsname, mthdname) ~signt:(paramtype, rtntype) =
  Hashtbl.replace table ~key:(clsname, mthdname) ~data:(paramtype, rtntype)

let find table ~name:(clsname, mthdname) =
  Hashtbl.find table (clsname, mthdname)

let iter table ~f =
  let g ~key:(clsname, mthdname) ~data:(param_types, retn_type) =
    f ~name:(clsname, mthdname) ~signt:(param_types, retn_type)
  in
  Hashtbl.iter table ~f:g
