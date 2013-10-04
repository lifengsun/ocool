open Core.Std
open Ast

module Scope = struct
  type t = (objid, typeid) Hashtbl.t
  let create () = Hashtbl.create ~hashable:String.hashable ()
  let add  scope ~objid ~typeid =
    Hashtbl.replace scope ~key:objid ~data:typeid
  let find scope ~objid =
    Hashtbl.find scope objid
end

type t = Scope.t list ref

let create () = ref []

let enter_new scopes =
  scopes := (Scope.create ()) :: !scopes

let exit_curr scopes =
  scopes := List.tl_exn !scopes

let add scopes (objid, typeid) =
  let curr = List.hd_exn !scopes in
  Scope.add curr ~objid ~typeid;
  scopes := curr :: List.tl_exn !scopes

let find scopes ~objid =
  let ans = ref None in
  if List.exists !scopes ~f:(fun scope ->
    match Scope.find scope objid with
    | Some _ as a ->
	ans := a;
	true
    | None -> false) then
    Some !ans
  else
    None
