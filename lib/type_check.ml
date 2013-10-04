open Core.Std

open Ast

(* is t1 <= t2 *)
let rec is_comform ~itree t1 t2 =
  if t1 = t2 || (t1 = "Object" && t2 <> "Object") then
    false
  else
    is_comform ~itree
      (Option.value_exn (Inherit_tree.parent itree ~name:t1)) t2

let is_legal_type ~itree = function
  | None -> false
  | Some t ->
      match Inherit_tree.find itree t with
      | None   -> false
      | Some _ -> true

(* least upper bound. *)
(* assume t1 and t2 are legal types. *)
let lub ~itree t1 t2 =
  let rec type_level t =
    if t = "Object" then
      0
    else
      1 + type_level (Option.value_exn (Inherit_tree.parent itree ~name:t))
  in
  let walk_up t =
    Option.value_exn (Inherit_tree.parent itree ~name:t)
  in
  let rec walk_up_to_level target curr t =
    if curr = target then
      t
    else
      walk_up_to_level target (curr - 1) (walk_up t)
  in
  let rec walk_up2 t1 t2 =
    if t1 = t2 then
      t1
    else
      walk_up2 (walk_up t1) (walk_up t2)
  in
  let l1, l2 = (type_level t1, type_level t2) in
  if l1 = l2 then
    walk_up2 t1 t2
  else if l1 > l2 then
    walk_up2 (walk_up_to_level l2 l1 t1) t2
  else
    walk_up2 t1 (walk_up_to_level l1 l2 t2)

let rec do_expr ~scopes ~itree ~mthdenv = function
  | `Bool (_, t) ->
      t := Some "Bool"
  | `Int (_, t) ->
      t := Some "Int"
  | `String (_, t) ->
      t := Some "String"
  | `Ident ("self", t) ->
      t := Some "SELF_TYPE"
  | `Ident (id, t) ->
      (match Scopes.find scopes id with
      | None ->
	  eprintf "identifier (%s) not declared.\n" id
      | Some tid ->
	  t := tid)
  | `Assign (objid, expr, t) ->
      do_expr ~scopes ~itree ~mthdenv expr;
      (match Scopes.find scopes objid with
	| None ->
	    eprintf "identifier (%s) not declared.\n" objid;
	    t := None
	| Some None ->
	    eprintf "cannot determine type of (%s) not declared.\n" objid;
	    t := None
	| Some (Some typeid) ->
	    (match !(type_of_expr expr) with
	      | None ->
		  eprintf "cannot determine type of sub-expression.\n";
		  t := None
	      | Some typeid' ->
		  if is_comform ~itree typeid' typeid then
		    t := Some typeid'
		  else
		    (t := None;
		     eprintf "(%s) not comform (%s).\n" typeid' typeid)))
  | `Dispatch (expr, typeid, objid, exprlst, t) ->
      do_expr ~scopes ~itree ~mthdenv expr;
      List.iter exprlst ~f:(do_expr ~scopes ~itree ~mthdenv)
  | `Cond (expr1, expr2, expr3, t) ->
      List.iter [expr1; expr2; expr3]
	~f:(do_expr ~scopes ~itree ~mthdenv);
      if !(type_of_expr expr1) <> Some "Bool" then
	eprintf "conditional expression is not Bool.\n";
      if is_legal_type ~itree !(type_of_expr expr2)
	  && is_legal_type ~itree !(type_of_expr expr3) then
	()
      (* t := Some (lub ~itree *)
      (* 		   (Option.value_exn !(type_of_expr expr2)) *)
      (* 		   (Option.value_exn !(type_of_expr expr3))) *)
  | `Loop (expr1, expr2, t) ->
      List.iter [expr1; expr2] ~f:(do_expr ~scopes ~itree ~mthdenv);
      t := Some "Object"
  | `Block (exprlst, t) ->
      List.iter exprlst ~f:(do_expr ~scopes ~itree ~mthdenv)
  | `Let (objid, typeid, init, expr, t) ->
      (* Scopes.enter_new scopes; *)
      (* Scopes.add scopes (objid, typeid); *)
      (* Scopes.exit_curr scopes; *)
      do_expr ~scopes ~itree ~mthdenv expr
  | `Case (expr, lst, t) ->
      (* Scopes.enter_new scopes; *)
      (* Scopes.add scopes (objid, typeid); *)
      (* Scopes.exit_curr scopes; *)
      do_expr ~scopes ~itree ~mthdenv expr;
      List.iter lst ~f:(fun (_, _, e) -> do_expr ~scopes ~itree ~mthdenv e)
  | `New (typeid, t) ->
      t := Some typeid
  | `Isvoid (expr, t) ->
      do_expr ~scopes ~itree ~mthdenv expr;
      t := Some "Bool"
  | `Plus (expr1, expr2, t) ->
      List.iter [expr1; expr2] ~f:(do_expr ~scopes ~itree ~mthdenv);
      t := Some "Int"
  | `Minus (expr1, expr2, t) ->
      List.iter [expr1; expr2] ~f:(do_expr ~scopes ~itree ~mthdenv);
      t := Some "Int"
  | `Times (expr1, expr2, t) ->
      List.iter [expr1; expr2] ~f:(do_expr ~scopes ~itree ~mthdenv);
      t := Some "Int"
  | `Div (expr1, expr2, t) ->
      List.iter [expr1; expr2] ~f:(do_expr ~scopes ~itree ~mthdenv);
      t := Some "Int"
  | `Lt (expr1, expr2, t) ->
      List.iter [expr1; expr2] ~f:(do_expr ~scopes ~itree ~mthdenv);
      t := Some "Bool"
  | `Le (expr1, expr2, t) ->
      List.iter [expr1; expr2] ~f:(do_expr ~scopes ~itree ~mthdenv);
      t := Some "Bool"
  | `Eq (expr1, expr2, t) ->
      List.iter [expr1; expr2] ~f:(do_expr ~scopes ~itree ~mthdenv);
      t := Some "Bool"
  | `Complmnt (expr, t) ->
      do_expr ~scopes ~itree ~mthdenv expr;
      t := Some "Int"
  | `Not (expr, t) ->
      do_expr ~scopes ~itree ~mthdenv expr;
      t := Some "Bool"
  | `Paren expr ->
      do_expr ~scopes ~itree ~mthdenv expr
  | `InterExpr _ -> ()

let do_formal ~scopes (`Formal (objid, typeid)) =
  Scopes.add scopes (objid, typeid)

let do_feature ~scopes ~itree ~mthdenv  = function
  | `Attr (_, _, None) ->
      ()
  | `Attr (_, _, Some e) ->
      do_expr ~scopes ~itree ~mthdenv e
  | `Method (_, formals, _, expr) ->
      Scopes.enter_new scopes;
      List.iter formals ~f:(do_formal ~scopes);
      do_expr ~scopes ~itree ~mthdenv expr;
      Scopes.exit_curr scopes

let do_class ~itree ~mthdenv (`Class (clsname, parent, features)) =
  let scopes = Scopes.create () in
  let insert_attr scopes features =
    List.iter features ~f:(fun feature ->
      match feature with
      | `Attr (objid, typeid, _) ->
          Scopes.add scopes (objid, typeid)
      | `Method _ -> ())
  in
  Scopes.enter_new scopes;
  insert_attr scopes features;
  List.iter features ~f:(do_feature ~scopes ~itree ~mthdenv);
  Scopes.exit_curr scopes

let run classes itree mthdenv =
  List.iter classes ~f:(do_class ~itree ~mthdenv)
