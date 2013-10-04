open Core.Std

open Ast

let clsname = ref ""
let scopes  = ref (Scopes.create ())
let mthdenv = ref (Method_env.create ())
let itree   = ref (Inherit_tree.create ())

(* is t1 <= t2 *)
let rec is_conform t1 t2 =
  let rec aux t1 t2 =
    if t1 = t2 || (t1 = "Object" && t2 <> "Object") then
      true
    else
      is_conform (Option.value_exn (Inherit_tree.parent !itree ~name:t1)) t2
  in
  aux (if t1 <> "SELF_TYPE" then t1 else !clsname)
    (if t2 <> "SELF_TYPE" then t2 else !clsname)

let is_legal_type = function
  | None -> false
  | Some "SELF_TYPE" -> true
  | Some t ->
      match Inherit_tree.find !itree t with
      | None   -> false
      | Some _ -> true

(* least upper bound. *)
(* assume t1 and t2 are legal types. *)
let lub t1 t2 =
  let rec type_level t =
    if t = "Object" then
      0
    else
      1 + type_level (Option.value_exn (Inherit_tree.parent !itree ~name:t))
  in
  let walk_up t =
    Option.value_exn (Inherit_tree.parent !itree ~name:t)
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
  if t1 = "SELF_TYPE" && t2 = "SELF_TYPE" then
    "SELF_TYPE"
  else
    let typ1, typ2 = ((if t1 <> "SELF_TYPE" then t1 else !clsname),
		      (if t2 <> "SELF_TYPE" then t2 else !clsname))
    in
    let l1, l2 = (type_level typ1, type_level typ2) in
    if l1 = l2 then
      walk_up2 typ1 typ2
    else if l1 > l2 then
      walk_up2 (walk_up_to_level l2 l1 typ1) typ2
    else
      walk_up2 typ1 (walk_up_to_level l1 l2 typ2)

let rec do_expr = function
  | `Bool (_, t) ->
      t := Some "Bool"
  | `Int (_, t) ->
      t := Some "Int"
  | `String (_, t) ->
      t := Some "String"
  | `Ident ("self", t) ->
      t := Some "SELF_TYPE"
  | `Ident (id, t) ->
      (match Scopes.find !scopes id with
      | None ->
	  eprintf "identifier (%s) not declared.\n" id;
	  t := Some "Object"
      | Some tid ->
	  t := tid)
  | `Assign (objid, expr, t) ->
      do_expr expr;
      (match Scopes.find !scopes objid with
	| None ->
	    eprintf "identifier (%s) not declared.\n" objid;
	    t := Some "Object"
	| Some None ->
	    eprintf "cannot determine type of (%s).\n" objid;
	    t := Some "Object"
	| Some (Some typeid) ->
	    (match type_of_expr expr with
	      | None ->
		  eprintf "cannot determine type of init expression in assignment.\n";
		  t := Some "Object"
	      | Some typeid' ->
		  if is_conform typeid' typeid then
		    t := Some typeid'
		  else
		    (eprintf "(%s) not conform (%s).\n" typeid' typeid;
		     t := Some "Object")))
(*TODO: combine two Dispatches.*)
  | `Dispatch (expr, None, objid, exprlst, tn1) ->
      do_expr expr;
      List.iter exprlst ~f:do_expr;
      if is_legal_type (type_of_expr expr) then
	let t0 = Option.value_exn (type_of_expr expr) in
	(match Method_env.find !mthdenv !itree
	    ~name:((if t0 <> "SELF_TYPE" then t0 else !clsname), objid) with
	| Some (params, ret) ->
	    if List.length params <> List.length exprlst then
	      eprintf "argument parameter length not match.\n"
	    else
	      if List.exists2_exn exprlst params ~f:(fun e ti' ->
		let ti = Option.value_exn (type_of_expr e) in (*FIXME*)
		not (is_conform ti ti')) then
		eprintf "argument types not conform.\n";
	    if ret <> "SELF_TYPE" then
	      tn1 := Some ret
	    else
	      tn1 := Some t0
	| None ->
	    eprintf "method (%s.%s) undefined.\n" !clsname objid;
	    tn1 := Some "Object")
  | `Dispatch (expr, Some t, objid, exprlst, tn1) ->
      do_expr expr;
      List.iter exprlst ~f:do_expr;
      if is_legal_type (type_of_expr expr) && is_legal_type (Some t) then
	let t0 = Option.value_exn (type_of_expr expr) in
	if not (is_conform t0 t) then
	  eprintf "type (%s) not conform (%s).\n" t0 t;
	(match Method_env.find !mthdenv !itree
	    ~name:((if t <> "SELF_TYPE" then t else !clsname), objid) with
	| Some (params, ret) ->
	    if List.length params <> List.length exprlst then
	      eprintf "argument parameter length not match.\n"
	    else
	      if List.exists2_exn exprlst params ~f:(fun e ti' ->
		let ti = Option.value_exn (type_of_expr e) in (*FIXME*)
		not (is_conform ti ti')) then
		eprintf "argument types not conform.\n";
	    if ret <> "SELF_TYPE" then
	      tn1 := Some ret
	    else
	      tn1 := Some t0
	| None ->
	    eprintf "method (%s.%s) undefined.\n" !clsname objid;
	    tn1 := Some "Object")
  | `Cond (expr1, expr2, expr3, t) ->
      List.iter [expr1; expr2; expr3]
	~f:do_expr;
      if type_of_expr expr1 <> Some "Bool" then
	eprintf "conditional expression in (if then else fi) is not Bool.\n";
      let t2, t3 = type_of_expr expr2, type_of_expr expr3 in
      if is_legal_type t2 && is_legal_type t3 then
	t := Some (lub (Option.value_exn t2) (Option.value_exn t3))
      else
	(eprintf "cannot determine types of conditional branches.\n";
	 t := Some "Object")
  | `Loop (expr1, expr2, t) ->
      List.iter [expr1; expr2] ~f:do_expr;
      if type_of_expr expr1 <> Some "Bool" then
	eprintf "conditional expression in (while loop pool) is not Bool.\n";
      if type_of_expr expr2 = None then
	eprintf "cannot determine type of loop body.\n";
      t := Some "Object"
  | `Block (exprlst, t) ->
      List.iter exprlst ~f:do_expr;
      t := type_of_expr (List.last_exn exprlst)
  | `Let (objid, typeid, init, expr, t) ->
      Scopes.enter_new !scopes;
      (match init with
      | None ->
	  Scopes.add !scopes (objid, typeid)
      | Some e ->
	  do_expr e;
	  if is_legal_type (Some typeid)
	      && is_legal_type (type_of_expr e) then
	    let t' = Option.value_exn (type_of_expr e) in
	    if is_conform t' typeid then
	       Scopes.add !scopes (objid, typeid)
	    else
	      (eprintf "(%s) not conform to (%s).\n" t' typeid;
	       Scopes.add !scopes (objid, "Object"));
	  else
	    (eprintf "illegal types ( %s ) in LET bindings.\n" typeid;
	     Scopes.add !scopes (objid, "Object")));
      do_expr expr;
      Scopes.exit_curr !scopes;
      t := type_of_expr expr
  | `Case (expr, lst, t) ->
      do_expr expr;
      List.iter lst ~f:(fun (objid, typeid, e) ->
	Scopes.enter_new !scopes;
	Scopes.add !scopes (objid, typeid);
	do_expr e;
	Scopes.exit_curr !scopes);
      let t1 =
	let _, _, e = List.hd_exn lst in
	type_of_expr e
      in
      t := Some (List.fold_left (List.tl_exn lst)
		   ~init:(Option.value_exn t1) ~f:(fun ti (_, _, e) ->
		     lub ti (Option.value_exn (type_of_expr e))))
  | `New (typeid, t) ->
      t := Some typeid
  | `Isvoid (expr, t) ->
      do_expr expr;
      t := Some "Bool"
  | `Plus (expr1, expr2, t) ->
      List.iter [expr1; expr2] ~f:do_expr;
      if type_of_expr expr1 <> Some "Int"
	  or type_of_expr expr2 <> Some "Int" then
	eprintf "operand of (+) is not Int.\n";
      t := Some "Int"
  | `Minus (expr1, expr2, t) ->
      List.iter [expr1; expr2] ~f:do_expr;
      if type_of_expr expr1 <> Some "Int"
	  or type_of_expr expr2 <> Some "Int" then
	eprintf "operand of (-) is not Int.\n";
      t := Some "Int"
  | `Times (expr1, expr2, t) ->
      List.iter [expr1; expr2] ~f:do_expr;
      if type_of_expr expr1 <> Some "Int"
	  or type_of_expr expr2 <> Some "Int" then
	eprintf "operand of ( * ) is not Int.\n";
      t := Some "Int"
  | `Div (expr1, expr2, t) ->
      List.iter [expr1; expr2] ~f:do_expr;
      if type_of_expr expr1 <> Some "Int"
	  or type_of_expr expr2 <> Some "Int" then
	eprintf "operand of (/) is not Int.\n";
      t := Some "Int"
  | `Lt (expr1, expr2, t) ->
      List.iter [expr1; expr2] ~f:do_expr;
      if type_of_expr expr1 <> Some "Int"
	  or type_of_expr expr2 <> Some "Int" then
	eprintf "operand of (<) is not Int.\n";
      t := Some "Bool"
  | `Le (expr1, expr2, t) ->
      List.iter [expr1; expr2] ~f:do_expr;
      if type_of_expr expr1 <> Some "Int"
	  or type_of_expr expr2 <> Some "Int" then
	eprintf "operand of (<=) is not Int.\n";
      t := Some "Bool"
  | `Eq (expr1, expr2, t) ->
      List.iter [expr1; expr2] ~f:do_expr;
      let t1, t2 = type_of_expr expr1, type_of_expr expr2 in
      if is_legal_type t1 && is_legal_type t2 && List.exists [t1; t2]
	  ~f:(fun e ->
	    List.mem ["Int"; "String"; "Bool"] (Option.value_exn e))
	  && t1 <> t2 then
        eprintf "operand of (=) in (Int, String, Bool) with different types.\n";
      t := Some "Bool"
  | `Complmnt (expr, t) ->
      do_expr expr;
      if type_of_expr expr <> Some "Int" then
	eprintf "operand of (~) is not Int.\n";
      t := Some "Int"
  | `Not (expr, t) ->
      do_expr expr;
      if type_of_expr expr <> Some "Bool" then
	eprintf "operand of (not) is not Bool.\n";
      t := Some "Bool"
  | `Paren expr ->
      do_expr expr
  | `InterExpr _ -> ()

let do_formal (`Formal (objid, typeid)) =
  Scopes.add !scopes (objid, typeid)

let do_feature = function
  | `Attr (_, _, None) ->
      ()
  | `Attr (objid, typeid, Some expr) ->
      do_expr expr;
      if not ((is_legal_type (type_of_expr expr))
                && is_conform
		(Option.value_exn (type_of_expr expr)) typeid) then
        eprintf "attribute (%s.%s) initialize type not conform.\n"
	  !clsname objid
  | `Method (objid, formals, typeid, expr) ->
      Scopes.enter_new !scopes;
      List.iter formals ~f:do_formal;
      do_expr expr;
      if not ((is_legal_type (type_of_expr expr))
                && is_conform
                (Option.value_exn (type_of_expr expr)) typeid) then
        eprintf "method (%s.%s) return type not conform.\n" !clsname objid;
      Scopes.exit_curr !scopes

let rec insert_attr clsname =
  if clsname <> "Object" then
    insert_attr
      (Option.value_exn (Inherit_tree.parent !itree ~name:clsname));
  let `Class (_, _ , features), _ =
    Option.value_exn (Inherit_tree.find !itree ~name:clsname)
  in
  Scopes.enter_new !scopes;
  List.iter features ~f:(fun feature ->
    match feature with
    | `Attr (objid, typeid, _) ->
        Scopes.add !scopes (objid, typeid)
    | `Method _ -> ())

let do_class (`Class (cls, parent, features)) =
  clsname := cls;
  Scopes.enter_new !scopes;
  Scopes.add !scopes ("self", "SELF_TYPE");
  insert_attr !clsname;
  List.iter features ~f:do_feature

let run classes inherit_tree method_env =
  itree   := inherit_tree;
  mthdenv := method_env;
  scopes  := Scopes.create ();
  List.iter classes ~f:do_class
