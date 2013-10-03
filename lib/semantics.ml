open Core.Std
open Cool

let insert_children tree =
  let module Itree = Inherit_tree in
  Itree.iter tree ~f:(fun ~name (parent, _) ->
    if name <> "Object" then	 (* Object class has no base class. *)
      match Itree.find tree parent with
      | None ->
          eprintf
	    "syntax error: class \"%s\" inherits from undefined class \"%s\".\n"
	    name parent;
          exit (-1)
      | Some (pparent, pchildren) ->
          Itree.insert tree parent
	    (pparent, Itree.Children.add pchildren name))

let create_inherit_tree classes =
  let module Itree = Inherit_tree in
  let inherit_tree = Itree.create () in
  List.iter classes ~f:(fun (`Class (name, parent, _)) ->
    match Itree.find inherit_tree ~name with
    | None ->
	Itree.insert inherit_tree name (parent, Itree.Children.empty)
    | Some _ ->
	eprintf "syntax error: class \"%s\" redefined\n" name;
	exit (-1));
  insert_children inherit_tree;
  inherit_tree

let semant classes =
  let module Itree = Inherit_tree in
  let inherit_tree = create_inherit_tree
      (Basic_classes.basic_classes @ classes)
  in
  Itree.print inherit_tree
