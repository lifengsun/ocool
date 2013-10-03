open Core.Std
open Cool

let create_inherit_tree classes =
  let inherit_tree = Inherit_tree.create () in
  Inherit_tree.insert inherit_tree "Object" "" Inherit_tree.Children.empty;
  List.iter ["IO"; "Int"; "String"; "Bool"] ~f:(fun c ->
    Inherit_tree.insert inherit_tree c "Object" Inherit_tree.Children.empty);
  List.iter classes ~f:(fun (`Class (name, parent, _)) ->
    match Inherit_tree.find inherit_tree ~name with
    | None ->
	Inherit_tree.insert inherit_tree name parent
	  Inherit_tree.Children.empty
    | Some _ ->
	eprintf "syntax error: class \"%s\" redefined\n" name;
	exit (-1));
  (* When a class is inserted later than its children, it cannot find
     them directly, so we traverse all classes and insert them as
     children. *)
  Inherit_tree.iter inherit_tree ~f:(fun ~name (parent, _) ->
    if name <> "Object" then	 (* Object class has no base class. *)
      match Inherit_tree.find inherit_tree parent with
      | None ->
          eprintf "syntax error: class \"%s\" inherits from undefined class \"%s\".\n" name parent;
          exit (-1)
      | Some (pparent, pchildren) ->
          Inherit_tree.insert inherit_tree ~name:parent ~parent:pparent
	    ~children:(Inherit_tree.Children.add pchildren name));
  inherit_tree

let semant classes =
  let inherit_tree = create_inherit_tree classes in
    Inherit_tree.print inherit_tree
