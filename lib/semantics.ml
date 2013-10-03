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

let check_main_class_defined tree =
  let module Itree = Inherit_tree in
  match Itree.find tree "Main" with
  | None ->
      eprintf "syntax error: Main class undefined.\n";
      exit (-1)
  | Some _ -> ()

let check_final_basis_classes tree =
  let module Itree = Inherit_tree in
  List.iter ["Int"; "String"; "Bool"] ~f:(fun c ->
    let (_, _, children) = Option.value_exn (Itree.find tree c) in
    if not (Itree.Children.is_empty children) then
      begin
	eprintf "syntax error: classes (";
	Itree.Children.iter children ~f:(eprintf " %s");
	eprintf " ) inherits from basic class ( %s ).\n" c;
	exit (-1)
      end)

let semant classes =
  let module Itree = Inherit_tree in
  let inherit_tree = create_inherit_tree
      (Basic_classes.basic_classes @ classes)
  in
  List.iter [check_main_class_defined;
	     check_final_basis_classes]
    ~f:(fun check -> check inherit_tree);
  Itree.print inherit_tree
