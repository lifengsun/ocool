open Core.Std

let insert_children tree =
  let module Itree = Inherit_tree in
  Itree.iter tree ~f:(fun ~name (`Class (_, parent, _), _) ->
    if name <> "Object" then	 (* Object class has no base class. *)
      Itree.change tree ~name:parent ~f:(fun v -> match v with
      | None ->
          eprintf "syntax error: class \"%s\" inherits from undefined class \"%s\".\n" name parent;
          exit (-1)
      | Some (pcls, pchildren) ->
	  Some (pcls, Itree.Children.add pchildren name)))

let create_inherit_tree classes =
  let module Itree = Inherit_tree in
  let inherit_tree = Itree.create () in
  List.iter classes ~f:(fun ((`Class (name, parent, _)) as cls) ->
    match Itree.find inherit_tree ~name with
    | None ->
	Itree.insert inherit_tree name (cls, Itree.Children.empty)
    | Some _ ->
	eprintf "syntax error: class \"%s\" redefined\n" name;
	exit (-1));
  insert_children inherit_tree;
  inherit_tree

let check_main_class_defined tree =
  let check_main_method features =
    if not (List.exists features ~f:(function
      | `Method (objid, formals, _, _) ->
	  if objid = "main" && formals <> [] then
	    begin
	      eprintf "syntax error: main method in Main class has formals.\n";
	      exit (-1)
	    end;
	  objid = "main"
      | `Attr _ -> false)) then
      begin
	eprintf "syntax error: main method in Main class undefined.\n";
	exit (-1)
      end
  in
  match Inherit_tree.find tree "Main" with
  | None ->
      eprintf "syntax error: Main class undefined.\n";
      exit (-1)
  | Some (`Class (_, _, features), _)  ->
      check_main_method features

let check_final_basis_classes tree =
  let module Itree = Inherit_tree in
  List.iter ["Int"; "String"; "Bool"] ~f:(fun c ->
    let (_, children) = Option.value_exn (Itree.find tree c) in
    if not (Itree.Children.is_empty children) then
      begin
	eprintf "syntax error: classes (";
	Itree.Children.iter children ~f:(eprintf " %s");
	eprintf " ) inherits from basic class ( %s ).\n" c;
	exit (-1)
      end)

let check_acycle tree =
  let module Itree = Inherit_tree in
  let visited = String.Table.create () in
  Itree.iter tree ~f:(fun ~name _ ->
    Hashtbl.replace visited name false);
  let rec traverse_from_root name =
    Hashtbl.replace visited name true;
    let _, children = Option.value_exn (Itree.find tree name) in
    Itree.Children.iter children ~f:(fun c ->
      traverse_from_root c)
  in
  let rec print_cycle curr target =
    let `Class (_, parent, _), _ =
      Option.value_exn (Itree.find tree curr)
    in
    if parent <> target then
      begin
	eprintf " %s ->" parent;
	print_cycle parent target
      end
  in
  traverse_from_root "Object";
  Hashtbl.iter visited ~f:(fun ~key ~data ->
    if not data then
      begin
	eprintf "syntax error: circular inheritance classes (%s ->" key;
	print_cycle key key;
	eprintf " %s).\n" key;
	exit (-1)
      end)

let create_method_environ classes =
  let method_env = Method_env.create () in
  List.iter classes ~f:(fun (`Class (clsname, _, features)) ->
    List.iter features ~f:(fun feature ->
      match feature with
      | `Attr _ -> ()
      | `Method (mthdname, formals, retn_type, _) ->
	  let param_types =
	    List.fold_right formals ~init:[] ~f:(fun (`Formal (_, x)) ls ->
	      x :: ls)
	  in
	  Method_env.insert method_env ~name:(clsname, mthdname)
	    ~signt:(param_types, retn_type)));
  method_env

let print_method_environ method_env =
  printf "========= method environment begin =========\n";
  Method_env.iter method_env ~f:(fun ~name:(clsname, mthdname)
      ~signt:(param_types, retn_type) ->
    printf "%s.%s( " clsname mthdname;
    List.iter param_types ~f:(printf "%s ");
    printf ") %s\n" retn_type);
  printf "========== method environment end ==========\n"

let semant classes =
  let all_classes = Basic_classes.basic_classes @ classes in
  let inherit_tree = create_inherit_tree all_classes in
  List.iter [check_main_class_defined;
	     check_final_basis_classes;
	     check_acycle]
    ~f:(fun check -> check inherit_tree);
  let method_env = create_method_environ classes in
  Inherit_tree.print inherit_tree;
  print_method_environ method_env
