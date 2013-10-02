open Core.Std
open Cool

let inherit_tree = Hashtbl.create ~hashable:String.hashable ()

let inherit_tree_insert name base =
  match Hashtbl.find inherit_tree name with
  | Some _ ->
      eprintf "syntax error: class \"%s\" redefined\n" name;
      exit (-1)
  | None ->
      Hashtbl.replace inherit_tree ~key:name ~data:(base, String.Set.empty);
      match Hashtbl.find inherit_tree base with
      | Some (basesbase, derives) ->
	  Hashtbl.replace inherit_tree ~key:base
	    ~data:(basesbase, String.Set.add derives name)
      | None -> ()

let print_inherit_tree () =
  printf "========= inherit tree begin =========\n";
  Hashtbl.iter inherit_tree ~f:(fun ~key ~data:(base, derives) ->
    printf "(\"%s\", \"%s\") " key base;
    String.Set.iter derives ~f:(printf "\"%s\" ");
    printf "\n");
  printf "========== inherit tree end ==========\n"

let create_inherit_tree classes =
  inherit_tree_insert "Object" "";
  List.iter ["IO"; "Int"; "String"; "Bool"] ~f:(fun c ->
    inherit_tree_insert c "Object");
  List.iter classes ~f:(fun (`Class (classname, basename, _)) ->
    inherit_tree_insert classname basename);
  (* When base class is inserted later than derived classes, it cannot
     find them immediately, so we traverse the tree to find them. *)
  Hashtbl.iter inherit_tree ~f:(fun ~key ~data:(base, derives) ->
    if key <> "Object" then	 (* Object class has no base class. *)
      match Hashtbl.find inherit_tree base with
      | None ->
          eprintf "syntax error: class \"%s\" inherits from undefined class \"%s\".\n" key base;
          exit (-1)
      | Some (basesbase, children) ->
          Hashtbl.replace inherit_tree ~key:base
            ~data:(basesbase, String.Set.add children key))

let semant classes =
  create_inherit_tree classes;
  print_inherit_tree ()
