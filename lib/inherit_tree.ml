open Core.Std

module Children = struct
  type t = String.Set.t
  let empty = String.Set.empty
  let is_empty = String.Set.is_empty
  let add = String.Set.add
  let iter = String.Set.iter
end

type name = String.t
type node = Ast.cls * Children.t

type t = (name, node) Hashtbl.t

let create () = Hashtbl.create ~hashable:String.hashable ()

let find tree ~name = Hashtbl.find tree name

let parent tree ~name =
  match find tree ~name with
  | None -> None
  | Some (`Class (_, parent, _), _) ->
      Some parent

let change tree ~name ~f = Hashtbl.change tree name f

let insert tree ~name ~node:(cls, children) =
  Hashtbl.replace tree ~key:name ~data:(cls, children)

let iter tree ~f =
  let g ~key:name ~data:(cls, children) =
    f ~name (cls, children)
  in
  Hashtbl.iter tree ~f:g

let insert_children tree =
  iter tree ~f:(fun ~name (`Class (_, parent, _), _) ->
    if name <> "Object" then	 (* Object class has no base class. *)
      change tree ~name:parent ~f:(fun v -> match v with
      | None ->
          eprintf "syntax error: class \"%s\" inherits from undefined class \"%s\".\n" name parent;
          exit (-1)
      | Some (pcls, pchildren) ->
	  Some (pcls, Children.add pchildren name)))

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
  match find tree "Main" with
  | None ->
      eprintf "syntax error: Main class undefined.\n";
      exit (-1)
  | Some (`Class (_, _, features), _)  ->
      check_main_method features

let check_final_basis_classes tree =
  List.iter ["Int"; "String"; "Bool"] ~f:(fun c ->
    let (_, children) = Option.value_exn (find tree c) in
    if not (Children.is_empty children) then
      begin
	eprintf "syntax error: classes (";
	Children.iter children ~f:(eprintf " %s");
	eprintf " ) inherits from basic class ( %s ).\n" c;
	exit (-1)
      end)

let check_acycle tree =
  let visited = String.Table.create () in
  iter tree ~f:(fun ~name _ ->
    Hashtbl.replace visited name false);
  let rec traverse_from_root name =
    Hashtbl.replace visited name true;
    let _, children = Option.value_exn (find tree name) in
    Children.iter children ~f:(fun c ->
      traverse_from_root c)
  in
  let rec print_cycle curr target =
    let `Class (_, parent, _), _ =
      Option.value_exn (find tree curr)
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

let build classes =
  let inherit_tree = create () in
  List.iter classes ~f:(fun ((`Class (name, parent, _)) as cls) ->
    match find inherit_tree ~name with
    | None ->
	insert inherit_tree name (cls, Children.empty)
    | Some _ ->
	eprintf "syntax error: class \"%s\" redefined\n" name;
	exit (-1));
  insert_children inherit_tree;
  List.iter [check_main_class_defined;
	     check_final_basis_classes;
	     check_acycle]
    ~f:(fun check -> check inherit_tree);
  inherit_tree

let print tree =
  printf "========= inherit tree begin =========\n";
  iter tree ~f:(fun ~name (`Class (_, parent, _), children) ->
    printf "(\"%s\", \"%s\") " name parent;
    Children.iter children ~f:(printf "\"%s\" ");
    printf "\n");
  printf "========== inherit tree end ==========\n"
