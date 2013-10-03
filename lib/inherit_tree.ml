open Core.Std

module Children = struct
  type t = String.Set.t
  let empty = String.Set.empty
  let add children child = String.Set.add children child
  let iter = String.Set.iter
end

type name = String.t
type child = String.t
type parent = String.t
type node = parent * Children.t

type t = (parent, node) Hashtbl.t

let create () = Hashtbl.create ~hashable:String.hashable ()

let find tree ~name = Hashtbl.find tree name

let insert tree ~name ~parent ~children =
  Hashtbl.replace tree ~key:name ~data:(parent, children);
  match find tree parent with
  | Some (pparent, pchildren) ->
      Hashtbl.replace tree ~key:parent
	~data:(pparent, Children.add pchildren name)
  | None -> ()

let iter tree ~f =
  let g ~key:name ~data:(parent, children) = f ~name (parent, children) in
  Hashtbl.iter tree ~f:g

let print tree =
  printf "========= inherit tree begin =========\n";
  iter tree ~f:(fun ~name (parent, children) ->
    printf "(\"%s\", \"%s\") " name parent;
    Children.iter children ~f:(printf "\"%s\" ");
    printf "\n");
  printf "========== inherit tree end ==========\n"
