open Core.Std

module Children = struct
  type t = String.Set.t
  let empty = String.Set.empty
  let is_empty = String.Set.is_empty
  let add children child = String.Set.add children child
  let iter = String.Set.iter
end

type name = String.t
type node = Ast.cls * Children.t

type t = (name, node) Hashtbl.t

let create () = Hashtbl.create ~hashable:String.hashable ()

let find tree ~name = Hashtbl.find tree name

let insert tree ~name ~node:(cls, children) =
  Hashtbl.replace tree ~key:name ~data:(cls, children)

let iter tree ~f =
  let g ~key:name ~data:(cls, children) =
    f ~name (cls, children)
  in
  Hashtbl.iter tree ~f:g

let print tree =
  printf "========= inherit tree begin =========\n";
  iter tree ~f:(fun ~name (`Class (_, parent, _), children) ->
    printf "(\"%s\", \"%s\") " name parent;
    Children.iter children ~f:(printf "\"%s\" ");
    printf "\n");
  printf "========== inherit tree end ==========\n"
