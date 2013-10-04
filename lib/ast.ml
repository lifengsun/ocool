open Core.Std

type objid = string
type typeid = string

type expr = [
  | `Bool      of bool * (typeid option ref)	(* 7.1 Constants *)
  | `Int       of int * (typeid option ref)
  | `String    of string * (typeid option ref)
  | `Ident     of objid * (typeid option ref)	(* 7.2 Identifiers *)
  | `Assign    of objid * expr * (typeid option ref)	(* 7.3 Assignment *)
  | `Dispatch  of expr * typeid option * objid
	* (expr list) * (typeid option ref)	(* 7.4 Dispatch *)
  | `Cond      of expr * expr * expr * (typeid option ref) (* 7.5 Conditionals *)
  | `Loop      of expr * expr * (typeid option ref)	    (* 7.6 Loops *)
  | `Block     of expr list * (typeid option ref)	    (* 7.7 Blocks *)
  | `Let       of objid * typeid * expr option
	* expr * (typeid option ref)				(* 7.8 Let *)
  | `Case      of expr * ((objid * typeid * expr) list)
	* (typeid option ref)			(* 7.9 Case *)
  | `New       of typeid * (typeid option ref)	(* 7.10 New *)
  | `Isvoid    of expr * (typeid option ref)	(* 7.11 Isvoid *)
  | `Plus      of expr * expr
	* (typeid option ref) (* 7.12 Arithmetic and Comparison Operations *)
  | `Minus     of expr * expr * (typeid option ref)
  | `Times     of expr * expr * (typeid option ref)
  | `Div       of expr * expr * (typeid option ref)
  | `Lt        of expr * expr * (typeid option ref)
  | `Le        of expr * expr * (typeid option ref)
  | `Eq        of expr * expr * (typeid option ref)
  | `Complmnt  of expr * (typeid option ref)
  | `Not       of expr * (typeid option ref)
  | `Paren     of expr			(* parenthesis *)
  | `InterExpr of string		(* internal expression *)
  ]

type formal = [
    `Formal of objid * typeid
  ]

type feature = [
  | `Method of objid * formal list * typeid * expr
  | `Attr   of objid * typeid * expr option
  ]

type cls = [
    `Class of typeid * typeid * feature list
  ]

exception SyntaxError of string

let rec type_of_expr = function
  | `Bool (_, t)              -> t
  | `Int (_, t)               -> t
  | `String (_, t)            -> t
  | `Ident (_, t)             -> t
  | `Assign (_, _, t)         -> t
  | `Dispatch (_, _, _, _, t) -> t
  | `Cond (_, _, _, t)        -> t
  | `Loop (_, _, t)           -> t
  | `Block (_, t)             -> t
  | `Let (_, _, _, _, t)      -> t
  | `Case (_, lst, t)         -> t
  | `New (_, t)               -> t
  | `Isvoid (_, t)            -> t
  | `Plus (_, _, t)           -> t
  | `Minus (_, _, t)          -> t
  | `Times (_, _, t)          -> t
  | `Div (_, _, t)            -> t
  | `Lt (_, _, t)             -> t
  | `Le (_, _, t)             -> t
  | `Eq (_, _, t)             -> t
  | `Complmnt (_, t)          -> t
  | `Not (_, t)               -> t
  | `Paren expr               -> type_of_expr expr
  | `InterExpr _              -> ref None

let out ~level ls =
  let print_space ~level =
    printf "%s" (String.init (2 * level) ~f:(fun _ -> ' '))
  in
  List.iter ls ~f:(fun (l, s) ->
    print_space (level + l); printf "%s\n" s)

let header level name = out ~level [(0, "#1"); (0, name)]

let rec print_expr level expr =
  let header = header level in
  let print_exprs level exprs =
    List.iter exprs ~f:(print_expr level)
  in
  begin
    match expr with
    | `Bool (v, t) ->
	header "_bool";
	out level [(1, if v then "1" else "0")]
    | `Int (v, t) ->
	header "_int";
	out ~level [(1, Int.to_string v)]
    | `String (s, t) ->
	header "_string";
	out ~level [(1, "\"" ^ s ^ "\"")]
    | `Ident (id, t) ->
	header "_object";
	out ~level [(1, id)]
    | `Assign (objid, expr, t) ->
	header "_assign";
	out ~level [(1, objid)];
	print_expr (level + 1) expr
    | `Dispatch (expr, typeid, objid, exprlst, t) ->
	(match typeid with
	| None   ->
	    header "_dispatch";
	    print_expr (level + 1) expr;
	| Some v ->
	    header "_static_dispatch";
	    print_expr (level + 1) expr;
	    out ~level [(1, v)]);
	out ~level [(1, objid); (1, "(")];
	print_exprs (level + 1) exprlst;
	out ~level [(1, ")")]
    | `Cond (expr1, expr2, expr3, t) ->
	header "_cond";
	print_exprs (level + 1) [expr1; expr2; expr3]
    | `Loop (expr1, expr2, t) ->
	header "_loop";
	print_exprs (level + 1) [expr1; expr2]
    | `Block (exprlst, t) ->
	header "_block";
	print_exprs (level + 1) exprlst
    | `Let (objid, typeid, init, expr, t) ->
	header "_let";
	out ~level [(1, objid); (1, typeid)];
	print_expr_option (level + 1) init;
	print_expr (level + 1) expr
    | `Case (expr, lst, t) ->
	header "_typcase";
	print_expr (level + 1) expr;
	List.iter lst ~f:(fun (objid, typeid, expr) ->
	  out ~level [(1, "#1"); (1, "_branch"); (2, objid); (2, typeid)];
	  print_expr (level + 2) expr)
    | `New (typeid, t) ->
	header "_new";
	out ~level [(1, typeid)]
    | `Isvoid (expr, t) ->
	header "_isvoid";
	print_expr (level + 1) expr
    | `Plus (expr1, expr2, t) ->
	header "_plus";
	print_exprs (level + 1) [expr1; expr2]
    | `Minus (expr1, expr2, t) ->
	header "_sub";
	print_exprs (level + 1) [expr1; expr2]
    | `Times (expr1, expr2, t) ->
	header "_mul";
	print_exprs (level + 1) [expr1; expr2]
    | `Div (expr1, expr2, t) ->
	header "_divide";
	print_exprs (level + 1) [expr1; expr2]
    | `Lt (expr1, expr2, t) ->
	header "_lt";
	print_exprs (level + 1) [expr1; expr2]
    | `Le (expr1, expr2, t) ->
	header "_leq";
	print_exprs (level + 1) [expr1; expr2]
    | `Eq (expr1, expr2, t) ->
	header "_eq";
	print_exprs (level + 1) [expr1; expr2]
    | `Complmnt (expr, t) ->
	header "_neg";
	print_expr (level + 1) expr
    | `Not (expr, t) ->
	header "_comp";
	print_expr (level + 1) expr
    | `Paren expr ->
	print_expr level expr
    | `InterExpr s ->
	out ~level [(1, "internal expression " ^ s)]
  end;
  match expr with
  | `Paren _ -> ()
  | _ ->
      match !(type_of_expr expr) with
      | None ->
	  out ~level [(0, ": _no_type")]
      | Some typeid ->
	  out ~level [(0, ": " ^ typeid)];
and print_expr_option level = function
  | None ->
      out ~level [(0, "#1"); (0, "_no_expr"); (0, ": _no_type")];
  | Some expr ->
      print_expr level expr

let print_formal level (`Formal (objid, typeid)) =
  header level "_formal";
  out ~level [(1, objid); (1, typeid)]

let print_feature ~level = function
  | `Method (objid, formals, typeid, expr) ->
      header level "_method";
      out ~level [(1, objid)];
      List.iter formals ~f:(print_formal (level + 1));
      out ~level [(1, typeid)];
      print_expr (level + 1) expr
  | `Attr (objid, typeid, expr) ->
      header level "_attr";
      out ~level [(1, objid); (1, typeid)];
      print_expr_option (level + 1) expr

let print_class fname level (`Class (clsname, basename, features)) =
  header level "_class";
  out ~level [(1, clsname); (1, basename); (1, "\"" ^ fname ^ "\""); (1, "(")];
  List.iter features ~f:(print_feature ~level:(level + 1));
  out ~level [(1, ")")]

let print fname classes =
  header 0 "_program";
  List.iter classes ~f:(print_class fname 1)
