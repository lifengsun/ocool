open Core.Std
open Lexing

open Ocool
open Parse
open Lex

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
  (match expr with
  | `Bool v ->
      header "_bool";
      out level [(1, if v then "1" else "0")]
  | `Int v ->
      header "_int";
      out ~level [(1, Int.to_string v)]
  | `String s ->
      header "_string";
      out ~level [(1, "\"" ^ s ^ "\"")]
  | `Ident id ->
      header "_object";
      out ~level [(1, id)]
  | `Assign (objid, expr) ->
      header "_assign";
      out ~level [(1, objid)];
      print_expr (level + 1) expr
  | `Dispatch (expr, typeid, objid, exprlst) ->
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
  | `Cond (expr1, expr2, expr3) ->
      header "_cond";
      print_exprs (level + 1) [expr1; expr2; expr3]
  | `Loop (expr1, expr2) ->
      header "_loop";
      print_exprs (level + 1) [expr1; expr2]
  | `Block exprlst ->
      header "_block";
      print_exprs (level + 1) exprlst
  | `Let (objid, typeid, init, expr) ->
      header "_let";
      out ~level [(1, objid); (1, typeid)];
      (match init with
      | None   ->
	  out ~level [(1, "#1"); (1, "_no_expr"); (1, ": _no_type")]
      | Some v -> print_expr (level + 1) v);
      print_expr (level + 1) expr
  | `Case (expr, lst) ->
      header "_typcase";
      print_expr (level + 1) expr;
      List.iter lst ~f:(fun (objid, typeid, expr) ->
	out ~level [(1, "#1"); (1, "_branch"); (2, objid); (2, typeid)];
	print_expr (level + 2) expr)
  | `New typeid ->
      header "_new";
      out ~level [(1, typeid)]
  | `Isvoid expr ->
      header "_isvoid";
      print_expr (level + 1) expr
  | `Plus (expr1, expr2) ->
      header "_plus";
      print_exprs (level + 1) [expr1; expr2]
  | `Minus (expr1, expr2) ->
      header "_sub";
      print_exprs (level + 1) [expr1; expr2]
  | `Times (expr1, expr2) ->
      header "_mul";
      print_exprs (level + 1) [expr1; expr2]
  | `Div (expr1, expr2) ->
      header "_divide";
      print_exprs (level + 1) [expr1; expr2]
  | `Lt (expr1, expr2) ->
      header "_lt";
      print_exprs (level + 1) [expr1; expr2]
  | `Le (expr1, expr2) ->
      header "_leq";
      print_exprs (level + 1) [expr1; expr2]
  | `Eq (expr1, expr2) ->
      header "_eq";
      print_exprs (level + 1) [expr1; expr2]
  | `Complmnt expr ->
      header "_neg";
      print_expr (level + 1) expr
  | `Not expr ->
      header "_comp";
      print_expr (level + 1) expr
  | `Paren expr ->
      print_expr level expr);
  match expr with
  | `Paren _ -> ()
  | _ ->
      out ~level [(0, ": _no_type")]

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
      (match expr with
      | None ->
	  out ~level [(1, "#1"); (1, "_no_expr"); (1, ": _no_type")];
      | Some v -> 
	  print_expr (level + 1) v)

let print_class fname level (`Class (clsname, basename, features)) =
  header level "_class";
  out ~level [(1, clsname); (1, basename); (1, "\"" ^ fname ^ "\""); (1, "(")];
  List.iter features ~f:(print_feature ~level:(level + 1));
  out ~level [(1, ")")]

let () =
  let fname = Sys.argv.(1) in
  header 0 "_program";
  List.iter (Cool.parse_exn fname) ~f:(print_class fname 1)
