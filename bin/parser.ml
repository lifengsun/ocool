open Core.Std
open Lexing

open Ocool
open Parse
open Lex

let fname = ref ""

let out ~level ls =
  let print_space ~level =
    printf "%s" (String.init (2 * level) ~f:(fun _ -> ' '))
  in
  List.iter ls ~f:(fun (l, s) ->
    print_space (level + l); printf "%s\n" s)

let rec print_expr level = function
  | `Paren expr ->
      print_expr level expr
  | expr ->
      out ~level [(0, "#1")];
      (match expr with
      | `Bool v ->
	  out ~level [(0, "_bool"); (1, if v then "1" else "0")]
      | `Int v ->
	  out ~level [(0, "_int"); (1, Int.to_string v)]
      | `String s ->
	  out ~level [(0, "_string"); (1, "\"" ^ s ^ "\"")]
      | `Ident id ->
	  out ~level [(0, "_object"); (1, id)]
      | `Assign (objid, expr) ->
	  out ~level [(0, "_assign"); (1, objid)];
	  print_expr (level + 1) expr
      | `Dispatch (expr, typeid, objid, exprlst) ->
	  out ~level [(0, match typeid with
	  | None   -> "_dispatch"
	  | Some v -> "_static_dispatch")];
	  print_expr (level + 1) expr;
	  (match typeid with
	  | None   -> ()
	  | Some v -> out ~level [(1, v)]);
	  out ~level [(1, objid); (1, "(")];
	  List.iter exprlst ~f:(print_expr (level + 1));
	  out ~level [(1, ")")]
      | `Cond (expr1, expr2, expr3) ->
	  out ~level [(0, "_cond")];
	  List.iter [expr1; expr2; expr3] ~f:(print_expr (level + 1))
      | `Loop (expr1, expr2) ->
	  out ~level [(0, "_loop")];
	  List.iter [expr1; expr2] ~f:(print_expr (level + 1))
      | `Block exprlst ->
	  out ~level [(0, "_block")];
	  List.iter exprlst ~f:(print_expr (level + 1))
      | `Let (objid, typeid, init, expr) ->
	  out ~level [(0, "_let"); (1, objid); (1, typeid)];
	  (match init with
	  | None   ->
	      out ~level [(1, "#1"); (1, "_no_expr"); (1, ": _no_type")]
	  | Some v -> print_expr (level + 1) v);
	  print_expr (level + 1) expr
      | `Case (expr, lst) ->
	  out ~level [(0, "_typcase")];
	  print_expr (level + 1) expr;
	  List.iter lst ~f:(fun (objid, typeid, expr) ->
	    out ~level [(1, "#1"); (1, "_branch"); (2, objid); (2, typeid)];
	    print_expr (level + 2) expr)
      | `New typeid ->
	  out ~level [(0, "_new"); (1, typeid)]
      | `Isvoid expr ->
	  out ~level [(0, "_isvoid")];
	  print_expr (level + 1) expr
      | `Plus (expr1, expr2) ->
	  out ~level [(0, "_plus")];
	  List.iter [expr1; expr2] ~f:(print_expr (level + 1))
      | `Minus (expr1, expr2) ->
	  out ~level [(0, "_sub")];
	  List.iter [expr1; expr2] ~f:(print_expr (level + 1))
      | `Times (expr1, expr2) ->
	  out ~level [(0, "_mul")];
	  List.iter [expr1; expr2] ~f:(print_expr (level + 1))
      | `Div (expr1, expr2) ->
	  out ~level [(0, "_divide")];
	  List.iter [expr1; expr2] ~f:(print_expr (level + 1))
      | `Lt (expr1, expr2) ->
	  out ~level [(0, "_lt")];
	  List.iter [expr1; expr2] ~f:(print_expr (level + 1))
      | `Le (expr1, expr2) ->
	  out ~level [(0, "_leq")];
	  List.iter [expr1; expr2] ~f:(print_expr (level + 1))
      | `Eq (expr1, expr2) ->
	  out ~level [(0, "_eq")];
	  List.iter [expr1; expr2] ~f:(print_expr (level + 1))
      | `Complmnt expr ->
	  out ~level [(0, "_neg")];
	  print_expr (level + 1) expr
      | `Not expr ->
	  out ~level [(0, "_comp")];
	  print_expr (level + 1) expr
      | `Paren _ -> ());
      match expr with
      | `Paren _ -> ()
      | _ ->
	  out ~level [(0, ": _no_type")]

let print_formal level (`Formal (objid, typeid)) =
  out ~level [(0, "#1"); (0, "_formal"); (1, objid); (1, typeid)]

let print_feature ~level = function
  | `Method (objid, formals, typeid, expr) ->
      out ~level [(0, "#1"); (0, "_method"); (1, objid)];
	List.iter formals ~f:(print_formal (level + 1));
      out ~level [(1, typeid)];
      print_expr (level + 1) expr
  | `Attr (objid, typeid, expr) ->
      out ~level [(0, "#1"); (0, "_attr"); (1, objid); (1, typeid)];
      (match expr with
      | None ->
	  out ~level [(1, "#1"); (1, "_no_expr"); (1, ": _no_type")];
      | Some v -> 
	  print_expr (level + 1) v)


let rec print_class ~level (`Class (clsname, basename, features)) =
  out ~level [(0, "#1"); (0, "_class"); (1, clsname); (1, basename);
	      (1, "\"" ^ !fname ^ "\""); (1, "(")];
  List.iter features ~f:(print_feature ~level:(level + 1));
  out ~level [(1, ")")]

let print_position ofile lexbuf =
  let pos = lexbuf.lex_curr_p in
  eprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parse.prog Lex.read lexbuf with
  | SyntaxError msg ->
      eprintf "%a: %s\n" print_position lexbuf msg;
      []
  | Parse.Error ->
      eprintf "%a: syntax error\n" print_position lexbuf;
      exit (-1)

let parse_and_print lexbuf =
  out 0 [(0, "#1"); (0, "_program")];
  List.iter (parse_with_error lexbuf) ~f:(print_class ~level:1)

let parse filename =
  fname := filename;
  In_channel.with_file !fname ~f:(fun ifile ->
    let lexbuf = Lexing.from_channel ifile in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = !fname };
    parse_and_print lexbuf)

let () = parse Sys.argv.(1)
