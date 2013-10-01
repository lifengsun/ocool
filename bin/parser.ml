open Core.Std
open Lexing

open Ocool
open Parse
open Lex

let fname = ref ""

let print_space ~level =
  printf "%s" (String.init (2 * level) ~f:(fun _ -> ' '))

let rec print_expr ~level expr =
  match expr with
  | `Bool v ->
      print_space level; printf "#1\n";
      print_space level; printf "_bool\n";
      print_space (level + 1); printf "%d\n" (if v then 1 else 0);
      print_space level; printf ": _no_type\n"
  | `Int v    ->
      print_space level; printf "#1\n";
      print_space level; printf "_int\n";
      print_space (level + 1); printf "%d\n" v;
      print_space level; printf ": _no_type\n"
  | `String s ->
      print_space level; printf "#1\n";
      print_space level; printf "_string\n";
      print_space (level + 1); printf "\"%s\"\n" s;
      print_space level; printf ": _no_type\n"
  | `Ident id ->
      print_space level; printf "#1\n";
      print_space level; printf "_object\n";
      print_space (level + 1); printf "%s\n" id;
      print_space level; printf ": _no_type\n"
  | `Assign (objid, expr) ->
      print_space level; printf "#1\n";
      print_space level; printf "_assign\n";
      print_space (level + 1); printf "%s\n" objid;
      print_expr (level + 1) expr;
      print_space level; printf ": _no_type\n"
  | `Dispatch (expr, typeid, objid, exprlst) ->
      print_space level; printf "#1\n";
      print_space level; printf "_dispatch\n";
      print_expr (level + 1) expr;
      print_space (level + 1); printf "%s\n" objid;
      print_space (level + 1); printf "(\n";
      List.iter exprlst ~f:(print_expr ~level:(level + 1));
      print_space (level + 1); printf ")\n";
      print_space level; printf ": _no_type\n"
  | `Cond (expr1, expr2, expr3) ->
      print_space level; printf "#1\n";
      print_space level; printf "_cond\n";
      print_expr (level + 1) expr1;
      print_expr (level + 1) expr2;
      print_expr (level + 1) expr3;
      print_space level; printf ": _no_type\n"
  | `Loop (expr1, expr2) ->
      print_space level; printf "#1\n";
      print_space level; printf "_loop\n";
      print_expr (level + 1) expr1;
      print_expr (level + 1) expr2;
      print_space level; printf ": _no_type\n"
  | `Block exprlst ->
      print_space level; printf "#1\n";
      print_space level; printf "_block\n";
      List.iter exprlst ~f:(print_expr ~level:(level + 1));
      print_space level; printf ": _no_type\n"
  | `Let (varlst, expr) ->
      print_space level; printf "#1\n";
      print_space level; printf "_let\n";
      List.iter varlst ~f:(fun (objid, typeid, init) ->
	print_space (level + 1); printf "%s\n" objid;
	print_space (level + 1); printf "%s\n" typeid;
	(match init with
	| None   -> ()
	| Some v -> print_expr (level + 1) v));
      print_expr (level + 1) expr;
      print_space level; printf ": _no_type\n"
  | `Case _   -> ()
  | `New typeid ->
      print_space level; printf "#1\n";
      print_space level; printf "_new\n";
      print_space (level + 1); printf "%s\n" typeid;
      print_space level; printf ": _no_type\n"
  | `Isvoid _ -> ()
  | `Plus (expr1, expr2) ->
      print_space level; printf "#1\n";
      print_space level; printf "_plus\n";
      print_expr (level + 1) expr1;
      print_expr (level + 1) expr2;
      print_space level; printf ": _no_type\n"
  | `Minus (expr1, expr2) ->
      print_space level; printf "#1\n";
      print_space level; printf "_sub\n";
      print_expr (level + 1) expr1;
      print_expr (level + 1) expr2;
      print_space level; printf ": _no_type\n"
  | `Times _ -> ()
  | `Div _ -> ()
  | `Lt _ -> ()
  | `Le _ -> ()
  | `Eq (expr1, expr2) ->
      print_space level; printf "#1\n";
      print_space level; printf "_eq\n";
      print_expr (level + 1) expr1;
      print_expr (level + 1) expr2;
      print_space level; printf ": _no_type\n"
  | `Complmnt expr ->
      print_space level; printf "#1\n";
      print_space level; printf "_neg\n";
      print_expr (level + 1) expr;
      print_space level; printf ": _no_type\n"
  | `Not expr ->
      print_space level; printf "#1\n";
      print_space level; printf "_comp\n";
      print_expr (level + 1) expr;
      print_space level; printf ": _no_type\n"
  | `Paren expr ->
      print_expr level expr
  | _ -> ()

let print_formal ~level (`Formal (objid, typeid)) =
  print_space level; printf "#1\n";
  print_space level; printf "_formal\n";
  print_space (level + 1); printf "%s\n" objid;
  print_space (level + 1); printf "%s\n" typeid

let print_feature ~level = function
  | `Method (objid, formals, typeid, expr) ->
      print_space level; printf "#1\n";
      print_space level; printf "_method\n";
      print_space (level + 1); printf "%s\n" objid;
	List.iter formals ~f:(print_formal ~level:(level + 1));
      print_space (level + 1); printf "%s\n" typeid;
      print_expr (level + 1) expr
  | `Attr (objid, typeid, expr) ->
      print_space level; printf "#1\n";
      print_space level; printf "_attr\n";
      print_space (level + 1); printf "%s\n" objid;
      print_space (level + 1); printf "%s\n" typeid;
      (match expr with
      | None ->
	  print_space (level + 1); printf "#1\n";
	  print_space (level + 1); printf "_no_expr\n";
	  print_space (level + 1); printf ": _no_type\n"
      | Some v -> 
	  print_expr (level + 1) v)


let rec print_class ~level (`Class (clsname, basename, features)) =
  print_space level; printf "#1\n";
  print_space level; printf "_class\n";
  print_space (level + 1); printf "%s\n" clsname;
  print_space (level + 1); printf "%s\n" basename;
  print_space (level + 1); printf "\"%s\"\n" !fname;
  print_space (level + 1); printf "(\n";
  List.iter features ~f:(print_feature ~level:(level + 1));
  print_space (level + 1); printf ")\n"

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
  printf "#1\n";
  printf "_program\n";
  List.iter (parse_with_error lexbuf) ~f:(print_class ~level:1)

let parse filename =
  fname := filename;
  In_channel.with_file !fname ~f:(fun ifile ->
    let lexbuf = Lexing.from_channel ifile in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = !fname };
    parse_and_print lexbuf)

let () = parse Sys.argv.(1)
