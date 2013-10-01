open Core.Std
open Lexing

open Ocool
open Parse
open Lex

let print_expr outx expr = ()
let print_formals outx formals = ()

let print_feature outx = function
  | `Method (objid, formals, typeid, expr) ->
      printf "method: %s(%a) : %s\n expr: %a\n"
	objid print_formals formals typeid print_expr expr
  | `Attr (objid, typeid, None) ->
      printf "attr: %s : %s\n" objid typeid
  | `Attr (objid, typeid, Some expr) ->
      printf "attr: %s : %s\ninit: %a\n" objid typeid print_expr expr

let print_features outx features =
  List.iter ~f:(print_feature outx) features
    
let rec print_ast outx = function
  | `Class (clsname, Some basename, features) ->
      printf "class %s derived from %s\n%a" clsname basename print_features features
  | `Class (clsname, None, features) ->
      printf "class %s\n%a" clsname print_features features

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parse.prog Lex.read lexbuf with
  | SyntaxError msg ->
      fprintf stderr "%a: %s\n" print_position lexbuf msg;
      []
  | Parse.Error ->
      fprintf stderr "%a: syntax error\n" print_position lexbuf;
      exit (-1)

let rec parse_and_print lexbuf =
  List.iter (parse_with_error lexbuf) ~f:(printf "%a" print_ast)

let parse filename =
  In_channel.with_file filename ~f:(fun ifile ->
    let lexbuf = Lexing.from_channel ifile in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  (* printf "#name \"%s\"\n" filename; *)
    parse_and_print lexbuf)

let () = parse Sys.argv.(1)
