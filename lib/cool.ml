open Core.Std
open Lexing

let parse_exn filename =
  In_channel.with_file filename ~f:(fun ifile ->
    let lexbuf = Lexing.from_channel ifile in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    try Parse.prog Lex.read lexbuf with
    | Parse.Error ->
	let pos = lexbuf.lex_curr_p in
	eprintf "%s:%d:%d: syntax error\n" pos.pos_fname pos.pos_lnum
	  (pos.pos_cnum - pos.pos_bol + 1);
	exit (-1))
