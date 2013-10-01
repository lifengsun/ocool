open Core.Std
open Lexing

open Ocool
open Parse

let rec parse_and_print lexbuf =
  let token = Lex.read lexbuf in
  let lineno = lexbuf.lex_curr_p.pos_lnum in
  if token <> EOF then printf "#%d " lineno;
  (match token with
  | CLASS           -> printf "CLASS\n"
  | ELSE            -> printf "ELSE\n"
  | FALSE           -> printf "BOOL_CONST false\n"
  | FI              -> printf "FI\n"
  | IF              -> printf "IF\n"
  | IN              -> printf "IN\n"
  | INHERITS        -> printf "INHERITS\n"
  | ISVOID          -> printf "ISVOID\n"
  | LET             -> printf "LET\n"
  | LOOP            -> printf "LOOP\n"
  | POOL            -> printf "POOL\n"
  | THEN            -> printf "THEN\n"
  | WHILE           -> printf "WHILE\n"
  | CASE            -> printf "CASE\n"
  | ESAC            -> printf "ESAC\n"
  | NEW             -> printf "NEW\n"
  | OF              -> printf "OF\n"
  | NOT             -> printf "NOT\n"
  | TRUE            -> printf "BOOL_CONST true\n"
  | INT n           -> printf "INT_CONST %d\n" n
  | SELF            -> printf "SELF\n"
  | SELF_TYPE       -> printf "SELF_TYPE\n"
  | TYPEID typeid   -> printf "TYPEID %s\n" typeid
  | OBJECTID objid  -> printf "OBJECTID %s\n" objid
  | ASSIGN          -> printf "ASSIGN\n"
  | LE              -> printf "LE\n"
  | DARROW          -> printf "DARROW\n"
  | STRING s        -> printf "STR_CONST \"%s\"\n" s
  | LBRACE          -> printf "\'{\'\n"
  | RBRACE          -> printf "\'}\'\n"
  | SEMICOLON       -> printf "\';\'\n"
  | LPAREN          -> printf "\'(\'\n"
  | RPAREN          -> printf "\')\'\n"
  | COMMA           -> printf "\',\'\n"
  | COLON           -> printf "\':\'\n"
  | AT              -> printf "\'@\'\n"
  | DOT             -> printf "\'.\'\n"
  | PLUS            -> printf "\'+\'\n"
  | MINUS           -> printf "\'-\'\n"
  | TIMES           -> printf "\'*\'\n"
  | DIV             -> printf "\'/\'\n"
  | COMPLMNT        -> printf "\'~\'\n"
  | LT              -> printf "\'<\'\n"
  | EQ              -> printf "\'=\'\n"
  | EOF             -> ()
  );
  if token <> EOF then parse_and_print lexbuf

let parse filename =
  In_channel.with_file filename ~f:(fun ifile ->
    let lexbuf = Lexing.from_channel ifile in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    printf "#name \"%s\"\n" filename;
    parse_and_print lexbuf)

let () = parse Sys.argv.(1)
