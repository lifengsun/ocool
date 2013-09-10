{
open Lexing

exception SyntaxError of string

type token =
  | CLASS
  | ELSE
  | FALSE
  | FI
  | IF
  | IN
  | INHERITS
  | ISVOID
  | LET
  | LOOP
  | POOL
  | THEN
  | WHILE
  | CASE
  | ESAC
  | NEW
  | OF
  | NOT
  | TRUE
  | SELF
  | SELF_TYPE
  | TYPEID of string
  | OBJECTID of string
  | INT of int
  | STRING of string
  | ASSIGN
  | LE
  | DARROW
  | SYM of string
  | EOF

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let class    = ['C''c']['L''l']['A''a']['S''s']['S''s']
let else     = ['E''e']['L''l']['S''s']['E''e']
let false    = 'f'['A''a']['L''l']['S''s']['E''e']
let fi       = ['F''f']['I''i']
let if       = ['I''i']['F''f']
let in       = ['I''i']['N''n']
let inherits = ['I''i']['N''n']['H''h']['E''e']['R''r']['I''i']['T''t']['S''s']
let isvoid   = ['I''i']['S''s']['V''v']['O''o']['I''i']['D''d']
let let_     = ['L''l']['E''e']['T''t']
let loop     = ['L''l']['O''o']['O''o']['P''p']
let pool     = ['P''p']['O''o']['O''o']['L''l']
let then     = ['T''t']['H''h']['E''e']['N''n']
let while    = ['W''w']['H''h']['I''i']['L''l']['E''e']
let case     = ['C''c']['A''a']['S''s']['E''e']
let esac     = ['E''e']['S''s']['A''a']['C''c']
let new      = ['N''n']['E''e']['W''w']
let of       = ['O''o']['F''f']
let not      = ['N''n']['O''o']['T''t']
let true     = 't'['R''r']['U''u']['E''e']

let int      = ['0'-'9']+
let typeid   = ['A'-'Z']['A'-'Z''a'-'z''0'-'9''_']*
let objectid = ['a'-'z']['A'-'Z''a'-'z''0'-'9''_']*

let space    = ' ' | '\014' | '\t' | '\013'
let newline  = '\r' | '\n' | "\r\n"

rule read = parse
| space+   { read lexbuf }
| class    { (lexbuf.lex_curr_p.pos_lnum, CLASS) }
| else     { (lexbuf.lex_curr_p.pos_lnum, ELSE) }
| false    { (lexbuf.lex_curr_p.pos_lnum, FALSE) }
| fi       { (lexbuf.lex_curr_p.pos_lnum, FI) }
| if       { (lexbuf.lex_curr_p.pos_lnum, IF) }
| in       { (lexbuf.lex_curr_p.pos_lnum, IN) }
| inherits { (lexbuf.lex_curr_p.pos_lnum, INHERITS) }
| isvoid   { (lexbuf.lex_curr_p.pos_lnum, ISVOID) }
| let_     { (lexbuf.lex_curr_p.pos_lnum, LET) }
| loop     { (lexbuf.lex_curr_p.pos_lnum, LOOP) }
| pool     { (lexbuf.lex_curr_p.pos_lnum, POOL) }
| then     { (lexbuf.lex_curr_p.pos_lnum, THEN) }
| while    { (lexbuf.lex_curr_p.pos_lnum, WHILE) }
| case     { (lexbuf.lex_curr_p.pos_lnum, CASE) }
| esac     { (lexbuf.lex_curr_p.pos_lnum, ESAC) }
| new      { (lexbuf.lex_curr_p.pos_lnum, NEW) }
| of       { (lexbuf.lex_curr_p.pos_lnum, OF) }
| not      { (lexbuf.lex_curr_p.pos_lnum, NOT) }
| true     { (lexbuf.lex_curr_p.pos_lnum, TRUE) }
| int      { (lexbuf.lex_curr_p.pos_lnum,
	      INT (int_of_string (Lexing.lexeme lexbuf))) }
| typeid   { (lexbuf.lex_curr_p.pos_lnum, TYPEID (Lexing.lexeme lexbuf)) }
| objectid { (lexbuf.lex_curr_p.pos_lnum, OBJECTID (Lexing.lexeme lexbuf)) }
| '"'      { read_string (Buffer.create 20) lexbuf }
| "<-"     { (lexbuf.lex_curr_p.pos_lnum, ASSIGN) }
| "=>"     { (lexbuf.lex_curr_p.pos_lnum, DARROW) }
| "(*"     { read_block_comment 1 lexbuf }
| "--"     { read_line_comment lexbuf }
| "<="     { (lexbuf.lex_curr_p.pos_lnum, LE) }
| ['{''}'';''('')'','':''@''.''+''-''*''/''~''<''=']
           { (lexbuf.lex_curr_p.pos_lnum, SYM (Lexing.lexeme lexbuf)) }
| newline  { next_line lexbuf; read lexbuf }
| eof      { (lexbuf.lex_curr_p.pos_lnum, EOF) }
and read_string buf = parse
| '"'      { (lexbuf.lex_curr_p.pos_lnum, STRING (Buffer.contents buf)) }
| '\\' _   { Buffer.add_string buf (Lexing.lexeme lexbuf);
	     read_string buf lexbuf }
| eof      { raise (SyntaxError ("String is not terminated")) }
| [^'"''\\']+
           { Buffer.add_string buf (Lexing.lexeme lexbuf);
	     read_string buf lexbuf }
and read_block_comment n = parse
| "(*"     { read_block_comment (n + 1) lexbuf }
| "*)"     { if n > 1 then read_block_comment (n - 1) lexbuf else read lexbuf }
| "\n"     { next_line lexbuf; read_block_comment n lexbuf }
| _        { read_block_comment n lexbuf }
and read_line_comment = parse
| "\n"     { next_line lexbuf; read lexbuf }
| _        { read_line_comment lexbuf }
