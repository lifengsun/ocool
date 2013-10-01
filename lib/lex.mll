{
open Lexing
open Parse

exception SyntaxError of string

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
| class    { CLASS }
| else     { ELSE }
| false    { FALSE }
| fi       { FI }
| if       { IF }
| in       { IN }
| inherits { INHERITS }
| isvoid   { ISVOID }
| let_     { LET }
| loop     { LOOP }
| pool     { POOL }
| then     { THEN }
| while    { WHILE }
| case     { CASE }
| esac     { ESAC }
| new      { NEW }
| of       { OF }
| not      { NOT }
| true     { TRUE }
| int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
| typeid   { TYPEID (Lexing.lexeme lexbuf) }
| objectid { OBJECTID (Lexing.lexeme lexbuf) }
| '"'      { read_string (Buffer.create 20) lexbuf }
| "<-"     { ASSIGN }
| "=>"     { DARROW }
| "(*"     { read_block_comment 1 lexbuf }
| "--"     { read_line_comment lexbuf }
| "<="     { LE }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMICOLON }
| '('      { LPAREN }
| ')'      { RPAREN }
| ','      { COMMA }
| ':'      { COLON }
| '@'      { AT }
| '.'      { DOT }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIV }
| '~'      { COMPLMNT }
| '<'      { LT }
| '='      { EQ }
| newline  { next_line lexbuf; read lexbuf }
| eof      { EOF }
and read_string buf = parse
| '"'      { STRING (Buffer.contents buf) }
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
| eof      { raise (SyntaxError ("Block comment is not terminated")) }
| _        { read_block_comment n lexbuf }
and read_line_comment = parse
| "\n"     { next_line lexbuf; read lexbuf }
| _        { read_line_comment lexbuf }
