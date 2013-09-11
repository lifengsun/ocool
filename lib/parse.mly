%{
open Lexing

exception SyntaxError of string
%}

%token CLASS
%token INHERITS
%token TRUE FALSE
%token IF THEN ELSE FI
%token ISVOID
%token LET IN
%token LOOP POOL
%token WHILE
%token CASE ESAC
%token NEW
%token OF
%token NOT
%token SELF
%token SELF_TYPE
%token <string> TYPEID OBJECTID STRING
%token LBRACE RBRACE LPAREN RPAREN
%token PLUS MINUS TIMES DIV
%token SEMICOLON COMMA COLON AT DOT TILDE LT EQ
%token <int> INT
%token ASSIGN
%token LE
%token DARROW
%token EOF
%start <Cool.cls option> prog
%%
prog:
| EOF  { None }
| cls  { Some $1 }

cls:
| CLASS; typeid; INHERITS; typeid; LBRACE; features; RBRACE
    { `Class ($2, Some $4, $6) }
| CLASS; typeid; LBRACE; features; RBRACE
    { `Class ($2, None, $4) }

typeid: TYPEID { $1 }

features:
| { [] }

expr:
| WHILE; expr; LOOP; expr; POOL { `While ($2, $4) }
