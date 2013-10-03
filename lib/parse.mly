%{
(* menhir does not work well with Core. *)
(* open Core.Std *)
open Lexing


(* menhir generates code for Error exception which conflicts with the
   one in Core, so we redefine it to make menhir happy. *)
(* exception Error *)
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
%token SEMICOLON COMMA COLON AT DOT COMPLMNT LT EQ
%token <int> INT
%token ASSIGN
%token LE
%token DARROW
%token EOF

%right    ASSIGN
%right    NOT
%nonassoc LE LT EQ
%left     PLUS MINUS
%left     TIMES DIV
%right    ISVOID
%right    COMPLMNT
%left     AT
%left     DOT

%start <Ast.cls list> prog
%%
prog:
| classes; EOF { $1 }

classes:
    rev_classes { List.rev $1 }

rev_classes:
| { [] }
| rev_classes; cls; SEMICOLON { $2 :: $1 }

cls:
| CLASS; typeid; INHERITS; typeid; LBRACE; features; RBRACE
    { `Class ($2, $4, $6) }
| CLASS; typeid; LBRACE; features; RBRACE
    { `Class ($2, "Object", $4) }

typeid:
| TYPEID { $1 }
| SELF_TYPE { "SELF_TYPE" }

features:
    rev_features { List.rev $1 }

rev_features:
| { [] }
| rev_features; feature; SEMICOLON  { $2 :: $1 }

feature:
| objid; LPAREN; formals; RPAREN; COLON; typeid; LBRACE; expr; RBRACE
    { `Method ($1, $3, $6, $8) }
| objid; COLON; typeid
    { `Attr ($1, $3, None) }
| objid; COLON; typeid; ASSIGN; expr
    { `Attr ($1, $3, Some $5) }

formals:
    lst = separated_list(COMMA, formal) { lst }

formal:
    objid; COLON; typeid { `Formal ($1, $3) }

objid:
| OBJECTID { $1 }
| SELF     { "self" }

args:
    lst = separated_list(COMMA, expr) { lst }

objinit:
| objid; COLON; typeid
    { ($1, $3, None) }
| objid; COLON; typeid; ASSIGN; expr
    { ($1, $3, Some $5) }

objinits:
    lst = separated_list(COMMA, objinit) { lst }

exprs:
    rev_exprs { List.rev $1 }

rev_exprs:
| { [] }
| rev_exprs; expr; SEMICOLON { $2 :: $1 }

case:
    objid; COLON; typeid; DARROW; expr { ($1, $3, $5) }

cases:
    rev_cases { List.rev $1 }

rev_cases:
| { [] }
| rev_cases; case; SEMICOLON { $2 :: $1 }

expr:
| objid; ASSIGN; expr
    { `Assign ($1, $3, ref None) }
| expr; AT; typeid; DOT; objid; LPAREN; args; RPAREN
    { `Dispatch ($1, Some $3, $5, $7, ref None) }
| expr; DOT; objid; LPAREN; args; RPAREN
    { `Dispatch ($1, None, $3, $5, ref None) }
| objid; LPAREN; args; RPAREN
    { `Dispatch (`Ident ("self", ref None), None, $1, $3, ref None) }
| IF; expr; THEN; expr; ELSE; expr; FI
    { `Cond ($2, $4, $6, ref None) }
| WHILE; expr; LOOP; expr; POOL
    { `Loop ($2, $4, ref None) }
| LBRACE; e = expr; SEMICOLON; es = exprs; RBRACE
    { `Block (e :: es, ref None) }
| LET; objinits; IN; expr
    { List.fold_right
	(fun (objid, typeid, ex) y ->
	  `Let (objid, typeid, ex, y, ref None)) $2 $4 }
| CASE; e = expr; OF; c = case; SEMICOLON; cs = cases; ESAC
    { `Case (e, c :: cs, ref None) }
| expr; PLUS;  expr { `Plus ($1, $3, ref None) }
| expr; MINUS; expr { `Minus ($1, $3, ref None) }
| expr; TIMES; expr { `Times ($1, $3, ref None) }
| expr; DIV;   expr { `Div ($1, $3, ref None) }
| expr; LT;    expr { `Lt ($1, $3, ref None) }
| expr; LE;    expr { `Le ($1, $3, ref None) }
| expr; EQ;    expr { `Eq ($1, $3, ref None) }
| NEW;      typeid { `New ($2, ref None) }
| ISVOID;   expr   { `Isvoid ($2, ref None) }
| COMPLMNT; expr   { `Complmnt ($2, ref None) }
| NOT;      expr   { `Not ($2, ref None) }
| LPAREN; expr; RPAREN { `Paren $2 }
| OBJECTID { `Ident ($1, ref None) }
| INT      { `Int ($1, ref None) }
| STRING   { `String ($1, ref None) }
| TRUE     { `Bool (true, ref None) }
| FALSE    { `Bool (false, ref None) }
