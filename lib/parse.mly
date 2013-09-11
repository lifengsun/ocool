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

%right    ASSIGN
%right    NOT
%nonassoc LE LT EQ
%left     PLUS MINUS
%left     TIMES DIV
%right    ISVOID
%right    TILDE
%left     AT
%left     DOT

%start <Cool.cls option> prog
%%
prog:
| EOF  { None }
| cls; SEMICOLON { Some $1 }

cls:
| CLASS; typeid; INHERITS; typeid; LBRACE; features; RBRACE
    { `Class ($2, Some $4, $6) }
| CLASS; typeid; LBRACE; features; RBRACE
    { `Class ($2, None, $4) }

typeid:
| TYPEID { $1 }
| SELF_TYPE { "SELF_TYPE" }

features:
| { [] }
| f = feature; SEMICOLON; fs = features { f :: fs }

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
| { [] }
| e = expr; SEMICOLON; es = exprs { e :: es }

case:
    objid; COLON; typeid; DARROW; expr { ($1, $3, $5) }

cases:
| { [] }
| c = case; SEMICOLON; cs = cases; { c :: cs }

expr:
| objid; ASSIGN; expr
    { `Assign ($1, $3) }
| expr; AT; typeid; DOT; objid; LPAREN; args; RPAREN
    { `DotMethod ($1, Some $3, $5, $7) }
| expr; DOT; objid; LPAREN; args; RPAREN
    { `DotMethod ($1, None, $3, $5) }
| objid; LPAREN; args; RPAREN
    { `Func ($1, $3) }
| IF; expr; THEN; expr; ELSE; expr; FI
    { `If ($2, $4, $6) }
| WHILE; expr; LOOP; expr; POOL
    { `While ($2, $4) }
| LBRACE; e = expr; SEMICOLON; es = exprs; RBRACE
    { `Block (e :: es) }
| LET; objinits; IN; expr
    { `Let ($2, $4) }
| CASE; e = expr; OF; c = case; SEMICOLON; cs = cases; ESAC
    { `Case (e, c :: cs) }
| expr; PLUS;  expr { `Plus ($1, $3) }
| expr; MINUS; expr { `Minus ($1, $3) }
| expr; TIMES; expr { `Times ($1, $3) }
| expr; DIV;   expr { `Div ($1, $3) }
| expr; LT;    expr { `Lt ($1, $3) }
| expr; LE;    expr { `Le ($1, $3) }
| expr; EQ;    expr { `Eq ($1, $3) }
| NEW;    typeid { `New $2 }
| ISVOID; expr   { `Isvoid $2 }
| TILDE;  expr   { `Comm $2 }
| NOT;    expr   { `Not $2 }
| LPAREN; expr; RPAREN { `PExpr $2 }
| OBJECTID { `Object $1 }
| INT      { `Int $1 }
| STRING   { `String $1 }
| TRUE     { `Bool true }
| FALSE    { `Bool false }
