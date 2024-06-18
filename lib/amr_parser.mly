%{
  open Amr_ast
%}

%token <string> IDENT
%token <string> LABEL
%token <string> DATA
%token LPAREN RPAREN SLASH MINUS PLUS EOF

%start <Amr_ast.Ast.node> amr

%%

amr:
  | a = node EOF { a }

node:
  | LPAREN i = IDENT SLASH c = IDENT s = list(value) RPAREN    { {Ast.id = i; concept= c; next = s} }
;

value:
  | l = LABEL s = DATA   { (l, Ast.Data s) }
  | l = LABEL MINUS      { (l, Ast.Data "-") }
  | l = LABEL PLUS       { (l, Ast.Data "+") }
  | l = LABEL i = IDENT  { (l, Ast.Ref i) }
  | l = LABEL a = node    { (l, Ast.Node a) }
;