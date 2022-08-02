%{
  open Amr_ast
%}

%token <string> IDENT
%token <string> LABEL
%token <string> DATA
%token LP RP SLASH MINUS PLUS

%start <Amr_ast.Ast.node> amr

%%

amr:
  | LP i = IDENT SLASH c = IDENT s = list(value) RP { {Ast.id = i; concept= c; next = s} }
;

value:
  | l = LABEL s = DATA   { (l, Ast.Data s) }
  | l = LABEL MINUS      { (l, Ast.Data "-") }
  | l = LABEL PLUS       { (l, Ast.Data "+") }
  | l = LABEL i = IDENT  { (l, Ast.Ref i) }
  | l = LABEL a = amr    { (l, Ast.Node a) }
;