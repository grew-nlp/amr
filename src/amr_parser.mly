%{
  open Amr_types
%}

%token <string> IDENT
%token <string> LABEL
%token <string> DATA
%token LP RP SLASH MINUS PLUS

%start <Amr_types.Amr.node> amr

%%

amr:
  | LP i = IDENT SLASH c = IDENT s = list(value) RP { {Amr.id = i; concept= c; next = s} }
;

value:
  | l = LABEL s = DATA   { (l, Amr.Data s) }
  | l = LABEL MINUS      { (l, Amr.Minus) }
  | l = LABEL PLUS       { (l, Amr.Plus) }
  | l = LABEL i = IDENT  { (l, Amr.Ref i) }
  | l = LABEL a = amr    { (l, Amr.Node a) }
;