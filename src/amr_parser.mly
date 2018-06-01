%{
  open Amr_types
%}

%token <string> IDENT
%token <string> LABEL
%token <string> STRING
%token <int> INT
%token LP RP SLASH MINUS

%start <Amr_types.Amr.node> amr

%%

amr:
  | LP i = IDENT SLASH c = IDENT s = list(value) RP { {Amr.id = i; concept= c; next = s} }
;

value:
  | l = LABEL i = INT    { (l, Amr.Int i) }
  | l = LABEL s = STRING { (l, Amr.String s) }
  | l = LABEL MINUS      { (l, Amr.Minus) }
  | l = LABEL i = IDENT  { (l, Amr.Ref i) }
  | l = LABEL a = amr    { (l, Amr.Node a) }
;