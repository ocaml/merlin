%token EOF
%token PARENS_OPEN PARENS_CLOSE
%token ARROW COMMA WILDCARD STAR
%token<string> WORD
%token<string> POLY

%start main
%type<Type_parsed.t> main

%%

main:
  | t=typ EOF { t }
;

typ:
  | t=typ2 { t }
  | a=typ2 ARROW b=typ { Type_parsed.Arrow (a, b) }
;

typ2:
  | xs=list1(typ1, STAR) { Type_parsed.tuple xs }
  ;

typ1:
  | { Type_parsed.Wildcard }
  | ts=typs { Type_parsed.tuple ts }
  | ts=typs w=WORD ws=list(WORD)
    {
      List.fold_left ( fun acc w ->
	  Type_parsed.Tycon (w, [acc])) (Type_parsed.Tycon (w, ts)) ws
    }
;

typ0:
  | WILDCARD { Type_parsed.Wildcard }
  | w=POLY { Type_parsed.Tyvar w }
  | w=WORD { Type_parsed.Tycon (w, []) }
;


typs:
  | t=typ0 { [t] }
  | PARENS_OPEN ts=list1(typ, COMMA) PARENS_CLOSE { ts }
;


list1(term, separator):
  | x=term { [x] }
  | x=term separator xs=list1(term, separator) { x::xs }
;

