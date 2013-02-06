(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: parser.mly 12800 2012-07-30 18:59:07Z doligez $ *)

(* The parser definition *)
%{
  open Outline_utils
%}
(* Tokens *)

%token AMPERAMPER
%token AMPERSAND
%token AND
%token AS
%token ASSERT
%token BACKQUOTE
%token BANG
%token BAR
%token BARBAR
%token BARRBRACKET
%token BEGIN
%token <char> CHAR
%token CLASS
%token COLON
%token COLONCOLON
%token COLONEQUAL
%token COLONGREATER
%token COMMA
%token CONSTRAINT
%token DO
%token DONE
%token DOT
%token DOTDOT
%token DOWNTO
%token ELSE
%token END
%token EOF
%token EQUAL
%token EXCEPTION
%token EXTERNAL
%token FALSE
%token <string> FLOAT
%token FOR
%token FUN
%token FUNCTION
%token FUNCTOR
%token GREATER
%token GREATERRBRACE
%token GREATERRBRACKET
%token IF
%token IN
%token INCLUDE
%token <string> INFIXOP0
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
%token <string> INFIXOP4
%token INHERIT
%token INITIALIZER
%token <int> INT
%token <int32> INT32
%token <int64> INT64
%token <string> LABEL
%token LAZY
%token LBRACE
%token LBRACELESS
%token LBRACKET
%token LBRACKETBAR
%token LBRACKETLESS
%token LBRACKETGREATER
%token LESS
%token LESSMINUS
%token LET
%token <string> LIDENT
%token LPAREN
%token MATCH
%token METHOD
%token MINUS
%token MINUSDOT
%token MINUSGREATER
%token MODULE
%token MUTABLE
%token <nativeint> NATIVEINT
%token NEW
%token OBJECT
%token OF
%token OPEN
%token <string> OPTLABEL
%token OR
(* %token PARSER *)
%token PLUS
%token PLUSDOT
%token <string> PREFIXOP
%token PRIVATE
%token QUESTION
%token QUESTIONQUESTION
%token QUOTE
%token RBRACE
%token RBRACKET
%token REC
%token RPAREN
%token SEMI
%token SEMISEMI
%token SHARP
%token SIG
%token STAR
%token <string> STRING
%token STRUCT
%token THEN
%token TILDE
%token TO
%token TRUE
%token TRY
%token TYPE
%token <string> UIDENT
%token UNDERSCORE
%token VAL
%token VIRTUAL
%token WHEN
%token WHILE
%token WITH
%token <string * Location.t> COMMENT

%token LET_LWT
%token TRY_LWT
%token MATCH_LWT
%token FINALLY_LWT
%token FOR_LWT
%token WHILE_LWT

(* Precedences and associativities.

Tokens and rules have precedences.  A reduce/reduce conflict is resolved
in favor of the first rule (in source file order).  A shift/reduce conflict
is resolved by comparing the precedence and associativity of the token to
be shifted with those of the rule to be reduced.

By default, a rule has the precedence of its rightmost terminal (if any).

When there is a shift/reduce conflict between a rule and a token that
have the same precedence, it is resolved using the associativity:
if the token is left-associative, the parser will reduce; if
right-associative, the parser will shift; if non-associative,
the parser will declare a syntax error.

We will only use associativities with operators of the kind  x * x -> x
for example, in the rules of the form    expr: expr BINOP expr
in all other cases, we define two precedences if needed to resolve
conflicts.

The precedences must be listed from low to high.
*)

%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI                          (* below EQUAL ({ () }) *)
%nonassoc LET                           (* above SEMI ( ...; let ... in ...) *)
%nonassoc below_WITH
%nonassoc FUNCTION WITH                 (* below BAR  (match ... with ...) *)
%nonassoc FINALLY_LWT
%nonassoc AND             (* above WITH (module rec A: SIG with ... and ...) *)
%nonassoc THEN                          (* below ELSE (if ... then ...) *)
%nonassoc ELSE                          (* (if ... then ... else ...) *)
%nonassoc LESSMINUS                     (* below COLONEQUAL (lbl <- x := e) *)
%right    COLONEQUAL                    (* expr (e := e := e) *)
%nonassoc AS
%left     BAR                           (* pattern (p|p|p) *)
%nonassoc below_COMMA
%left     COMMA                         (* expr/expr_comma_list (e,e,e) *)
%right    MINUSGREATER                  (* core_type2 (t -> t -> t) *)
%right    OR BARBAR                     (* expr (e || e || e) *)
%right    AMPERSAND AMPERAMPER          (* expr (e && e && e) *)
%nonassoc below_EQUAL
%left     INFIXOP0 EQUAL LESS GREATER   (* expr (e OP e OP e) *)
%right    INFIXOP1                      (* expr (e OP e OP e) *)
%right    COLONCOLON                    (* expr (e :: e :: e) *)
%left     INFIXOP2 PLUS PLUSDOT MINUS MINUSDOT  (* expr (e OP e OP e) *)
%left     INFIXOP3 STAR                 (* expr (e OP e OP e) *)
%right    INFIXOP4                      (* expr (e OP e OP e) *)
%nonassoc prec_unary_minus prec_unary_plus (* unary - *)
%nonassoc prec_constant_constructor     (* cf. simple_expr (C versus C x) *)
%nonassoc prec_constr_appl              (* above AS BAR COLONCOLON COMMA *)
%nonassoc SHARP                         (* simple_expr/toplevel_directive *)
%nonassoc below_DOT
%nonassoc DOT
(* Finally, the first tokens of simple_expr are above everything else. *)
%nonassoc BACKQUOTE BANG BEGIN CHAR FALSE FLOAT INT INT32 INT64
          LBRACE LBRACELESS LBRACKET LBRACKETBAR LIDENT LPAREN
          NEW NATIVEINT PREFIXOP STRING TRUE UIDENT


(* Entry points *)

%start implementation                   (* for implementation files *)
%type <unit> implementation
%start interface                        (* for interface files *)
%type <unit> interface
%%

(* Entry points *)

implementation:
  | top_structure EOF                    { () }
  | AND                                  { emit_top Rollback $endpos }
  | BAR                                  { emit_top Rollback $endpos }
  | EOF                                  { () }
  | SEMISEMI                             { () }
;
interface:
    signature EOF                        { () }
;
top_structure:
    structure_item                        { () }
  | structure_item top_structure          { () }
  | END                                   { emit_top Leave_module $endpos }
;

(* Module expressions *)

emit_enter:
  { emit_top Enter_module $endpos }

module_expr:
    mod_longident
      { () }
  | STRUCT emit_enter structure END
      { () }
  (* | STRUCT emit_enter structure error
      { () } *)
  | FUNCTOR LPAREN UIDENT COLON module_type RPAREN MINUSGREATER module_expr
      { () }
  | module_expr LPAREN enter_sub module_expr leave_sub RPAREN
      { () }
  (* | module_expr LPAREN module_expr error
      { () } *)
  | LPAREN enter_sub module_expr leave_sub COLON module_type RPAREN
      { () }
  (* | LPAREN module_expr COLON module_type error
      { () } *)
  | LPAREN enter_sub module_expr leave_sub RPAREN
      { () }
  (* | LPAREN module_expr error
      { () } *)
  | LPAREN VAL expr RPAREN
      { () }
  | LPAREN VAL expr COLON package_type RPAREN
      { () }
  | LPAREN VAL expr COLON package_type COLONGREATER package_type RPAREN
      { () }
  | LPAREN VAL expr COLONGREATER package_type RPAREN
      { () }
  (* | LPAREN VAL expr COLON error
      { () } *)
  (* | LPAREN VAL expr COLONGREATER error
      { () } *)
  (* | LPAREN VAL expr error
      { () } *)
;
structure:
    structure_tail                              { () }
  | seq_expr structure_tail                     { () }
;
structure_tail:
    (* empty *)                                 { () }
  | SEMISEMI                                    { () }
  | SEMISEMI seq_expr structure_tail            { () }
  | SEMISEMI structure_item structure_tail      { () }
  | structure_item structure_tail               { () }
;

with_extension:
  | WITH LIDENT comma_ext_list { () }
  | { () }
;

comma_ext_list:
  | COMMA LIDENT comma_ext_list { () }
  | { () }

structure_item:
    LET enter_partial rec_flag let_bindings commit_partial leave_partial
      { emit_top Definition $endpos }
  | EXTERNAL val_ident COLON core_type EQUAL primitive_declaration
      { emit_top Definition $endpos }
  | TYPE type_declarations with_extension
      { emit_top Definition $endpos }
  | EXCEPTION UIDENT constructor_arguments
      { emit_top Definition $endpos }
  | EXCEPTION UIDENT EQUAL constr_longident
      { emit_top Definition $endpos }
  | MODULE UIDENT module_binding
      { emit_top Definition $endpos }
  | MODULE REC module_rec_bindings
      { emit_top Definition $endpos }
  | MODULE TYPE ident EQUAL module_type
      { emit_top Definition $endpos }
  | OPEN mod_longident
      { emit_top Definition $endpos }
  | CLASS class_declarations
      { emit_top Definition $endpos }
  | CLASS TYPE class_type_declarations
      { emit_top Definition $endpos }
      (*FIXME: Should be possible to handle INCLUDE interactively *)
  | INCLUDE enter_sub module_expr leave_sub 
      { emit_top Definition $endpos }
;
module_binding:
    EQUAL module_expr
      { () }
  | COLON module_type EQUAL module_expr
      { () }
  | LPAREN UIDENT COLON module_type RPAREN module_binding
      { () }
;
module_rec_bindings:
    module_rec_binding                            { () }
  | module_rec_bindings AND module_rec_binding    { () }
;
module_rec_binding:
    UIDENT COLON module_type EQUAL module_expr    { () }
;

(* Module types *)

module_type:
    mty_longident
      { () }
  | SIG signature END
      { () }
  (* | SIG signature error
      { () } *)
  | FUNCTOR LPAREN UIDENT COLON module_type RPAREN MINUSGREATER module_type
      %prec below_WITH
      { () }
  | module_type WITH with_constraints
      { () }
  | MODULE TYPE OF enter_sub module_expr leave_sub
      { () }
  | LPAREN module_type RPAREN
      { () }
  (* | LPAREN module_type error
      { () } *)
;
signature:
    (* empty *)                                 { () }
  | signature signature_item                    { () }
  | signature signature_item SEMISEMI           { () }
;
signature_item:
    VAL val_ident COLON core_type
      { () }
  | EXTERNAL val_ident COLON core_type EQUAL primitive_declaration
      { () }
  | TYPE type_declarations
      { () }
  | EXCEPTION UIDENT constructor_arguments
      { () }
  | MODULE UIDENT module_declaration
      { () }
  | MODULE REC module_rec_declarations
      { () }
  | MODULE TYPE ident
      { () }
  | MODULE TYPE ident EQUAL module_type
      { () }
  | OPEN mod_longident
      { () }
  | INCLUDE module_type
      { () }
  | CLASS class_descriptions
      { () }
  | CLASS TYPE class_type_declarations
      { () }
;

module_declaration:
    COLON module_type
      { () }
  | LPAREN UIDENT COLON module_type RPAREN module_declaration
      { () }
;
module_rec_declarations:
    module_rec_declaration                              { () }
  | module_rec_declarations AND module_rec_declaration  { () }
;
module_rec_declaration:
    UIDENT COLON module_type                            { () }
;

(* Class expressions *)

class_declarations:
    class_declarations AND class_declaration    { () }
  | class_declaration                           { () }
;
class_declaration:
    virtual_flag class_type_parameters LIDENT class_fun_binding
      { () }
;
class_fun_binding:
    EQUAL class_expr
      { () }
  | COLON class_type EQUAL class_expr
      { () }
  | labeled_simple_pattern class_fun_binding
      { () }
;
class_type_parameters:
    (*empty*)                                   { () }
  | LBRACKET type_parameter_list RBRACKET       { () }
;
class_fun_def:
    labeled_simple_pattern MINUSGREATER class_expr
      { () }
  | labeled_simple_pattern class_fun_def
      { () }
;
class_expr:
    class_simple_expr
      { () }
  | FUN class_fun_def
      { () }
  | class_simple_expr simple_labeled_expr_list
      { () }
  | LET rec_flag let_bindings IN class_expr
      { () }
;
class_simple_expr:
    LBRACKET core_type_comma_list RBRACKET class_longident
      { () }
  | class_longident
      { () }
  | OBJECT class_structure END
      { () }
  (* | OBJECT class_structure error
      { () } *)
  | LPAREN class_expr COLON class_type RPAREN
      { () }
  (* | LPAREN class_expr COLON class_type error
      { () } *)
  | LPAREN class_expr RPAREN
      { () }
  (* | LPAREN class_expr error
      { () } *)
;
class_structure:
    class_self_pattern class_fields
      { () }
;
class_self_pattern:
    LPAREN pattern RPAREN
      { () }
  | LPAREN pattern COLON core_type RPAREN
      { () }
  | (* empty *)
      { () }
;
class_fields:
    (* empty *)
      { () }
  | class_fields class_field
      { () }
;
class_field:
  | INHERIT override_flag class_expr parent_binder
      { () }
  | VAL virtual_value
      { () }
  | VAL value
      { () }
  | virtual_method
      { () }
  | concrete_method
      { () }
  | CONSTRAINT constrain_field
      { () }
  | INITIALIZER seq_expr
      { () }
;
parent_binder:
    AS LIDENT
          { () }
  | (* empty *)
          { () }
;
virtual_value:
    override_flag MUTABLE VIRTUAL label COLON core_type
      { () }
  | VIRTUAL mutable_flag label COLON core_type
      { () }
;
value:
    override_flag mutable_flag label EQUAL seq_expr
      { () }
  | override_flag mutable_flag label type_constraint EQUAL seq_expr
      { () }
;
virtual_method:
    METHOD override_flag PRIVATE VIRTUAL label COLON poly_type
      { () }
  | METHOD override_flag VIRTUAL private_flag label COLON poly_type
      { () }
;
concrete_method :
    METHOD override_flag private_flag label strict_binding
      { () }
  | METHOD override_flag private_flag label COLON poly_type EQUAL seq_expr
      { () }
  | METHOD override_flag private_flag label COLON TYPE lident_list
    DOT core_type EQUAL seq_expr
      { () }
;

(* Class types *)

class_type:
    class_signature
      { () }
  | QUESTION LIDENT COLON simple_core_type_or_tuple MINUSGREATER class_type
      { () }
  | OPTLABEL simple_core_type_or_tuple MINUSGREATER class_type
      { () }
  | LIDENT COLON simple_core_type_or_tuple MINUSGREATER class_type
      { () }
  | simple_core_type_or_tuple MINUSGREATER class_type
      { () }
;
class_signature:
    LBRACKET core_type_comma_list RBRACKET clty_longident
      { () }
  | clty_longident
      { () }
  | OBJECT class_sig_body END
      { () }
  (* | OBJECT class_sig_body error
      { () } *)
;
class_sig_body:
    class_self_type class_sig_fields
    { () }
;
class_self_type:
    LPAREN core_type RPAREN
      { () }
  | (* empty *)
      { () }
;
class_sig_fields:
    (* empty *)                                 { () }
| class_sig_fields class_sig_field     { () }
;
class_sig_field:
    INHERIT class_signature       { () }
  | VAL value_type              { () }
  | virtual_method_type         { () }
  | method_type                 { () }
  | CONSTRAINT constrain_field        { () }
;
value_type:
    VIRTUAL mutable_flag label COLON core_type
      { () }
  | MUTABLE virtual_flag label COLON core_type
      { () }
  | label COLON core_type
      { () }
;
method_type:
    METHOD private_flag label COLON poly_type
      { () }
;
virtual_method_type:
    METHOD PRIVATE VIRTUAL label COLON poly_type
      { () }
  | METHOD VIRTUAL private_flag label COLON poly_type
      { () }
;
constrain:
        core_type EQUAL core_type          { () }
;
constrain_field:
        core_type EQUAL core_type          { () }
;
class_descriptions:
    class_descriptions AND class_description    { () }
  | class_description                           { () }
;
class_description:
    virtual_flag class_type_parameters LIDENT COLON class_type
      { () }
;
class_type_declarations:
    class_type_declarations AND class_type_declaration  { () }
  | class_type_declaration                              { () }
;
class_type_declaration:
    virtual_flag class_type_parameters LIDENT EQUAL class_signature
      { () }
;

(* Core expressions *)

seq_expr:
  | expr        %prec below_SEMI  { () }
  | expr SEMI                     { () }
  | expr SEMI seq_expr            { () }
;
labeled_simple_pattern:
    QUESTION LPAREN label_let_pattern opt_default RPAREN
      { () }
  | QUESTION label_var
      { () }
  | OPTLABEL LPAREN let_pattern opt_default RPAREN
      { () }
  | OPTLABEL pattern_var
      { () }
  | TILDE LPAREN label_let_pattern RPAREN
      { () }
  | TILDE label_var
      { () }
  | LABEL simple_pattern
      { () }
  | simple_pattern
      { () }
;
pattern_var:
    LIDENT            { () }
  | UNDERSCORE        { () }
;
opt_default:
    (* empty *)                         { () }
  | EQUAL seq_expr                      { () }
;
label_let_pattern:
    label_var
      { () }
  | label_var COLON core_type
      { () }
;
label_var:
    LIDENT    { () }
;
let_pattern:
    pattern
      { () }
  | pattern COLON core_type
      { () }
;

(* Partially correct expressions support *)
enter_partial:
  | { enter_partial $startpos }
commit_partial:
  | { commit_partial $startpos }
leave_partial:
  | { leave_partial () }

expr:
    simple_expr
      { () }
  | simple_expr simple_labeled_expr_list
      { () }
  | LET enter_partial rec_flag let_bindings commit_partial IN seq_expr leave_partial
      { () }
  | LET_LWT rec_flag let_bindings IN seq_expr
      { () }
  | LET MODULE UIDENT enter_sub module_binding leave_sub IN seq_expr
      { () }
  | LET OPEN enter_partial mod_longident commit_partial IN seq_expr leave_partial 
      { () }
  | FUNCTION opt_bar match_cases
      { () }
  | FUN labeled_simple_pattern fun_def
      { () }
  | FUN LPAREN TYPE LIDENT RPAREN fun_def
      { () }
  | MATCH seq_expr WITH opt_bar match_cases
      { () }
  | MATCH_LWT seq_expr WITH opt_bar match_cases
      { () }
  | TRY seq_expr WITH opt_bar match_cases
      { () }
  | TRY_LWT seq_expr WITH opt_bar match_cases
      { () }
  | TRY_LWT seq_expr FINALLY_LWT seq_expr
      { () }
  | TRY_LWT seq_expr WITH opt_bar match_cases FINALLY_LWT seq_expr
      { () }
  (* | TRY seq_expr WITH error
      { () } *)
  | expr_comma_list %prec below_COMMA
      { () }
  | constr_longident simple_expr
      { () }
  | name_tag simple_expr
      { () }
  | IF seq_expr THEN expr ELSE expr
      { () }
  | IF seq_expr THEN expr
      { () }
  | WHILE seq_expr DO seq_expr DONE
      { () }
  | WHILE_LWT seq_expr DO seq_expr DONE
      { () }
  | FOR val_ident EQUAL seq_expr direction_flag seq_expr DO seq_expr DONE
      { () }
  | FOR_LWT val_ident EQUAL seq_expr direction_flag seq_expr DO seq_expr DONE
      { () }
  | FOR_LWT pattern IN seq_expr DO seq_expr DONE
      { () }
  | expr COLONCOLON expr
      { () }
  | LPAREN COLONCOLON RPAREN LPAREN expr COMMA expr RPAREN
      { () }
  | expr INFIXOP0 expr
      { () }
  | expr INFIXOP1 expr
      { () }
  | expr INFIXOP2 expr
      { () }
  | expr INFIXOP3 expr
      { () }
  | expr INFIXOP4 expr
      { () }
  | expr PLUS expr
      { () }
  | expr PLUSDOT expr
      { () }
  | expr MINUS expr
      { () }
  | expr MINUSDOT expr
      { () }
  | expr STAR expr
      { () }
  | expr EQUAL expr
      { () }
  | expr LESS expr
      { () }
  | expr GREATER expr
      { () }
  | expr OR expr
      { () }
  | expr BARBAR expr
      { () }
  | expr AMPERSAND expr
      { () }
  | expr AMPERAMPER expr
      { () }
  | expr COLONEQUAL expr
      { () }
  | subtractive expr %prec prec_unary_minus
      { () }
  | additive expr %prec prec_unary_plus
      { () }
  | simple_expr DOT label_longident LESSMINUS expr
      { () }
  | simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS expr
      { () }
  | simple_expr DOT LBRACKET seq_expr RBRACKET LESSMINUS expr
      { () }
  | simple_expr DOT LBRACE expr RBRACE LESSMINUS expr
      { () }
  | label LESSMINUS expr
      { () }
  | ASSERT simple_expr
      { () }
  | LAZY simple_expr
      { () }
  | OBJECT class_structure END
      { () }
  (* | OBJECT class_structure error
      { () } *)
;
simple_expr:
    val_longident
      { () }
  | constant
      { () }
  | constr_longident %prec prec_constant_constructor
      { () }
  | name_tag %prec prec_constant_constructor
      { () }
  | LPAREN seq_expr RPAREN
      { () }
  (* | LPAREN seq_expr error
      { () } *)
  | BEGIN seq_expr END
      { () }
  | BEGIN END
      { () }
  (* | BEGIN seq_expr error
      { () } *)
  | LPAREN seq_expr type_constraint RPAREN
      { () }
  | simple_expr DOT label_longident
      { () }
  | mod_longident DOT LPAREN seq_expr RPAREN
      { () }
  (* | mod_longident DOT LPAREN seq_expr error
      { () } *)
  | simple_expr DOT LPAREN seq_expr RPAREN
      { () }
  (* | simple_expr DOT LPAREN seq_expr error
      { () } *)
  | simple_expr DOT LBRACKET seq_expr RBRACKET
      { () }
  (* | simple_expr DOT LBRACKET seq_expr error
      { () } *)
  | simple_expr DOT LBRACE expr RBRACE
      { () }
  (* | simple_expr DOT LBRACE expr_comma_list error
      { () } *)
  | LBRACE record_expr RBRACE
      { () }
  (* | LBRACE record_expr error
      { () } *)
  | LBRACKETBAR expr_semi_list opt_semi BARRBRACKET
      { () }
  (* | LBRACKETBAR expr_semi_list opt_semi error
      { () } *)
  | LBRACKETBAR BARRBRACKET
      { () }
  | LBRACKET expr_semi_list opt_semi RBRACKET
      { () }
  (* | LBRACKET expr_semi_list opt_semi error
      { () } *)
  | PREFIXOP simple_expr
      { () }
  | BANG simple_expr
      { () }
  | NEW class_longident
      { () }
  | LBRACELESS field_expr_list opt_semi GREATERRBRACE
      { () }
  (* | LBRACELESS field_expr_list opt_semi error
      { () } *)
  | LBRACELESS GREATERRBRACE
      { () }
  | simple_expr SHARP label
      { () }
  | LPAREN MODULE enter_sub module_expr leave_sub RPAREN 
      { () }
  | LPAREN MODULE enter_sub module_expr leave_sub COLON package_type RPAREN
      { () }
  (* | LPAREN MODULE enter_sub module_expr leave_sub COLON error
      { () } *)
;
enter_sub:
  { enter_sub () }
leave_sub:
  { leave_sub () }

simple_labeled_expr_list:
    labeled_simple_expr
      { () }
  | simple_labeled_expr_list labeled_simple_expr
      { () }
;
labeled_simple_expr:
    simple_expr
      { () }
  | label_expr
      { () }
;
label_expr:
    LABEL simple_expr
      { () }
  | TILDE label_ident
      { () }
  | QUESTION label_ident
      { () }
  | OPTLABEL simple_expr
      { () }
;
label_ident:
    LIDENT   { () }
;
let_bindings:
    let_binding                                 { () }
  | let_bindings AND let_binding                { () }
;

lident_list:
    LIDENT                            { () }
  | LIDENT lident_list                { () }
;
let_binding:
    val_ident fun_binding
      { () }
  | val_ident COLON typevar_list DOT core_type EQUAL seq_expr
      { () }
  | val_ident COLON TYPE lident_list DOT core_type EQUAL seq_expr
      { () }
  | pattern EQUAL seq_expr
      { () }
;
fun_binding:
    strict_binding
      { () }
  | type_constraint EQUAL seq_expr
      { () }
;
strict_binding:
    EQUAL seq_expr
      { () }
  | labeled_simple_pattern fun_binding
      { () }
  | LPAREN TYPE LIDENT RPAREN fun_binding
      { () }
;
match_cases:
    pattern match_action                        { () }
  | match_cases BAR pattern match_action        { () }
;
fun_def:
    match_action                                { () }
  | labeled_simple_pattern fun_def
      { () }
  | LPAREN TYPE LIDENT RPAREN fun_def
      { () }
;
match_action:
    MINUSGREATER seq_expr                       { () }
  | WHEN seq_expr MINUSGREATER seq_expr         { () }
;
expr_comma_list:
    expr_comma_list COMMA expr                  { () }
  | expr COMMA expr                             { () }
;
record_expr:
    simple_expr WITH lbl_expr_list              { () }
  | lbl_expr_list                               { () }
;
lbl_expr_list:
     lbl_expr { () }
  |  lbl_expr SEMI lbl_expr_list { () }
  |  lbl_expr SEMI { () }
;
lbl_expr:
    label_longident EQUAL expr
      { () }
  | label_longident
      { () }
;
field_expr_list:
    label EQUAL expr
      { () }
  | field_expr_list SEMI label EQUAL expr
      { () }
;
expr_semi_list:
    expr                                        { () }
  | expr_semi_list SEMI expr                    { () }
;
type_constraint:
    COLON core_type                             { () }
  | COLON core_type COLONGREATER core_type      { () }
  | COLONGREATER core_type                      { () }
  | COLON error                                 { () }
  | COLONGREATER error                          { () }
;

(* Patterns *)

pattern:
    simple_pattern
      { () }
  | pattern AS val_ident
      { () }
  | pattern_comma_list  %prec below_COMMA
      { () }
  | constr_longident pattern %prec prec_constr_appl
      { () }
  | name_tag pattern %prec prec_constr_appl
      { () }
  | pattern COLONCOLON pattern
      { () }
  | LPAREN COLONCOLON RPAREN LPAREN pattern COMMA pattern RPAREN
      { () }
  | pattern BAR pattern
      { () }
  | LAZY simple_pattern
      { () }
;
simple_pattern:
    val_ident %prec below_EQUAL
      { () }
  | UNDERSCORE
      { () }
  | signed_constant
      { () }
  | CHAR DOTDOT CHAR
      { () }
  | constr_longident
      { () }
  | name_tag
      { () }
  | SHARP type_longident
      { () }
  | LBRACE lbl_pattern_list RBRACE
      { () }
  (* | LBRACE lbl_pattern_list error
      { () } *)
  | LBRACKET pattern_semi_list opt_semi RBRACKET
      { () }
  (* | LBRACKET pattern_semi_list opt_semi error
      { () } *)
  | LBRACKETBAR pattern_semi_list opt_semi BARRBRACKET
      { () }
  | LBRACKETBAR BARRBRACKET
      { () }
  (* | LBRACKETBAR pattern_semi_list opt_semi error
      { () } *)
  | LPAREN pattern RPAREN
      { () }
  (* | LPAREN pattern error
      { () } *)
  | LPAREN pattern COLON core_type RPAREN
      { () }
  (* | LPAREN pattern COLON core_type error
      { () } *)
  | LPAREN MODULE UIDENT RPAREN
      { () }
  | LPAREN MODULE UIDENT COLON package_type RPAREN
      { () }
  (* | LPAREN MODULE UIDENT COLON package_type error
      { () } *)
;

pattern_comma_list:
    pattern_comma_list COMMA pattern            { () }
  | pattern COMMA pattern                       { () }
;
pattern_semi_list:
    pattern                                     { () }
  | pattern_semi_list SEMI pattern              { () }
;
lbl_pattern_list:
     lbl_pattern { () }
  |  lbl_pattern SEMI { () }
  |  lbl_pattern SEMI UNDERSCORE opt_semi { () }
  |  lbl_pattern SEMI lbl_pattern_list { () }
;
lbl_pattern:
    label_longident EQUAL pattern
      { () }
  | label_longident
      { () }
;

(* Primitive declarations *)

primitive_declaration:
    STRING                                      { () }
  | STRING primitive_declaration                { () }
;

(* Type declarations *)

type_declarations:
    type_declaration                            { () }
  | type_declarations AND type_declaration      { () }
;

type_declaration:
    optional_type_parameters LIDENT type_kind constraints
      { () }
;
constraints:
        constraints CONSTRAINT constrain        { () }
      | (* empty *)                             { () }
;
type_kind:
    (*empty*)
      { () }
  | EQUAL core_type
      { () }
  | EQUAL PRIVATE core_type
      { () }
  | EQUAL constructor_declarations
      { () }
  | EQUAL PRIVATE constructor_declarations
      { () }
  | EQUAL private_flag BAR constructor_declarations
      { () }
  | EQUAL private_flag LBRACE label_declarations opt_semi RBRACE
      { () }
  | EQUAL core_type EQUAL private_flag opt_bar constructor_declarations
      { () }
  | EQUAL core_type EQUAL private_flag LBRACE label_declarations opt_semi RBRACE
      { () }
;
optional_type_parameters:
    (*empty*)                                   { () }
  | optional_type_parameter                              { () }
  | LPAREN optional_type_parameter_list RPAREN  { () }
;
optional_type_parameter:
    type_variance QUOTE ident                   { () }
  | type_variance UNDERSCORE                    { () }
;
optional_type_parameter_list:
    optional_type_parameter                              { () }
  | optional_type_parameter_list COMMA optional_type_parameter    { () }
;



type_parameters:
    (*empty*)                                   { () }
  | type_parameter                              { () }
  | LPAREN type_parameter_list RPAREN           { () }
;
type_parameter:
    type_variance QUOTE ident                   { () }
;
type_variance:
    (* empty *)                                 { () }
  | PLUS                                        { () }
  | MINUS                                       { () }
;
type_parameter_list:
    type_parameter                              { () }
  | type_parameter_list COMMA type_parameter    { () }
;
constructor_declarations:
    constructor_declaration                     { () }
  | constructor_declarations BAR constructor_declaration { () }
;
constructor_declaration:

  | constr_ident generalized_constructor_arguments
      { () }
;

constructor_arguments:
    (*empty*)                                   { () }
  | OF core_type_list                           { () }
;

generalized_constructor_arguments:
    (*empty*)                                   { () }
  | OF core_type_list                           { () }
  | COLON core_type_list MINUSGREATER simple_core_type
                                                { () }
  | COLON simple_core_type                      { () }
;



label_declarations:
    label_declaration                           { () }
  | label_declarations SEMI label_declaration   { () }
;
label_declaration:
    mutable_flag label COLON poly_type          { () }
;

(* "with" constraints (additional type equations over signature components) *)

with_constraints:
    with_constraint                             { () }
  | with_constraints AND with_constraint        { () }
;
with_constraint:
    TYPE type_parameters label_longident with_type_binder core_type constraints
      { () }
    (* used label_longident instead of type_longident to disallow
       functor applications in type path *)
  | TYPE type_parameters label_longident COLONEQUAL core_type
      { () }
  | MODULE mod_longident EQUAL mod_ext_longident
      { () }
  | MODULE mod_longident COLONEQUAL mod_ext_longident
      { () }
;
with_type_binder:
    EQUAL          { () }
  | EQUAL PRIVATE  { () }
;

(* Polymorphic types *)

typevar_list:
        QUOTE ident                             { () }
      | typevar_list QUOTE ident                { () }
;
poly_type:
        core_type
          { () }
      | typevar_list DOT core_type
          { () }
;

(* Core types *)

core_type:
    core_type2
      { () }
  | core_type2 AS QUOTE ident
      { () }
;
core_type2:
    simple_core_type_or_tuple
      { () }
  | QUESTION LIDENT COLON core_type2 MINUSGREATER core_type2
      { () }
  | OPTLABEL core_type2 MINUSGREATER core_type2
      { () }
  | LIDENT COLON core_type2 MINUSGREATER core_type2
      { () }
  | core_type2 MINUSGREATER core_type2
      { () }
;

simple_core_type:
    simple_core_type2
      { () }
  | LPAREN core_type_comma_list RPAREN
      { () }
;
simple_core_type2:
    QUOTE ident
      { () }
  | UNDERSCORE
      { () }
  | type_longident
      { () }
  | simple_core_type2 type_longident
      { () }
  | LPAREN core_type_comma_list RPAREN type_longident
      { () }
  | LESS meth_list GREATER
      { () }
  | LESS GREATER
      { () }
  | SHARP class_longident opt_present
      { () }
  | simple_core_type2 SHARP class_longident opt_present
      { () }
  | LPAREN core_type_comma_list RPAREN SHARP class_longident opt_present
      { () }
  | LBRACKET tag_field RBRACKET
      { () }
(* PR#3835: this is not LR(1), would need lookahead=2
  | LBRACKET simple_core_type2 RBRACKET
      { () }
*)
  | LBRACKET BAR row_field_list RBRACKET
      { () }
  | LBRACKET row_field BAR row_field_list RBRACKET
      { () }
  | LBRACKETGREATER opt_bar row_field_list RBRACKET
      { () }
  | LBRACKETGREATER RBRACKET
      { () }
  | LBRACKETLESS opt_bar row_field_list RBRACKET
      { () }
  | LBRACKETLESS opt_bar row_field_list GREATER name_tag_list RBRACKET
      { () }
  | LPAREN MODULE package_type RPAREN
      { () }
;
package_type:
    mty_longident { () }
  | mty_longident WITH package_type_cstrs { () }
;
package_type_cstr:
    TYPE label_longident EQUAL core_type { () }
;
package_type_cstrs:
    package_type_cstr { () }
  | package_type_cstr AND package_type_cstrs { () }
;
row_field_list:
    row_field                                   { () }
  | row_field_list BAR row_field                { () }
;
row_field:
    tag_field                                   { () }
  | simple_core_type2                           { () }
;
tag_field:
    name_tag OF opt_ampersand amper_type_list
      { () }
  | name_tag
      { () }
;
opt_ampersand:
    AMPERSAND                                   { () }
  | (* empty *)                                 { () }
;
amper_type_list:
    core_type                                   { () }
  | amper_type_list AMPERSAND core_type         { () }
;
opt_present:
    LBRACKETGREATER name_tag_list RBRACKET      { () }
  | (* empty *)                                 { () }
;
name_tag_list:
    name_tag                                    { () }
  | name_tag_list name_tag                      { () }
;
simple_core_type_or_tuple:
    simple_core_type                            { () }
  | simple_core_type STAR core_type_list
      { () }
;
core_type_comma_list:
    core_type                                   { () }
  | core_type_comma_list COMMA core_type        { () }
;
core_type_list:
    simple_core_type                            { () }
  | core_type_list STAR simple_core_type        { () }
;
meth_list:
    field SEMI meth_list                        { () }
  | field opt_semi                              { () }
  | DOTDOT                                      { () }
;
field:
    label COLON poly_type                       { () }
;
label:
    LIDENT                                      { () }
;

(* Constants *)

constant:
    INT                                         { () }
  | CHAR                                        { () }
  | STRING                                      { () }
  | FLOAT                                       { () }
  | INT32                                       { () }
  | INT64                                       { () }
  | NATIVEINT                                   { () }
;
signed_constant:
    constant                                    { () }
  | MINUS INT                                   { () }
  | MINUS FLOAT                                 { () }
  | MINUS INT32                                 { () }
  | MINUS INT64                                 { () }
  | MINUS NATIVEINT                             { () }
  | PLUS INT                                    { () }
  | PLUS FLOAT                                  { () }
  | PLUS INT32                                  { () }
  | PLUS INT64                                  { () }
  | PLUS NATIVEINT                              { () }
;

(* Identifiers and long identifiers *)

ident:
    UIDENT                                      { () }
  | LIDENT                                      { () }
;
val_ident:
    LIDENT                                      { () }
  | LPAREN operator RPAREN                      { () }
;
operator:
    PREFIXOP                                    { () }
  | INFIXOP0                                    { () }
  | INFIXOP1                                    { () }
  | INFIXOP2                                    { () }
  | INFIXOP3                                    { () }
  | INFIXOP4                                    { () }
  | BANG                                        { () }
  | PLUS                                        { () }
  | PLUSDOT                                     { () }
  | MINUS                                       { () }
  | MINUSDOT                                    { () }
  | STAR                                        { () }
  | EQUAL                                       { () }
  | LESS                                        { () }
  | GREATER                                     { () }
  | OR                                          { () }
  | BARBAR                                      { () }
  | AMPERSAND                                   { () }
  | AMPERAMPER                                  { () }
  | COLONEQUAL                                  { () }
;
constr_ident:
    UIDENT                                      { () }
(*  | LBRACKET RBRACKET                           { () } *)
  | LPAREN RPAREN                               { () }
  | COLONCOLON                                  { () }
(*  | LPAREN COLONCOLON RPAREN                    { () } *)
  | FALSE                                       { () }
  | TRUE                                        { () }
;

val_longident:
    val_ident                                   { () }
  | mod_longident DOT val_ident                 { () }
;
constr_longident:
    mod_longident       %prec below_DOT         { () }
  | LBRACKET RBRACKET                           { () }
  | LPAREN RPAREN                               { () }
  | FALSE                                       { () }
  | TRUE                                        { () }
;
label_longident:
    LIDENT                                      { () }
  | mod_longident DOT LIDENT                    { () }
;
type_longident:
    LIDENT                                      { () }
  | mod_ext_longident DOT LIDENT                { () }
;
mod_longident:
    UIDENT                                      { () }
  | mod_longident DOT UIDENT                    { () }
;
mod_ext_longident:
    UIDENT                                      { () }
  | mod_ext_longident DOT UIDENT                { () }
  | mod_ext_longident LPAREN mod_ext_longident RPAREN { () }
;
mty_longident:
    ident                                       { () }
  | mod_ext_longident DOT ident                 { () }
;
clty_longident:
    LIDENT                                      { () }
  | mod_ext_longident DOT LIDENT                { () }
;
class_longident:
    LIDENT                                      { () }
  | mod_longident DOT LIDENT                    { () }
;

(* Miscellaneous *)

name_tag:
    BACKQUOTE ident                             { () }
;
rec_flag:
    (* empty *)                                 { () }
  | REC                                         { () }
;
direction_flag:
    TO                                          { () }
  | DOWNTO                                      { () }
;
private_flag:
    (* empty *)                                 { () }
  | PRIVATE                                     { () }
;
mutable_flag:
    (* empty *)                                 { () }
  | MUTABLE                                     { () }
;
virtual_flag:
    (* empty *)                                 { () }
  | VIRTUAL                                     { () }
;
override_flag:
    (* empty *)                                 { () }
  | BANG                                        { () }
;
opt_bar:
    (* empty *)                                 { () }
  | BAR                                         { () }
;
opt_semi:
  | (* empty *)                                 { () }
  | SEMI                                        { () }
;
subtractive:
  | MINUS                                       { () }
  | MINUSDOT                                    { () }
;
additive:
  | PLUS                                        { () }
  | PLUSDOT                                     { () }
;
%%
