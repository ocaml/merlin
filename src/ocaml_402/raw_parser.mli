exception Error

type token =
  | WITH
  | WHILE_LWT
  | WHILE
  | WHEN
  | VIRTUAL
  | VAL
  | UNDERSCORE
  | UIDENT of (string)
  | TYPE
  | TRY_LWT
  | TRY
  | TRUE
  | TO
  | TILDE
  | THEN
  | STRUCT
  | STRING of (string * string option)
  | STAR
  | SIG
  | SHARP
  | SEMISEMI
  | SEMI
  | RPAREN
  | REC
  | RBRACKET
  | RBRACE
  | QUOTE
  | QUESTION
  | PRIVATE
  | PREFIXOP of (string)
  | PLUSEQ
  | PLUSDOT
  | PLUS
  | PERCENT
  | P4_QUOTATION
  | OUNIT_TEST_UNIT
  | OUNIT_TEST_MODULE
  | OUNIT_TEST
  | OUNIT_BENCH_MODULE
  | OUNIT_BENCH_INDEXED
  | OUNIT_BENCH_FUN
  | OUNIT_BENCH
  | OR
  | OPTLABEL of (string)
  | OPEN
  | OF
  | OBJECT
  | NONREC
  | NEW
  | NATIVEINT of (nativeint)
  | MUTABLE
  | MODULE
  | MINUSGREATER
  | MINUSDOT
  | MINUS
  | METHOD
  | MATCH_LWT
  | MATCH
  | LPAREN
  | LIDENT of (string)
  | LET_LWT
  | LET
  | LESSMINUS
  | LESS
  | LBRACKETPERCENTPERCENT
  | LBRACKETPERCENT
  | LBRACKETLESS
  | LBRACKETGREATER
  | LBRACKETBAR
  | LBRACKETATATAT
  | LBRACKETATAT
  | LBRACKETAT
  | LBRACKET
  | LBRACELESS
  | LBRACE
  | LAZY
  | LABEL of (string)
  | JSNEW
  | INT64 of (int64)
  | INT32 of (int32)
  | INT of (int)
  | INITIALIZER
  | INHERIT
  | INFIXOP4 of (string)
  | INFIXOP3 of (string)
  | INFIXOP2 of (string)
  | INFIXOP1 of (string)
  | INFIXOP0 of (string)
  | INCLUDE
  | IN
  | IF
  | GREATERRBRACKET
  | GREATERRBRACE
  | GREATER
  | FUNCTOR
  | FUNCTION
  | FUN
  | FOR_LWT
  | FOR
  | FLOAT of (string)
  | FINALLY_LWT
  | FALSE
  | EXTERNAL
  | EXITPOINT
  | EXCEPTION
  | EQUAL
  | EOL
  | EOF
  | ENTRYPOINT
  | END
  | ELSE
  | DOWNTO
  | DOTDOT
  | DOT
  | DONE
  | DO
  | CUSTOM_BANG
  | CONSTRAINT
  | COMMENT of (string * Location.t)
  | COMMA
  | COLONGREATER
  | COLONEQUAL
  | COLONCOLON
  | COLON
  | CLASS
  | CHAR of (char)
  | BEGIN
  | BARRBRACKET
  | BARBAR
  | BAR
  | BANG
  | BACKQUOTE
  | ASSERT
  | AS
  | AND
  | AMPERSAND
  | AMPERAMPER

and _ token_class =
  | T_WITH : unit token_class
  | T_WHILE_LWT : unit token_class
  | T_WHILE : unit token_class
  | T_WHEN : unit token_class
  | T_VIRTUAL : unit token_class
  | T_VAL : unit token_class
  | T_UNDERSCORE : unit token_class
  | T_UIDENT : (string) token_class
  | T_TYPE : unit token_class
  | T_TRY_LWT : unit token_class
  | T_TRY : unit token_class
  | T_TRUE : unit token_class
  | T_TO : unit token_class
  | T_TILDE : unit token_class
  | T_THEN : unit token_class
  | T_STRUCT : unit token_class
  | T_STRING : (string * string option) token_class
  | T_STAR : unit token_class
  | T_SIG : unit token_class
  | T_SHARP : unit token_class
  | T_SEMISEMI : unit token_class
  | T_SEMI : unit token_class
  | T_RPAREN : unit token_class
  | T_REC : unit token_class
  | T_RBRACKET : unit token_class
  | T_RBRACE : unit token_class
  | T_QUOTE : unit token_class
  | T_QUESTION : unit token_class
  | T_PRIVATE : unit token_class
  | T_PREFIXOP : (string) token_class
  | T_PLUSEQ : unit token_class
  | T_PLUSDOT : unit token_class
  | T_PLUS : unit token_class
  | T_PERCENT : unit token_class
  | T_P4_QUOTATION : unit token_class
  | T_OUNIT_TEST_UNIT : unit token_class
  | T_OUNIT_TEST_MODULE : unit token_class
  | T_OUNIT_TEST : unit token_class
  | T_OUNIT_BENCH_MODULE : unit token_class
  | T_OUNIT_BENCH_INDEXED : unit token_class
  | T_OUNIT_BENCH_FUN : unit token_class
  | T_OUNIT_BENCH : unit token_class
  | T_OR : unit token_class
  | T_OPTLABEL : (string) token_class
  | T_OPEN : unit token_class
  | T_OF : unit token_class
  | T_OBJECT : unit token_class
  | T_NONREC : unit token_class
  | T_NEW : unit token_class
  | T_NATIVEINT : (nativeint) token_class
  | T_MUTABLE : unit token_class
  | T_MODULE : unit token_class
  | T_MINUSGREATER : unit token_class
  | T_MINUSDOT : unit token_class
  | T_MINUS : unit token_class
  | T_METHOD : unit token_class
  | T_MATCH_LWT : unit token_class
  | T_MATCH : unit token_class
  | T_LPAREN : unit token_class
  | T_LIDENT : (string) token_class
  | T_LET_LWT : unit token_class
  | T_LET : unit token_class
  | T_LESSMINUS : unit token_class
  | T_LESS : unit token_class
  | T_LBRACKETPERCENTPERCENT : unit token_class
  | T_LBRACKETPERCENT : unit token_class
  | T_LBRACKETLESS : unit token_class
  | T_LBRACKETGREATER : unit token_class
  | T_LBRACKETBAR : unit token_class
  | T_LBRACKETATATAT : unit token_class
  | T_LBRACKETATAT : unit token_class
  | T_LBRACKETAT : unit token_class
  | T_LBRACKET : unit token_class
  | T_LBRACELESS : unit token_class
  | T_LBRACE : unit token_class
  | T_LAZY : unit token_class
  | T_LABEL : (string) token_class
  | T_JSNEW : unit token_class
  | T_INT64 : (int64) token_class
  | T_INT32 : (int32) token_class
  | T_INT : (int) token_class
  | T_INITIALIZER : unit token_class
  | T_INHERIT : unit token_class
  | T_INFIXOP4 : (string) token_class
  | T_INFIXOP3 : (string) token_class
  | T_INFIXOP2 : (string) token_class
  | T_INFIXOP1 : (string) token_class
  | T_INFIXOP0 : (string) token_class
  | T_INCLUDE : unit token_class
  | T_IN : unit token_class
  | T_IF : unit token_class
  | T_GREATERRBRACKET : unit token_class
  | T_GREATERRBRACE : unit token_class
  | T_GREATER : unit token_class
  | T_FUNCTOR : unit token_class
  | T_FUNCTION : unit token_class
  | T_FUN : unit token_class
  | T_FOR_LWT : unit token_class
  | T_FOR : unit token_class
  | T_FLOAT : (string) token_class
  | T_FINALLY_LWT : unit token_class
  | T_FALSE : unit token_class
  | T_EXTERNAL : unit token_class
  | T_EXITPOINT : unit token_class
  | T_EXCEPTION : unit token_class
  | T_EQUAL : unit token_class
  | T_EOL : unit token_class
  | T_EOF : unit token_class
  | T_ENTRYPOINT : unit token_class
  | T_END : unit token_class
  | T_ELSE : unit token_class
  | T_DOWNTO : unit token_class
  | T_DOTDOT : unit token_class
  | T_DOT : unit token_class
  | T_DONE : unit token_class
  | T_DO : unit token_class
  | T_CUSTOM_BANG : unit token_class
  | T_CONSTRAINT : unit token_class
  | T_COMMENT : (string * Location.t) token_class
  | T_COMMA : unit token_class
  | T_COLONGREATER : unit token_class
  | T_COLONEQUAL : unit token_class
  | T_COLONCOLON : unit token_class
  | T_COLON : unit token_class
  | T_CLASS : unit token_class
  | T_CHAR : (char) token_class
  | T_BEGIN : unit token_class
  | T_BARRBRACKET : unit token_class
  | T_BARBAR : unit token_class
  | T_BAR : unit token_class
  | T_BANG : unit token_class
  | T_BACKQUOTE : unit token_class
  | T_ASSERT : unit token_class
  | T_AS : unit token_class
  | T_AND : unit token_class
  | T_AMPERSAND : unit token_class
  | T_AMPERAMPER : unit token_class

and _ nonterminal_class =
  | N_with_type_binder : (Asttypes.private_flag) nonterminal_class
  | N_with_extensions : (Fake.TypeWith.generator list) nonterminal_class
  | N_with_constraints : (Parsetree.with_constraint list) nonterminal_class
  | N_with_constraint : (Parsetree.with_constraint list) nonterminal_class
  | N_virtual_flag : (Asttypes.virtual_flag) nonterminal_class
  | N_value_type : (string * Asttypes.mutable_flag * Asttypes.virtual_flag *
  Parsetree.core_type) nonterminal_class
  | N_value : (string Asttypes.loc * Asttypes.mutable_flag * Parsetree.class_field_kind) nonterminal_class
  | N_val_longident : (Longident.t) nonterminal_class
  | N_val_ident : (string) nonterminal_class
  | N_typevar_list : (Asttypes.label list) nonterminal_class
  | N_type_variance : (Asttypes.variance) nonterminal_class
  | N_type_variable : (Parsetree.core_type) nonterminal_class
  | N_type_parameters : ((Parsetree.core_type * Asttypes.variance) list) nonterminal_class
  | N_type_parameter_list : ((Parsetree.core_type * Asttypes.variance) list) nonterminal_class
  | N_type_parameter : (Parsetree.core_type * Asttypes.variance) nonterminal_class
  | N_type_longident : (Longident.t) nonterminal_class
  | N_type_kind : (Parsetree.type_kind * Asttypes.private_flag * Parsetree.core_type option) nonterminal_class
  | N_type_declarations : (Parsetree.type_declaration list) nonterminal_class
  | N_type_declaration : (Parsetree.type_declaration) nonterminal_class
  | N_type_constraint : (Parsetree.core_type option * Parsetree.core_type option) nonterminal_class
  | N_toplevel_directives : (unit) nonterminal_class
  | N_tag_field : (Parsetree.row_field) nonterminal_class
  | N_subtractive : (string) nonterminal_class
  | N_structure_tail : (Parsetree.structure) nonterminal_class
  | N_structure_item : (Parsetree.structure_item list) nonterminal_class
  | N_structure_head : (Parsetree.structure) nonterminal_class
  | N_structure : (Parsetree.structure) nonterminal_class
  | N_strict_binding : (Parsetree.expression) nonterminal_class
  | N_str_type_extension : (Parsetree.type_extension) nonterminal_class
  | N_str_extension_constructors : (Parsetree.extension_constructor list) nonterminal_class
  | N_str_exception_declaration : (Parsetree.extension_constructor) nonterminal_class
  | N_single_attr_id : (string) nonterminal_class
  | N_simple_pattern_not_ident : (Parsetree.pattern) nonterminal_class
  | N_simple_pattern : (Parsetree.pattern) nonterminal_class
  | N_simple_labeled_expr_list : ((Asttypes.label * Parsetree.expression) list) nonterminal_class
  | N_simple_expr : (Parsetree.expression) nonterminal_class
  | N_simple_core_type_or_tuple_no_attr : (Parsetree.core_type) nonterminal_class
  | N_simple_core_type_or_tuple : (Parsetree.core_type) nonterminal_class
  | N_simple_core_type_no_attr : (Parsetree.core_type) nonterminal_class
  | N_simple_core_type2 : (Parsetree.core_type) nonterminal_class
  | N_simple_core_type : (Parsetree.core_type) nonterminal_class
  | N_signed_constant : (Asttypes.constant) nonterminal_class
  | N_signature_item : (Parsetree.signature_item list) nonterminal_class
  | N_signature : (Parsetree.signature) nonterminal_class
  | N_sig_type_extension : (Parsetree.type_extension) nonterminal_class
  | N_sig_extension_constructors : (Parsetree.extension_constructor list) nonterminal_class
  | N_sig_exception_declaration : (Parsetree.extension_constructor) nonterminal_class
  | N_seq_expr : (Parsetree.expression) nonterminal_class
  | N_row_field_list : (Parsetree.row_field list) nonterminal_class
  | N_row_field : (Parsetree.row_field) nonterminal_class
  | N_record_expr : (Parsetree.expression option *
  (Longident.t Asttypes.loc * Parsetree.expression) list) nonterminal_class
  | N_rec_flag : (Asttypes.rec_flag) nonterminal_class
  | N_private_virtual_flags : (Asttypes.private_flag * Asttypes.virtual_flag) nonterminal_class
  | N_private_flag : (Asttypes.private_flag) nonterminal_class
  | N_primitive_declaration : (string list) nonterminal_class
  | N_post_item_attributes : (Ast_helper.attrs) nonterminal_class
  | N_post_item_attribute : (Parsetree.attribute) nonterminal_class
  | N_poly_type : (Parsetree.core_type) nonterminal_class
  | N_payload : (Parsetree.payload) nonterminal_class
  | N_pattern_var : (Parsetree.pattern) nonterminal_class
  | N_pattern_semi_list : (Parsetree.pattern list) nonterminal_class
  | N_pattern_comma_list : (Parsetree.pattern list) nonterminal_class
  | N_pattern : (Parsetree.pattern) nonterminal_class
  | N_parse_expression : (Parsetree.expression) nonterminal_class
  | N_parent_binder : (string option) nonterminal_class
  | N_package_type_cstrs : ((Longident.t Asttypes.loc * Parsetree.core_type) list) nonterminal_class
  | N_package_type_cstr : (Longident.t Asttypes.loc * Parsetree.core_type) nonterminal_class
  | N_package_type : (Parsetree.package_type) nonterminal_class
  | N_override_flag : (Asttypes.override_flag) nonterminal_class
  | N_optional_type_variable : (Parsetree.core_type) nonterminal_class
  | N_optional_type_parameters : ((Parsetree.core_type * Asttypes.variance) list) nonterminal_class
  | N_optional_type_parameter_list : ((Parsetree.core_type * Asttypes.variance) list) nonterminal_class
  | N_optional_type_parameter : (Parsetree.core_type * Asttypes.variance) nonterminal_class
  | N_option_STRING_ : ((string * string option) option) nonterminal_class
  | N_opt_semi : (unit) nonterminal_class
  | N_opt_default : (Parsetree.expression option) nonterminal_class
  | N_opt_bar : (unit) nonterminal_class
  | N_opt_ampersand : (bool) nonterminal_class
  | N_operator : (string) nonterminal_class
  | N_open_statement : (Parsetree.open_description) nonterminal_class
  | N_newtype : (string) nonterminal_class
  | N_name_tag_list : (Asttypes.label list) nonterminal_class
  | N_name_tag : (Asttypes.label) nonterminal_class
  | N_mutable_flag : (Asttypes.mutable_flag) nonterminal_class
  | N_mty_longident : (Longident.t) nonterminal_class
  | N_module_type : (Parsetree.module_type) nonterminal_class
  | N_module_rec_declarations : (Parsetree.module_declaration list) nonterminal_class
  | N_module_rec_declaration : (Parsetree.module_declaration) nonterminal_class
  | N_module_expr : (Parsetree.module_expr) nonterminal_class
  | N_module_declaration : (Parsetree.module_type) nonterminal_class
  | N_module_bindings : (Parsetree.module_binding list) nonterminal_class
  | N_module_binding_body : (Parsetree.module_expr) nonterminal_class
  | N_module_binding : (Parsetree.module_binding) nonterminal_class
  | N_mod_longident : (Longident.t) nonterminal_class
  | N_mod_ext_longident : (Longident.t) nonterminal_class
  | N_method_ : (string Asttypes.loc * Asttypes.private_flag * Parsetree.class_field_kind) nonterminal_class
  | N_meth_list : ((string * Parsetree.attributes * Parsetree.core_type) list *
  Asttypes.closed_flag) nonterminal_class
  | N_match_cases : (Parsetree.case list) nonterminal_class
  | N_match_case : (Parsetree.case) nonterminal_class
  | N_lident_list : (string list) nonterminal_class
  | N_let_pattern : (Parsetree.pattern) nonterminal_class
  | N_let_bindings_no_attrs : (Parsetree.value_binding list) nonterminal_class
  | N_let_bindings : (Parsetree.value_binding list) nonterminal_class
  | N_let_binding_ : (Parsetree.pattern * Parsetree.expression) nonterminal_class
  | N_let_binding : (Parsetree.value_binding) nonterminal_class
  | N_lbl_pattern_list : ((Longident.t Asttypes.loc * Parsetree.pattern) list * Asttypes.closed_flag) nonterminal_class
  | N_lbl_pattern : (Longident.t Asttypes.loc * Parsetree.pattern) nonterminal_class
  | N_lbl_expr_list : ((Longident.t Asttypes.loc * Parsetree.expression) list) nonterminal_class
  | N_lbl_expr : (Longident.t Asttypes.loc * Parsetree.expression) nonterminal_class
  | N_labeled_simple_pattern : (Asttypes.label * Parsetree.expression option * Parsetree.pattern) nonterminal_class
  | N_labeled_simple_expr : (Asttypes.label * Parsetree.expression) nonterminal_class
  | N_label_var : (Asttypes.label * Parsetree.pattern) nonterminal_class
  | N_label_longident : (Longident.t) nonterminal_class
  | N_label_let_pattern : (Asttypes.label * Parsetree.pattern) nonterminal_class
  | N_label_ident : (Asttypes.label * Parsetree.expression) nonterminal_class
  | N_label_expr : (Asttypes.label * Parsetree.expression) nonterminal_class
  | N_label_declarations : (Parsetree.label_declaration list) nonterminal_class
  | N_label_declaration : (Parsetree.label_declaration) nonterminal_class
  | N_label : (string) nonterminal_class
  | N_item_extension : (Parsetree.extension) nonterminal_class
  | N_interface : (Parsetree.signature) nonterminal_class
  | N_implementation : (Parsetree.structure) nonterminal_class
  | N_ident : (Asttypes.label) nonterminal_class
  | N_generalized_constructor_arguments : (Parsetree.core_type list * Parsetree.core_type option) nonterminal_class
  | N_functor_args : ((string Asttypes.loc * Parsetree.module_type option) list) nonterminal_class
  | N_functor_arg_name : (string) nonterminal_class
  | N_functor_arg : (string Asttypes.loc * Parsetree.module_type option) nonterminal_class
  | N_fun_def : (Parsetree.expression) nonterminal_class
  | N_fun_binding : (Parsetree.expression) nonterminal_class
  | N_floating_attribute : (Parsetree.attribute) nonterminal_class
  | N_field_expr_list : ((string Asttypes.loc * Parsetree.expression) list) nonterminal_class
  | N_field : (string * Parsetree.attributes * Parsetree.core_type) nonterminal_class
  | N_extension_constructor_rebind : (Parsetree.extension_constructor) nonterminal_class
  | N_extension_constructor_declaration : (Parsetree.extension_constructor) nonterminal_class
  | N_extension : (Parsetree.extension) nonterminal_class
  | N_ext_attributes : (string Asttypes.loc option * Parsetree.attributes) nonterminal_class
  | N_expr_semi_list : (Parsetree.expression list) nonterminal_class
  | N_expr_open : (Asttypes.override_flag * Longident.t Asttypes.loc *
  (string Asttypes.loc option * Parsetree.attributes)) nonterminal_class
  | N_expr_comma_opt_list : (Parsetree.expression list) nonterminal_class
  | N_expr_comma_list : (Parsetree.expression list) nonterminal_class
  | N_expr : (Parsetree.expression) nonterminal_class
  | N_dummy : (unit) nonterminal_class
  | N_direction_flag : (Asttypes.direction_flag) nonterminal_class
  | N_core_type_list_no_attr : (Parsetree.core_type list) nonterminal_class
  | N_core_type_list : (Parsetree.core_type list) nonterminal_class
  | N_core_type_comma_list : (Parsetree.core_type list) nonterminal_class
  | N_core_type2 : (Parsetree.core_type) nonterminal_class
  | N_core_type : (Parsetree.core_type) nonterminal_class
  | N_constructor_declarations : (Parsetree.constructor_declaration list) nonterminal_class
  | N_constructor_declaration : (Parsetree.constructor_declaration) nonterminal_class
  | N_constraints : ((Parsetree.core_type * Parsetree.core_type * Ast_helper.loc) list) nonterminal_class
  | N_constrain_field : (Parsetree.core_type * Parsetree.core_type) nonterminal_class
  | N_constrain : (Parsetree.core_type * Parsetree.core_type * Ast_helper.loc) nonterminal_class
  | N_constr_longident : (Longident.t) nonterminal_class
  | N_constr_ident : (string) nonterminal_class
  | N_constant : (Asttypes.constant) nonterminal_class
  | N_clty_longident : (Longident.t) nonterminal_class
  | N_class_type_parameters : ((Parsetree.core_type * Asttypes.variance) list) nonterminal_class
  | N_class_type_declarations : (Parsetree.class_type_declaration list) nonterminal_class
  | N_class_type_declaration : (Parsetree.class_type_declaration list) nonterminal_class
  | N_class_type : (Parsetree.class_type) nonterminal_class
  | N_class_structure : (Parsetree.class_structure) nonterminal_class
  | N_class_simple_expr : (Parsetree.class_expr) nonterminal_class
  | N_class_signature : (Parsetree.class_type) nonterminal_class
  | N_class_sig_fields : (Parsetree.class_type_field list) nonterminal_class
  | N_class_sig_field : (Parsetree.class_type_field) nonterminal_class
  | N_class_sig_body : (Parsetree.class_signature) nonterminal_class
  | N_class_self_type : (Parsetree.core_type) nonterminal_class
  | N_class_self_pattern : (Parsetree.pattern) nonterminal_class
  | N_class_longident : (Longident.t) nonterminal_class
  | N_class_fun_def : (Parsetree.class_expr) nonterminal_class
  | N_class_fun_binding : (Parsetree.class_expr) nonterminal_class
  | N_class_fields : (Parsetree.class_field list) nonterminal_class
  | N_class_field : (Parsetree.class_field list) nonterminal_class
  | N_class_expr : (Parsetree.class_expr) nonterminal_class
  | N_class_descriptions : (Parsetree.class_description list) nonterminal_class
  | N_class_description : (Parsetree.class_description list) nonterminal_class
  | N_class_declarations : (Parsetree.class_declaration list) nonterminal_class
  | N_class_declaration : (Parsetree.class_declaration list) nonterminal_class
  | N_attributes : (Parsetree.attributes) nonterminal_class
  | N_attribute : (Parsetree.attribute) nonterminal_class
  | N_attr_id : (string Asttypes.loc) nonterminal_class
  | N_amper_type_list : (Parsetree.core_type list) nonterminal_class
  | N_additive : (string) nonterminal_class

and annotation = ([ `Shift of int | `Shift_token of int * token | `Cost of int
        | `Indent of int
        | `Unclosed of string | `Close
        | `Item of string ])

and symbol_class =
  | CT_ : 'a token_class * annotation list -> symbol_class
  | CN_ : 'a nonterminal_class * annotation list -> symbol_class

and symbol =
  | T_ : 'a token_class * 'a -> symbol
  | N_ : 'a nonterminal_class * 'a -> symbol
  | Bottom

and state = int

and feed = [ `Feed | `Feed_error ]

and step = [ `Step_run | `Step_error | `Step_action ]

and 'a parser = {
  env: (state, symbol, token) MenhirLib.EngineTypes.env;
  tag: 'a
}


val parse_expression: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.expression)
val interface: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.signature)
val implementation: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.structure)
val dummy: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (unit)
val initial: state -> Lexing.position * token * Lexing.position -> step parser
val step: step parser ->   [ `Step of step parser
  | `Feed of feed parser
  | `Accept of symbol
  | `Reject of step parser ]
val feed: feed parser -> Lexing.position * token * Lexing.position -> step parser
val parse_expression_state: state
val interface_state: state
val implementation_state: state
val dummy_state: state

module Query : MenhirLib.EngineTypes.QUERY_ENGINE
   with type production := int
    and type producer := symbol_class
    and type annotation := annotation
    and type semantic_action =
               (state, symbol, token) MenhirLib.EngineTypes.env ->
               (state, symbol) MenhirLib.EngineTypes.stack

module MenhirInterpreterTable : MenhirLib.TableFormat.TABLES
  with type token = token
   and type semantic_value = symbol
