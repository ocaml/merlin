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
  | DEFAULT
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

and nonterminal = 
  | NT'with_type_binder of (Asttypes.private_flag)
  | NT'with_constraints of (Parsetree.with_constraint list)
  | NT'with_constraint of (Parsetree.with_constraint)
  | NT'virtual_flag of (Asttypes.virtual_flag)
  | NT'value_type of (string * Asttypes.mutable_flag * Asttypes.virtual_flag *
  Parsetree.core_type)
  | NT'value of (string Asttypes.loc * Asttypes.mutable_flag * Parsetree.class_field_kind)
  | NT'val_longident of (Longident.t)
  | NT'val_ident of (string)
  | NT'typevar_list of (Asttypes.label list)
  | NT'type_variance of (Asttypes.variance)
  | NT'type_variable of (Parsetree.core_type)
  | NT'type_parameters of ((Parsetree.core_type * Asttypes.variance) list)
  | NT'type_parameter_list of ((Parsetree.core_type * Asttypes.variance) list)
  | NT'type_parameter of (Parsetree.core_type * Asttypes.variance)
  | NT'type_longident of (Longident.t)
  | NT'type_kind of (Parsetree.type_kind * Asttypes.private_flag * Parsetree.core_type option)
  | NT'type_declarations of (Parsetree.type_declaration list)
  | NT'type_declaration of (Parsetree.type_declaration)
  | NT'type_constraint of (Parsetree.core_type option * Parsetree.core_type option)
  | NT'tag_field of (Parsetree.row_field)
  | NT'subtractive of (string)
  | NT'structure_tail of (Parsetree.structure)
  | NT'structure_item of (Parsetree.structure_item list)
  | NT'structure of (Parsetree.structure)
  | NT'strict_binding of (Parsetree.expression)
  | NT'str_type_extension of (Parsetree.type_extension)
  | NT'str_extension_constructors of (Parsetree.extension_constructor list)
  | NT'str_exception_declaration of (Parsetree.extension_constructor)
  | NT'single_attr_id of (string)
  | NT'simple_pattern_not_ident of (Parsetree.pattern)
  | NT'simple_pattern of (Parsetree.pattern)
  | NT'simple_labeled_expr_list of ((Asttypes.label * Parsetree.expression) list)
  | NT'simple_expr of (Parsetree.expression)
  | NT'simple_core_type_or_tuple_no_attr of (Parsetree.core_type)
  | NT'simple_core_type_or_tuple of (Parsetree.core_type)
  | NT'simple_core_type_no_attr of (Parsetree.core_type)
  | NT'simple_core_type2 of (Parsetree.core_type)
  | NT'simple_core_type of (Parsetree.core_type)
  | NT'signed_constant of (Asttypes.constant)
  | NT'signature_item of (Parsetree.signature_item list)
  | NT'signature of (Parsetree.signature)
  | NT'sig_type_extension of (Parsetree.type_extension)
  | NT'sig_extension_constructors of (Parsetree.extension_constructor list)
  | NT'sig_exception_declaration of (Parsetree.extension_constructor)
  | NT'seq_expr of (Parsetree.expression)
  | NT'row_field_list of (Parsetree.row_field list)
  | NT'row_field of (Parsetree.row_field)
  | NT'record_expr of (Parsetree.expression option *
  (Longident.t Asttypes.loc * Parsetree.expression) list)
  | NT'rec_flag of (Asttypes.rec_flag)
  | NT'private_virtual_flags of (Asttypes.private_flag * Asttypes.virtual_flag)
  | NT'private_flag of (Asttypes.private_flag)
  | NT'primitive_declaration of (string list)
  | NT'post_item_attributes of (Ast_helper.attrs)
  | NT'post_item_attribute of (Parsetree.attribute)
  | NT'poly_type of (Parsetree.core_type)
  | NT'payload of (Parsetree.payload)
  | NT'pattern_var of (Parsetree.pattern)
  | NT'pattern_semi_list of (Parsetree.pattern list)
  | NT'pattern_comma_list of (Parsetree.pattern list)
  | NT'pattern of (Parsetree.pattern)
  | NT'parse_expression of (Parsetree.expression)
  | NT'parent_binder of (string option)
  | NT'package_type_cstrs of ((Longident.t Asttypes.loc * Parsetree.core_type) list)
  | NT'package_type_cstr of (Longident.t Asttypes.loc * Parsetree.core_type)
  | NT'package_type of (Parsetree.package_type)
  | NT'override_flag of (Asttypes.override_flag)
  | NT'optional_type_variable of (Parsetree.core_type)
  | NT'optional_type_parameters of ((Parsetree.core_type * Asttypes.variance) list)
  | NT'optional_type_parameter_list of ((Parsetree.core_type * Asttypes.variance) list)
  | NT'optional_type_parameter of (Parsetree.core_type * Asttypes.variance)
  | NT'opt_semi of (unit)
  | NT'opt_default of (Parsetree.expression option)
  | NT'opt_bar of (unit)
  | NT'opt_ampersand of (bool)
  | NT'operator of (string)
  | NT'open_statement of (Parsetree.open_description)
  | NT'newtype of (string)
  | NT'name_tag_list of (Asttypes.label list)
  | NT'name_tag of (Asttypes.label)
  | NT'mutable_flag of (Asttypes.mutable_flag)
  | NT'mty_longident of (Longident.t)
  | NT'module_type of (Parsetree.module_type)
  | NT'module_rec_declarations of (Parsetree.module_declaration list)
  | NT'module_rec_declaration of (Parsetree.module_declaration)
  | NT'module_expr of (Parsetree.module_expr)
  | NT'module_declaration of (Parsetree.module_type)
  | NT'module_bindings of (Parsetree.module_binding list)
  | NT'module_binding_body of (Parsetree.module_expr)
  | NT'module_binding of (Parsetree.module_binding)
  | NT'mod_longident of (Longident.t)
  | NT'mod_ext_longident of (Longident.t)
  | NT'method_ of (string Asttypes.loc * Asttypes.private_flag * Parsetree.class_field_kind)
  | NT'meth_list of ((string * Parsetree.attributes * Parsetree.core_type) list *
  Asttypes.closed_flag)
  | NT'match_cases of (Parsetree.case list)
  | NT'match_case of (Parsetree.case)
  | NT'lident_list of (string list)
  | NT'let_pattern of (Parsetree.pattern)
  | NT'let_bindings of (Parsetree.value_binding list)
  | NT'let_binding_ of (Parsetree.pattern * Parsetree.expression)
  | NT'let_binding of (Parsetree.value_binding)
  | NT'lbl_pattern_list of ((Longident.t Asttypes.loc * Parsetree.pattern) list * Asttypes.closed_flag)
  | NT'lbl_pattern of (Longident.t Asttypes.loc * Parsetree.pattern)
  | NT'lbl_expr_list of ((Longident.t Asttypes.loc * Parsetree.expression) list)
  | NT'lbl_expr of (Longident.t Asttypes.loc * Parsetree.expression)
  | NT'labeled_simple_pattern of (Asttypes.label * Parsetree.expression option * Parsetree.pattern)
  | NT'labeled_simple_expr of (Asttypes.label * Parsetree.expression)
  | NT'label_var of (Asttypes.label * Parsetree.pattern)
  | NT'label_longident of (Longident.t)
  | NT'label_let_pattern of (Asttypes.label * Parsetree.pattern)
  | NT'label_ident of (Asttypes.label * Parsetree.expression)
  | NT'label_expr of (Asttypes.label * Parsetree.expression)
  | NT'label_declarations of (Parsetree.label_declaration list)
  | NT'label_declaration of (Parsetree.label_declaration)
  | NT'label of (string)
  | NT'item_extension of (Parsetree.extension)
  | NT'interface of (Parsetree.signature)
  | NT'implementation of (Parsetree.structure)
  | NT'ident of (Asttypes.label)
  | NT'generalized_constructor_arguments of (Parsetree.core_type list * Parsetree.core_type option)
  | NT'functor_args of ((string Asttypes.loc * Parsetree.module_type option) list)
  | NT'functor_arg_name of (string)
  | NT'functor_arg of (string Asttypes.loc * Parsetree.module_type option)
  | NT'fun_def of (Parsetree.expression)
  | NT'fun_binding of (Parsetree.expression)
  | NT'floating_attribute of (Parsetree.attribute)
  | NT'field_expr_list of ((string Asttypes.loc * Parsetree.expression) list)
  | NT'field of (string * Parsetree.attributes * Parsetree.core_type)
  | NT'extension_constructor_rebind of (Parsetree.extension_constructor)
  | NT'extension_constructor_declaration of (Parsetree.extension_constructor)
  | NT'extension of (Parsetree.extension)
  | NT'ext_attributes of (string Asttypes.loc option * Parsetree.attributes)
  | NT'expr_semi_list of (Parsetree.expression list)
  | NT'expr_open of (Asttypes.override_flag * Longident.t Asttypes.loc *
  (string Asttypes.loc option * Parsetree.attributes))
  | NT'expr_comma_list of (Parsetree.expression list)
  | NT'expr of (Parsetree.expression)
  | NT'dummy of (unit)
  | NT'direction_flag of (Asttypes.direction_flag)
  | NT'core_type_list_no_attr of (Parsetree.core_type list)
  | NT'core_type_list of (Parsetree.core_type list)
  | NT'core_type_comma_list of (Parsetree.core_type list)
  | NT'core_type2 of (Parsetree.core_type)
  | NT'core_type of (Parsetree.core_type)
  | NT'constructor_declarations of (Parsetree.constructor_declaration list)
  | NT'constructor_declaration of (Parsetree.constructor_declaration)
  | NT'constraints of ((Parsetree.core_type * Parsetree.core_type * Ast_helper.loc) list)
  | NT'constrain_field of (Parsetree.core_type * Parsetree.core_type)
  | NT'constrain of (Parsetree.core_type * Parsetree.core_type * Ast_helper.loc)
  | NT'constr_longident of (Longident.t)
  | NT'constr_ident of (string)
  | NT'constant of (Asttypes.constant)
  | NT'clty_longident of (Longident.t)
  | NT'class_type_parameters of ((Parsetree.core_type * Asttypes.variance) list)
  | NT'class_type_declarations of (Parsetree.class_type_declaration list)
  | NT'class_type_declaration of (Parsetree.class_type_declaration list)
  | NT'class_type of (Parsetree.class_type)
  | NT'class_structure of (Parsetree.class_structure)
  | NT'class_simple_expr of (Parsetree.class_expr)
  | NT'class_signature of (Parsetree.class_type)
  | NT'class_sig_fields of (Parsetree.class_type_field list)
  | NT'class_sig_field of (Parsetree.class_type_field)
  | NT'class_sig_body of (Parsetree.class_signature)
  | NT'class_self_type of (Parsetree.core_type)
  | NT'class_self_pattern of (Parsetree.pattern)
  | NT'class_longident of (Longident.t)
  | NT'class_fun_def of (Parsetree.class_expr)
  | NT'class_fun_binding of (Parsetree.class_expr)
  | NT'class_fields of (Parsetree.class_field list)
  | NT'class_field of (Parsetree.class_field list)
  | NT'class_expr of (Parsetree.class_expr)
  | NT'class_descriptions of (Parsetree.class_description list)
  | NT'class_description of (Parsetree.class_description list)
  | NT'class_declarations of (Parsetree.class_declaration list)
  | NT'class_declaration of (Parsetree.class_declaration list)
  | NT'attributes of (Parsetree.attributes)
  | NT'attribute of (Parsetree.attribute)
  | NT'attr_id of (string Asttypes.loc)
  | NT'amper_type_list of (Parsetree.core_type list)
  | NT'additive of (string)

and semantic_value = 
  | Bottom
  | Terminal of token
  | Nonterminal of nonterminal

and state = private int

and feed = [ `Feed | `Feed_error ]

and step = [ `Step_run | `Step_error | `Step_action ]

and 'a parser = {
  env: (state, semantic_value, token) MenhirLib.EngineTypes.env;
  tag: 'a
}


val parse_expression: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.expression)
val interface: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.signature)
val implementation: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.structure)
val dummy: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (unit)
val initial: state -> Lexing.position * token * Lexing.position -> step parser
val step: step parser ->   [ `Step of step parser
  | `Feed of feed parser
  | `Accept of semantic_value
  | `Reject ]
val feed: feed parser -> Lexing.position * token * Lexing.position -> step parser
val reduce_default: feed parser -> feed parser
val parse_expression_state: state
val interface_state: state
val implementation_state: state
val dummy_state: state

module Query : sig

  type terminal = private int
  
  
  val index: token -> terminal
  val action: state -> terminal -> [`Shift of [`Discard | `Keep] * state | `Reduce | `Fail]
  val default_reduction: state -> bool
  val iter_states: (state -> unit) -> unit
  val forward_references: terminal -> terminal list

end
