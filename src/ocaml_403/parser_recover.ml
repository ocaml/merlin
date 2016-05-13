open Parser_raw

module Default = struct

  open Asttypes
  open Parsetree
  let default_loc = ref Location.none
  let default_expr () = {Fake.any_val' with pexp_loc = !default_loc}
  let default_pattern () = Ast_helper.Pat.any ~loc:!default_loc ()
  let default_module_expr () = Ast_helper.Mod.structure ~loc:!default_loc[]
  let default_module_type () = Ast_helper.Mty.signature ~loc:!default_loc[]

  let value (type a) : a MenhirInterpreter.symbol -> a = function
    | MenhirInterpreter.T MenhirInterpreter.T_error -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WITH -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WHILE_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WHILE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WHEN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_VIRTUAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_VAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_UNDERSCORE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_UIDENT -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_TYPE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TRY_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TRY -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TRUE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TO -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TILDE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_THEN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_STRUCT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_STRING -> ("", None)
    | MenhirInterpreter.T MenhirInterpreter.T_STAR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_SIG -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_SHARPSHARP -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_SHARPOP -> ""
    | MenhirInterpreter.T MenhirInterpreter.T_SHARP -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_SEMISEMI -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_SEMI -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_RPAREN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_REC -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_RBRACKET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_RBRACE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_QUOTE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_QUESTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PRIVATE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PREFIXOP -> "!"
    | MenhirInterpreter.T MenhirInterpreter.T_PLUSEQ -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PLUSDOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PLUS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PERCENT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_P4_QUOTATION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OUNIT_TEST_UNIT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OUNIT_TEST_MODULE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OUNIT_TEST -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OUNIT_BENCH_MODULE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OUNIT_BENCH_INDEXED -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OUNIT_BENCH_FUN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OUNIT_BENCH -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OPTLABEL -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_OPEN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OF -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_OBJECT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_NONREC -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_NEW -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MUTABLE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MODULE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MINUSGREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MINUSDOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MINUS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_METHOD -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MATCH_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MATCH -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LPAREN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LIDENT -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_LET_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LETOP -> ""
    | MenhirInterpreter.T MenhirInterpreter.T_LET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LESSMINUS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LESS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETPERCENTPERCENT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETPERCENT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETLESS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETGREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETBAR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETATATAT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETATAT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKETAT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACKET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACELESS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LBRACE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LAZY -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LABEL -> raise Not_found
    | MenhirInterpreter.T MenhirInterpreter.T_JSNEW -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_INT -> ("0",None)
    | MenhirInterpreter.T MenhirInterpreter.T_INITIALIZER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_INHERIT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP4 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP3 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP2 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP1 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INFIXOP0 -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_INCLUDE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_IN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_IF -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_GREATERRBRACKET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_GREATERRBRACE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_GREATERDOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_GREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FUNCTOR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FUNCTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FUN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FOR_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FOR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FLOAT -> ("0.",None)
    | MenhirInterpreter.T MenhirInterpreter.T_FINALLY_LWT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FALSE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EXTERNAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EXCEPTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EQUAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EOL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EOF -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_END -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_ELSE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOWNTO -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOTTILDE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOTLESS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOTDOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DONE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOCSTRING -> raise Not_found
    | MenhirInterpreter.T MenhirInterpreter.T_DO -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_CUSTOM_BANG -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_CONSTRAINT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COMMENT -> ("", Location.none)
    | MenhirInterpreter.T MenhirInterpreter.T_COMMA -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COLONGREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COLONEQUAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COLONCOLON -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_COLON -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_CLASS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_CHAR -> '_'
    | MenhirInterpreter.T MenhirInterpreter.T_BEGIN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BARRBRACKET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BARBAR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BAR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BANG -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_BACKQUOTE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_ASSERT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_AS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_AND -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_AMPERSAND -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_AMPERAMPER -> ()
    | MenhirInterpreter.N MenhirInterpreter.N_with_type_binder -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_with_constraints -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_with_constraint -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_virtual_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_value_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_value_description -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_value -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_val_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_val_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_typevar_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_variance -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_variable -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_parameters -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_parameter_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_parameter -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_kind -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_constraint -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_toplevel_directive -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_tag_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_subtractive -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_structure_tail -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_structure_item -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_structure -> []
    | MenhirInterpreter.N MenhirInterpreter.N_strict_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_str_type_extension -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_str_include_statement -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_str_extension_constructors -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_str_exception_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_single_attr_id -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_pattern_not_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_labeled_expr_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_core_type_or_tuple -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_core_type2 -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_core_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_signed_constant -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_signature_item -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_signature -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_sig_type_extension -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_sig_include_statement -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_sig_extension_constructors -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_sig_exception_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_seq_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_row_field_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_row_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_record_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_rec_module_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_rec_module_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_rec_module_bindings -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_rec_module_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_rec_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_private_virtual_flags -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_private_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_primitive_declaration_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_primitive_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_post_item_attributes -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_post_item_attribute -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_poly_type_no_attr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_poly_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_payload -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_var -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_semi_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_comma_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern -> default_pattern ()
    | MenhirInterpreter.N MenhirInterpreter.N_parse_expression -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parent_binder -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_package_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_override_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_optional_type_variable -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_optional_type_parameters -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_optional_type_parameter_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_optional_type_parameter -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_type_constraint -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_semi -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_pattern_type_constraint -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_default -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_bar -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_ampersand -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_operator -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_open_statement -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_nonrec_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_name_tag_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_name_tag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mutable_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mty_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_type_declaration_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_type_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_type -> default_module_type ()
    | MenhirInterpreter.N MenhirInterpreter.N_module_expr -> default_module_expr ()
    | MenhirInterpreter.N MenhirInterpreter.N_module_declaration_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_binding_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_alias -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mod_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mod_ext_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_method_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_meth_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_match_cases -> []
    | MenhirInterpreter.N MenhirInterpreter.N_match_case -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lident_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_pattern -> default_pattern ()
    | MenhirInterpreter.N MenhirInterpreter.N_let_bindings -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_binding_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lbl_pattern_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lbl_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lbl_expr_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lbl_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_labeled_simple_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_labeled_simple_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_var -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_let_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_declaration_semi -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_item_extension -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_interface -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_implementation -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generalized_constructor_arguments -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_functor_args -> []
    | MenhirInterpreter.N MenhirInterpreter.N_functor_arg_name -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_functor_arg -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_fun_def -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_fun_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_floating_attribute -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_field_semi -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_field_expr_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_field_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_rebind -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_ext_attributes -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_expr_semi_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_expr_comma_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_expr -> default_expr ()
    | MenhirInterpreter.N MenhirInterpreter.N_direction_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_core_type_no_attr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_core_type_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_core_type_comma_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_core_type2 -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_core_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constructor_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constructor_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constructor_arguments -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constraints -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constrain_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constrain -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constr_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constr_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constant -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_clty_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_type_parameters -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_type_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_type_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_structure -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_simple_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_signature -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_sig_fields -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_sig_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_sig_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_self_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_self_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_fun_def -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_fun_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_fields -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_descriptions -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_description -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_bar_extension_constructor_rebind -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_bar_extension_constructor_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_bar_constructor_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_attributes -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_attribute -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_attr_id -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_and_type_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_and_module_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_and_module_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_and_let_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_and_class_type_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_and_class_description -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_and_class_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_amper_type_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_additive -> raise Not_found
end

let default_value = Default.value

open MenhirInterpreter

type action =
  | Abort
  | R of int
  | S : 'a symbol -> action
  | Sub of action list

type decision =
  | Nothing
  | One of action list
  | Select of (int -> action list)

let depth =
  [|0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;2;1;2;1;2;1;2;1;1;1;1;1;1;1;1;2;3;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;3;1;3;4;1;1;1;1;1;1;2;3;3;2;1;1;1;2;1;2;3;1;1;2;3;1;1;2;1;2;1;3;1;1;1;1;1;1;2;2;2;3;2;3;1;4;5;1;1;1;2;1;2;1;2;1;1;2;1;2;3;1;2;1;2;1;2;1;2;3;4;5;6;1;7;1;1;2;3;1;2;1;1;2;1;1;1;1;2;1;2;2;1;2;1;2;1;1;1;2;3;2;1;2;3;4;2;3;2;3;1;3;4;2;3;1;2;1;3;1;1;2;1;1;3;2;3;1;1;2;3;2;3;4;4;2;3;5;1;2;2;1;2;3;2;3;4;5;3;4;3;4;4;5;6;2;1;1;1;1;2;3;1;2;3;2;3;4;5;6;7;1;2;3;1;2;1;1;2;3;1;1;2;1;2;1;1;2;4;5;1;2;2;3;4;3;4;8;1;2;1;2;1;1;1;2;3;1;2;1;1;1;1;1;1;2;1;1;1;2;1;2;2;1;2;2;1;1;2;3;4;5;5;1;2;3;4;1;2;1;2;3;3;3;3;3;1;3;2;3;1;1;1;2;3;4;5;1;2;1;2;3;4;1;1;1;2;1;1;1;2;2;3;1;4;2;1;1;1;1;2;2;2;3;2;3;1;2;1;1;2;3;3;1;2;4;5;6;2;1;1;2;3;2;3;3;4;5;1;1;2;3;4;2;3;4;3;2;3;1;5;2;3;2;1;2;3;3;1;1;3;4;5;2;1;2;3;2;5;6;2;3;1;1;2;3;1;1;1;2;1;1;1;2;3;1;2;3;4;5;2;3;3;4;2;1;1;4;5;5;6;7;1;1;1;1;1;2;3;4;5;6;1;1;2;1;2;1;1;1;1;1;1;2;1;2;1;1;2;1;1;2;3;4;4;5;6;7;8;9;1;2;1;2;3;1;1;1;1;2;3;1;2;3;1;4;3;1;1;2;2;3;1;2;1;1;1;2;1;1;1;1;2;3;1;1;2;3;2;3;2;1;2;1;1;2;3;2;3;2;3;3;3;4;5;2;3;2;3;3;1;1;4;2;2;3;3;4;1;2;2;3;4;1;2;1;2;1;2;1;1;2;3;3;4;2;1;2;3;1;1;2;3;4;1;2;3;2;3;2;3;1;1;1;1;2;2;3;1;2;1;2;1;1;1;1;2;2;3;4;5;6;1;2;1;2;3;1;1;2;1;2;3;2;3;2;3;2;1;2;1;2;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;3;2;3;2;3;1;2;3;1;2;1;1;1;2;1;2;3;3;4;5;1;2;1;2;1;2;3;4;1;2;1;2;1;2;1;2;3;4;1;2;3;1;1;1;2;5;2;3;4;5;6;7;8;3;4;5;6;7;2;1;2;3;4;5;1;2;3;3;4;2;7;3;4;5;6;7;3;4;5;6;7;3;4;5;2;1;2;1;1;2;4;5;3;4;5;3;4;5;3;5;1;1;6;7;8;9;10;2;4;5;2;3;4;5;6;1;2;1;2;3;4;1;2;3;4;1;2;5;7;3;4;3;4;5;2;3;3;4;2;3;1;3;4;5;6;7;1;4;5;3;4;4;2;3;5;6;1;3;4;4;5;6;3;4;5;1;2;3;2;3;1;2;3;1;2;3;1;1;2;3;4;4;5;1;2;3;1;3;3;3;4;2;3;3;2;3;2;3;4;7;2;3;4;1;2;3;4;5;6;7;1;2;2;1;3;4;5;2;3;4;5;6;7;8;2;3;3;4;3;4;5;6;4;5;5;6;7;5;6;7;7;8;9;2;3;3;4;5;2;4;5;3;4;5;6;1;2;1;2;3;4;1;2;1;2;3;4;1;2;3;4;5;6;1;2;7;8;1;2;3;4;5;6;7;4;5;6;1;1;1;2;3;1;2;3;4;5;1;2;6;2;3;4;5;6;4;5;3;4;5;6;7;1;2;3;4;1;2;3;1;2;3;1;4;1;2;3;5;6;7;1;2;1;2;3;3;4;1;2;1;2;1;2;3;4;5;1;2;3;4;5;3;4;1;2;3;1;2;1;2;1;2;3;4;5;1;2;3;6;7;1;1;2;1;1;2;3;1;2;3;4;2;3;1;1;1;3;4;2;1;2;1;2;3;3;4;1;2;1;2;8;9;2;3;4;5;6;1;2;3;4;5;1;2;3;1;2;3;4;1;1;1;2;1;2;3;1;3;1;5;4;6;7;8;1;1;1;2;3;4;5;6;7;1;2;1;1;2;1;1;1;1;1;2;3;4;5;6;2;1;1;1;1;1;1;1;2;1;1;1;2;3;4;5;6;7;8;2;1;1;1;2;3;4;5;6;7;8;2;1;2;2;2;1;2;3;4;5;1;1;2;3;4;1;2;1;2;3;1;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;5;6;7;1;1;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;8;9;10;11;6;7;8;5;2;3;1;2;3;4;1;2;3;1;1;1;2;3;4;1;2;1;2;1;2;1;1;2;1;3;2;3;2;3;2;3;3;4;5;4;1;2;5;6;1;2;3;4;1;2;1;2;2;1;2;3;4;1;2;6;7;1;1;1;1;1;2;1;1;1;1;1;1;2;3;4;5;2;1;1;1;1;1;1;1;2;1;1;1;1;2;3;4;5;6;7;2;1;2;1;2;3;1;1;3;4;3;4;2;3;4;5;6;7;2;3;4;5;6;7;8;2;3;3;4;5;3;4;2;2;3;4;2;3;4;5;6;3;4;9;2;2;1;1;1;2;3;4;2;3;1;1;3;4;3;4;5;6;1;2;1;3;4;5;2;3;4;5;4;1;1;2;3;4;3;3;2;1;1;2;3;1;2;2;3;4;5;2;3;4;4;5;6;7;5;2;6;7;8;5;6;7;1;2;8;9;2;1;1;1;3;4;4;5;2;3;4;4;5;6;5;6;3;4;3;2;3;4;5;6;1;2;1;0;1;2;1;0;1;2;1;|]

let can_pop (type a) : a terminal -> bool = function
  | T_WITH -> true
  | T_WHILE_LWT -> true
  | T_WHILE -> true
  | T_WHEN -> true
  | T_VIRTUAL -> true
  | T_VAL -> true
  | T_UNDERSCORE -> true
  | T_TYPE -> true
  | T_TRY_LWT -> true
  | T_TRY -> true
  | T_TRUE -> true
  | T_TO -> true
  | T_TILDE -> true
  | T_THEN -> true
  | T_STRUCT -> true
  | T_STAR -> true
  | T_SIG -> true
  | T_SHARPSHARP -> true
  | T_SHARP -> true
  | T_SEMISEMI -> true
  | T_SEMI -> true
  | T_RPAREN -> true
  | T_REC -> true
  | T_RBRACKET -> true
  | T_RBRACE -> true
  | T_QUOTE -> true
  | T_QUESTION -> true
  | T_PRIVATE -> true
  | T_PLUSEQ -> true
  | T_PLUSDOT -> true
  | T_PLUS -> true
  | T_PERCENT -> true
  | T_P4_QUOTATION -> true
  | T_OUNIT_TEST_UNIT -> true
  | T_OUNIT_TEST_MODULE -> true
  | T_OUNIT_TEST -> true
  | T_OUNIT_BENCH_MODULE -> true
  | T_OUNIT_BENCH_INDEXED -> true
  | T_OUNIT_BENCH_FUN -> true
  | T_OUNIT_BENCH -> true
  | T_OR -> true
  | T_OPEN -> true
  | T_OF -> true
  | T_OBJECT -> true
  | T_NONREC -> true
  | T_NEW -> true
  | T_MUTABLE -> true
  | T_MODULE -> true
  | T_MINUSGREATER -> true
  | T_MINUSDOT -> true
  | T_MINUS -> true
  | T_METHOD -> true
  | T_MATCH_LWT -> true
  | T_MATCH -> true
  | T_LPAREN -> true
  | T_LET_LWT -> true
  | T_LET -> true
  | T_LESSMINUS -> true
  | T_LESS -> true
  | T_LBRACKETPERCENTPERCENT -> true
  | T_LBRACKETPERCENT -> true
  | T_LBRACKETLESS -> true
  | T_LBRACKETGREATER -> true
  | T_LBRACKETBAR -> true
  | T_LBRACKETATATAT -> true
  | T_LBRACKETATAT -> true
  | T_LBRACKETAT -> true
  | T_LBRACKET -> true
  | T_LBRACELESS -> true
  | T_LBRACE -> true
  | T_LAZY -> true
  | T_JSNEW -> true
  | T_INITIALIZER -> true
  | T_INHERIT -> true
  | T_INCLUDE -> true
  | T_IN -> true
  | T_IF -> true
  | T_GREATERRBRACKET -> true
  | T_GREATERRBRACE -> true
  | T_GREATERDOT -> true
  | T_GREATER -> true
  | T_FUNCTOR -> true
  | T_FUNCTION -> true
  | T_FUN -> true
  | T_FOR_LWT -> true
  | T_FOR -> true
  | T_FINALLY_LWT -> true
  | T_FALSE -> true
  | T_EXTERNAL -> true
  | T_EXCEPTION -> true
  | T_EQUAL -> true
  | T_EOL -> true
  | T_END -> true
  | T_ELSE -> true
  | T_DOWNTO -> true
  | T_DOTTILDE -> true
  | T_DOTLESS -> true
  | T_DOTDOT -> true
  | T_DOT -> true
  | T_DONE -> true
  | T_DO -> true
  | T_CUSTOM_BANG -> true
  | T_CONSTRAINT -> true
  | T_COMMA -> true
  | T_COLONGREATER -> true
  | T_COLONEQUAL -> true
  | T_COLONCOLON -> true
  | T_COLON -> true
  | T_CLASS -> true
  | T_BEGIN -> true
  | T_BARRBRACKET -> true
  | T_BARBAR -> true
  | T_BAR -> true
  | T_BANG -> true
  | T_BACKQUOTE -> true
  | T_ASSERT -> true
  | T_AS -> true
  | T_AND -> true
  | T_AMPERSAND -> true
  | T_AMPERAMPER -> true
  | _ -> false

let recover =
  let r0 = [R 430] in
  let r1 = S (N N_expr) :: r0 in
  let r2 = [R 142] in
  let r3 = S (T T_DONE) :: r2 in
  let r4 = Sub (r1) :: r3 in
  let r5 = S (T T_DO) :: r4 in
  let r6 = Sub (r1) :: r5 in
  let r7 = [R 541] in
  let r8 = S (T T_AND) :: r7 in
  let r9 = [R 14] in
  let r10 = Sub (r8) :: r9 in
  let r11 = [R 183] in
  let r12 = R 17 :: r11 in
  let r13 = [R 15] in
  let r14 = [R 394] in
  let r15 = S (N N_structure) :: r14 in
  let r16 = [R 16] in
  let r17 = S (T T_RBRACKET) :: r16 in
  let r18 = Sub (r15) :: r17 in
  let r19 = [R 668] in
  let r20 = S (T T_error) :: r19 in
  let r21 = S (T T_LPAREN) :: r20 in
  let r22 = [R 465] in
  let r23 = S (T T_UNDERSCORE) :: r22 in
  let r24 = [R 462] in
  let r25 = Sub (r23) :: r24 in
  let r26 = [R 483] in
  let r27 = Sub (r25) :: r26 in
  let r28 = [R 114] in
  let r29 = Sub (r27) :: r28 in
  let r30 = [R 123] in
  let r31 = Sub (r29) :: r30 in
  let r32 = [R 112] in
  let r33 = Sub (r31) :: r32 in
  let r34 = [R 676] in
  let r35 = R 404 :: r34 in
  let r36 = Sub (r33) :: r35 in
  let r37 = S (T T_COLON) :: r36 in
  let r38 = Sub (r21) :: r37 in
  let r39 = [R 669] in
  let r40 = [R 666] in
  let r41 = [R 278] in
  let r42 = [R 48] in
  let r43 = S (T T_LIDENT) :: r42 in
  let r44 = [R 471] in
  let r45 = [R 281] in
  let r46 = [R 49] in
  let r47 = S (T T_LIDENT) :: r46 in
  let r48 = [R 282] in
  let r49 = [R 211] in
  let r50 = S (T T_LIDENT) :: r49 in
  let r51 = [R 464] in
  let r52 = Sub (r50) :: r51 in
  let r53 = [R 115] in
  let r54 = Sub (r29) :: r53 in
  let r55 = S (T T_MINUSGREATER) :: r54 in
  let r56 = Sub (r29) :: r55 in
  let r57 = S (T T_COLON) :: r56 in
  let r58 = [R 116] in
  let r59 = Sub (r29) :: r58 in
  let r60 = S (T T_MINUSGREATER) :: r59 in
  let r61 = [R 372] in
  let r62 = S (N N_module_type) :: r61 in
  let r63 = [R 481] in
  let r64 = S (T T_RPAREN) :: r63 in
  let r65 = Sub (r62) :: r64 in
  let r66 = R 181 :: r65 in
  let r67 = [R 306] in
  let r68 = S (T T_END) :: r67 in
  let r69 = R 439 :: r68 in
  let r70 = [R 638] in
  let r71 = R 404 :: r70 in
  let r72 = R 105 :: r71 in
  let r73 = R 641 :: r72 in
  let r74 = S (T T_LIDENT) :: r73 in
  let r75 = R 365 :: r74 in
  let r76 = R 324 :: r75 in
  let r77 = R 181 :: r76 in
  let r78 = [R 369] in
  let r79 = S (T T_UNDERSCORE) :: r78 in
  let r80 = [R 362] in
  let r81 = Sub (r79) :: r80 in
  let r82 = R 660 :: r81 in
  let r83 = [R 363] in
  let r84 = Sub (r82) :: r83 in
  let r85 = [R 367] in
  let r86 = S (T T_RPAREN) :: r85 in
  let r87 = [R 368] in
  let r88 = [R 364] in
  let r89 = [R 646] in
  let r90 = [R 95] in
  let r91 = S (T T_FALSE) :: r90 in
  let r92 = [R 108] in
  let r93 = R 17 :: r92 in
  let r94 = R 206 :: r93 in
  let r95 = Sub (r91) :: r94 in
  let r96 = [R 109] in
  let r97 = Sub (r95) :: r96 in
  let r98 = [R 645] in
  let r99 = [R 93] in
  let r100 = [R 651] in
  let r101 = [R 117] in
  let r102 = Sub (r29) :: r101 in
  let r103 = S (T T_MINUSGREATER) :: r102 in
  let r104 = [R 470] in
  let r105 = [R 215] in
  let r106 = [R 469] in
  let r107 = [R 401] in
  let r108 = Sub (r31) :: r107 in
  let r109 = [R 192] in
  let r110 = R 17 :: r109 in
  let r111 = S (T T_SEMI) :: r110 in
  let r112 = R 17 :: r111 in
  let r113 = Sub (r108) :: r112 in
  let r114 = [R 663] in
  let r115 = [R 184] in
  let r116 = S (T T_RBRACKET) :: r115 in
  let r117 = Sub (r15) :: r116 in
  let r118 = [R 185] in
  let r119 = R 17 :: r118 in
  let r120 = R 206 :: r119 in
  let r121 = Sub (r91) :: r120 in
  let r122 = [R 592] in
  let r123 = Sub (r121) :: r122 in
  let r124 = [R 599] in
  let r125 = R 404 :: r124 in
  let r126 = Sub (r123) :: r125 in
  let r127 = R 409 :: r126 in
  let r128 = [R 94] in
  let r129 = [R 92] in
  let r130 = [R 20] in
  let r131 = R 17 :: r130 in
  let r132 = R 206 :: r131 in
  let r133 = Sub (r91) :: r132 in
  let r134 = [R 100] in
  let r135 = S (T T_FALSE) :: r134 in
  let r136 = [R 21] in
  let r137 = R 17 :: r136 in
  let r138 = Sub (r135) :: r137 in
  let r139 = S (T T_EQUAL) :: r138 in
  let r140 = [R 121] in
  let r141 = Sub (r25) :: r140 in
  let r142 = [R 106] in
  let r143 = Sub (r141) :: r142 in
  let r144 = [R 207] in
  let r145 = [R 427] in
  let r146 = Sub (r25) :: r145 in
  let r147 = [R 428] in
  let r148 = Sub (r146) :: r147 in
  let r149 = [R 479] in
  let r150 = S (T T_RBRACKET) :: r149 in
  let r151 = Sub (r148) :: r150 in
  let r152 = [R 478] in
  let r153 = [R 477] in
  let r154 = S (T T_RBRACKET) :: r153 in
  let r155 = [R 475] in
  let r156 = S (T T_RBRACKET) :: r155 in
  let r157 = Sub (r148) :: r156 in
  let r158 = [R 321] in
  let r159 = Sub (r50) :: r158 in
  let r160 = [R 472] in
  let r161 = [R 652] in
  let r162 = S (T T_LIDENT) :: r161 in
  let r163 = S (T T_DOT) :: r162 in
  let r164 = S (T T_UIDENT) :: r41 in
  let r165 = [R 280] in
  let r166 = S (T T_RPAREN) :: r165 in
  let r167 = [R 279] in
  let r168 = [R 429] in
  let r169 = [R 627] in
  let r170 = [R 5] in
  let r171 = Sub (r31) :: r170 in
  let r172 = [R 626] in
  let r173 = R 17 :: r172 in
  let r174 = Sub (r171) :: r173 in
  let r175 = [R 484] in
  let r176 = [R 122] in
  let r177 = [R 118] in
  let r178 = [R 124] in
  let r179 = Sub (r50) :: r178 in
  let r180 = [R 6] in
  let r181 = [R 18] in
  let r182 = [R 474] in
  let r183 = [R 476] in
  let r184 = S (T T_RBRACKET) :: r183 in
  let r185 = Sub (r148) :: r184 in
  let r186 = S (T T_BACKQUOTE) :: r159 in
  let r187 = [R 322] in
  let r188 = Sub (r186) :: r187 in
  let r189 = [R 480] in
  let r190 = S (T T_RBRACKET) :: r189 in
  let r191 = S (T T_LIDENT) :: r105 in
  let r192 = [R 216] in
  let r193 = R 17 :: r192 in
  let r194 = Sub (r108) :: r193 in
  let r195 = S (T T_COLON) :: r194 in
  let r196 = Sub (r191) :: r195 in
  let r197 = R 319 :: r196 in
  let r198 = [R 218] in
  let r199 = Sub (r197) :: r198 in
  let r200 = [R 107] in
  let r201 = S (T T_RBRACE) :: r200 in
  let r202 = [R 217] in
  let r203 = R 17 :: r202 in
  let r204 = S (T T_SEMI) :: r203 in
  let r205 = R 17 :: r204 in
  let r206 = Sub (r108) :: r205 in
  let r207 = S (T T_COLON) :: r206 in
  let r208 = [R 402] in
  let r209 = Sub (r31) :: r208 in
  let r210 = [R 664] in
  let r211 = [R 99] in
  let r212 = [R 98] in
  let r213 = [R 209] in
  let r214 = [R 208] in
  let r215 = Sub (r25) :: r214 in
  let r216 = [R 403] in
  let r217 = S (T T_RBRACKET) :: r216 in
  let r218 = Sub (r15) :: r217 in
  let r219 = [R 136] in
  let r220 = S (N N_match_cases) :: r219 in
  let r221 = R 352 :: r220 in
  let r222 = S (T T_WITH) :: r221 in
  let r223 = Sub (r1) :: r222 in
  let r224 = [R 487] in
  let r225 = Sub (r135) :: r224 in
  let r226 = [R 508] in
  let r227 = [R 510] in
  let r228 = Sub (r43) :: r227 in
  let r229 = [R 182] in
  let r230 = [R 502] in
  let r231 = [R 74] in
  let r232 = R 41 :: r231 in
  let r233 = R 52 :: r232 in
  let r234 = [R 175] in
  let r235 = S (T T_END) :: r234 in
  let r236 = Sub (r233) :: r235 in
  let r237 = [R 50] in
  let r238 = S (T T_RPAREN) :: r237 in
  let r239 = [R 524] in
  let r240 = S (T T_LIDENT) :: r100 in
  let r241 = [R 529] in
  let r242 = [R 460] in
  let r243 = [R 458] in
  let r244 = [R 536] in
  let r245 = S (T T_RPAREN) :: r244 in
  let r246 = [R 537] in
  let r247 = S (T T_RPAREN) :: r246 in
  let r248 = [R 310] in
  let r249 = S (N N_module_expr) :: r248 in
  let r250 = R 17 :: r249 in
  let r251 = S (T T_OF) :: r250 in
  let r252 = [R 293] in
  let r253 = S (T T_END) :: r252 in
  let r254 = S (N N_structure) :: r253 in
  let r255 = [R 628] in
  let r256 = [R 622] in
  let r257 = S (T T_UIDENT) :: r45 in
  let r258 = [R 326] in
  let r259 = R 404 :: r258 in
  let r260 = Sub (r257) :: r259 in
  let r261 = R 181 :: r260 in
  let r262 = [R 405] in
  let r263 = [R 285] in
  let r264 = S (N N_module_expr) :: r263 in
  let r265 = S (T T_EQUAL) :: r264 in
  let r266 = [R 418] in
  let r267 = R 404 :: r266 in
  let r268 = Sub (r265) :: r267 in
  let r269 = S (T T_UIDENT) :: r268 in
  let r270 = S (T T_REC) :: r269 in
  let r271 = [R 314] in
  let r272 = R 404 :: r271 in
  let r273 = R 315 :: r272 in
  let r274 = Sub (r50) :: r273 in
  let r275 = R 181 :: r274 in
  let r276 = [R 316] in
  let r277 = [R 311] in
  let r278 = S (T T_RPAREN) :: r277 in
  let r279 = [R 307] in
  let r280 = S (N N_module_type) :: r279 in
  let r281 = S (T T_MINUSGREATER) :: r280 in
  let r282 = S (N N_functor_args) :: r281 in
  let r283 = [R 200] in
  let r284 = [R 201] in
  let r285 = S (T T_RPAREN) :: r284 in
  let r286 = S (N N_module_type) :: r285 in
  let r287 = [R 685] in
  let r288 = Sub (r164) :: r287 in
  let r289 = S (T T_COLONEQUAL) :: r288 in
  let r290 = S (T T_UIDENT) :: r289 in
  let r291 = S (T T_MODULE) :: r290 in
  let r292 = [R 686] in
  let r293 = Sub (r291) :: r292 in
  let r294 = [R 309] in
  let r295 = [R 683] in
  let r296 = Sub (r31) :: r295 in
  let r297 = S (T T_COLONEQUAL) :: r296 in
  let r298 = Sub (r191) :: r297 in
  let r299 = [R 659] in
  let r300 = Sub (r50) :: r299 in
  let r301 = S (T T_QUOTE) :: r300 in
  let r302 = [R 653] in
  let r303 = Sub (r301) :: r302 in
  let r304 = R 660 :: r303 in
  let r305 = [R 654] in
  let r306 = Sub (r304) :: r305 in
  let r307 = [R 658] in
  let r308 = S (T T_RPAREN) :: r307 in
  let r309 = [R 655] in
  let r310 = [R 229] in
  let r311 = S (T T_LIDENT) :: r310 in
  let r312 = [R 688] in
  let r313 = S (T T_EQUAL) :: r312 in
  let r314 = [R 682] in
  let r315 = R 105 :: r314 in
  let r316 = Sub (r31) :: r315 in
  let r317 = [R 102] in
  let r318 = Sub (r33) :: r317 in
  let r319 = S (T T_EQUAL) :: r318 in
  let r320 = Sub (r33) :: r319 in
  let r321 = [R 104] in
  let r322 = [R 684] in
  let r323 = Sub (r164) :: r322 in
  let r324 = [R 687] in
  let r325 = [R 308] in
  let r326 = [R 318] in
  let r327 = Sub (r50) :: r326 in
  let r328 = [R 284] in
  let r329 = R 404 :: r328 in
  let r330 = Sub (r265) :: r329 in
  let r331 = [R 298] in
  let r332 = S (T T_RPAREN) :: r331 in
  let r333 = [R 299] in
  let r334 = S (T T_RPAREN) :: r333 in
  let r335 = S (N N_expr) :: r334 in
  let r336 = [R 135] in
  let r337 = S (N N_match_cases) :: r336 in
  let r338 = R 352 :: r337 in
  let r339 = S (T T_WITH) :: r338 in
  let r340 = Sub (r1) :: r339 in
  let r341 = [R 516] in
  let r342 = S (T T_RPAREN) :: r341 in
  let r343 = [R 294] in
  let r344 = S (N N_module_expr) :: r343 in
  let r345 = S (T T_MINUSGREATER) :: r344 in
  let r346 = S (N N_functor_args) :: r345 in
  let r347 = [R 296] in
  let r348 = [R 295] in
  let r349 = [R 517] in
  let r350 = S (T T_RPAREN) :: r349 in
  let r351 = [R 256] in
  let r352 = Sub (r1) :: r351 in
  let r353 = S (T T_EQUAL) :: r352 in
  let r354 = S (N N_pattern) :: r353 in
  let r355 = [R 252] in
  let r356 = R 404 :: r355 in
  let r357 = Sub (r354) :: r356 in
  let r358 = R 416 :: r357 in
  let r359 = R 181 :: r358 in
  let r360 = [R 131] in
  let r361 = Sub (r1) :: r360 in
  let r362 = S (T T_IN) :: r361 in
  let r363 = Sub (r257) :: r362 in
  let r364 = R 181 :: r363 in
  let r365 = R 370 :: r364 in
  let r366 = [R 179] in
  let r367 = S (N N_expr) :: r366 in
  let r368 = [R 505] in
  let r369 = S (T T_RBRACKET) :: r368 in
  let r370 = R 358 :: r369 in
  let r371 = [R 512] in
  let r372 = [R 189] in
  let r373 = [R 188] in
  let r374 = [R 228] in
  let r375 = S (T T_LIDENT) :: r374 in
  let r376 = [R 242] in
  let r377 = R 361 :: r376 in
  let r378 = Sub (r375) :: r377 in
  let r379 = [R 243] in
  let r380 = Sub (r378) :: r379 in
  let r381 = [R 425] in
  let r382 = Sub (r380) :: r381 in
  let r383 = [R 499] in
  let r384 = S (T T_RBRACE) :: r383 in
  let r385 = [R 87] in
  let r386 = [R 491] in
  let r387 = S (T T_END) :: r386 in
  let r388 = [R 174] in
  let r389 = Sub (r225) :: r388 in
  let r390 = [R 509] in
  let r391 = [R 495] in
  let r392 = S (T T_RPAREN) :: r391 in
  let r393 = S (T T_LPAREN) :: r392 in
  let r394 = S (T T_DOT) :: r393 in
  let r395 = [R 518] in
  let r396 = S (T T_RPAREN) :: r395 in
  let r397 = Sub (r62) :: r396 in
  let r398 = S (T T_COLON) :: r397 in
  let r399 = [R 141] in
  let r400 = S (N N_expr) :: r399 in
  let r401 = S (T T_THEN) :: r400 in
  let r402 = Sub (r1) :: r401 in
  let r403 = [R 132] in
  let r404 = S (N N_match_cases) :: r403 in
  let r405 = R 352 :: r404 in
  let r406 = [R 533] in
  let r407 = [R 390] in
  let r408 = S (N N_pattern) :: r407 in
  let r409 = [R 531] in
  let r410 = S (T T_RBRACKET) :: r409 in
  let r411 = R 358 :: r410 in
  let r412 = [R 247] in
  let r413 = R 357 :: r412 in
  let r414 = Sub (r375) :: r413 in
  let r415 = [R 248] in
  let r416 = Sub (r414) :: r415 in
  let r417 = [R 530] in
  let r418 = S (T T_RBRACE) :: r417 in
  let r419 = [R 250] in
  let r420 = [R 356] in
  let r421 = [R 246] in
  let r422 = S (T T_UNDERSCORE) :: r239 in
  let r423 = [R 523] in
  let r424 = Sub (r422) :: r423 in
  let r425 = [R 384] in
  let r426 = Sub (r424) :: r425 in
  let r427 = [R 385] in
  let r428 = S (N N_pattern) :: r427 in
  let r429 = S (T T_INT) :: r385 in
  let r430 = [R 457] in
  let r431 = Sub (r429) :: r430 in
  let r432 = [R 526] in
  let r433 = [R 387] in
  let r434 = [R 381] in
  let r435 = [R 380] in
  let r436 = [R 379] in
  let r437 = [R 534] in
  let r438 = [R 388] in
  let r439 = [R 535] in
  let r440 = S (T T_RPAREN) :: r439 in
  let r441 = [R 383] in
  let r442 = [R 377] in
  let r443 = [R 532] in
  let r444 = S (T T_BARRBRACKET) :: r443 in
  let r445 = [R 266] in
  let r446 = S (T T_DOT) :: r445 in
  let r447 = S (T T_MINUSGREATER) :: r446 in
  let r448 = [R 265] in
  let r449 = Sub (r1) :: r448 in
  let r450 = S (T T_MINUSGREATER) :: r449 in
  let r451 = [R 240] in
  let r452 = Sub (r424) :: r451 in
  let r453 = [R 196] in
  let r454 = Sub (r1) :: r453 in
  let r455 = S (T T_MINUSGREATER) :: r454 in
  let r456 = [R 133] in
  let r457 = Sub (r455) :: r456 in
  let r458 = Sub (r452) :: r457 in
  let r459 = [R 230] in
  let r460 = S (T T_LIDENT) :: r459 in
  let r461 = [R 238] in
  let r462 = [R 226] in
  let r463 = Sub (r460) :: r462 in
  let r464 = [R 237] in
  let r465 = S (T T_RPAREN) :: r464 in
  let r466 = [R 227] in
  let r467 = [R 234] in
  let r468 = [R 233] in
  let r469 = S (T T_RPAREN) :: r468 in
  let r470 = R 354 :: r469 in
  let r471 = [R 355] in
  let r472 = [R 126] in
  let r473 = S (T T_DOWNTO) :: r472 in
  let r474 = [R 143] in
  let r475 = S (T T_DONE) :: r474 in
  let r476 = Sub (r1) :: r475 in
  let r477 = S (T T_DO) :: r476 in
  let r478 = Sub (r1) :: r477 in
  let r479 = Sub (r473) :: r478 in
  let r480 = Sub (r1) :: r479 in
  let r481 = S (T T_EQUAL) :: r480 in
  let r482 = S (N N_pattern) :: r481 in
  let r483 = [R 173] in
  let r484 = Sub (r225) :: r483 in
  let r485 = [R 515] in
  let r486 = [R 498] in
  let r487 = S (T T_RBRACE) :: r486 in
  let r488 = S (N N_expr) :: r487 in
  let r489 = S (T T_LBRACE) :: r488 in
  let r490 = [R 496] in
  let r491 = S (T T_RPAREN) :: r490 in
  let r492 = Sub (r1) :: r491 in
  let r493 = [R 166] in
  let r494 = [R 225] in
  let r495 = S (T T_LIDENT) :: r494 in
  let r496 = [R 222] in
  let r497 = [R 514] in
  let r498 = [R 223] in
  let r499 = [R 224] in
  let r500 = [R 221] in
  let r501 = [R 169] in
  let r502 = [R 129] in
  let r503 = Sub (r1) :: r502 in
  let r504 = [R 172] in
  let r505 = S (N N_expr) :: r504 in
  let r506 = [R 177] in
  let r507 = [R 156] in
  let r508 = [R 150] in
  let r509 = [R 167] in
  let r510 = [R 153] in
  let r511 = [R 157] in
  let r512 = [R 149] in
  let r513 = [R 152] in
  let r514 = [R 151] in
  let r515 = [R 161] in
  let r516 = [R 155] in
  let r517 = [R 154] in
  let r518 = [R 159] in
  let r519 = [R 148] in
  let r520 = [R 147] in
  let r521 = [R 144] in
  let r522 = [R 146] in
  let r523 = [R 160] in
  let r524 = [R 158] in
  let r525 = [R 162] in
  let r526 = [R 163] in
  let r527 = [R 164] in
  let r528 = [R 178] in
  let r529 = [R 165] in
  let r530 = [R 10] in
  let r531 = R 404 :: r530 in
  let r532 = Sub (r354) :: r531 in
  let r533 = [R 393] in
  let r534 = S (T T_UNDERSCORE) :: r533 in
  let r535 = [R 236] in
  let r536 = [R 235] in
  let r537 = S (T T_RPAREN) :: r536 in
  let r538 = R 354 :: r537 in
  let r539 = [R 261] in
  let r540 = [R 262] in
  let r541 = S (T T_LIDENT) :: r540 in
  let r542 = [R 600] in
  let r543 = Sub (r1) :: r542 in
  let r544 = S (T T_EQUAL) :: r543 in
  let r545 = [R 194] in
  let r546 = Sub (r544) :: r545 in
  let r547 = [R 602] in
  let r548 = Sub (r546) :: r547 in
  let r549 = S (T T_RPAREN) :: r548 in
  let r550 = Sub (r541) :: r549 in
  let r551 = [R 239] in
  let r552 = [R 637] in
  let r553 = [R 635] in
  let r554 = Sub (r33) :: r553 in
  let r555 = [R 636] in
  let r556 = [R 195] in
  let r557 = Sub (r1) :: r556 in
  let r558 = [R 601] in
  let r559 = [R 255] in
  let r560 = Sub (r1) :: r559 in
  let r561 = S (T T_EQUAL) :: r560 in
  let r562 = Sub (r33) :: r561 in
  let r563 = S (T T_DOT) :: r562 in
  let r564 = [R 254] in
  let r565 = Sub (r1) :: r564 in
  let r566 = S (T T_EQUAL) :: r565 in
  let r567 = Sub (r33) :: r566 in
  let r568 = [R 257] in
  let r569 = Sub (r1) :: r568 in
  let r570 = S (T T_EQUAL) :: r569 in
  let r571 = [R 497] in
  let r572 = S (T T_RBRACKET) :: r571 in
  let r573 = Sub (r1) :: r572 in
  let r574 = [R 170] in
  let r575 = [R 171] in
  let r576 = [R 168] in
  let r577 = [R 134] in
  let r578 = Sub (r455) :: r577 in
  let r579 = S (T T_RPAREN) :: r578 in
  let r580 = [R 199] in
  let r581 = Sub (r455) :: r580 in
  let r582 = S (T T_RPAREN) :: r581 in
  let r583 = [R 197] in
  let r584 = Sub (r1) :: r583 in
  let r585 = S (T T_MINUSGREATER) :: r584 in
  let r586 = [R 198] in
  let r587 = S (N N_pattern) :: r447 in
  let r588 = [R 268] in
  let r589 = [R 140] in
  let r590 = [R 494] in
  let r591 = [R 504] in
  let r592 = [R 503] in
  let r593 = S (T T_BARRBRACKET) :: r592 in
  let r594 = [R 507] in
  let r595 = [R 506] in
  let r596 = S (T T_RBRACKET) :: r595 in
  let r597 = Sub (r191) :: r372 in
  let r598 = [R 190] in
  let r599 = R 358 :: r598 in
  let r600 = Sub (r597) :: r599 in
  let r601 = [R 513] in
  let r602 = S (T T_GREATERRBRACE) :: r601 in
  let r603 = [R 500] in
  let r604 = S (T T_RBRACE) :: r603 in
  let r605 = [R 424] in
  let r606 = Sub (r380) :: r605 in
  let r607 = [R 241] in
  let r608 = [R 490] in
  let r609 = [R 511] in
  let r610 = [R 130] in
  let r611 = Sub (r1) :: r610 in
  let r612 = S (T T_IN) :: r611 in
  let r613 = Sub (r265) :: r612 in
  let r614 = S (T T_UIDENT) :: r613 in
  let r615 = [R 286] in
  let r616 = S (N N_module_expr) :: r615 in
  let r617 = S (T T_EQUAL) :: r616 in
  let r618 = [R 287] in
  let r619 = [R 145] in
  let r620 = S (T T_RPAREN) :: r619 in
  let r621 = S (N N_expr) :: r620 in
  let r622 = S (T T_COMMA) :: r621 in
  let r623 = S (N N_expr) :: r622 in
  let r624 = S (T T_LPAREN) :: r623 in
  let r625 = [R 489] in
  let r626 = [R 492] in
  let r627 = [R 302] in
  let r628 = S (T T_RPAREN) :: r627 in
  let r629 = [R 300] in
  let r630 = S (T T_RPAREN) :: r629 in
  let r631 = [R 301] in
  let r632 = S (T T_RPAREN) :: r631 in
  let r633 = [R 297] in
  let r634 = S (T T_RPAREN) :: r633 in
  let r635 = [R 214] in
  let r636 = S (T T_RBRACKET) :: r635 in
  let r637 = Sub (r15) :: r636 in
  let r638 = [R 397] in
  let r639 = [R 398] in
  let r640 = [R 193] in
  let r641 = S (T T_RBRACKET) :: r640 in
  let r642 = Sub (r15) :: r641 in
  let r643 = [R 598] in
  let r644 = R 404 :: r643 in
  let r645 = S (N N_module_expr) :: r644 in
  let r646 = [R 407] in
  let r647 = S (T T_STRING) :: r646 in
  let r648 = [R 406] in
  let r649 = R 404 :: r648 in
  let r650 = Sub (r647) :: r649 in
  let r651 = S (T T_EQUAL) :: r650 in
  let r652 = Sub (r33) :: r651 in
  let r653 = S (T T_COLON) :: r652 in
  let r654 = Sub (r21) :: r653 in
  let r655 = [R 591] in
  let r656 = R 404 :: r655 in
  let r657 = R 17 :: r656 in
  let r658 = Sub (r135) :: r657 in
  let r659 = S (T T_EQUAL) :: r658 in
  let r660 = Sub (r91) :: r659 in
  let r661 = [R 433] in
  let r662 = R 404 :: r661 in
  let r663 = R 17 :: r662 in
  let r664 = R 206 :: r663 in
  let r665 = Sub (r91) :: r664 in
  let r666 = R 181 :: r665 in
  let r667 = [R 395] in
  let r668 = [R 440] in
  let r669 = [R 421] in
  let r670 = R 404 :: r669 in
  let r671 = S (N N_module_type) :: r670 in
  let r672 = S (T T_COLON) :: r671 in
  let r673 = S (T T_UIDENT) :: r672 in
  let r674 = S (T T_REC) :: r673 in
  let r675 = [R 289] in
  let r676 = S (N N_module_type) :: r675 in
  let r677 = S (T T_COLON) :: r676 in
  let r678 = [R 288] in
  let r679 = R 404 :: r678 in
  let r680 = [R 291] in
  let r681 = Sub (r677) :: r680 in
  let r682 = [R 290] in
  let r683 = Sub (r677) :: r682 in
  let r684 = S (T T_RPAREN) :: r683 in
  let r685 = S (N N_module_type) :: r684 in
  let r686 = [R 283] in
  let r687 = R 404 :: r686 in
  let r688 = [R 437] in
  let r689 = R 404 :: r688 in
  let r690 = S (N N_module_type) :: r689 in
  let r691 = [R 85] in
  let r692 = S (T T_LIDENT) :: r691 in
  let r693 = [R 65] in
  let r694 = Sub (r692) :: r693 in
  let r695 = [R 80] in
  let r696 = R 404 :: r695 in
  let r697 = Sub (r694) :: r696 in
  let r698 = S (T T_EQUAL) :: r697 in
  let r699 = S (T T_LIDENT) :: r698 in
  let r700 = R 83 :: r699 in
  let r701 = R 680 :: r700 in
  let r702 = R 181 :: r701 in
  let r703 = [R 84] in
  let r704 = S (T T_RBRACKET) :: r703 in
  let r705 = [R 55] in
  let r706 = R 62 :: r705 in
  let r707 = R 54 :: r706 in
  let r708 = [R 66] in
  let r709 = S (T T_END) :: r708 in
  let r710 = Sub (r707) :: r709 in
  let r711 = [R 53] in
  let r712 = S (T T_RPAREN) :: r711 in
  let r713 = [R 679] in
  let r714 = Sub (r33) :: r713 in
  let r715 = S (T T_COLON) :: r714 in
  let r716 = Sub (r191) :: r715 in
  let r717 = [R 57] in
  let r718 = R 404 :: r717 in
  let r719 = Sub (r716) :: r718 in
  let r720 = [R 677] in
  let r721 = Sub (r33) :: r720 in
  let r722 = S (T T_COLON) :: r721 in
  let r723 = Sub (r191) :: r722 in
  let r724 = [R 678] in
  let r725 = Sub (r33) :: r724 in
  let r726 = S (T T_COLON) :: r725 in
  let r727 = Sub (r191) :: r726 in
  let r728 = [R 399] in
  let r729 = Sub (r33) :: r728 in
  let r730 = [R 58] in
  let r731 = R 404 :: r730 in
  let r732 = Sub (r729) :: r731 in
  let r733 = S (T T_COLON) :: r732 in
  let r734 = Sub (r191) :: r733 in
  let r735 = R 411 :: r734 in
  let r736 = [R 400] in
  let r737 = Sub (r33) :: r736 in
  let r738 = [R 56] in
  let r739 = R 404 :: r738 in
  let r740 = Sub (r694) :: r739 in
  let r741 = [R 119] in
  let r742 = Sub (r33) :: r741 in
  let r743 = [R 64] in
  let r744 = Sub (r692) :: r743 in
  let r745 = S (T T_RBRACKET) :: r744 in
  let r746 = [R 86] in
  let r747 = S (T T_LIDENT) :: r746 in
  let r748 = [R 120] in
  let r749 = [R 103] in
  let r750 = Sub (r33) :: r749 in
  let r751 = S (T T_EQUAL) :: r750 in
  let r752 = Sub (r33) :: r751 in
  let r753 = [R 59] in
  let r754 = R 404 :: r753 in
  let r755 = Sub (r752) :: r754 in
  let r756 = [R 60] in
  let r757 = [R 75] in
  let r758 = Sub (r694) :: r757 in
  let r759 = [R 25] in
  let r760 = R 404 :: r759 in
  let r761 = Sub (r758) :: r760 in
  let r762 = S (T T_COLON) :: r761 in
  let r763 = S (T T_LIDENT) :: r762 in
  let r764 = R 83 :: r763 in
  let r765 = [R 76] in
  let r766 = Sub (r758) :: r765 in
  let r767 = S (T T_MINUSGREATER) :: r766 in
  let r768 = Sub (r27) :: r767 in
  let r769 = S (T T_COLON) :: r768 in
  let r770 = [R 77] in
  let r771 = Sub (r758) :: r770 in
  let r772 = S (T T_MINUSGREATER) :: r771 in
  let r773 = [R 78] in
  let r774 = Sub (r758) :: r773 in
  let r775 = S (T T_MINUSGREATER) :: r774 in
  let r776 = [R 79] in
  let r777 = Sub (r758) :: r776 in
  let r778 = [R 13] in
  let r779 = R 404 :: r778 in
  let r780 = R 105 :: r779 in
  let r781 = R 641 :: r780 in
  let r782 = S (T T_LIDENT) :: r781 in
  let r783 = R 365 :: r782 in
  let r784 = [R 441] in
  let r785 = [R 12] in
  let r786 = R 404 :: r785 in
  let r787 = S (N N_module_type) :: r786 in
  let r788 = S (T T_COLON) :: r787 in
  let r789 = S (T T_UIDENT) :: r788 in
  let r790 = [R 455] in
  let r791 = [R 9] in
  let r792 = R 404 :: r791 in
  let r793 = Sub (r694) :: r792 in
  let r794 = S (T T_EQUAL) :: r793 in
  let r795 = S (T T_LIDENT) :: r794 in
  let r796 = R 83 :: r795 in
  let r797 = R 680 :: r796 in
  let r798 = [R 8] in
  let r799 = R 404 :: r798 in
  let r800 = Sub (r758) :: r799 in
  let r801 = S (T T_COLON) :: r800 in
  let r802 = S (T T_LIDENT) :: r801 in
  let r803 = R 83 :: r802 in
  let r804 = R 680 :: r803 in
  let r805 = [R 70] in
  let r806 = Sub (r43) :: r805 in
  let r807 = [R 28] in
  let r808 = Sub (r806) :: r807 in
  let r809 = [R 43] in
  let r810 = Sub (r808) :: r809 in
  let r811 = S (T T_EQUAL) :: r810 in
  let r812 = [R 22] in
  let r813 = R 404 :: r812 in
  let r814 = Sub (r811) :: r813 in
  let r815 = S (T T_LIDENT) :: r814 in
  let r816 = R 83 :: r815 in
  let r817 = [R 71] in
  let r818 = S (T T_END) :: r817 in
  let r819 = Sub (r233) :: r818 in
  let r820 = [R 674] in
  let r821 = Sub (r1) :: r820 in
  let r822 = S (T T_EQUAL) :: r821 in
  let r823 = Sub (r191) :: r822 in
  let r824 = R 319 :: r823 in
  let r825 = R 17 :: r824 in
  let r826 = R 370 :: r825 in
  let r827 = [R 35] in
  let r828 = R 404 :: r827 in
  let r829 = [R 673] in
  let r830 = Sub (r33) :: r829 in
  let r831 = S (T T_COLON) :: r830 in
  let r832 = Sub (r191) :: r831 in
  let r833 = [R 672] in
  let r834 = Sub (r33) :: r833 in
  let r835 = S (T T_COLON) :: r834 in
  let r836 = [R 675] in
  let r837 = Sub (r1) :: r836 in
  let r838 = [R 275] in
  let r839 = Sub (r544) :: r838 in
  let r840 = Sub (r191) :: r839 in
  let r841 = R 409 :: r840 in
  let r842 = R 17 :: r841 in
  let r843 = R 370 :: r842 in
  let r844 = [R 36] in
  let r845 = R 404 :: r844 in
  let r846 = [R 274] in
  let r847 = Sub (r729) :: r846 in
  let r848 = S (T T_COLON) :: r847 in
  let r849 = Sub (r191) :: r848 in
  let r850 = [R 273] in
  let r851 = Sub (r729) :: r850 in
  let r852 = S (T T_COLON) :: r851 in
  let r853 = [R 276] in
  let r854 = Sub (r1) :: r853 in
  let r855 = S (T T_EQUAL) :: r854 in
  let r856 = [R 277] in
  let r857 = Sub (r1) :: r856 in
  let r858 = S (T T_EQUAL) :: r857 in
  let r859 = Sub (r33) :: r858 in
  let r860 = S (T T_DOT) :: r859 in
  let r861 = [R 38] in
  let r862 = R 404 :: r861 in
  let r863 = Sub (r1) :: r862 in
  let r864 = [R 34] in
  let r865 = R 404 :: r864 in
  let r866 = R 374 :: r865 in
  let r867 = Sub (r808) :: r866 in
  let r868 = R 17 :: r867 in
  let r869 = [R 73] in
  let r870 = S (T T_RPAREN) :: r869 in
  let r871 = [R 69] in
  let r872 = Sub (r43) :: r871 in
  let r873 = S (T T_RBRACKET) :: r872 in
  let r874 = [R 46] in
  let r875 = Sub (r808) :: r874 in
  let r876 = S (T T_MINUSGREATER) :: r875 in
  let r877 = Sub (r452) :: r876 in
  let r878 = [R 29] in
  let r879 = Sub (r877) :: r878 in
  let r880 = [R 31] in
  let r881 = Sub (r808) :: r880 in
  let r882 = [R 72] in
  let r883 = S (T T_RPAREN) :: r882 in
  let r884 = [R 373] in
  let r885 = [R 37] in
  let r886 = R 404 :: r885 in
  let r887 = Sub (r752) :: r886 in
  let r888 = [R 39] in
  let r889 = [R 44] in
  let r890 = Sub (r808) :: r889 in
  let r891 = S (T T_EQUAL) :: r890 in
  let r892 = [R 45] in
  let r893 = [R 604] in
  let r894 = [R 623] in
  let r895 = [R 11] in
  let r896 = R 404 :: r895 in
  let r897 = Sub (r265) :: r896 in
  let r898 = S (T T_UIDENT) :: r897 in
  let r899 = [R 619] in
  let r900 = [R 7] in
  let r901 = R 404 :: r900 in
  let r902 = Sub (r811) :: r901 in
  let r903 = S (T T_LIDENT) :: r902 in
  let r904 = R 83 :: r903 in
  let r905 = R 680 :: r904 in
  let r906 = [R 603] in
  let r907 = R 621 :: r906 in
  let r908 = [R 382] in
  let r909 = S (T T_RPAREN) :: r908 in
  let r910 = S (N N_pattern) :: r909 in
  let r911 = S (T T_COMMA) :: r910 in
  let r912 = S (N N_pattern) :: r911 in
  let r913 = S (T T_LPAREN) :: r912 in
  let r914 = [R 51] in
  let r915 = S (T T_RPAREN) :: r914 in
  let r916 = [R 501] in
  let r917 = S (T T_BARRBRACKET) :: r916 in
  let r918 = [R 186] in
  let r919 = R 17 :: r918 in
  let r920 = Sub (r135) :: r919 in
  let r921 = [R 463] in
  let r922 = [R 473] in
  let r923 = [R 19] in
  let r924 = R 17 :: r923 in
  let r925 = R 206 :: r924 in
  let r926 = [R 647] in
  let r927 = S (T T_RBRACE) :: r926 in
  let r928 = Sub (r199) :: r927 in
  let r929 = [R 649] in
  let r930 = [R 648] in
  let r931 = [R 650] in
  let r932 = S (T T_RBRACE) :: r931 in
  let r933 = [R 434] in
  let r934 = Sub (r121) :: r933 in
  let r935 = [R 438] in
  let r936 = R 404 :: r935 in
  let r937 = Sub (r934) :: r936 in
  let r938 = R 409 :: r937 in
  let r939 = [R 212] in
  let r940 = [R 213] in
  let r941 = [R 375] in
  function
  | 0 | 1461 | 1465 -> Nothing
  | 1460 -> One ([R 0])
  | 1464 -> One ([R 1])
  | 1468 -> One ([R 2])
  | 304 -> One ([R 3])
  | 303 -> One ([R 4])
  | 117 -> One (R 17 :: r69)
  | 206 -> One (R 17 :: r169)
  | 229 -> One (R 17 :: r181)
  | 298 -> One (R 17 :: r229)
  | 328 -> One (R 17 :: r254)
  | 356 -> One (R 17 :: r282)
  | 435 -> One (R 17 :: r335)
  | 446 -> One (R 17 :: r346)
  | 687 -> One (R 17 :: r532)
  | 1009 -> One (R 17 :: r710)
  | 1018 -> One (R 17 :: r719)
  | 1035 -> One (R 17 :: r735)
  | 1050 -> One (R 17 :: r740)
  | 1068 -> One (R 17 :: r755)
  | 1115 -> One (R 17 :: r783)
  | 1131 -> One (R 17 :: r789)
  | 1148 -> One (R 17 :: r797)
  | 1159 -> One (R 17 :: r804)
  | 1178 -> One (R 17 :: r819)
  | 1234 -> One (R 17 :: r863)
  | 1247 -> One (R 17 :: r879)
  | 1272 -> One (R 17 :: r887)
  | 1300 -> One (R 17 :: r898)
  | 1317 -> One (R 17 :: r905)
  | 1325 -> One ([R 23])
  | 1324 -> One ([R 24])
  | 1168 -> One ([R 26])
  | 1167 -> One ([R 27])
  | 1255 -> One ([R 30])
  | 1258 -> One ([R 32])
  | 1253 -> One ([R 33])
  | 1278 -> One ([R 40])
  | 1279 -> One ([R 42])
  | 1260 -> One ([R 47])
  | 1077 -> One ([R 61])
  | 1078 -> One ([R 63])
  | 1067 -> One ([R 67])
  | 1063 -> One ([R 68])
  | 1157 -> One ([R 81])
  | 1156 -> One ([R 82])
  | 482 -> One ([R 88])
  | 293 -> One ([R 89])
  | 481 -> One ([R 90])
  | 139 | 166 -> One ([R 91])
  | 140 -> One ([R 96])
  | 277 -> One ([R 97])
  | 271 -> One ([R 101])
  | 1409 -> One ([R 110])
  | 1404 -> One ([R 111])
  | 394 -> One ([R 113])
  | 779 -> One ([R 125])
  | 603 -> One ([R 127])
  | 763 -> One ([R 128])
  | 631 -> One ([R 137])
  | 640 -> One ([R 138])
  | 624 -> One ([R 139])
  | 638 -> One ([R 176])
  | 832 -> One ([R 180])
  | 1 -> One (R 181 :: r6)
  | 60 -> One (R 181 :: r38)
  | 291 -> One (R 181 :: r223)
  | 295 -> One (R 181 :: r228)
  | 305 -> One (R 181 :: r236)
  | 439 -> One (R 181 :: r340)
  | 483 -> One (R 181 :: r387)
  | 485 -> One (R 181 :: r389)
  | 501 -> One (R 181 :: r402)
  | 503 -> One (R 181 :: r405)
  | 523 -> One (R 181 :: r426)
  | 527 -> One (R 181 :: r428)
  | 576 -> One (R 181 :: r458)
  | 591 -> One (R 181 :: r482)
  | 595 -> One (R 181 :: r484)
  | 879 -> One (R 181 :: r614)
  | 942 -> One (R 181 :: r645)
  | 946 -> One (R 181 :: r654)
  | 968 -> One (R 181 :: r674)
  | 991 -> One (R 181 :: r690)
  | 846 -> One ([R 191])
  | 360 -> One ([R 202])
  | 359 -> One ([R 203])
  | 424 -> One ([R 204])
  | 425 -> One ([R 205])
  | 106 | 417 -> One ([R 210])
  | 265 -> One ([R 219])
  | 266 -> One ([R 220])
  | 764 -> One ([R 231])
  | 766 -> One ([R 232])
  | 854 -> One ([R 244])
  | 853 -> One ([R 245])
  | 513 -> One ([R 249])
  | 517 -> One ([R 251])
  | 737 -> One ([R 253])
  | 628 -> One ([R 258])
  | 748 -> One ([R 259])
  | 693 -> One ([R 260])
  | 704 -> One ([R 263])
  | 815 -> One ([R 264])
  | 819 -> One ([R 267])
  | 1387 -> One ([R 269])
  | 1386 -> One ([R 270])
  | 1388 -> One ([R 271])
  | 149 -> One ([R 272])
  | 456 -> One ([R 292])
  | 455 -> One ([R 303])
  | 457 -> One ([R 304])
  | 364 -> One ([R 305])
  | 420 -> One ([R 312])
  | 414 -> One ([R 313])
  | 419 -> One ([R 317])
  | 1020 -> One (R 319 :: r723)
  | 1189 -> One (R 319 :: r832)
  | 249 | 1194 -> One ([R 320])
  | 246 -> One ([R 323])
  | 121 -> One ([R 325])
  | 66 | 301 -> One ([R 327])
  | 81 -> One ([R 328])
  | 80 -> One ([R 329])
  | 79 -> One ([R 330])
  | 78 -> One ([R 331])
  | 77 -> One ([R 332])
  | 65 -> One ([R 333])
  | 86 | 825 -> One ([R 334])
  | 69 | 318 | 443 -> One ([R 335])
  | 68 | 442 -> One ([R 336])
  | 75 | 465 | 526 -> One ([R 337])
  | 74 | 464 -> One ([R 338])
  | 64 -> One ([R 339])
  | 83 -> One ([R 340])
  | 76 -> One ([R 341])
  | 82 -> One ([R 342])
  | 71 -> One ([R 343])
  | 85 -> One ([R 344])
  | 87 -> One ([R 345])
  | 88 -> One ([R 346])
  | 84 -> One ([R 347])
  | 67 -> One ([R 348])
  | 70 -> One ([R 349])
  | 208 -> One ([R 350])
  | 207 -> One (R 351 :: r174)
  | 177 -> One (R 352 :: r151)
  | 178 -> One ([R 353])
  | 514 -> One (R 358 :: r419)
  | 571 -> One (R 358 :: r444)
  | 830 -> One (R 358 :: r593)
  | 838 -> One (R 358 :: r596)
  | 1357 -> One (R 358 :: r917)
  | 515 | 565 | 831 | 845 -> One ([R 359])
  | 856 -> One ([R 360])
  | 1122 -> One ([R 366])
  | 342 -> One (R 370 :: r261)
  | 1238 -> One (R 370 :: r868)
  | 343 -> One ([R 371])
  | 531 -> One ([R 376])
  | 536 -> One ([R 378])
  | 541 -> One ([R 386])
  | 551 -> One ([R 389])
  | 566 -> One ([R 391])
  | 699 -> One ([R 392])
  | 1171 -> One ([R 396])
  | 348 -> One (R 404 :: r262)
  | 1075 -> One (R 404 :: r756)
  | 1144 -> One (R 404 :: r790)
  | 1276 -> One (R 404 :: r888)
  | 1312 -> One (R 404 :: r899)
  | 1327 -> One (R 404 :: r907)
  | 953 -> One ([R 408])
  | 1209 -> One (R 409 :: r849)
  | 164 | 1214 -> One ([R 410])
  | 1039 -> One ([R 412])
  | 1037 -> One ([R 413])
  | 1040 -> One ([R 414])
  | 1038 -> One ([R 415])
  | 892 -> One ([R 417])
  | 1306 -> One ([R 419])
  | 1305 -> One ([R 420])
  | 1138 -> One ([R 422])
  | 1137 -> One ([R 423])
  | 188 -> One ([R 426])
  | 685 -> One ([R 431])
  | 686 -> One ([R 432])
  | 1437 -> One ([R 435])
  | 1434 -> One ([R 436])
  | 966 -> One (R 439 :: r667)
  | 967 -> One (R 439 :: r668)
  | 1125 -> One (R 439 :: r784)
  | 1113 -> One ([R 442])
  | 1139 -> One ([R 443])
  | 1114 -> One ([R 444])
  | 1127 -> One ([R 445])
  | 1129 -> One ([R 446])
  | 1142 -> One ([R 447])
  | 1143 -> One ([R 448])
  | 1130 -> One ([R 449])
  | 1141 -> One ([R 450])
  | 1140 -> One ([R 451])
  | 1128 -> One ([R 452])
  | 1158 -> One ([R 453])
  | 1147 -> One ([R 454])
  | 1146 -> One ([R 456])
  | 316 -> One ([R 459])
  | 313 -> One ([R 461])
  | 187 -> One ([R 466])
  | 192 -> One ([R 467])
  | 1396 -> One ([R 468])
  | 214 | 1105 -> One ([R 482])
  | 488 -> One ([R 485])
  | 615 -> One ([R 486])
  | 490 | 623 -> One ([R 488])
  | 760 | 777 -> One ([R 493])
  | 613 -> One ([R 519])
  | 767 -> One ([R 520])
  | 765 -> One ([R 521])
  | 529 | 689 -> One ([R 522])
  | 532 -> One ([R 525])
  | 562 -> One ([R 527])
  | 561 -> One ([R 528])
  | 544 -> One ([R 538])
  | 28 -> One ([R 539])
  | 8 -> One ([R 540])
  | 52 -> One ([R 542])
  | 51 -> One ([R 543])
  | 50 -> One ([R 544])
  | 49 -> One ([R 545])
  | 48 -> One ([R 546])
  | 47 -> One ([R 547])
  | 46 -> One ([R 548])
  | 45 -> One ([R 549])
  | 44 -> One ([R 550])
  | 43 -> One ([R 551])
  | 42 -> One ([R 552])
  | 41 -> One ([R 553])
  | 40 -> One ([R 554])
  | 39 -> One ([R 555])
  | 38 -> One ([R 556])
  | 37 -> One ([R 557])
  | 36 -> One ([R 558])
  | 35 -> One ([R 559])
  | 34 -> One ([R 560])
  | 33 -> One ([R 561])
  | 32 -> One ([R 562])
  | 31 -> One ([R 563])
  | 30 -> One ([R 564])
  | 29 -> One ([R 565])
  | 27 -> One ([R 566])
  | 26 -> One ([R 567])
  | 25 -> One ([R 568])
  | 24 -> One ([R 569])
  | 23 -> One ([R 570])
  | 22 -> One ([R 571])
  | 21 -> One ([R 572])
  | 20 -> One ([R 573])
  | 19 -> One ([R 574])
  | 18 -> One ([R 575])
  | 17 -> One ([R 576])
  | 16 -> One ([R 577])
  | 15 -> One ([R 578])
  | 14 -> One ([R 579])
  | 13 -> One ([R 580])
  | 12 -> One ([R 581])
  | 11 -> One ([R 582])
  | 10 -> One ([R 583])
  | 9 -> One ([R 584])
  | 7 -> One ([R 585])
  | 6 -> One ([R 586])
  | 5 -> One ([R 587])
  | 4 -> One ([R 588])
  | 3 -> One ([R 589])
  | 1298 -> One ([R 590])
  | 1379 -> One ([R 593])
  | 1370 -> One ([R 594])
  | 1378 -> One ([R 595])
  | 1369 -> One ([R 596])
  | 1368 -> One ([R 597])
  | 1292 -> One ([R 605])
  | 1311 | 1330 -> One ([R 606])
  | 1307 -> One ([R 607])
  | 1289 -> One ([R 608])
  | 1290 -> One ([R 609])
  | 1295 -> One ([R 610])
  | 1297 -> One ([R 611])
  | 1310 -> One ([R 612])
  | 1299 -> One ([R 613])
  | 1309 -> One ([R 614])
  | 1308 -> One ([R 615])
  | 1316 -> One ([R 616])
  | 1315 -> One ([R 617])
  | 1296 -> One ([R 618])
  | 1314 -> One ([R 620])
  | 1293 -> One (R 621 :: r894)
  | 438 -> One ([R 624])
  | 437 -> One ([R 625])
  | 333 -> One ([R 629])
  | 334 -> One ([R 630])
  | 336 -> One ([R 631])
  | 338 -> One ([R 632])
  | 335 -> One ([R 633])
  | 332 -> One ([R 634])
  | 1124 -> One ([R 639])
  | 1123 -> One ([R 640])
  | 1415 -> One ([R 642])
  | 1402 -> One ([R 643])
  | 1423 -> One ([R 644])
  | 367 -> One (R 656 :: r298)
  | 399 -> One ([R 657])
  | 123 -> One ([R 661])
  | 124 -> One ([R 662])
  | 92 | 479 | 864 -> One ([R 665])
  | 90 -> One ([R 667])
  | 337 -> One ([R 670])
  | 340 -> One ([R 671])
  | 1025 -> One (R 680 :: r727)
  | 1081 -> One (R 680 :: r764)
  | 1173 -> One (R 680 :: r816)
  | 1001 -> One ([R 681])
  | 385 -> One ([R 689])
  | 72 | 319 | 444 | 495 -> One (S (T T_error) :: r39)
  | 849 -> One (S (T T_WITH) :: r606)
  | 278 | 339 -> One (S (T T_UIDENT) :: r48)
  | 197 -> One (S (T T_UIDENT) :: r167)
  | 320 -> One (S (T T_UIDENT) :: r245)
  | 324 -> One (S (T T_TYPE) :: r251)
  | 701 -> One (S (T T_TYPE) :: r550)
  | 998 | 1172 -> One (S (T T_TYPE) :: r702)
  | 89 -> One (S (T T_RPAREN) :: r40)
  | 142 | 167 -> One (S (T T_RPAREN) :: r99)
  | 168 -> One (S (T T_RPAREN) :: r128)
  | 272 -> One (S (T T_RPAREN) :: r211)
  | 358 -> One (S (T T_RPAREN) :: r283)
  | 451 -> One (S (T T_RPAREN) :: r347)
  | 453 -> One (S (T T_RPAREN) :: r348)
  | 548 -> One (S (T T_RPAREN) :: r437)
  | 826 -> One (S (T T_RPAREN) :: r590)
  | 896 -> One (S (T T_RPAREN) :: r624)
  | 903 -> One (S (T T_RPAREN) :: r625)
  | 905 -> One (S (T T_RPAREN) :: r626)
  | 971 -> One (S (T T_RPAREN) :: r681)
  | 1342 -> One (S (T T_RPAREN) :: r913)
  | 1392 -> One (S (T T_RPAREN) :: r921)
  | 170 | 1397 -> One (S (T T_RBRACKET) :: r129)
  | 180 -> One (S (T T_RBRACKET) :: r152)
  | 233 -> One (S (T T_RBRACKET) :: r182)
  | 274 -> One (S (T T_RBRACKET) :: r212)
  | 836 -> One (S (T T_RBRACKET) :: r594)
  | 222 -> One (S (T T_QUOTE) :: r179)
  | 162 -> One (S (T T_PLUSEQ) :: r127)
  | 1427 -> One (S (T T_PLUSEQ) :: r938)
  | 113 -> One (S (T T_MODULE) :: r66)
  | 283 -> One (S (T T_MINUSGREATER) :: r215)
  | 1100 -> One (S (T T_MINUSGREATER) :: r777)
  | 109 -> One (S (T T_LIDENT) :: r57)
  | 1086 -> One (S (T T_LIDENT) :: r769)
  | 1268 -> One (S (T T_LIDENT) :: r884)
  | 629 -> One (S (T T_LESSMINUS) :: r505)
  | 1411 -> One (S (T T_LBRACE) :: r928)
  | 311 -> One (S (T T_INT) :: r242)
  | 314 -> One (S (T T_INT) :: r243)
  | 625 -> One (S (T T_IN) :: r503)
  | 1251 -> One (S (T T_IN) :: r881)
  | 474 -> One (S (T T_GREATERRBRACE) :: r371)
  | 873 -> One (S (T T_GREATERRBRACE) :: r609)
  | 146 -> One (S (T T_GREATER) :: r104)
  | 150 -> One (S (T T_GREATER) :: r106)
  | 404 -> One (S (T T_EQUAL) :: r323)
  | 717 -> One (S (T T_EQUAL) :: r557)
  | 1203 -> One (S (T T_EQUAL) :: r837)
  | 1372 -> One (S (T T_EQUAL) :: r920)
  | 1458 -> One (S (T T_EOF) :: r939)
  | 1462 -> One (S (T T_EOF) :: r940)
  | 1466 -> One (S (T T_EOF) :: r941)
  | 868 -> One (S (T T_END) :: r608)
  | 138 -> One (S (T T_DOTDOT) :: r89)
  | 1416 -> One (S (T T_DOTDOT) :: r929)
  | 100 -> One (S (T T_DOT) :: r47)
  | 253 -> One (S (T T_DOT) :: r209)
  | 380 -> One (S (T T_DOT) :: r311)
  | 415 -> One (S (T T_DOT) :: r327)
  | 732 -> One (S (T T_DOT) :: r567)
  | 1044 -> One (S (T T_DOT) :: r737)
  | 1056 -> One (S (T T_DOT) :: r747)
  | 152 -> One (S (T T_COLON) :: r113)
  | 362 -> One (S (T T_COLON) :: r286)
  | 972 -> One (S (T T_COLON) :: r685)
  | 302 -> One (S (T T_BARRBRACKET) :: r230)
  | 506 -> One (S (T T_BARRBRACKET) :: r406)
  | 828 -> One (S (T T_BARRBRACKET) :: r591)
  | 183 | 1098 -> One (S (T T_BAR) :: r157)
  | 235 -> One (S (T T_BAR) :: r185)
  | 341 -> One (S (N N_structure) :: r256)
  | 1291 -> One (S (N N_structure) :: r893)
  | 307 -> One (S (N N_pattern) :: r238)
  | 522 -> One (S (N N_pattern) :: r421)
  | 537 -> One (S (N N_pattern) :: r433)
  | 539 -> One (S (N N_pattern) :: r434)
  | 542 -> One (S (N N_pattern) :: r435)
  | 545 -> One (S (N N_pattern) :: r436)
  | 550 -> One (S (N N_pattern) :: r438)
  | 556 -> One (S (N N_pattern) :: r441)
  | 936 -> One (S (N N_pattern) :: r638)
  | 354 -> One (S (N N_module_type) :: r276)
  | 355 -> One (S (N N_module_type) :: r278)
  | 412 -> One (S (N N_module_type) :: r325)
  | 882 -> One (S (N N_module_type) :: r617)
  | 924 -> One (S (N N_module_type) :: r634)
  | 434 -> One (S (N N_module_expr) :: r332)
  | 445 -> One (S (N N_module_expr) :: r342)
  | 496 -> One (S (N N_module_expr) :: r398)
  | 692 -> One (S (N N_let_pattern) :: r538)
  | 477 -> One (S (N N_expr) :: r373)
  | 602 -> One (S (N N_expr) :: r493)
  | 622 -> One (S (N N_expr) :: r501)
  | 632 -> One (S (N N_expr) :: r506)
  | 634 -> One (S (N N_expr) :: r507)
  | 636 -> One (S (N N_expr) :: r508)
  | 641 -> One (S (N N_expr) :: r509)
  | 643 -> One (S (N N_expr) :: r510)
  | 645 -> One (S (N N_expr) :: r511)
  | 647 -> One (S (N N_expr) :: r512)
  | 649 -> One (S (N N_expr) :: r513)
  | 651 -> One (S (N N_expr) :: r514)
  | 653 -> One (S (N N_expr) :: r515)
  | 655 -> One (S (N N_expr) :: r516)
  | 657 -> One (S (N N_expr) :: r517)
  | 659 -> One (S (N N_expr) :: r518)
  | 661 -> One (S (N N_expr) :: r519)
  | 663 -> One (S (N N_expr) :: r520)
  | 665 -> One (S (N N_expr) :: r521)
  | 667 -> One (S (N N_expr) :: r522)
  | 669 -> One (S (N N_expr) :: r523)
  | 671 -> One (S (N N_expr) :: r524)
  | 673 -> One (S (N N_expr) :: r525)
  | 675 -> One (S (N N_expr) :: r526)
  | 677 -> One (S (N N_expr) :: r527)
  | 680 -> One (S (N N_expr) :: r528)
  | 682 -> One (S (N N_expr) :: r529)
  | 753 -> One (S (N N_expr) :: r574)
  | 758 -> One (S (N N_expr) :: r575)
  | 761 -> One (S (N N_expr) :: r576)
  | 823 -> One (S (N N_expr) :: r589)
  | 858 -> One (S (N N_expr) :: r607)
  | 575 -> One (Sub (r1) :: r450)
  | 590 -> One (Sub (r1) :: r471)
  | 938 -> One (Sub (r1) :: r639)
  | 2 -> One (Sub (r10) :: r12)
  | 55 -> One (Sub (r10) :: r13)
  | 58 -> One (Sub (r10) :: r18)
  | 156 -> One (Sub (r10) :: r117)
  | 289 -> One (Sub (r10) :: r218)
  | 934 -> One (Sub (r10) :: r637)
  | 940 -> One (Sub (r10) :: r642)
  | 558 -> One (Sub (r21) :: r442)
  | 216 -> One (Sub (r25) :: r176)
  | 281 -> One (Sub (r25) :: r213)
  | 800 -> One (Sub (r25) :: r585)
  | 1091 -> One (Sub (r27) :: r772)
  | 1095 -> One (Sub (r27) :: r775)
  | 112 -> One (Sub (r29) :: r60)
  | 145 -> One (Sub (r29) :: r103)
  | 220 -> One (Sub (r29) :: r177)
  | 226 -> One (Sub (r31) :: r180)
  | 519 -> One (Sub (r33) :: r420)
  | 553 -> One (Sub (r33) :: r440)
  | 582 -> One (Sub (r33) :: r466)
  | 694 -> One (Sub (r33) :: r539)
  | 711 -> One (Sub (r33) :: r552)
  | 715 -> One (Sub (r33) :: r555)
  | 739 -> One (Sub (r33) :: r570)
  | 1011 -> One (Sub (r33) :: r712)
  | 1060 -> One (Sub (r33) :: r748)
  | 1351 -> One (Sub (r33) :: r915)
  | 97 -> One (Sub (r43) :: r44)
  | 190 -> One (Sub (r43) :: r160)
  | 1394 -> One (Sub (r43) :: r922)
  | 128 -> One (Sub (r50) :: r87)
  | 254 -> One (Sub (r50) :: r210)
  | 330 -> One (Sub (r50) :: r255)
  | 323 -> One (Sub (r62) :: r247)
  | 460 -> One (Sub (r62) :: r350)
  | 913 -> One (Sub (r62) :: r628)
  | 916 -> One (Sub (r62) :: r630)
  | 919 -> One (Sub (r62) :: r632)
  | 133 -> One (Sub (r82) :: r88)
  | 125 -> One (Sub (r84) :: r86)
  | 1398 -> One (Sub (r91) :: r925)
  | 141 -> One (Sub (r97) :: r98)
  | 1418 -> One (Sub (r97) :: r930)
  | 212 -> One (Sub (r141) :: r175)
  | 175 -> One (Sub (r143) :: r144)
  | 204 -> One (Sub (r146) :: r168)
  | 182 -> One (Sub (r148) :: r154)
  | 194 -> One (Sub (r164) :: r166)
  | 243 -> One (Sub (r188) :: r190)
  | 250 -> One (Sub (r191) :: r207)
  | 607 -> One (Sub (r191) :: r497)
  | 1195 -> One (Sub (r191) :: r835)
  | 1215 -> One (Sub (r191) :: r852)
  | 248 -> One (Sub (r199) :: r201)
  | 1419 -> One (Sub (r199) :: r932)
  | 294 -> One (Sub (r225) :: r226)
  | 487 -> One (Sub (r225) :: r390)
  | 598 -> One (Sub (r225) :: r485)
  | 611 -> One (Sub (r225) :: r499)
  | 616 -> One (Sub (r225) :: r500)
  | 309 -> One (Sub (r240) :: r241)
  | 981 -> One (Sub (r257) :: r687)
  | 889 -> One (Sub (r265) :: r618)
  | 408 -> One (Sub (r291) :: r324)
  | 366 -> One (Sub (r293) :: r294)
  | 375 -> One (Sub (r304) :: r309)
  | 368 -> One (Sub (r306) :: r308)
  | 1003 -> One (Sub (r306) :: r704)
  | 383 -> One (Sub (r313) :: r316)
  | 389 -> One (Sub (r320) :: r321)
  | 478 -> One (Sub (r382) :: r384)
  | 848 -> One (Sub (r382) :: r604)
  | 507 -> One (Sub (r408) :: r411)
  | 508 -> One (Sub (r416) :: r418)
  | 707 -> One (Sub (r424) :: r551)
  | 533 -> One (Sub (r431) :: r432)
  | 804 -> One (Sub (r455) :: r586)
  | 578 -> One (Sub (r460) :: r461)
  | 587 -> One (Sub (r460) :: r467)
  | 579 -> One (Sub (r463) :: r465)
  | 588 -> One (Sub (r463) :: r470)
  | 604 -> One (Sub (r495) :: r496)
  | 609 -> One (Sub (r495) :: r498)
  | 690 -> One (Sub (r534) :: r535)
  | 726 -> One (Sub (r541) :: r563)
  | 791 -> One (Sub (r541) :: r579)
  | 797 -> One (Sub (r541) :: r582)
  | 1222 -> One (Sub (r541) :: r860)
  | 722 -> One (Sub (r546) :: r558)
  | 817 -> One (Sub (r587) :: r588)
  | 841 -> One (Sub (r600) :: r602)
  | 970 -> One (Sub (r677) :: r679)
  | 1221 -> One (Sub (r729) :: r855)
  | 1053 -> One (Sub (r742) :: r745)
  | 1243 -> One (Sub (r742) :: r873)
  | 1264 -> One (Sub (r758) :: r883)
  | 1281 -> One (Sub (r758) :: r891)
  | 1241 -> One (Sub (r808) :: r870)
  | 1285 -> One (Sub (r811) :: r892)
  | 1184 -> One (Sub (r826) :: r828)
  | 1206 -> One (Sub (r843) :: r845)
  | 684 -> One (r0)
  | 1457 -> One (r2)
  | 1456 -> One (r3)
  | 1455 -> One (r4)
  | 1454 -> One (r5)
  | 1453 -> One (r6)
  | 53 -> One (r7)
  | 54 -> One (r9)
  | 1452 -> One (r11)
  | 57 -> One (r12)
  | 56 -> One (r13)
  | 1331 -> One (r14)
  | 1451 -> One (r16)
  | 1450 -> One (r17)
  | 59 -> One (r18)
  | 63 -> One (r19)
  | 62 | 300 | 317 | 441 | 525 | 790 | 796 -> One (r20)
  | 95 -> One (r22)
  | 189 -> One (r24)
  | 211 -> One (r26)
  | 210 -> One (r28)
  | 219 -> One (r30)
  | 390 -> One (r32)
  | 1449 -> One (r34)
  | 1448 -> One (r35)
  | 94 -> One (r36)
  | 93 -> One (r37)
  | 61 -> One (r38)
  | 73 -> One (r39)
  | 91 -> One (r40)
  | 96 | 116 -> One (r41)
  | 99 -> One (r42)
  | 104 -> One (r44)
  | 98 -> One (r45)
  | 103 -> One (r46)
  | 101 -> One (r47)
  | 102 -> One (r48)
  | 107 -> One (r49)
  | 108 -> One (r51)
  | 105 -> One (r52)
  | 1447 -> One (r53)
  | 1446 -> One (r54)
  | 1445 -> One (r55)
  | 111 -> One (r56)
  | 110 -> One (r57)
  | 1444 -> One (r58)
  | 1443 -> One (r59)
  | 1442 -> One (r60)
  | 463 -> One (r61)
  | 1441 -> One (r63)
  | 1440 -> One (r64)
  | 115 -> One (r65)
  | 114 -> One (r66)
  | 1439 -> One (r67)
  | 1438 -> One (r68)
  | 118 -> One (r69)
  | 1426 -> One (r70)
  | 1425 -> One (r71)
  | 1424 -> One (r72)
  | 137 -> One (r73)
  | 136 | 161 -> One (r74)
  | 122 | 160 -> One (r75)
  | 120 | 159 -> One (r76)
  | 119 | 158 -> One (r77)
  | 127 -> One (r78)
  | 130 -> One (r80)
  | 126 -> One (r81)
  | 135 -> One (r83)
  | 132 -> One (r85)
  | 131 -> One (r86)
  | 129 -> One (r87)
  | 134 -> One (r88)
  | 1410 -> One (r89)
  | 172 -> One (r90)
  | 1408 -> One (r92)
  | 1407 -> One (r93)
  | 1406 -> One (r94)
  | 1405 -> One (r96)
  | 1403 -> One (r98)
  | 143 -> One (r99)
  | 144 | 176 | 1094 -> One (r100)
  | 1391 -> One (r101)
  | 1390 -> One (r102)
  | 1389 -> One (r103)
  | 148 -> One (r104)
  | 147 | 379 -> One (r105)
  | 151 -> One (r106)
  | 262 -> One (r107)
  | 1385 -> One (r109)
  | 1384 -> One (r110)
  | 1383 -> One (r111)
  | 1382 -> One (r112)
  | 153 -> One (r113)
  | 1381 -> One (r115)
  | 1380 -> One (r116)
  | 157 -> One (r117)
  | 1377 -> One (r118)
  | 1376 -> One (r119)
  | 1436 -> One (r120)
  | 1371 -> One (r122)
  | 1367 -> One (r124)
  | 288 -> One (r125)
  | 165 -> One (r126)
  | 163 -> One (r127)
  | 169 -> One (r128)
  | 171 -> One (r129)
  | 287 -> One (r130)
  | 286 -> One (r131)
  | 1431 -> One (r132)
  | 1430 -> One (r133)
  | 276 -> One (r134)
  | 280 -> One (r136)
  | 279 -> One (r137)
  | 270 -> One (r138)
  | 213 -> One (r140)
  | 268 -> One (r142)
  | 269 -> One (r144)
  | 201 | 1099 -> One (r145)
  | 232 -> One (r147)
  | 242 -> One (r149)
  | 241 -> One (r150)
  | 179 -> One (r151)
  | 181 -> One (r152)
  | 240 -> One (r153)
  | 239 -> One (r154)
  | 203 -> One (r155)
  | 202 -> One (r156)
  | 184 -> One (r157)
  | 186 -> One (r158)
  | 185 -> One (r159)
  | 191 -> One (r160)
  | 200 | 1104 -> One (r161)
  | 199 | 1103 -> One (r162)
  | 193 | 1102 -> One (r163)
  | 196 -> One (r165)
  | 195 -> One (r166)
  | 198 -> One (r167)
  | 205 -> One (r168)
  | 231 -> One (r169)
  | 218 -> One (r170)
  | 228 -> One (r172)
  | 225 -> One (r173)
  | 209 -> One (r174)
  | 215 -> One (r175)
  | 217 -> One (r176)
  | 221 -> One (r177)
  | 224 -> One (r178)
  | 223 -> One (r179)
  | 227 -> One (r180)
  | 230 -> One (r181)
  | 234 -> One (r182)
  | 238 -> One (r183)
  | 237 -> One (r184)
  | 236 -> One (r185)
  | 247 -> One (r187)
  | 245 -> One (r189)
  | 244 -> One (r190)
  | 267 -> One (r198)
  | 264 -> One (r200)
  | 263 -> One (r201)
  | 261 -> One (r202)
  | 260 -> One (r203)
  | 259 -> One (r204)
  | 258 -> One (r205)
  | 252 -> One (r206)
  | 251 -> One (r207)
  | 257 -> One (r208)
  | 256 -> One (r209)
  | 255 -> One (r210)
  | 273 -> One (r211)
  | 275 -> One (r212)
  | 282 -> One (r213)
  | 285 -> One (r214)
  | 284 -> One (r215)
  | 1366 -> One (r216)
  | 1365 -> One (r217)
  | 290 -> One (r218)
  | 1364 -> One (r219)
  | 1363 -> One (r220)
  | 1362 -> One (r221)
  | 1361 -> One (r222)
  | 292 -> One (r223)
  | 614 | 639 -> One (r224)
  | 1360 -> One (r226)
  | 297 -> One (r227)
  | 296 -> One (r228)
  | 299 -> One (r229)
  | 1356 -> One (r230)
  | 1183 -> One (r231)
  | 1182 -> One (r232)
  | 1355 -> One (r234)
  | 1354 -> One (r235)
  | 306 -> One (r236)
  | 1350 -> One (r237)
  | 1349 -> One (r238)
  | 308 -> One (r239)
  | 310 -> One (r241)
  | 312 -> One (r242)
  | 315 -> One (r243)
  | 322 -> One (r244)
  | 321 -> One (r245)
  | 1341 -> One (r246)
  | 1340 -> One (r247)
  | 1339 -> One (r248)
  | 327 -> One (r249)
  | 326 -> One (r250)
  | 325 -> One (r251)
  | 1338 -> One (r252)
  | 1337 -> One (r253)
  | 329 -> One (r254)
  | 331 -> One (r255)
  | 1336 -> One (r256)
  | 347 -> One (r258)
  | 346 -> One (r259)
  | 345 -> One (r260)
  | 344 -> One (r261)
  | 349 -> One (r262)
  | 927 -> One (r263)
  | 433 -> One (r264)
  | 933 -> One (r266)
  | 932 -> One (r267)
  | 931 -> One (r268)
  | 930 -> One (r269)
  | 430 -> One (r271)
  | 429 -> One (r272)
  | 353 -> One (r273)
  | 352 -> One (r274)
  | 351 -> One (r275)
  | 428 -> One (r276)
  | 427 -> One (r277)
  | 426 -> One (r278)
  | 423 -> One (r279)
  | 422 -> One (r280)
  | 421 -> One (r281)
  | 357 -> One (r282)
  | 361 -> One (r283)
  | 411 -> One (r284)
  | 365 -> One (r285)
  | 363 -> One (r286)
  | 403 -> One (r287)
  | 402 -> One (r288)
  | 401 -> One (r289)
  | 400 -> One (r290)
  | 410 -> One (r292)
  | 407 -> One (r294)
  | 398 -> One (r295)
  | 397 -> One (r296)
  | 396 -> One (r297)
  | 378 -> One (r298)
  | 371 -> One (r299)
  | 370 -> One (r300)
  | 372 -> One (r302)
  | 369 -> One (r303)
  | 377 -> One (r305)
  | 374 -> One (r307)
  | 373 -> One (r308)
  | 376 -> One (r309)
  | 382 -> One (r310)
  | 381 -> One (r311)
  | 384 -> One (r312)
  | 388 -> One (r314)
  | 387 -> One (r315)
  | 386 -> One (r316)
  | 393 -> One (r317)
  | 392 -> One (r318)
  | 391 -> One (r319)
  | 395 -> One (r321)
  | 406 -> One (r322)
  | 405 -> One (r323)
  | 409 -> One (r324)
  | 413 -> One (r325)
  | 418 -> One (r326)
  | 416 -> One (r327)
  | 929 -> One (r328)
  | 928 -> One (r329)
  | 432 -> One (r330)
  | 923 -> One (r331)
  | 922 -> One (r332)
  | 912 -> One (r333)
  | 911 -> One (r334)
  | 436 -> One (r335)
  | 910 -> One (r336)
  | 909 -> One (r337)
  | 908 -> One (r338)
  | 907 -> One (r339)
  | 440 -> One (r340)
  | 459 -> One (r341)
  | 458 -> One (r342)
  | 450 -> One (r343)
  | 449 -> One (r344)
  | 448 -> One (r345)
  | 447 -> One (r346)
  | 452 -> One (r347)
  | 454 -> One (r348)
  | 462 -> One (r349)
  | 461 -> One (r350)
  | 745 -> One (r351)
  | 744 -> One (r352)
  | 743 -> One (r353)
  | 895 -> One (r355)
  | 894 -> One (r356)
  | 893 -> One (r357)
  | 891 -> One (r358)
  | 1242 -> One (r359)
  | 878 -> One (r360)
  | 472 -> One (r361)
  | 471 -> One (r362)
  | 470 -> One (r363)
  | 469 -> One (r364)
  | 468 -> One (r365)
  | 835 -> One (r366)
  | 877 -> One (r368)
  | 876 -> One (r369)
  | 875 -> One (r370)
  | 475 -> One (r371)
  | 476 -> One (r372)
  | 872 -> One (r373)
  | 509 -> One (r374)
  | 857 -> One (r376)
  | 855 -> One (r377)
  | 852 -> One (r379)
  | 865 -> One (r381)
  | 871 -> One (r383)
  | 870 -> One (r384)
  | 480 -> One (r385)
  | 867 -> One (r386)
  | 484 -> One (r387)
  | 866 -> One (r388)
  | 486 -> One (r389)
  | 489 -> One (r390)
  | 494 -> One (r391)
  | 493 -> One (r392)
  | 492 | 863 -> One (r393)
  | 862 -> One (r394)
  | 500 -> One (r395)
  | 499 -> One (r396)
  | 498 -> One (r397)
  | 497 -> One (r398)
  | 822 -> One (r399)
  | 821 -> One (r400)
  | 820 -> One (r401)
  | 502 -> One (r402)
  | 816 -> One (r403)
  | 505 -> One (r404)
  | 504 -> One (r405)
  | 570 -> One (r406)
  | 569 -> One (r407)
  | 568 -> One (r409)
  | 567 -> One (r410)
  | 564 -> One (r411)
  | 521 -> One (r412)
  | 518 -> One (r413)
  | 512 -> One (r415)
  | 511 -> One (r417)
  | 510 -> One (r418)
  | 516 -> One (r419)
  | 520 -> One (r420)
  | 563 -> One (r421)
  | 530 | 738 -> One (r423)
  | 560 -> One (r425)
  | 524 -> One (r426)
  | 547 -> One (r427)
  | 528 -> One (r428)
  | 535 -> One (r430)
  | 534 -> One (r432)
  | 538 -> One (r433)
  | 540 -> One (r434)
  | 543 -> One (r435)
  | 546 -> One (r436)
  | 549 -> One (r437)
  | 552 -> One (r438)
  | 555 -> One (r439)
  | 554 -> One (r440)
  | 557 -> One (r441)
  | 559 -> One (r442)
  | 573 -> One (r443)
  | 572 -> One (r444)
  | 814 -> One (r445)
  | 813 -> One (r446)
  | 574 -> One (r447)
  | 812 -> One (r448)
  | 811 -> One (r449)
  | 810 -> One (r450)
  | 721 -> One (r451)
  | 795 -> One (r453)
  | 794 -> One (r454)
  | 809 -> One (r456)
  | 808 -> One (r457)
  | 577 -> One (r458)
  | 580 -> One (r459)
  | 586 -> One (r461)
  | 581 -> One (r462)
  | 585 -> One (r464)
  | 584 -> One (r465)
  | 583 -> One (r466)
  | 789 -> One (r467)
  | 788 -> One (r468)
  | 787 -> One (r469)
  | 589 -> One (r470)
  | 786 -> One (r471)
  | 780 -> One (r472)
  | 785 -> One (r474)
  | 784 -> One (r475)
  | 783 -> One (r476)
  | 782 -> One (r477)
  | 781 -> One (r478)
  | 778 -> One (r479)
  | 594 -> One (r480)
  | 593 -> One (r481)
  | 592 -> One (r482)
  | 597 -> One (r483)
  | 596 -> One (r484)
  | 599 -> One (r485)
  | 757 | 776 -> One (r486)
  | 756 | 775 -> One (r487)
  | 755 | 774 -> One (r488)
  | 600 | 618 -> One (r489)
  | 621 | 770 -> One (r490)
  | 620 | 769 -> One (r491)
  | 601 | 619 -> One (r492)
  | 768 -> One (r493)
  | 605 -> One (r494)
  | 606 -> One (r496)
  | 608 -> One (r497)
  | 610 -> One (r498)
  | 612 -> One (r499)
  | 617 -> One (r500)
  | 749 -> One (r501)
  | 627 -> One (r502)
  | 626 -> One (r503)
  | 679 -> One (r504)
  | 630 -> One (r505)
  | 633 -> One (r506)
  | 635 -> One (r507)
  | 637 -> One (r508)
  | 642 -> One (r509)
  | 644 -> One (r510)
  | 646 -> One (r511)
  | 648 -> One (r512)
  | 650 -> One (r513)
  | 652 -> One (r514)
  | 654 -> One (r515)
  | 656 -> One (r516)
  | 658 -> One (r517)
  | 660 -> One (r518)
  | 662 -> One (r519)
  | 664 -> One (r520)
  | 666 -> One (r521)
  | 668 -> One (r522)
  | 670 -> One (r523)
  | 672 -> One (r524)
  | 674 -> One (r525)
  | 676 -> One (r526)
  | 678 -> One (r527)
  | 681 -> One (r528)
  | 683 -> One (r529)
  | 747 -> One (r530)
  | 746 -> One (r531)
  | 688 -> One (r532)
  | 691 -> One (r533)
  | 700 -> One (r535)
  | 698 -> One (r536)
  | 697 -> One (r537)
  | 696 -> One (r538)
  | 695 -> One (r539)
  | 703 -> One (r540)
  | 710 -> One (r542)
  | 709 -> One (r543)
  | 720 -> One (r545)
  | 724 -> One (r547)
  | 706 -> One (r548)
  | 705 -> One (r549)
  | 702 -> One (r550)
  | 708 -> One (r551)
  | 712 -> One (r552)
  | 714 -> One (r553)
  | 713 | 725 -> One (r554)
  | 716 -> One (r555)
  | 719 -> One (r556)
  | 718 -> One (r557)
  | 723 -> One (r558)
  | 731 -> One (r559)
  | 730 -> One (r560)
  | 729 -> One (r561)
  | 728 -> One (r562)
  | 727 -> One (r563)
  | 736 -> One (r564)
  | 735 -> One (r565)
  | 734 -> One (r566)
  | 733 -> One (r567)
  | 742 -> One (r568)
  | 741 -> One (r569)
  | 740 -> One (r570)
  | 752 | 773 -> One (r571)
  | 751 | 772 -> One (r572)
  | 750 | 771 -> One (r573)
  | 754 -> One (r574)
  | 759 -> One (r575)
  | 762 -> One (r576)
  | 807 -> One (r577)
  | 793 -> One (r578)
  | 792 -> One (r579)
  | 806 -> One (r580)
  | 799 -> One (r581)
  | 798 -> One (r582)
  | 803 -> One (r583)
  | 802 -> One (r584)
  | 801 -> One (r585)
  | 805 -> One (r586)
  | 818 -> One (r588)
  | 824 -> One (r589)
  | 827 -> One (r590)
  | 829 -> One (r591)
  | 834 -> One (r592)
  | 833 -> One (r593)
  | 837 -> One (r594)
  | 840 -> One (r595)
  | 839 -> One (r596)
  | 847 -> One (r598)
  | 844 -> One (r599)
  | 843 -> One (r601)
  | 842 -> One (r602)
  | 861 -> One (r603)
  | 860 -> One (r604)
  | 851 -> One (r605)
  | 850 -> One (r606)
  | 859 -> One (r607)
  | 869 -> One (r608)
  | 874 -> One (r609)
  | 888 -> One (r610)
  | 887 -> One (r611)
  | 886 -> One (r612)
  | 881 -> One (r613)
  | 880 -> One (r614)
  | 885 -> One (r615)
  | 884 -> One (r616)
  | 883 -> One (r617)
  | 890 -> One (r618)
  | 902 -> One (r619)
  | 901 -> One (r620)
  | 900 -> One (r621)
  | 899 -> One (r622)
  | 898 -> One (r623)
  | 897 -> One (r624)
  | 904 -> One (r625)
  | 906 -> One (r626)
  | 915 -> One (r627)
  | 914 -> One (r628)
  | 918 -> One (r629)
  | 917 -> One (r630)
  | 921 -> One (r631)
  | 920 -> One (r632)
  | 926 -> One (r633)
  | 925 -> One (r634)
  | 1335 -> One (r635)
  | 1334 -> One (r636)
  | 935 -> One (r637)
  | 937 -> One (r638)
  | 939 -> One (r639)
  | 1333 -> One (r640)
  | 1332 -> One (r641)
  | 941 -> One (r642)
  | 945 -> One (r643)
  | 944 -> One (r644)
  | 943 -> One (r645)
  | 952 -> One (r646)
  | 955 -> One (r648)
  | 954 -> One (r649)
  | 951 -> One (r650)
  | 950 -> One (r651)
  | 949 -> One (r652)
  | 948 -> One (r653)
  | 947 -> One (r654)
  | 962 -> One (r655)
  | 961 -> One (r656)
  | 960 -> One (r657)
  | 959 -> One (r658)
  | 965 -> One (r661)
  | 964 -> One (r662)
  | 963 -> One (r663)
  | 997 -> One (r664)
  | 996 -> One (r665)
  | 995 -> One (r666)
  | 1170 -> One (r667)
  | 1169 -> One (r668)
  | 990 -> One (r669)
  | 989 -> One (r670)
  | 988 -> One (r671)
  | 987 -> One (r672)
  | 986 -> One (r673)
  | 969 -> One (r674)
  | 977 -> One (r675)
  | 976 -> One (r676)
  | 985 -> One (r678)
  | 984 -> One (r679)
  | 980 -> One (r680)
  | 979 -> One (r681)
  | 978 -> One (r682)
  | 975 -> One (r683)
  | 974 -> One (r684)
  | 973 -> One (r685)
  | 983 -> One (r686)
  | 982 -> One (r687)
  | 994 -> One (r688)
  | 993 -> One (r689)
  | 992 -> One (r690)
  | 1052 -> One (r691)
  | 1064 -> One (r693)
  | 1080 -> One (r695)
  | 1079 -> One (r696)
  | 1008 -> One (r697)
  | 1007 -> One (r698)
  | 1006 -> One (r699)
  | 1002 -> One (r700)
  | 1000 -> One (r701)
  | 999 -> One (r702)
  | 1005 -> One (r703)
  | 1004 -> One (r704)
  | 1017 -> One (r705)
  | 1016 -> One (r706)
  | 1015 -> One (r708)
  | 1014 -> One (r709)
  | 1010 -> One (r710)
  | 1013 -> One (r711)
  | 1012 -> One (r712)
  | 1034 -> One (r713)
  | 1033 -> One (r714)
  | 1032 -> One (r715)
  | 1031 -> One (r717)
  | 1030 -> One (r718)
  | 1019 -> One (r719)
  | 1024 -> One (r720)
  | 1023 -> One (r721)
  | 1022 -> One (r722)
  | 1021 -> One (r723)
  | 1029 -> One (r724)
  | 1028 -> One (r725)
  | 1027 -> One (r726)
  | 1026 -> One (r727)
  | 1049 -> One (r728)
  | 1048 -> One (r730)
  | 1047 -> One (r731)
  | 1043 -> One (r732)
  | 1042 -> One (r733)
  | 1041 -> One (r734)
  | 1036 -> One (r735)
  | 1046 -> One (r736)
  | 1045 -> One (r737)
  | 1066 -> One (r738)
  | 1065 -> One (r739)
  | 1051 -> One (r740)
  | 1062 -> One (r741)
  | 1059 -> One (r743)
  | 1055 -> One (r744)
  | 1054 -> One (r745)
  | 1058 -> One (r746)
  | 1057 -> One (r747)
  | 1061 -> One (r748)
  | 1072 -> One (r749)
  | 1071 -> One (r750)
  | 1070 -> One (r751)
  | 1074 -> One (r753)
  | 1073 -> One (r754)
  | 1069 -> One (r755)
  | 1076 -> One (r756)
  | 1107 -> One (r757)
  | 1112 -> One (r759)
  | 1111 -> One (r760)
  | 1085 -> One (r761)
  | 1084 -> One (r762)
  | 1083 -> One (r763)
  | 1082 -> One (r764)
  | 1110 -> One (r765)
  | 1090 -> One (r766)
  | 1089 -> One (r767)
  | 1088 -> One (r768)
  | 1087 -> One (r769)
  | 1109 -> One (r770)
  | 1093 -> One (r771)
  | 1092 -> One (r772)
  | 1108 -> One (r773)
  | 1097 -> One (r774)
  | 1096 -> One (r775)
  | 1106 -> One (r776)
  | 1101 -> One (r777)
  | 1121 -> One (r778)
  | 1120 -> One (r779)
  | 1119 -> One (r780)
  | 1118 -> One (r781)
  | 1117 -> One (r782)
  | 1116 -> One (r783)
  | 1126 -> One (r784)
  | 1136 -> One (r785)
  | 1135 -> One (r786)
  | 1134 -> One (r787)
  | 1133 -> One (r788)
  | 1132 -> One (r789)
  | 1145 -> One (r790)
  | 1155 -> One (r791)
  | 1154 -> One (r792)
  | 1153 -> One (r793)
  | 1152 -> One (r794)
  | 1151 -> One (r795)
  | 1150 -> One (r796)
  | 1149 -> One (r797)
  | 1166 -> One (r798)
  | 1165 -> One (r799)
  | 1164 -> One (r800)
  | 1163 -> One (r801)
  | 1162 -> One (r802)
  | 1161 -> One (r803)
  | 1160 -> One (r804)
  | 1256 -> One (r805)
  | 1254 -> One (r807)
  | 1280 -> One (r809)
  | 1177 -> One (r810)
  | 1288 -> One (r812)
  | 1287 -> One (r813)
  | 1176 -> One (r814)
  | 1175 -> One (r815)
  | 1174 -> One (r816)
  | 1181 -> One (r817)
  | 1180 -> One (r818)
  | 1179 -> One (r819)
  | 1202 -> One (r820)
  | 1201 -> One (r821)
  | 1200 -> One (r822)
  | 1199 -> One (r823)
  | 1188 -> One (r824)
  | 1187 -> One (r825)
  | 1186 -> One (r827)
  | 1185 -> One (r828)
  | 1193 -> One (r829)
  | 1192 -> One (r830)
  | 1191 -> One (r831)
  | 1190 -> One (r832)
  | 1198 -> One (r833)
  | 1197 -> One (r834)
  | 1196 -> One (r835)
  | 1205 -> One (r836)
  | 1204 -> One (r837)
  | 1231 -> One (r838)
  | 1220 -> One (r839)
  | 1219 -> One (r840)
  | 1208 -> One (r841)
  | 1207 -> One (r842)
  | 1233 -> One (r844)
  | 1232 -> One (r845)
  | 1213 -> One (r846)
  | 1212 -> One (r847)
  | 1211 -> One (r848)
  | 1210 -> One (r849)
  | 1218 -> One (r850)
  | 1217 -> One (r851)
  | 1216 -> One (r852)
  | 1230 -> One (r853)
  | 1229 -> One (r854)
  | 1228 -> One (r855)
  | 1227 -> One (r856)
  | 1226 -> One (r857)
  | 1225 -> One (r858)
  | 1224 -> One (r859)
  | 1223 -> One (r860)
  | 1237 -> One (r861)
  | 1236 -> One (r862)
  | 1235 -> One (r863)
  | 1271 -> One (r864)
  | 1270 -> One (r865)
  | 1267 -> One (r866)
  | 1240 -> One (r867)
  | 1239 -> One (r868)
  | 1263 -> One (r869)
  | 1262 -> One (r870)
  | 1246 -> One (r871)
  | 1245 -> One (r872)
  | 1244 -> One (r873)
  | 1259 -> One (r874)
  | 1250 -> One (r875)
  | 1249 -> One (r876)
  | 1261 -> One (r878)
  | 1248 -> One (r879)
  | 1257 -> One (r880)
  | 1252 -> One (r881)
  | 1266 -> One (r882)
  | 1265 -> One (r883)
  | 1269 -> One (r884)
  | 1275 -> One (r885)
  | 1274 -> One (r886)
  | 1273 -> One (r887)
  | 1277 -> One (r888)
  | 1284 -> One (r889)
  | 1283 -> One (r890)
  | 1282 -> One (r891)
  | 1286 -> One (r892)
  | 1326 -> One (r893)
  | 1294 -> One (r894)
  | 1304 -> One (r895)
  | 1303 -> One (r896)
  | 1302 -> One (r897)
  | 1301 -> One (r898)
  | 1313 -> One (r899)
  | 1323 -> One (r900)
  | 1322 -> One (r901)
  | 1321 -> One (r902)
  | 1320 -> One (r903)
  | 1319 -> One (r904)
  | 1318 -> One (r905)
  | 1329 -> One (r906)
  | 1328 -> One (r907)
  | 1348 -> One (r908)
  | 1347 -> One (r909)
  | 1346 -> One (r910)
  | 1345 -> One (r911)
  | 1344 -> One (r912)
  | 1343 -> One (r913)
  | 1353 -> One (r914)
  | 1352 -> One (r915)
  | 1359 -> One (r916)
  | 1358 -> One (r917)
  | 1375 -> One (r918)
  | 1374 -> One (r919)
  | 1373 -> One (r920)
  | 1393 -> One (r921)
  | 1395 -> One (r922)
  | 1401 -> One (r923)
  | 1400 -> One (r924)
  | 1399 -> One (r925)
  | 1414 -> One (r926)
  | 1413 -> One (r927)
  | 1412 -> One (r928)
  | 1417 -> One (r929)
  | 1422 -> One (r930)
  | 1421 -> One (r931)
  | 1420 -> One (r932)
  | 1435 -> One (r933)
  | 1433 -> One (r935)
  | 1432 -> One (r936)
  | 1429 -> One (r937)
  | 1428 -> One (r938)
  | 1459 -> One (r939)
  | 1463 -> One (r940)
  | 1467 -> One (r941)
  | 491 -> Select (function
    | -1 | -1 -> [R 97]
    | _ -> r394)
  | 350 -> Select (function
    | -1 -> S (T T_TYPE) :: r275
    | _ -> R 181 :: r270)
  | 956 -> Select (function
    | -1 -> r666
    | _ -> R 181 :: r660)
  | 466 -> Select (function
    | -1 | -1 -> [R 665]
    | _ -> r105)
  | 431 -> Select (function
    | -1 -> S (T T_UIDENT) :: r330
    | _ -> r270)
  | 473 -> Select (function
    | -1 | -1 -> S (T T_RBRACKET) :: r212
    | _ -> Sub (r367) :: r370)
  | 467 -> Select (function
    | -1 | -1 | 59 | 157 | 290 | 329 | 341 | 935 | 941 | 1291 -> r359
    | _ -> S (T T_OPEN) :: r365)
  | 154 -> Select (function
    | 725 -> r52
    | _ -> Sub (r50) :: r114)
  | 173 -> Select (function
    | 288 -> r133
    | _ -> Sub (r91) :: r139)
  | 155 -> Select (function
    | 725 -> r51
    | _ -> r114)
  | 174 -> Select (function
    | 165 -> r139
    | _ -> r132)
  | 958 -> Select (function
    | -1 -> r664
    | _ -> r659)
  | 957 -> Select (function
    | -1 -> r665
    | _ -> r660)
  | _ -> raise Not_found
