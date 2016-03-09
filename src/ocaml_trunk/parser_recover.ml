open Parser_raw

module Default = struct

  open Asttypes
  let default_expr = Fake.any_val'
  let default_type = Ast_helper.Typ.any ()
  let default_pattern = Ast_helper.Pat.any ()
  let default_longident = Longident.Lident "_"
  let default_longident_loc = Location.mknoloc (Longident.Lident "_")
  let default_payload = Parsetree.PStr []
  let default_attribute = Location.mknoloc "", default_payload
  let default_module_expr = Ast_helper.Mod.structure []
  let default_module_type = Ast_helper.Mty.signature []
  let default_module_decl = Ast_helper.Md.mk (Location.mknoloc "_") default_module_type
  let default_module_bind = Ast_helper.Mb.mk (Location.mknoloc "_") default_module_expr
  let default_value_bind = Ast_helper.Vb.mk default_pattern default_expr

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
    | MenhirInterpreter.N MenhirInterpreter.N_structure_tail -> []
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
    | MenhirInterpreter.N MenhirInterpreter.N_pattern -> default_pattern
    | MenhirInterpreter.N MenhirInterpreter.N_parse_expression -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parent_binder -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_package_type_cstrs -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_package_type_cstr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_package_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_override_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_optional_type_variable -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_optional_type_parameters -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_optional_type_parameter_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_optional_type_parameter -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_semi -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_default -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_bar -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_assign_arrow -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_module_type -> default_module_type
    | MenhirInterpreter.N MenhirInterpreter.N_module_expr -> default_module_expr
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
    | MenhirInterpreter.N MenhirInterpreter.N_let_pattern -> default_pattern
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
    | MenhirInterpreter.N MenhirInterpreter.N_index_operator_core -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_index_operator -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_expr -> default_expr
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
  [|0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;2;1;2;1;2;1;2;1;1;1;1;1;1;1;1;2;3;1;1;1;1;1;1;1;1;1;1;1;1;2;1;2;1;2;2;3;3;4;5;3;4;2;1;3;1;1;1;1;1;2;3;3;1;1;3;4;1;1;1;1;1;1;2;3;3;2;1;1;1;2;1;2;3;1;1;2;3;1;4;5;1;2;1;1;1;2;3;2;3;1;2;1;1;2;1;2;3;1;2;1;2;1;2;1;2;1;3;1;1;1;1;1;1;2;2;2;3;2;3;1;4;5;1;1;1;2;1;2;1;1;2;1;1;2;2;1;2;1;2;1;1;1;2;3;2;1;2;3;4;2;3;2;3;1;3;4;2;3;1;2;1;3;1;1;2;1;1;3;2;3;1;1;2;3;2;3;4;4;2;3;5;1;2;2;1;2;3;2;3;4;5;3;4;3;4;4;5;6;2;1;2;3;1;2;3;4;5;4;2;3;2;1;1;2;1;1;1;1;1;2;1;1;1;1;2;3;1;2;3;2;3;4;5;6;7;1;2;3;1;2;1;1;2;1;2;2;3;4;3;4;3;3;2;1;1;2;3;1;2;2;3;4;5;2;3;1;4;4;5;6;7;5;2;6;7;1;2;1;2;1;1;1;1;2;3;1;2;1;2;1;1;1;1;1;2;1;1;1;2;1;2;2;1;2;2;1;1;2;3;4;5;5;6;7;1;2;3;3;1;1;1;1;2;1;2;3;1;2;3;1;4;3;1;2;1;2;1;1;1;1;1;2;1;1;1;1;1;2;3;1;1;2;3;2;3;2;1;2;1;2;1;1;2;3;2;3;2;3;3;3;4;5;2;3;2;3;3;1;1;3;2;2;3;3;4;1;2;2;3;4;2;3;4;5;6;7;8;2;3;3;4;5;3;4;1;2;1;2;1;2;3;4;5;1;3;4;1;2;1;2;3;4;5;6;2;3;4;1;1;1;2;1;1;1;2;3;1;2;1;2;3;3;3;3;3;1;3;2;3;1;1;2;3;4;5;1;2;3;4;1;1;2;1;2;3;4;5;6;7;1;2;3;4;8;9;2;1;1;2;3;1;1;1;1;2;3;1;2;3;1;2;3;4;1;2;3;1;1;2;3;4;5;6;1;1;2;1;2;1;1;1;2;1;2;1;1;2;1;1;2;3;4;4;5;1;2;1;1;1;2;2;3;1;1;2;3;4;1;5;2;1;1;1;1;2;2;2;3;2;3;1;2;1;3;1;2;4;5;6;2;1;2;3;3;3;4;5;1;1;2;3;4;2;3;4;3;2;3;1;2;1;2;1;2;3;4;5;3;4;5;2;3;3;4;2;1;1;6;7;8;9;1;1;1;2;1;2;3;1;2;1;2;1;2;1;1;2;3;3;4;2;1;2;3;1;1;2;3;4;1;2;3;2;3;2;3;1;1;1;1;2;2;3;1;2;1;2;1;1;1;1;2;2;3;4;5;6;1;2;1;2;3;1;1;2;1;2;3;2;3;2;3;2;1;2;1;2;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;3;2;3;2;3;1;2;3;1;2;1;1;1;2;1;2;3;3;4;5;1;2;1;2;1;2;3;4;1;2;1;2;1;2;1;2;3;4;1;2;3;1;1;1;2;5;2;3;4;5;6;7;8;3;4;5;6;7;2;1;2;3;4;5;1;2;3;3;4;2;7;3;4;5;6;7;3;4;5;6;7;3;4;5;2;1;2;1;1;2;4;5;3;4;5;3;4;5;3;5;1;1;6;7;8;9;10;2;4;5;2;3;4;5;6;1;2;1;2;3;4;1;2;3;4;1;2;5;7;3;4;3;4;5;2;3;4;2;3;1;3;4;5;6;7;1;4;5;3;4;4;2;3;5;6;1;3;4;4;5;6;3;4;5;1;2;3;2;3;1;2;3;1;2;3;1;2;3;4;5;1;2;3;1;3;3;3;4;2;3;3;2;3;2;3;4;7;2;3;4;1;2;1;2;3;4;5;6;7;1;2;2;1;3;4;5;4;5;5;6;7;5;6;7;7;8;9;2;3;3;4;5;5;4;5;1;2;6;2;3;4;5;6;4;5;3;4;5;6;7;1;2;1;2;3;4;1;2;1;2;3;4;1;2;3;4;5;6;1;2;7;8;1;2;3;4;1;1;5;6;7;4;5;6;1;1;2;3;4;1;2;3;1;2;3;1;4;1;2;3;5;6;7;1;1;2;3;2;3;1;2;1;1;2;3;4;5;1;2;3;4;5;2;3;1;2;3;1;1;2;1;2;2;3;4;1;2;3;5;6;1;1;1;1;2;3;1;2;3;4;1;1;2;3;2;1;1;2;3;2;3;1;2;1;2;8;9;2;3;4;5;6;1;2;3;4;5;1;2;3;1;2;3;4;1;1;1;2;1;2;3;1;3;1;5;4;6;7;8;1;1;1;2;3;4;5;6;7;2;1;1;2;1;1;1;1;1;2;3;4;5;6;2;1;1;1;1;1;1;1;2;1;1;1;2;3;4;5;6;7;8;2;1;1;1;2;3;4;5;6;7;8;2;1;2;2;1;2;3;4;5;1;1;2;3;4;1;1;1;2;3;4;1;2;1;2;1;2;1;1;2;1;3;2;3;2;3;2;3;3;4;5;2;1;2;3;4;1;2;6;7;1;1;1;1;1;2;1;1;1;1;1;1;2;3;4;5;2;1;1;1;1;1;1;1;2;1;1;1;1;2;3;4;5;6;7;2;1;2;1;2;3;1;1;3;4;3;4;2;3;4;2;5;6;2;3;4;5;3;4;5;6;2;3;4;4;5;5;6;7;2;3;4;5;6;7;8;2;3;3;4;3;4;5;6;5;4;5;6;1;1;2;3;4;5;6;7;2;3;4;5;6;7;2;3;4;5;6;7;8;9;10;11;6;7;8;5;2;3;1;2;3;4;1;2;3;4;1;2;5;6;1;2;3;4;1;2;1;2;2;2;3;4;2;3;4;5;6;3;4;8;5;6;7;1;2;3;4;5;8;9;2;2;1;1;1;2;3;4;1;1;3;4;3;4;5;6;1;2;1;3;4;5;4;3;1;2;3;2;3;4;4;5;6;5;6;3;4;3;2;3;4;5;6;1;2;1;0;1;2;1;0;1;2;1;|]

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
  let r0 = [R 438] in
  let r1 = S (N N_expr) :: r0 in
  let r2 = [R 142] in
  let r3 = S (T T_DONE) :: r2 in
  let r4 = Sub (r1) :: r3 in
  let r5 = S (T T_DO) :: r4 in
  let r6 = Sub (r1) :: r5 in
  let r7 = [R 549] in
  let r8 = S (T T_AND) :: r7 in
  let r9 = [R 14] in
  let r10 = Sub (r8) :: r9 in
  let r11 = [R 183] in
  let r12 = R 17 :: r11 in
  let r13 = [R 15] in
  let r14 = [R 402] in
  let r15 = S (N N_structure) :: r14 in
  let r16 = [R 16] in
  let r17 = S (T T_RBRACKET) :: r16 in
  let r18 = Sub (r15) :: r17 in
  let r19 = [R 676] in
  let r20 = S (T T_error) :: r19 in
  let r21 = S (T T_LPAREN) :: r20 in
  let r22 = [R 473] in
  let r23 = S (T T_UNDERSCORE) :: r22 in
  let r24 = [R 470] in
  let r25 = Sub (r23) :: r24 in
  let r26 = [R 491] in
  let r27 = Sub (r25) :: r26 in
  let r28 = [R 114] in
  let r29 = Sub (r27) :: r28 in
  let r30 = [R 123] in
  let r31 = Sub (r29) :: r30 in
  let r32 = [R 112] in
  let r33 = Sub (r31) :: r32 in
  let r34 = [R 684] in
  let r35 = R 412 :: r34 in
  let r36 = Sub (r33) :: r35 in
  let r37 = S (T T_COLON) :: r36 in
  let r38 = Sub (r21) :: r37 in
  let r39 = [R 677] in
  let r40 = [R 216] in
  let r41 = S (T T_RBRACE) :: r40 in
  let r42 = S (T T_LBRACE) :: r41 in
  let r43 = [R 213] in
  let r44 = R 358 :: r43 in
  let r45 = [R 214] in
  let r46 = [R 215] in
  let r47 = [R 217] in
  let r48 = [R 219] in
  let r49 = S (T T_RBRACE) :: r48 in
  let r50 = [R 218] in
  let r51 = [R 674] in
  let r52 = [R 284] in
  let r53 = [R 48] in
  let r54 = S (T T_LIDENT) :: r53 in
  let r55 = [R 479] in
  let r56 = [R 287] in
  let r57 = [R 49] in
  let r58 = S (T T_LIDENT) :: r57 in
  let r59 = [R 288] in
  let r60 = [R 211] in
  let r61 = S (T T_LIDENT) :: r60 in
  let r62 = [R 472] in
  let r63 = Sub (r61) :: r62 in
  let r64 = [R 115] in
  let r65 = Sub (r29) :: r64 in
  let r66 = S (T T_MINUSGREATER) :: r65 in
  let r67 = Sub (r29) :: r66 in
  let r68 = S (T T_COLON) :: r67 in
  let r69 = [R 116] in
  let r70 = Sub (r29) :: r69 in
  let r71 = S (T T_MINUSGREATER) :: r70 in
  let r72 = [R 119] in
  let r73 = Sub (r33) :: r72 in
  let r74 = [R 471] in
  let r75 = S (T T_RPAREN) :: r74 in
  let r76 = [R 322] in
  let r77 = Sub (r61) :: r76 in
  let r78 = [R 376] in
  let r79 = Sub (r77) :: r78 in
  let r80 = [R 489] in
  let r81 = S (T T_RPAREN) :: r80 in
  let r82 = Sub (r79) :: r81 in
  let r83 = [R 235] in
  let r84 = S (T T_LIDENT) :: r83 in
  let r85 = [R 378] in
  let r86 = Sub (r33) :: r85 in
  let r87 = S (T T_EQUAL) :: r86 in
  let r88 = Sub (r84) :: r87 in
  let r89 = S (T T_TYPE) :: r88 in
  let r90 = [R 379] in
  let r91 = Sub (r89) :: r90 in
  let r92 = [R 377] in
  let r93 = [R 236] in
  let r94 = S (T T_LIDENT) :: r93 in
  let r95 = [R 659] in
  let r96 = [R 117] in
  let r97 = Sub (r29) :: r96 in
  let r98 = S (T T_MINUSGREATER) :: r97 in
  let r99 = [R 478] in
  let r100 = [R 222] in
  let r101 = [R 477] in
  let r102 = [R 409] in
  let r103 = Sub (r31) :: r102 in
  let r104 = [R 192] in
  let r105 = R 17 :: r104 in
  let r106 = S (T T_SEMI) :: r105 in
  let r107 = R 17 :: r106 in
  let r108 = Sub (r103) :: r107 in
  let r109 = [R 671] in
  let r110 = [R 184] in
  let r111 = S (T T_RBRACKET) :: r110 in
  let r112 = Sub (r15) :: r111 in
  let r113 = [R 646] in
  let r114 = R 412 :: r113 in
  let r115 = R 105 :: r114 in
  let r116 = R 649 :: r115 in
  let r117 = S (T T_LIDENT) :: r116 in
  let r118 = R 369 :: r117 in
  let r119 = R 329 :: r118 in
  let r120 = R 181 :: r119 in
  let r121 = [R 373] in
  let r122 = S (T T_UNDERSCORE) :: r121 in
  let r123 = [R 366] in
  let r124 = Sub (r122) :: r123 in
  let r125 = R 668 :: r124 in
  let r126 = [R 367] in
  let r127 = Sub (r125) :: r126 in
  let r128 = [R 371] in
  let r129 = S (T T_RPAREN) :: r128 in
  let r130 = [R 372] in
  let r131 = [R 368] in
  let r132 = [R 654] in
  let r133 = [R 95] in
  let r134 = S (T T_FALSE) :: r133 in
  let r135 = [R 108] in
  let r136 = R 17 :: r135 in
  let r137 = R 206 :: r136 in
  let r138 = Sub (r134) :: r137 in
  let r139 = [R 109] in
  let r140 = Sub (r138) :: r139 in
  let r141 = [R 653] in
  let r142 = [R 93] in
  let r143 = [R 435] in
  let r144 = Sub (r25) :: r143 in
  let r145 = [R 436] in
  let r146 = Sub (r144) :: r145 in
  let r147 = [R 487] in
  let r148 = S (T T_RBRACKET) :: r147 in
  let r149 = Sub (r146) :: r148 in
  let r150 = [R 486] in
  let r151 = [R 485] in
  let r152 = S (T T_RBRACKET) :: r151 in
  let r153 = [R 483] in
  let r154 = S (T T_RBRACKET) :: r153 in
  let r155 = Sub (r146) :: r154 in
  let r156 = [R 326] in
  let r157 = Sub (r61) :: r156 in
  let r158 = [R 480] in
  let r159 = [R 660] in
  let r160 = S (T T_LIDENT) :: r159 in
  let r161 = S (T T_DOT) :: r160 in
  let r162 = S (T T_UIDENT) :: r52 in
  let r163 = [R 286] in
  let r164 = S (T T_RPAREN) :: r163 in
  let r165 = [R 285] in
  let r166 = [R 437] in
  let r167 = [R 635] in
  let r168 = [R 5] in
  let r169 = Sub (r31) :: r168 in
  let r170 = [R 634] in
  let r171 = R 17 :: r170 in
  let r172 = Sub (r169) :: r171 in
  let r173 = [R 121] in
  let r174 = Sub (r25) :: r173 in
  let r175 = [R 492] in
  let r176 = [R 122] in
  let r177 = [R 118] in
  let r178 = [R 124] in
  let r179 = Sub (r61) :: r178 in
  let r180 = [R 6] in
  let r181 = [R 18] in
  let r182 = [R 482] in
  let r183 = [R 484] in
  let r184 = S (T T_RBRACKET) :: r183 in
  let r185 = Sub (r146) :: r184 in
  let r186 = S (T T_BACKQUOTE) :: r157 in
  let r187 = [R 327] in
  let r188 = Sub (r186) :: r187 in
  let r189 = [R 488] in
  let r190 = S (T T_RBRACKET) :: r189 in
  let r191 = [R 94] in
  let r192 = [R 481] in
  let r193 = [R 120] in
  let r194 = [R 92] in
  let r195 = [R 19] in
  let r196 = R 17 :: r195 in
  let r197 = R 206 :: r196 in
  let r198 = [R 106] in
  let r199 = Sub (r174) :: r198 in
  let r200 = [R 207] in
  let r201 = S (T T_LIDENT) :: r100 in
  let r202 = [R 223] in
  let r203 = R 17 :: r202 in
  let r204 = Sub (r103) :: r203 in
  let r205 = S (T T_COLON) :: r204 in
  let r206 = Sub (r201) :: r205 in
  let r207 = R 324 :: r206 in
  let r208 = [R 225] in
  let r209 = Sub (r207) :: r208 in
  let r210 = [R 107] in
  let r211 = S (T T_RBRACE) :: r210 in
  let r212 = [R 224] in
  let r213 = R 17 :: r212 in
  let r214 = S (T T_SEMI) :: r213 in
  let r215 = R 17 :: r214 in
  let r216 = Sub (r103) :: r215 in
  let r217 = S (T T_COLON) :: r216 in
  let r218 = [R 410] in
  let r219 = Sub (r31) :: r218 in
  let r220 = [R 672] in
  let r221 = [R 209] in
  let r222 = [R 208] in
  let r223 = Sub (r25) :: r222 in
  let r224 = [R 655] in
  let r225 = S (T T_RBRACE) :: r224 in
  let r226 = Sub (r209) :: r225 in
  let r227 = [R 657] in
  let r228 = [R 656] in
  let r229 = [R 658] in
  let r230 = S (T T_RBRACE) :: r229 in
  let r231 = [R 411] in
  let r232 = S (T T_RBRACKET) :: r231 in
  let r233 = Sub (r15) :: r232 in
  let r234 = [R 136] in
  let r235 = S (N N_match_cases) :: r234 in
  let r236 = R 360 :: r235 in
  let r237 = S (T T_WITH) :: r236 in
  let r238 = Sub (r1) :: r237 in
  let r239 = [R 100] in
  let r240 = S (T T_FALSE) :: r239 in
  let r241 = [R 495] in
  let r242 = Sub (r240) :: r241 in
  let r243 = [R 516] in
  let r244 = [R 518] in
  let r245 = Sub (r54) :: r244 in
  let r246 = [R 182] in
  let r247 = [R 99] in
  let r248 = [R 510] in
  let r249 = [R 74] in
  let r250 = R 41 :: r249 in
  let r251 = R 52 :: r250 in
  let r252 = [R 175] in
  let r253 = S (T T_END) :: r252 in
  let r254 = Sub (r251) :: r253 in
  let r255 = [R 50] in
  let r256 = S (T T_RPAREN) :: r255 in
  let r257 = [R 532] in
  let r258 = S (T T_LIDENT) :: r95 in
  let r259 = [R 537] in
  let r260 = [R 468] in
  let r261 = [R 466] in
  let r262 = [R 544] in
  let r263 = S (T T_RPAREN) :: r262 in
  let r264 = [R 545] in
  let r265 = S (T T_RPAREN) :: r264 in
  let r266 = [R 323] in
  let r267 = Sub (r61) :: r266 in
  let r268 = [R 541] in
  let r269 = [R 398] in
  let r270 = S (N N_pattern) :: r269 in
  let r271 = [R 539] in
  let r272 = S (T T_RBRACKET) :: r271 in
  let r273 = R 364 :: r272 in
  let r274 = [R 98] in
  let r275 = [R 254] in
  let r276 = Sub (r84) :: r275 in
  let r277 = [R 255] in
  let r278 = Sub (r276) :: r277 in
  let r279 = [R 538] in
  let r280 = S (T T_RBRACE) :: r279 in
  let r281 = [R 257] in
  let r282 = [R 253] in
  let r283 = S (T T_UNDERSCORE) :: r257 in
  let r284 = [R 531] in
  let r285 = Sub (r283) :: r284 in
  let r286 = [R 392] in
  let r287 = Sub (r285) :: r286 in
  let r288 = [R 87] in
  let r289 = [R 393] in
  let r290 = S (N N_pattern) :: r289 in
  let r291 = S (T T_INT) :: r288 in
  let r292 = [R 465] in
  let r293 = Sub (r291) :: r292 in
  let r294 = [R 534] in
  let r295 = [R 395] in
  let r296 = [R 389] in
  let r297 = [R 388] in
  let r298 = [R 387] in
  let r299 = [R 542] in
  let r300 = [R 396] in
  let r301 = [R 543] in
  let r302 = S (T T_RPAREN) :: r301 in
  let r303 = [R 391] in
  let r304 = [R 385] in
  let r305 = [R 540] in
  let r306 = S (T T_BARRBRACKET) :: r305 in
  let r307 = [R 390] in
  let r308 = S (T T_RPAREN) :: r307 in
  let r309 = S (N N_pattern) :: r308 in
  let r310 = S (T T_COMMA) :: r309 in
  let r311 = S (N N_pattern) :: r310 in
  let r312 = S (T T_LPAREN) :: r311 in
  let r313 = [R 51] in
  let r314 = S (T T_RPAREN) :: r313 in
  let r315 = [R 682] in
  let r316 = Sub (r1) :: r315 in
  let r317 = S (T T_EQUAL) :: r316 in
  let r318 = Sub (r201) :: r317 in
  let r319 = R 324 :: r318 in
  let r320 = R 374 :: r319 in
  let r321 = [R 35] in
  let r322 = R 412 :: r321 in
  let r323 = Sub (r320) :: r322 in
  let r324 = [R 681] in
  let r325 = Sub (r33) :: r324 in
  let r326 = S (T T_COLON) :: r325 in
  let r327 = Sub (r201) :: r326 in
  let r328 = [R 413] in
  let r329 = [R 680] in
  let r330 = Sub (r33) :: r329 in
  let r331 = S (T T_COLON) :: r330 in
  let r332 = [R 135] in
  let r333 = S (N N_match_cases) :: r332 in
  let r334 = R 360 :: r333 in
  let r335 = S (T T_WITH) :: r334 in
  let r336 = Sub (r1) :: r335 in
  let r337 = [R 524] in
  let r338 = S (T T_RPAREN) :: r337 in
  let r339 = [R 299] in
  let r340 = S (T T_END) :: r339 in
  let r341 = S (N N_structure) :: r340 in
  let r342 = [R 636] in
  let r343 = [R 630] in
  let r344 = S (T T_UIDENT) :: r56 in
  let r345 = [R 331] in
  let r346 = R 412 :: r345 in
  let r347 = Sub (r344) :: r346 in
  let r348 = R 181 :: r347 in
  let r349 = [R 291] in
  let r350 = S (N N_module_expr) :: r349 in
  let r351 = S (T T_EQUAL) :: r350 in
  let r352 = [R 426] in
  let r353 = R 412 :: r352 in
  let r354 = Sub (r351) :: r353 in
  let r355 = S (T T_UIDENT) :: r354 in
  let r356 = S (T T_REC) :: r355 in
  let r357 = [R 319] in
  let r358 = R 412 :: r357 in
  let r359 = R 320 :: r358 in
  let r360 = Sub (r61) :: r359 in
  let r361 = R 181 :: r360 in
  let r362 = [R 321] in
  let r363 = [R 312] in
  let r364 = S (T T_END) :: r363 in
  let r365 = R 447 :: r364 in
  let r366 = [R 185] in
  let r367 = R 17 :: r366 in
  let r368 = R 206 :: r367 in
  let r369 = Sub (r134) :: r368 in
  let r370 = [R 442] in
  let r371 = Sub (r369) :: r370 in
  let r372 = [R 446] in
  let r373 = R 412 :: r372 in
  let r374 = Sub (r371) :: r373 in
  let r375 = R 417 :: r374 in
  let r376 = [R 20] in
  let r377 = R 17 :: r376 in
  let r378 = R 206 :: r377 in
  let r379 = Sub (r134) :: r378 in
  let r380 = [R 448] in
  let r381 = [R 429] in
  let r382 = R 412 :: r381 in
  let r383 = S (N N_module_type) :: r382 in
  let r384 = S (T T_COLON) :: r383 in
  let r385 = S (T T_UIDENT) :: r384 in
  let r386 = S (T T_REC) :: r385 in
  let r387 = [R 295] in
  let r388 = S (N N_module_type) :: r387 in
  let r389 = S (T T_COLON) :: r388 in
  let r390 = [R 294] in
  let r391 = R 412 :: r390 in
  let r392 = [R 297] in
  let r393 = Sub (r389) :: r392 in
  let r394 = [R 296] in
  let r395 = Sub (r389) :: r394 in
  let r396 = S (T T_RPAREN) :: r395 in
  let r397 = S (N N_module_type) :: r396 in
  let r398 = [R 315] in
  let r399 = S (N N_module_expr) :: r398 in
  let r400 = R 17 :: r399 in
  let r401 = S (T T_OF) :: r400 in
  let r402 = [R 304] in
  let r403 = S (T T_RPAREN) :: r402 in
  let r404 = [R 305] in
  let r405 = S (T T_RPAREN) :: r404 in
  let r406 = S (N N_expr) :: r405 in
  let r407 = [R 263] in
  let r408 = Sub (r1) :: r407 in
  let r409 = S (T T_EQUAL) :: r408 in
  let r410 = S (N N_pattern) :: r409 in
  let r411 = [R 259] in
  let r412 = R 412 :: r411 in
  let r413 = Sub (r410) :: r412 in
  let r414 = R 424 :: r413 in
  let r415 = R 181 :: r414 in
  let r416 = [R 131] in
  let r417 = Sub (r1) :: r416 in
  let r418 = S (T T_IN) :: r417 in
  let r419 = Sub (r344) :: r418 in
  let r420 = R 181 :: r419 in
  let r421 = R 374 :: r420 in
  let r422 = [R 179] in
  let r423 = S (N N_expr) :: r422 in
  let r424 = [R 513] in
  let r425 = S (T T_RBRACKET) :: r424 in
  let r426 = R 364 :: r425 in
  let r427 = [R 520] in
  let r428 = [R 189] in
  let r429 = [R 188] in
  let r430 = [R 249] in
  let r431 = Sub (r84) :: r430 in
  let r432 = [R 250] in
  let r433 = Sub (r431) :: r432 in
  let r434 = [R 433] in
  let r435 = Sub (r433) :: r434 in
  let r436 = [R 507] in
  let r437 = S (T T_RBRACE) :: r436 in
  let r438 = [R 499] in
  let r439 = S (T T_END) :: r438 in
  let r440 = [R 174] in
  let r441 = Sub (r242) :: r440 in
  let r442 = [R 517] in
  let r443 = [R 503] in
  let r444 = S (T T_RPAREN) :: r443 in
  let r445 = S (T T_LPAREN) :: r444 in
  let r446 = S (T T_DOT) :: r445 in
  let r447 = [R 526] in
  let r448 = S (T T_RPAREN) :: r447 in
  let r449 = Sub (r79) :: r448 in
  let r450 = S (T T_COLON) :: r449 in
  let r451 = [R 300] in
  let r452 = S (N N_module_expr) :: r451 in
  let r453 = S (T T_MINUSGREATER) :: r452 in
  let r454 = S (N N_functor_args) :: r453 in
  let r455 = [R 200] in
  let r456 = [R 201] in
  let r457 = S (T T_RPAREN) :: r456 in
  let r458 = S (N N_module_type) :: r457 in
  let r459 = [R 316] in
  let r460 = S (T T_RPAREN) :: r459 in
  let r461 = [R 313] in
  let r462 = S (N N_module_type) :: r461 in
  let r463 = S (T T_MINUSGREATER) :: r462 in
  let r464 = S (N N_functor_args) :: r463 in
  let r465 = [R 693] in
  let r466 = Sub (r162) :: r465 in
  let r467 = S (T T_COLONEQUAL) :: r466 in
  let r468 = S (T T_UIDENT) :: r467 in
  let r469 = S (T T_MODULE) :: r468 in
  let r470 = [R 694] in
  let r471 = Sub (r469) :: r470 in
  let r472 = [R 314] in
  let r473 = [R 691] in
  let r474 = Sub (r31) :: r473 in
  let r475 = S (T T_COLONEQUAL) :: r474 in
  let r476 = Sub (r201) :: r475 in
  let r477 = [R 667] in
  let r478 = Sub (r61) :: r477 in
  let r479 = S (T T_QUOTE) :: r478 in
  let r480 = [R 661] in
  let r481 = Sub (r479) :: r480 in
  let r482 = R 668 :: r481 in
  let r483 = [R 662] in
  let r484 = Sub (r482) :: r483 in
  let r485 = [R 666] in
  let r486 = S (T T_RPAREN) :: r485 in
  let r487 = [R 663] in
  let r488 = [R 696] in
  let r489 = S (T T_EQUAL) :: r488 in
  let r490 = [R 690] in
  let r491 = R 105 :: r490 in
  let r492 = Sub (r31) :: r491 in
  let r493 = [R 102] in
  let r494 = Sub (r33) :: r493 in
  let r495 = S (T T_EQUAL) :: r494 in
  let r496 = Sub (r33) :: r495 in
  let r497 = [R 104] in
  let r498 = [R 692] in
  let r499 = Sub (r162) :: r498 in
  let r500 = [R 695] in
  let r501 = [R 302] in
  let r502 = [R 301] in
  let r503 = [R 141] in
  let r504 = S (N N_expr) :: r503 in
  let r505 = S (T T_THEN) :: r504 in
  let r506 = Sub (r1) :: r505 in
  let r507 = [R 132] in
  let r508 = S (N N_match_cases) :: r507 in
  let r509 = R 360 :: r508 in
  let r510 = [R 271] in
  let r511 = Sub (r1) :: r510 in
  let r512 = S (T T_MINUSGREATER) :: r511 in
  let r513 = [R 272] in
  let r514 = Sub (r1) :: r513 in
  let r515 = S (T T_MINUSGREATER) :: r514 in
  let r516 = [R 247] in
  let r517 = Sub (r285) :: r516 in
  let r518 = [R 196] in
  let r519 = Sub (r1) :: r518 in
  let r520 = S (T T_MINUSGREATER) :: r519 in
  let r521 = [R 133] in
  let r522 = Sub (r520) :: r521 in
  let r523 = Sub (r517) :: r522 in
  let r524 = [R 237] in
  let r525 = S (T T_LIDENT) :: r524 in
  let r526 = [R 245] in
  let r527 = [R 233] in
  let r528 = Sub (r525) :: r527 in
  let r529 = [R 244] in
  let r530 = S (T T_RPAREN) :: r529 in
  let r531 = [R 234] in
  let r532 = [R 241] in
  let r533 = [R 240] in
  let r534 = S (T T_RPAREN) :: r533 in
  let r535 = R 362 :: r534 in
  let r536 = [R 363] in
  let r537 = [R 126] in
  let r538 = S (T T_DOWNTO) :: r537 in
  let r539 = [R 143] in
  let r540 = S (T T_DONE) :: r539 in
  let r541 = Sub (r1) :: r540 in
  let r542 = S (T T_DO) :: r541 in
  let r543 = Sub (r1) :: r542 in
  let r544 = Sub (r538) :: r543 in
  let r545 = Sub (r1) :: r544 in
  let r546 = S (T T_EQUAL) :: r545 in
  let r547 = S (N N_pattern) :: r546 in
  let r548 = [R 173] in
  let r549 = Sub (r242) :: r548 in
  let r550 = [R 523] in
  let r551 = [R 506] in
  let r552 = S (T T_RBRACE) :: r551 in
  let r553 = S (N N_expr) :: r552 in
  let r554 = S (T T_LBRACE) :: r553 in
  let r555 = [R 504] in
  let r556 = S (T T_RPAREN) :: r555 in
  let r557 = Sub (r1) :: r556 in
  let r558 = [R 166] in
  let r559 = [R 232] in
  let r560 = S (T T_LIDENT) :: r559 in
  let r561 = [R 229] in
  let r562 = [R 522] in
  let r563 = [R 230] in
  let r564 = [R 231] in
  let r565 = [R 228] in
  let r566 = [R 169] in
  let r567 = [R 129] in
  let r568 = Sub (r1) :: r567 in
  let r569 = [R 172] in
  let r570 = S (N N_expr) :: r569 in
  let r571 = [R 177] in
  let r572 = [R 156] in
  let r573 = [R 150] in
  let r574 = [R 167] in
  let r575 = [R 153] in
  let r576 = [R 157] in
  let r577 = [R 149] in
  let r578 = [R 152] in
  let r579 = [R 151] in
  let r580 = [R 161] in
  let r581 = [R 155] in
  let r582 = [R 154] in
  let r583 = [R 159] in
  let r584 = [R 148] in
  let r585 = [R 147] in
  let r586 = [R 144] in
  let r587 = [R 146] in
  let r588 = [R 160] in
  let r589 = [R 158] in
  let r590 = [R 162] in
  let r591 = [R 163] in
  let r592 = [R 164] in
  let r593 = [R 178] in
  let r594 = [R 165] in
  let r595 = [R 10] in
  let r596 = R 412 :: r595 in
  let r597 = Sub (r410) :: r596 in
  let r598 = [R 401] in
  let r599 = S (T T_UNDERSCORE) :: r598 in
  let r600 = [R 243] in
  let r601 = [R 242] in
  let r602 = S (T T_RPAREN) :: r601 in
  let r603 = R 362 :: r602 in
  let r604 = [R 268] in
  let r605 = [R 269] in
  let r606 = S (T T_LIDENT) :: r605 in
  let r607 = [R 608] in
  let r608 = Sub (r1) :: r607 in
  let r609 = S (T T_EQUAL) :: r608 in
  let r610 = [R 194] in
  let r611 = Sub (r609) :: r610 in
  let r612 = [R 610] in
  let r613 = Sub (r611) :: r612 in
  let r614 = S (T T_RPAREN) :: r613 in
  let r615 = Sub (r606) :: r614 in
  let r616 = [R 246] in
  let r617 = [R 645] in
  let r618 = [R 643] in
  let r619 = Sub (r33) :: r618 in
  let r620 = [R 644] in
  let r621 = [R 195] in
  let r622 = Sub (r1) :: r621 in
  let r623 = [R 609] in
  let r624 = [R 262] in
  let r625 = Sub (r1) :: r624 in
  let r626 = S (T T_EQUAL) :: r625 in
  let r627 = Sub (r33) :: r626 in
  let r628 = S (T T_DOT) :: r627 in
  let r629 = [R 261] in
  let r630 = Sub (r1) :: r629 in
  let r631 = S (T T_EQUAL) :: r630 in
  let r632 = Sub (r33) :: r631 in
  let r633 = [R 264] in
  let r634 = Sub (r1) :: r633 in
  let r635 = S (T T_EQUAL) :: r634 in
  let r636 = [R 505] in
  let r637 = S (T T_RBRACKET) :: r636 in
  let r638 = Sub (r1) :: r637 in
  let r639 = [R 170] in
  let r640 = [R 171] in
  let r641 = [R 168] in
  let r642 = [R 134] in
  let r643 = Sub (r520) :: r642 in
  let r644 = S (T T_RPAREN) :: r643 in
  let r645 = [R 199] in
  let r646 = Sub (r520) :: r645 in
  let r647 = S (T T_RPAREN) :: r646 in
  let r648 = [R 197] in
  let r649 = Sub (r1) :: r648 in
  let r650 = S (T T_MINUSGREATER) :: r649 in
  let r651 = [R 198] in
  let r652 = S (N N_pattern) :: r512 in
  let r653 = [R 274] in
  let r654 = [R 140] in
  let r655 = [R 502] in
  let r656 = [R 512] in
  let r657 = [R 511] in
  let r658 = S (T T_BARRBRACKET) :: r657 in
  let r659 = [R 515] in
  let r660 = [R 514] in
  let r661 = S (T T_RBRACKET) :: r660 in
  let r662 = Sub (r201) :: r428 in
  let r663 = [R 190] in
  let r664 = R 364 :: r663 in
  let r665 = Sub (r662) :: r664 in
  let r666 = [R 521] in
  let r667 = S (T T_GREATERRBRACE) :: r666 in
  let r668 = [R 508] in
  let r669 = S (T T_RBRACE) :: r668 in
  let r670 = [R 432] in
  let r671 = Sub (r433) :: r670 in
  let r672 = [R 248] in
  let r673 = [R 498] in
  let r674 = [R 519] in
  let r675 = [R 130] in
  let r676 = Sub (r1) :: r675 in
  let r677 = S (T T_IN) :: r676 in
  let r678 = Sub (r351) :: r677 in
  let r679 = S (T T_UIDENT) :: r678 in
  let r680 = [R 292] in
  let r681 = S (N N_module_expr) :: r680 in
  let r682 = S (T T_EQUAL) :: r681 in
  let r683 = [R 293] in
  let r684 = [R 308] in
  let r685 = S (T T_RPAREN) :: r684 in
  let r686 = [R 306] in
  let r687 = S (T T_RPAREN) :: r686 in
  let r688 = [R 307] in
  let r689 = S (T T_RPAREN) :: r688 in
  let r690 = [R 303] in
  let r691 = S (T T_RPAREN) :: r690 in
  let r692 = [R 289] in
  let r693 = R 412 :: r692 in
  let r694 = [R 221] in
  let r695 = S (T T_RBRACKET) :: r694 in
  let r696 = Sub (r15) :: r695 in
  let r697 = [R 405] in
  let r698 = [R 406] in
  let r699 = [R 193] in
  let r700 = S (T T_RBRACKET) :: r699 in
  let r701 = Sub (r15) :: r700 in
  let r702 = [R 606] in
  let r703 = R 412 :: r702 in
  let r704 = S (N N_module_expr) :: r703 in
  let r705 = [R 415] in
  let r706 = S (T T_STRING) :: r705 in
  let r707 = [R 414] in
  let r708 = R 412 :: r707 in
  let r709 = Sub (r706) :: r708 in
  let r710 = S (T T_EQUAL) :: r709 in
  let r711 = Sub (r33) :: r710 in
  let r712 = S (T T_COLON) :: r711 in
  let r713 = Sub (r21) :: r712 in
  let r714 = [R 599] in
  let r715 = R 412 :: r714 in
  let r716 = R 17 :: r715 in
  let r717 = Sub (r240) :: r716 in
  let r718 = S (T T_EQUAL) :: r717 in
  let r719 = Sub (r134) :: r718 in
  let r720 = [R 441] in
  let r721 = R 412 :: r720 in
  let r722 = R 17 :: r721 in
  let r723 = R 206 :: r722 in
  let r724 = Sub (r134) :: r723 in
  let r725 = R 181 :: r724 in
  let r726 = [R 403] in
  let r727 = [R 445] in
  let r728 = R 412 :: r727 in
  let r729 = S (N N_module_type) :: r728 in
  let r730 = [R 85] in
  let r731 = S (T T_LIDENT) :: r730 in
  let r732 = [R 65] in
  let r733 = Sub (r731) :: r732 in
  let r734 = [R 80] in
  let r735 = R 412 :: r734 in
  let r736 = Sub (r733) :: r735 in
  let r737 = S (T T_EQUAL) :: r736 in
  let r738 = S (T T_LIDENT) :: r737 in
  let r739 = R 83 :: r738 in
  let r740 = R 688 :: r739 in
  let r741 = R 181 :: r740 in
  let r742 = [R 84] in
  let r743 = S (T T_RBRACKET) :: r742 in
  let r744 = [R 55] in
  let r745 = R 62 :: r744 in
  let r746 = R 54 :: r745 in
  let r747 = [R 66] in
  let r748 = S (T T_END) :: r747 in
  let r749 = [R 53] in
  let r750 = S (T T_RPAREN) :: r749 in
  let r751 = [R 687] in
  let r752 = Sub (r33) :: r751 in
  let r753 = S (T T_COLON) :: r752 in
  let r754 = Sub (r201) :: r753 in
  let r755 = [R 57] in
  let r756 = R 412 :: r755 in
  let r757 = [R 685] in
  let r758 = Sub (r33) :: r757 in
  let r759 = S (T T_COLON) :: r758 in
  let r760 = Sub (r201) :: r759 in
  let r761 = [R 686] in
  let r762 = Sub (r33) :: r761 in
  let r763 = S (T T_COLON) :: r762 in
  let r764 = Sub (r201) :: r763 in
  let r765 = [R 407] in
  let r766 = Sub (r33) :: r765 in
  let r767 = [R 58] in
  let r768 = R 412 :: r767 in
  let r769 = Sub (r766) :: r768 in
  let r770 = S (T T_COLON) :: r769 in
  let r771 = Sub (r201) :: r770 in
  let r772 = [R 408] in
  let r773 = Sub (r33) :: r772 in
  let r774 = [R 56] in
  let r775 = R 412 :: r774 in
  let r776 = [R 64] in
  let r777 = Sub (r731) :: r776 in
  let r778 = S (T T_RBRACKET) :: r777 in
  let r779 = [R 86] in
  let r780 = S (T T_LIDENT) :: r779 in
  let r781 = [R 103] in
  let r782 = Sub (r33) :: r781 in
  let r783 = S (T T_EQUAL) :: r782 in
  let r784 = Sub (r33) :: r783 in
  let r785 = [R 59] in
  let r786 = R 412 :: r785 in
  let r787 = [R 60] in
  let r788 = [R 75] in
  let r789 = Sub (r733) :: r788 in
  let r790 = [R 25] in
  let r791 = R 412 :: r790 in
  let r792 = Sub (r789) :: r791 in
  let r793 = S (T T_COLON) :: r792 in
  let r794 = S (T T_LIDENT) :: r793 in
  let r795 = R 83 :: r794 in
  let r796 = [R 76] in
  let r797 = Sub (r789) :: r796 in
  let r798 = S (T T_MINUSGREATER) :: r797 in
  let r799 = Sub (r27) :: r798 in
  let r800 = S (T T_COLON) :: r799 in
  let r801 = [R 77] in
  let r802 = Sub (r789) :: r801 in
  let r803 = S (T T_MINUSGREATER) :: r802 in
  let r804 = [R 78] in
  let r805 = Sub (r789) :: r804 in
  let r806 = S (T T_MINUSGREATER) :: r805 in
  let r807 = [R 79] in
  let r808 = Sub (r789) :: r807 in
  let r809 = [R 13] in
  let r810 = R 412 :: r809 in
  let r811 = R 105 :: r810 in
  let r812 = R 649 :: r811 in
  let r813 = S (T T_LIDENT) :: r812 in
  let r814 = R 369 :: r813 in
  let r815 = [R 449] in
  let r816 = [R 12] in
  let r817 = R 412 :: r816 in
  let r818 = S (N N_module_type) :: r817 in
  let r819 = S (T T_COLON) :: r818 in
  let r820 = S (T T_UIDENT) :: r819 in
  let r821 = [R 463] in
  let r822 = [R 9] in
  let r823 = R 412 :: r822 in
  let r824 = Sub (r733) :: r823 in
  let r825 = S (T T_EQUAL) :: r824 in
  let r826 = S (T T_LIDENT) :: r825 in
  let r827 = R 83 :: r826 in
  let r828 = R 688 :: r827 in
  let r829 = [R 8] in
  let r830 = R 412 :: r829 in
  let r831 = Sub (r789) :: r830 in
  let r832 = S (T T_COLON) :: r831 in
  let r833 = S (T T_LIDENT) :: r832 in
  let r834 = R 83 :: r833 in
  let r835 = R 688 :: r834 in
  let r836 = [R 70] in
  let r837 = Sub (r54) :: r836 in
  let r838 = [R 28] in
  let r839 = Sub (r837) :: r838 in
  let r840 = [R 43] in
  let r841 = Sub (r839) :: r840 in
  let r842 = S (T T_EQUAL) :: r841 in
  let r843 = [R 22] in
  let r844 = R 412 :: r843 in
  let r845 = Sub (r842) :: r844 in
  let r846 = S (T T_LIDENT) :: r845 in
  let r847 = R 83 :: r846 in
  let r848 = [R 71] in
  let r849 = S (T T_END) :: r848 in
  let r850 = Sub (r251) :: r849 in
  let r851 = [R 73] in
  let r852 = S (T T_RPAREN) :: r851 in
  let r853 = [R 69] in
  let r854 = Sub (r54) :: r853 in
  let r855 = S (T T_RBRACKET) :: r854 in
  let r856 = [R 46] in
  let r857 = Sub (r839) :: r856 in
  let r858 = S (T T_MINUSGREATER) :: r857 in
  let r859 = Sub (r517) :: r858 in
  let r860 = [R 29] in
  let r861 = Sub (r859) :: r860 in
  let r862 = [R 31] in
  let r863 = Sub (r839) :: r862 in
  let r864 = [R 72] in
  let r865 = S (T T_RPAREN) :: r864 in
  let r866 = [R 44] in
  let r867 = Sub (r839) :: r866 in
  let r868 = S (T T_EQUAL) :: r867 in
  let r869 = [R 45] in
  let r870 = [R 612] in
  let r871 = [R 631] in
  let r872 = [R 11] in
  let r873 = R 412 :: r872 in
  let r874 = Sub (r351) :: r873 in
  let r875 = S (T T_UIDENT) :: r874 in
  let r876 = [R 627] in
  let r877 = [R 7] in
  let r878 = R 412 :: r877 in
  let r879 = Sub (r842) :: r878 in
  let r880 = S (T T_LIDENT) :: r879 in
  let r881 = R 83 :: r880 in
  let r882 = R 688 :: r881 in
  let r883 = [R 611] in
  let r884 = S (N N_structure_tail) :: r883 in
  let r885 = [R 290] in
  let r886 = R 412 :: r885 in
  let r887 = Sub (r351) :: r886 in
  let r888 = [R 525] in
  let r889 = S (T T_RPAREN) :: r888 in
  let r890 = [R 145] in
  let r891 = S (T T_RPAREN) :: r890 in
  let r892 = S (N N_expr) :: r891 in
  let r893 = S (T T_COMMA) :: r892 in
  let r894 = S (N N_expr) :: r893 in
  let r895 = S (T T_LPAREN) :: r894 in
  let r896 = [R 497] in
  let r897 = [R 500] in
  let r898 = [R 683] in
  let r899 = Sub (r1) :: r898 in
  let r900 = [R 281] in
  let r901 = Sub (r609) :: r900 in
  let r902 = Sub (r201) :: r901 in
  let r903 = R 17 :: r902 in
  let r904 = R 417 :: r903 in
  let r905 = R 374 :: r904 in
  let r906 = [R 36] in
  let r907 = R 412 :: r906 in
  let r908 = [R 280] in
  let r909 = Sub (r766) :: r908 in
  let r910 = S (T T_COLON) :: r909 in
  let r911 = Sub (r201) :: r910 in
  let r912 = R 17 :: r911 in
  let r913 = [R 279] in
  let r914 = Sub (r766) :: r913 in
  let r915 = S (T T_COLON) :: r914 in
  let r916 = Sub (r201) :: r915 in
  let r917 = [R 282] in
  let r918 = Sub (r1) :: r917 in
  let r919 = S (T T_EQUAL) :: r918 in
  let r920 = [R 283] in
  let r921 = Sub (r1) :: r920 in
  let r922 = S (T T_EQUAL) :: r921 in
  let r923 = Sub (r33) :: r922 in
  let r924 = S (T T_DOT) :: r923 in
  let r925 = [R 38] in
  let r926 = R 412 :: r925 in
  let r927 = Sub (r1) :: r926 in
  let r928 = [R 34] in
  let r929 = R 412 :: r928 in
  let r930 = R 382 :: r929 in
  let r931 = Sub (r839) :: r930 in
  let r932 = R 17 :: r931 in
  let r933 = [R 381] in
  let r934 = [R 37] in
  let r935 = R 412 :: r934 in
  let r936 = Sub (r784) :: r935 in
  let r937 = [R 39] in
  let r938 = [R 509] in
  let r939 = S (T T_BARRBRACKET) :: r938 in
  let r940 = [R 600] in
  let r941 = Sub (r369) :: r940 in
  let r942 = [R 607] in
  let r943 = R 412 :: r942 in
  let r944 = Sub (r941) :: r943 in
  let r945 = R 417 :: r944 in
  let r946 = [R 21] in
  let r947 = R 17 :: r946 in
  let r948 = Sub (r240) :: r947 in
  let r949 = S (T T_EQUAL) :: r948 in
  let r950 = [R 186] in
  let r951 = R 17 :: r950 in
  let r952 = Sub (r240) :: r951 in
  let r953 = [R 380] in
  let r954 = [R 212] in
  let r955 = [R 220] in
  let r956 = [R 383] in
  function
  | 0 | 1477 | 1481 -> Nothing
  | 1476 -> One ([R 0])
  | 1480 -> One ([R 1])
  | 1484 -> One ([R 2])
  | 349 -> One ([R 3])
  | 348 -> One ([R 4])
  | 215 -> One (R 17 :: r167)
  | 238 -> One (R 17 :: r181)
  | 342 -> One (R 17 :: r246)
  | 463 -> One (R 17 :: r323)
  | 493 -> One (R 17 :: r341)
  | 517 -> One (R 17 :: r365)
  | 551 -> One (R 17 :: r406)
  | 581 -> One (R 17 :: r454)
  | 590 -> One (R 17 :: r464)
  | 778 -> One (R 17 :: r597)
  | 1171 -> One (R 17 :: r814)
  | 1186 -> One (R 17 :: r820)
  | 1203 -> One (R 17 :: r828)
  | 1214 -> One (R 17 :: r835)
  | 1232 -> One (R 17 :: r850)
  | 1242 -> One (R 17 :: r861)
  | 1282 -> One (R 17 :: r875)
  | 1299 -> One (R 17 :: r882)
  | 1368 -> One (R 17 :: r916)
  | 1389 -> One (R 17 :: r927)
  | 1401 -> One (R 17 :: r936)
  | 1307 -> One ([R 23])
  | 1306 -> One ([R 24])
  | 1223 -> One ([R 26])
  | 1222 -> One ([R 27])
  | 1250 -> One ([R 30])
  | 1253 -> One ([R 32])
  | 1248 -> One ([R 33])
  | 1407 -> One ([R 40])
  | 1408 -> One ([R 42])
  | 1255 -> One ([R 47])
  | 1133 -> One ([R 61])
  | 1134 -> One ([R 63])
  | 1124 -> One ([R 67])
  | 1120 -> One ([R 68])
  | 1212 -> One ([R 81])
  | 1211 -> One ([R 82])
  | 399 -> One ([R 88])
  | 337 -> One ([R 89])
  | 395 -> One ([R 90])
  | 180 | 273 -> One ([R 91])
  | 181 -> One ([R 96])
  | 415 -> One ([R 97])
  | 336 -> One ([R 101])
  | 314 -> One ([R 110])
  | 309 -> One ([R 111])
  | 267 -> One ([R 113])
  | 870 -> One ([R 125])
  | 694 -> One ([R 127])
  | 854 -> One ([R 128])
  | 722 -> One ([R 137])
  | 731 -> One ([R 138])
  | 715 -> One ([R 139])
  | 729 -> One ([R 176])
  | 922 -> One ([R 180])
  | 1 -> One (R 181 :: r6)
  | 60 -> One (R 181 :: r38)
  | 132 -> One (R 181 :: r82)
  | 334 -> One (R 181 :: r238)
  | 339 -> One (R 181 :: r245)
  | 350 -> One (R 181 :: r254)
  | 391 -> One (R 181 :: r287)
  | 397 -> One (R 181 :: r290)
  | 486 -> One (R 181 :: r336)
  | 540 -> One (R 181 :: r386)
  | 567 -> One (R 181 :: r439)
  | 569 -> One (R 181 :: r441)
  | 660 -> One (R 181 :: r506)
  | 662 -> One (R 181 :: r509)
  | 667 -> One (R 181 :: r523)
  | 682 -> One (R 181 :: r547)
  | 686 -> One (R 181 :: r549)
  | 967 -> One (R 181 :: r679)
  | 1028 -> One (R 181 :: r704)
  | 1032 -> One (R 181 :: r713)
  | 1055 -> One (R 181 :: r729)
  | 936 -> One ([R 191])
  | 585 -> One ([R 202])
  | 584 -> One ([R 203])
  | 638 -> One ([R 204])
  | 639 -> One ([R 205])
  | 124 | 373 -> One ([R 210])
  | 295 -> One ([R 226])
  | 296 -> One ([R 227])
  | 855 -> One ([R 238])
  | 857 -> One ([R 239])
  | 944 -> One ([R 251])
  | 943 -> One ([R 252])
  | 384 -> One ([R 256])
  | 388 -> One ([R 258])
  | 828 -> One ([R 260])
  | 719 -> One ([R 265])
  | 839 -> One ([R 266])
  | 784 -> One ([R 267])
  | 795 -> One ([R 270])
  | 909 -> One ([R 273])
  | 1448 -> One ([R 275])
  | 1447 -> One ([R 276])
  | 1449 -> One ([R 277])
  | 151 -> One ([R 278])
  | 652 -> One ([R 298])
  | 651 -> One ([R 309])
  | 653 -> One ([R 310])
  | 594 -> One ([R 311])
  | 637 -> One ([R 317])
  | 636 -> One ([R 318])
  | 465 -> One (R 324 :: r327)
  | 1082 -> One (R 324 :: r760)
  | 279 | 476 -> One ([R 325])
  | 255 -> One ([R 328])
  | 162 -> One ([R 330])
  | 66 | 346 -> One ([R 332])
  | 81 -> One ([R 333])
  | 80 -> One ([R 334])
  | 79 -> One ([R 335])
  | 78 -> One ([R 336])
  | 77 -> One ([R 337])
  | 65 -> One ([R 338])
  | 103 | 915 -> One ([R 339])
  | 69 | 363 | 490 -> One ([R 340])
  | 68 | 489 -> One ([R 341])
  | 75 | 376 | 659 -> One ([R 342])
  | 74 | 658 -> One ([R 343])
  | 64 -> One ([R 344])
  | 83 -> One ([R 345])
  | 76 -> One ([R 346])
  | 82 -> One ([R 347])
  | 71 -> One ([R 348])
  | 102 -> One ([R 349])
  | 104 -> One ([R 350])
  | 105 -> One ([R 351])
  | 101 -> One ([R 352])
  | 67 -> One ([R 353])
  | 70 -> One ([R 354])
  | 109 -> One ([R 355])
  | 217 -> One ([R 356])
  | 216 -> One (R 357 :: r172)
  | 99 -> One ([R 359])
  | 185 -> One (R 360 :: r149)
  | 186 -> One ([R 361])
  | 385 -> One (R 364 :: r281)
  | 444 -> One (R 364 :: r306)
  | 920 -> One (R 364 :: r658)
  | 928 -> One (R 364 :: r661)
  | 1410 -> One (R 364 :: r939)
  | 386 | 438 | 921 | 935 -> One ([R 365])
  | 538 -> One ([R 370])
  | 507 -> One (R 374 :: r348)
  | 1393 -> One (R 374 :: r932)
  | 470 -> One ([R 375])
  | 402 -> One ([R 384])
  | 407 -> One ([R 386])
  | 412 -> One ([R 394])
  | 424 -> One ([R 397])
  | 439 -> One ([R 399])
  | 790 -> One ([R 400])
  | 1225 -> One ([R 404])
  | 473 -> One (R 412 :: r328)
  | 1131 -> One (R 412 :: r787)
  | 1199 -> One (R 412 :: r821)
  | 1294 -> One (R 412 :: r876)
  | 1309 -> One (R 412 :: r884)
  | 1405 -> One (R 412 :: r937)
  | 1039 -> One ([R 416])
  | 1361 -> One (R 417 :: r912)
  | 322 | 1367 -> One ([R 418])
  | 1097 -> One (R 419 :: r771)
  | 1100 -> One ([R 420])
  | 1098 -> One ([R 421])
  | 1101 -> One ([R 422])
  | 1099 -> One ([R 423])
  | 982 -> One ([R 425])
  | 1288 -> One ([R 427])
  | 1287 -> One ([R 428])
  | 1193 -> One ([R 430])
  | 1192 -> One ([R 431])
  | 197 -> One ([R 434])
  | 776 -> One ([R 439])
  | 777 -> One ([R 440])
  | 537 -> One ([R 443])
  | 532 -> One ([R 444])
  | 539 -> One (R 447 :: r380)
  | 1054 -> One (R 447 :: r726)
  | 1180 -> One (R 447 :: r815)
  | 1169 -> One ([R 450])
  | 1194 -> One ([R 451])
  | 1170 -> One ([R 452])
  | 1182 -> One ([R 453])
  | 1184 -> One ([R 454])
  | 1197 -> One ([R 455])
  | 1198 -> One ([R 456])
  | 1185 -> One ([R 457])
  | 1196 -> One ([R 458])
  | 1195 -> One ([R 459])
  | 1183 -> One ([R 460])
  | 1213 -> One ([R 461])
  | 1202 -> One ([R 462])
  | 1201 -> One ([R 464])
  | 361 -> One ([R 467])
  | 358 -> One ([R 469])
  | 196 -> One ([R 474])
  | 201 -> One ([R 475])
  | 264 -> One ([R 476])
  | 223 | 1161 -> One ([R 490])
  | 572 -> One ([R 493])
  | 706 -> One ([R 494])
  | 574 | 714 -> One ([R 496])
  | 851 | 868 -> One ([R 501])
  | 704 -> One ([R 527])
  | 858 -> One ([R 528])
  | 856 -> One ([R 529])
  | 400 | 780 -> One ([R 530])
  | 403 -> One ([R 533])
  | 435 -> One ([R 535])
  | 434 -> One ([R 536])
  | 417 -> One ([R 546])
  | 28 -> One ([R 547])
  | 8 -> One ([R 548])
  | 52 -> One ([R 550])
  | 51 -> One ([R 551])
  | 50 -> One ([R 552])
  | 49 -> One ([R 553])
  | 48 -> One ([R 554])
  | 47 -> One ([R 555])
  | 46 -> One ([R 556])
  | 45 -> One ([R 557])
  | 44 -> One ([R 558])
  | 43 -> One ([R 559])
  | 42 -> One ([R 560])
  | 41 -> One ([R 561])
  | 40 -> One ([R 562])
  | 39 -> One ([R 563])
  | 38 -> One ([R 564])
  | 37 -> One ([R 565])
  | 36 -> One ([R 566])
  | 35 -> One ([R 567])
  | 34 -> One ([R 568])
  | 33 -> One ([R 569])
  | 32 -> One ([R 570])
  | 31 -> One ([R 571])
  | 30 -> One ([R 572])
  | 29 -> One ([R 573])
  | 27 -> One ([R 574])
  | 26 -> One ([R 575])
  | 25 -> One ([R 576])
  | 24 -> One ([R 577])
  | 23 -> One ([R 578])
  | 22 -> One ([R 579])
  | 21 -> One ([R 580])
  | 20 -> One ([R 581])
  | 19 -> One ([R 582])
  | 18 -> One ([R 583])
  | 17 -> One ([R 584])
  | 16 -> One ([R 585])
  | 15 -> One ([R 586])
  | 14 -> One ([R 587])
  | 13 -> One ([R 588])
  | 12 -> One ([R 589])
  | 11 -> One ([R 590])
  | 10 -> One ([R 591])
  | 9 -> One ([R 592])
  | 7 -> One ([R 593])
  | 6 -> One ([R 594])
  | 5 -> One ([R 595])
  | 4 -> One ([R 596])
  | 3 -> One ([R 597])
  | 1280 -> One ([R 598])
  | 1440 -> One ([R 601])
  | 1433 -> One ([R 602])
  | 1439 -> One ([R 603])
  | 1432 -> One ([R 604])
  | 1431 -> One ([R 605])
  | 1274 -> One ([R 613])
  | 1293 | 1312 -> One ([R 614])
  | 1289 -> One ([R 615])
  | 1271 -> One ([R 616])
  | 1272 -> One ([R 617])
  | 1277 -> One ([R 618])
  | 1279 -> One ([R 619])
  | 1292 -> One ([R 620])
  | 1281 -> One ([R 621])
  | 1291 -> One ([R 622])
  | 1290 -> One ([R 623])
  | 1298 -> One ([R 624])
  | 1297 -> One ([R 625])
  | 1278 -> One ([R 626])
  | 1296 -> One ([R 628])
  | 485 -> One ([R 632])
  | 484 -> One ([R 633])
  | 498 -> One ([R 637])
  | 499 -> One ([R 638])
  | 501 -> One ([R 639])
  | 503 -> One ([R 640])
  | 500 -> One ([R 641])
  | 497 -> One ([R 642])
  | 1179 -> One ([R 647])
  | 1178 -> One ([R 648])
  | 320 -> One ([R 650])
  | 307 -> One ([R 651])
  | 329 -> One ([R 652])
  | 597 -> One (R 664 :: r476)
  | 624 -> One ([R 665])
  | 164 -> One ([R 669])
  | 165 -> One ([R 670])
  | 110 | 566 | 952 -> One ([R 673])
  | 107 -> One ([R 675])
  | 502 -> One ([R 678])
  | 505 -> One ([R 679])
  | 1087 -> One (R 688 :: r764)
  | 1137 -> One (R 688 :: r795)
  | 1227 -> One (R 688 :: r847)
  | 1065 -> One ([R 689])
  | 612 -> One ([R 697])
  | 72 | 364 | 491 | 579 -> One (S (T T_error) :: r39)
  | 939 -> One (S (T T_WITH) :: r671)
  | 416 | 504 -> One (S (T T_UIDENT) :: r59)
  | 206 -> One (S (T T_UIDENT) :: r165)
  | 365 -> One (S (T T_UIDENT) :: r263)
  | 546 -> One (S (T T_TYPE) :: r401)
  | 792 -> One (S (T T_TYPE) :: r615)
  | 1062 | 1226 -> One (S (T T_TYPE) :: r741)
  | 85 -> One (S (T T_RPAREN) :: r45)
  | 106 -> One (S (T T_RPAREN) :: r51)
  | 183 | 274 -> One (S (T T_RPAREN) :: r142)
  | 257 -> One (S (T T_RPAREN) :: r191)
  | 1046 -> One (S (T T_RPAREN) :: r247)
  | 421 -> One (S (T T_RPAREN) :: r299)
  | 447 -> One (S (T T_RPAREN) :: r312)
  | 543 -> One (S (T T_RPAREN) :: r393)
  | 583 -> One (S (T T_RPAREN) :: r455)
  | 647 -> One (S (T T_RPAREN) :: r501)
  | 649 -> One (S (T T_RPAREN) :: r502)
  | 916 -> One (S (T T_RPAREN) :: r655)
  | 1340 -> One (S (T T_RPAREN) :: r895)
  | 1347 -> One (S (T T_RPAREN) :: r896)
  | 1349 -> One (S (T T_RPAREN) :: r897)
  | 87 -> One (S (T T_RBRACKET) :: r46)
  | 189 -> One (S (T T_RBRACKET) :: r150)
  | 242 -> One (S (T T_RBRACKET) :: r182)
  | 269 | 275 -> One (S (T T_RBRACKET) :: r194)
  | 1047 -> One (S (T T_RBRACKET) :: r274)
  | 926 -> One (S (T T_RBRACKET) :: r659)
  | 91 -> One (S (T T_RBRACE) :: r47)
  | 96 -> One (S (T T_RBRACE) :: r50)
  | 231 -> One (S (T T_QUOTE) :: r179)
  | 523 -> One (S (T T_PLUSEQ) :: r375)
  | 1421 -> One (S (T T_PLUSEQ) :: r945)
  | 302 -> One (S (T T_MINUSGREATER) :: r223)
  | 1156 -> One (S (T T_MINUSGREATER) :: r808)
  | 127 -> One (S (T T_LIDENT) :: r68)
  | 1142 -> One (S (T T_LIDENT) :: r800)
  | 1397 -> One (S (T T_LIDENT) :: r933)
  | 720 -> One (S (T T_LESSMINUS) :: r570)
  | 316 -> One (S (T T_LBRACE) :: r226)
  | 356 -> One (S (T T_INT) :: r260)
  | 359 -> One (S (T T_INT) :: r261)
  | 716 -> One (S (T T_IN) :: r568)
  | 1246 -> One (S (T T_IN) :: r863)
  | 561 -> One (S (T T_GREATERRBRACE) :: r427)
  | 961 -> One (S (T T_GREATERRBRACE) :: r674)
  | 148 -> One (S (T T_GREATER) :: r99)
  | 152 -> One (S (T T_GREATER) :: r101)
  | 629 -> One (S (T T_EQUAL) :: r499)
  | 808 -> One (S (T T_EQUAL) :: r622)
  | 1356 -> One (S (T T_EQUAL) :: r899)
  | 1435 -> One (S (T T_EQUAL) :: r952)
  | 1474 -> One (S (T T_EOF) :: r954)
  | 1478 -> One (S (T T_EOF) :: r955)
  | 1482 -> One (S (T T_EOF) :: r956)
  | 956 -> One (S (T T_END) :: r673)
  | 179 -> One (S (T T_DOTDOT) :: r132)
  | 321 -> One (S (T T_DOTDOT) :: r227)
  | 118 -> One (S (T T_DOT) :: r58)
  | 141 -> One (S (T T_DOT) :: r94)
  | 283 -> One (S (T T_DOT) :: r219)
  | 371 -> One (S (T T_DOT) :: r267)
  | 823 -> One (S (T T_DOT) :: r632)
  | 1105 -> One (S (T T_DOT) :: r773)
  | 1116 -> One (S (T T_DOT) :: r780)
  | 93 -> One (S (T T_COMMA) :: r49)
  | 154 -> One (S (T T_COLON) :: r108)
  | 544 -> One (S (T T_COLON) :: r397)
  | 587 -> One (S (T T_COLON) :: r458)
  | 347 -> One (S (T T_BARRBRACKET) :: r248)
  | 377 -> One (S (T T_BARRBRACKET) :: r268)
  | 918 -> One (S (T T_BARRBRACKET) :: r656)
  | 192 | 1154 -> One (S (T T_BAR) :: r155)
  | 244 -> One (S (T T_BAR) :: r185)
  | 1275 -> One (S (N N_structure_tail) :: r871)
  | 506 -> One (S (N N_structure) :: r343)
  | 1273 -> One (S (N N_structure) :: r870)
  | 352 -> One (S (N N_pattern) :: r256)
  | 390 -> One (S (N N_pattern) :: r282)
  | 408 -> One (S (N N_pattern) :: r295)
  | 410 -> One (S (N N_pattern) :: r296)
  | 413 -> One (S (N N_pattern) :: r297)
  | 418 -> One (S (N N_pattern) :: r298)
  | 423 -> One (S (N N_pattern) :: r300)
  | 429 -> One (S (N N_pattern) :: r303)
  | 1022 -> One (S (N N_pattern) :: r697)
  | 516 -> One (S (N N_module_type) :: r362)
  | 589 -> One (S (N N_module_type) :: r460)
  | 972 -> One (S (N N_module_type) :: r682)
  | 999 -> One (S (N N_module_type) :: r691)
  | 492 -> One (S (N N_module_expr) :: r338)
  | 550 -> One (S (N N_module_expr) :: r403)
  | 580 -> One (S (N N_module_expr) :: r450)
  | 783 -> One (S (N N_let_pattern) :: r603)
  | 564 -> One (S (N N_expr) :: r429)
  | 693 -> One (S (N N_expr) :: r558)
  | 713 -> One (S (N N_expr) :: r566)
  | 723 -> One (S (N N_expr) :: r571)
  | 725 -> One (S (N N_expr) :: r572)
  | 727 -> One (S (N N_expr) :: r573)
  | 732 -> One (S (N N_expr) :: r574)
  | 734 -> One (S (N N_expr) :: r575)
  | 736 -> One (S (N N_expr) :: r576)
  | 738 -> One (S (N N_expr) :: r577)
  | 740 -> One (S (N N_expr) :: r578)
  | 742 -> One (S (N N_expr) :: r579)
  | 744 -> One (S (N N_expr) :: r580)
  | 746 -> One (S (N N_expr) :: r581)
  | 748 -> One (S (N N_expr) :: r582)
  | 750 -> One (S (N N_expr) :: r583)
  | 752 -> One (S (N N_expr) :: r584)
  | 754 -> One (S (N N_expr) :: r585)
  | 756 -> One (S (N N_expr) :: r586)
  | 758 -> One (S (N N_expr) :: r587)
  | 760 -> One (S (N N_expr) :: r588)
  | 762 -> One (S (N N_expr) :: r589)
  | 764 -> One (S (N N_expr) :: r590)
  | 766 -> One (S (N N_expr) :: r591)
  | 768 -> One (S (N N_expr) :: r592)
  | 771 -> One (S (N N_expr) :: r593)
  | 773 -> One (S (N N_expr) :: r594)
  | 844 -> One (S (N N_expr) :: r639)
  | 849 -> One (S (N N_expr) :: r640)
  | 852 -> One (S (N N_expr) :: r641)
  | 913 -> One (S (N N_expr) :: r654)
  | 946 -> One (S (N N_expr) :: r672)
  | 666 -> One (Sub (r1) :: r515)
  | 681 -> One (Sub (r1) :: r536)
  | 1024 -> One (Sub (r1) :: r698)
  | 2 -> One (Sub (r10) :: r12)
  | 55 -> One (Sub (r10) :: r13)
  | 58 -> One (Sub (r10) :: r18)
  | 158 -> One (Sub (r10) :: r112)
  | 332 -> One (Sub (r10) :: r233)
  | 1020 -> One (Sub (r10) :: r696)
  | 1026 -> One (Sub (r10) :: r701)
  | 431 -> One (Sub (r21) :: r304)
  | 225 -> One (Sub (r25) :: r176)
  | 300 -> One (Sub (r25) :: r221)
  | 891 -> One (Sub (r25) :: r650)
  | 1147 -> One (Sub (r27) :: r803)
  | 1151 -> One (Sub (r27) :: r806)
  | 130 -> One (Sub (r29) :: r71)
  | 147 -> One (Sub (r29) :: r98)
  | 229 -> One (Sub (r29) :: r177)
  | 235 -> One (Sub (r31) :: r180)
  | 265 -> One (Sub (r33) :: r193)
  | 426 -> One (Sub (r33) :: r302)
  | 456 -> One (Sub (r33) :: r314)
  | 673 -> One (Sub (r33) :: r531)
  | 785 -> One (Sub (r33) :: r604)
  | 802 -> One (Sub (r33) :: r617)
  | 806 -> One (Sub (r33) :: r620)
  | 830 -> One (Sub (r33) :: r635)
  | 1074 -> One (Sub (r33) :: r750)
  | 84 -> One (Sub (r42) :: r44)
  | 115 -> One (Sub (r54) :: r55)
  | 199 -> One (Sub (r54) :: r158)
  | 262 -> One (Sub (r54) :: r192)
  | 169 -> One (Sub (r61) :: r130)
  | 284 -> One (Sub (r61) :: r220)
  | 495 -> One (Sub (r61) :: r342)
  | 131 -> One (Sub (r73) :: r75)
  | 1113 -> One (Sub (r73) :: r778)
  | 1238 -> One (Sub (r73) :: r855)
  | 368 -> One (Sub (r79) :: r265)
  | 988 -> One (Sub (r79) :: r685)
  | 991 -> One (Sub (r79) :: r687)
  | 994 -> One (Sub (r79) :: r689)
  | 1337 -> One (Sub (r79) :: r889)
  | 138 -> One (Sub (r91) :: r92)
  | 1456 -> One (Sub (r91) :: r953)
  | 174 -> One (Sub (r125) :: r131)
  | 166 -> One (Sub (r127) :: r129)
  | 272 -> One (Sub (r134) :: r197)
  | 182 -> One (Sub (r140) :: r141)
  | 324 -> One (Sub (r140) :: r228)
  | 213 -> One (Sub (r144) :: r166)
  | 191 -> One (Sub (r146) :: r152)
  | 203 -> One (Sub (r162) :: r164)
  | 221 -> One (Sub (r174) :: r175)
  | 252 -> One (Sub (r188) :: r190)
  | 277 -> One (Sub (r199) :: r200)
  | 280 -> One (Sub (r201) :: r217)
  | 477 -> One (Sub (r201) :: r331)
  | 698 -> One (Sub (r201) :: r562)
  | 278 -> One (Sub (r209) :: r211)
  | 325 -> One (Sub (r209) :: r230)
  | 338 -> One (Sub (r242) :: r243)
  | 571 -> One (Sub (r242) :: r442)
  | 689 -> One (Sub (r242) :: r550)
  | 702 -> One (Sub (r242) :: r564)
  | 707 -> One (Sub (r242) :: r565)
  | 354 -> One (Sub (r258) :: r259)
  | 378 -> One (Sub (r270) :: r273)
  | 380 -> One (Sub (r278) :: r280)
  | 798 -> One (Sub (r285) :: r616)
  | 404 -> One (Sub (r293) :: r294)
  | 1010 -> One (Sub (r344) :: r693)
  | 979 -> One (Sub (r351) :: r683)
  | 542 -> One (Sub (r389) :: r391)
  | 565 -> One (Sub (r435) :: r437)
  | 938 -> One (Sub (r435) :: r669)
  | 633 -> One (Sub (r469) :: r500)
  | 596 -> One (Sub (r471) :: r472)
  | 605 -> One (Sub (r482) :: r487)
  | 598 -> One (Sub (r484) :: r486)
  | 1067 -> One (Sub (r484) :: r743)
  | 610 -> One (Sub (r489) :: r492)
  | 616 -> One (Sub (r496) :: r497)
  | 895 -> One (Sub (r520) :: r651)
  | 669 -> One (Sub (r525) :: r526)
  | 678 -> One (Sub (r525) :: r532)
  | 670 -> One (Sub (r528) :: r530)
  | 679 -> One (Sub (r528) :: r535)
  | 695 -> One (Sub (r560) :: r561)
  | 700 -> One (Sub (r560) :: r563)
  | 781 -> One (Sub (r599) :: r600)
  | 817 -> One (Sub (r606) :: r628)
  | 882 -> One (Sub (r606) :: r644)
  | 888 -> One (Sub (r606) :: r647)
  | 1377 -> One (Sub (r606) :: r924)
  | 813 -> One (Sub (r611) :: r623)
  | 907 -> One (Sub (r652) :: r653)
  | 931 -> One (Sub (r665) :: r667)
  | 1111 -> One (Sub (r733) :: r775)
  | 1073 -> One (Sub (r746) :: r748)
  | 1081 -> One (Sub (r754) :: r756)
  | 1376 -> One (Sub (r766) :: r919)
  | 1125 -> One (Sub (r784) :: r786)
  | 1259 -> One (Sub (r789) :: r865)
  | 1263 -> One (Sub (r789) :: r868)
  | 1236 -> One (Sub (r839) :: r852)
  | 1267 -> One (Sub (r842) :: r869)
  | 1359 -> One (Sub (r905) :: r907)
  | 775 -> One (r0)
  | 1473 -> One (r2)
  | 1472 -> One (r3)
  | 1471 -> One (r4)
  | 1470 -> One (r5)
  | 1469 -> One (r6)
  | 53 -> One (r7)
  | 54 -> One (r9)
  | 1468 -> One (r11)
  | 57 -> One (r12)
  | 56 -> One (r13)
  | 1313 -> One (r14)
  | 1467 -> One (r16)
  | 1466 -> One (r17)
  | 59 -> One (r18)
  | 63 -> One (r19)
  | 62 | 344 | 362 | 393 | 488 | 881 | 887 -> One (r20)
  | 113 -> One (r22)
  | 198 -> One (r24)
  | 220 -> One (r26)
  | 219 -> One (r28)
  | 228 -> One (r30)
  | 259 -> One (r32)
  | 1465 -> One (r34)
  | 1464 -> One (r35)
  | 112 -> One (r36)
  | 111 -> One (r37)
  | 61 -> One (r38)
  | 73 -> One (r39)
  | 90 -> One (r40)
  | 89 -> One (r41)
  | 100 -> One (r43)
  | 98 -> One (r44)
  | 86 -> One (r45)
  | 88 -> One (r46)
  | 92 -> One (r47)
  | 95 -> One (r48)
  | 94 -> One (r49)
  | 97 -> One (r50)
  | 108 -> One (r51)
  | 114 | 134 -> One (r52)
  | 117 -> One (r53)
  | 122 -> One (r55)
  | 116 -> One (r56)
  | 121 -> One (r57)
  | 119 -> One (r58)
  | 120 -> One (r59)
  | 125 -> One (r60)
  | 126 -> One (r62)
  | 123 -> One (r63)
  | 1463 -> One (r64)
  | 1462 -> One (r65)
  | 1461 -> One (r66)
  | 129 -> One (r67)
  | 128 -> One (r68)
  | 1460 -> One (r69)
  | 1459 -> One (r70)
  | 1458 -> One (r71)
  | 268 -> One (r72)
  | 261 -> One (r74)
  | 260 -> One (r75)
  | 375 -> One (r76)
  | 137 -> One (r78)
  | 136 -> One (r80)
  | 135 -> One (r81)
  | 133 -> One (r82)
  | 140 -> One (r83)
  | 1453 -> One (r85)
  | 145 -> One (r86)
  | 144 -> One (r87)
  | 139 -> One (r88)
  | 1455 -> One (r90)
  | 1454 -> One (r92)
  | 143 -> One (r93)
  | 142 -> One (r94)
  | 146 | 188 | 1150 -> One (r95)
  | 1452 -> One (r96)
  | 1451 -> One (r97)
  | 1450 -> One (r98)
  | 150 -> One (r99)
  | 149 | 609 -> One (r100)
  | 153 -> One (r101)
  | 292 -> One (r102)
  | 1446 -> One (r104)
  | 1445 -> One (r105)
  | 1444 -> One (r106)
  | 1443 -> One (r107)
  | 155 -> One (r108)
  | 1442 -> One (r110)
  | 1441 -> One (r111)
  | 159 -> One (r112)
  | 1420 -> One (r113)
  | 331 -> One (r114)
  | 330 -> One (r115)
  | 178 -> One (r116)
  | 177 | 522 -> One (r117)
  | 163 | 521 -> One (r118)
  | 161 | 520 -> One (r119)
  | 160 | 519 -> One (r120)
  | 168 -> One (r121)
  | 171 -> One (r123)
  | 167 -> One (r124)
  | 176 -> One (r126)
  | 173 -> One (r128)
  | 172 -> One (r129)
  | 170 -> One (r130)
  | 175 -> One (r131)
  | 315 -> One (r132)
  | 271 -> One (r133)
  | 313 -> One (r135)
  | 312 -> One (r136)
  | 311 -> One (r137)
  | 310 -> One (r139)
  | 308 -> One (r141)
  | 184 -> One (r142)
  | 210 | 1155 -> One (r143)
  | 241 -> One (r145)
  | 251 -> One (r147)
  | 250 -> One (r148)
  | 187 -> One (r149)
  | 190 -> One (r150)
  | 249 -> One (r151)
  | 248 -> One (r152)
  | 212 -> One (r153)
  | 211 -> One (r154)
  | 193 -> One (r155)
  | 195 -> One (r156)
  | 194 -> One (r157)
  | 200 -> One (r158)
  | 209 | 1160 -> One (r159)
  | 208 | 1159 -> One (r160)
  | 202 | 1158 -> One (r161)
  | 205 -> One (r163)
  | 204 -> One (r164)
  | 207 -> One (r165)
  | 214 -> One (r166)
  | 240 -> One (r167)
  | 227 -> One (r168)
  | 237 -> One (r170)
  | 234 -> One (r171)
  | 218 -> One (r172)
  | 222 -> One (r173)
  | 224 -> One (r175)
  | 226 -> One (r176)
  | 230 -> One (r177)
  | 233 -> One (r178)
  | 232 -> One (r179)
  | 236 -> One (r180)
  | 239 -> One (r181)
  | 243 -> One (r182)
  | 247 -> One (r183)
  | 246 -> One (r184)
  | 245 -> One (r185)
  | 256 -> One (r187)
  | 254 -> One (r189)
  | 253 -> One (r190)
  | 258 -> One (r191)
  | 263 -> One (r192)
  | 266 -> One (r193)
  | 270 -> One (r194)
  | 306 -> One (r195)
  | 305 -> One (r196)
  | 276 -> One (r197)
  | 298 -> One (r198)
  | 299 -> One (r200)
  | 297 -> One (r208)
  | 294 -> One (r210)
  | 293 -> One (r211)
  | 291 -> One (r212)
  | 290 -> One (r213)
  | 289 -> One (r214)
  | 288 -> One (r215)
  | 282 -> One (r216)
  | 281 -> One (r217)
  | 287 -> One (r218)
  | 286 -> One (r219)
  | 285 -> One (r220)
  | 301 -> One (r221)
  | 304 -> One (r222)
  | 303 -> One (r223)
  | 319 -> One (r224)
  | 318 -> One (r225)
  | 317 -> One (r226)
  | 323 -> One (r227)
  | 328 -> One (r228)
  | 327 -> One (r229)
  | 326 -> One (r230)
  | 1419 -> One (r231)
  | 1418 -> One (r232)
  | 333 -> One (r233)
  | 1417 -> One (r234)
  | 1416 -> One (r235)
  | 1415 -> One (r236)
  | 1414 -> One (r237)
  | 335 -> One (r238)
  | 396 -> One (r239)
  | 705 | 730 -> One (r241)
  | 1413 -> One (r243)
  | 341 -> One (r244)
  | 340 -> One (r245)
  | 343 -> One (r246)
  | 345 -> One (r247)
  | 1409 -> One (r248)
  | 462 -> One (r249)
  | 461 -> One (r250)
  | 460 -> One (r252)
  | 459 -> One (r253)
  | 351 -> One (r254)
  | 455 -> One (r255)
  | 454 -> One (r256)
  | 353 -> One (r257)
  | 355 -> One (r259)
  | 357 -> One (r260)
  | 360 -> One (r261)
  | 367 -> One (r262)
  | 366 -> One (r263)
  | 370 -> One (r264)
  | 369 -> One (r265)
  | 374 -> One (r266)
  | 372 -> One (r267)
  | 443 -> One (r268)
  | 442 -> One (r269)
  | 441 -> One (r271)
  | 440 -> One (r272)
  | 437 -> One (r273)
  | 379 -> One (r274)
  | 389 -> One (r275)
  | 383 -> One (r277)
  | 382 -> One (r279)
  | 381 -> One (r280)
  | 387 -> One (r281)
  | 436 -> One (r282)
  | 401 | 829 -> One (r284)
  | 433 -> One (r286)
  | 392 -> One (r287)
  | 394 -> One (r288)
  | 420 -> One (r289)
  | 398 -> One (r290)
  | 406 -> One (r292)
  | 405 -> One (r294)
  | 409 -> One (r295)
  | 411 -> One (r296)
  | 414 -> One (r297)
  | 419 -> One (r298)
  | 422 -> One (r299)
  | 425 -> One (r300)
  | 428 -> One (r301)
  | 427 -> One (r302)
  | 430 -> One (r303)
  | 432 -> One (r304)
  | 446 -> One (r305)
  | 445 -> One (r306)
  | 453 -> One (r307)
  | 452 -> One (r308)
  | 451 -> One (r309)
  | 450 -> One (r310)
  | 449 -> One (r311)
  | 448 -> One (r312)
  | 458 -> One (r313)
  | 457 -> One (r314)
  | 1355 -> One (r315)
  | 483 -> One (r316)
  | 482 -> One (r317)
  | 481 -> One (r318)
  | 475 -> One (r319)
  | 472 -> One (r321)
  | 471 -> One (r322)
  | 464 -> One (r323)
  | 469 -> One (r324)
  | 468 -> One (r325)
  | 467 -> One (r326)
  | 466 -> One (r327)
  | 474 -> One (r328)
  | 480 -> One (r329)
  | 479 -> One (r330)
  | 478 -> One (r331)
  | 1354 -> One (r332)
  | 1353 -> One (r333)
  | 1352 -> One (r334)
  | 1351 -> One (r335)
  | 487 -> One (r336)
  | 1336 -> One (r337)
  | 1335 -> One (r338)
  | 1334 -> One (r339)
  | 1333 -> One (r340)
  | 494 -> One (r341)
  | 496 -> One (r342)
  | 1332 -> One (r343)
  | 511 -> One (r345)
  | 510 -> One (r346)
  | 509 -> One (r347)
  | 508 -> One (r348)
  | 971 -> One (r349)
  | 970 -> One (r350)
  | 1331 -> One (r352)
  | 1330 -> One (r353)
  | 1329 -> One (r354)
  | 1328 -> One (r355)
  | 1323 -> One (r357)
  | 1322 -> One (r358)
  | 515 -> One (r359)
  | 514 -> One (r360)
  | 513 -> One (r361)
  | 1321 -> One (r362)
  | 1320 -> One (r363)
  | 1319 -> One (r364)
  | 518 -> One (r365)
  | 536 -> One (r366)
  | 535 -> One (r367)
  | 534 -> One (r368)
  | 533 -> One (r370)
  | 531 -> One (r372)
  | 530 -> One (r373)
  | 525 -> One (r374)
  | 524 -> One (r375)
  | 529 -> One (r376)
  | 528 -> One (r377)
  | 527 -> One (r378)
  | 526 -> One (r379)
  | 1318 -> One (r380)
  | 1019 -> One (r381)
  | 1018 -> One (r382)
  | 1017 -> One (r383)
  | 1016 -> One (r384)
  | 1015 -> One (r385)
  | 541 -> One (r386)
  | 1006 -> One (r387)
  | 1005 -> One (r388)
  | 1014 -> One (r390)
  | 1013 -> One (r391)
  | 1009 -> One (r392)
  | 1008 -> One (r393)
  | 1007 -> One (r394)
  | 1004 -> One (r395)
  | 1003 -> One (r396)
  | 545 -> One (r397)
  | 1002 -> One (r398)
  | 549 -> One (r399)
  | 548 -> One (r400)
  | 547 -> One (r401)
  | 998 -> One (r402)
  | 997 -> One (r403)
  | 987 -> One (r404)
  | 986 -> One (r405)
  | 552 -> One (r406)
  | 836 -> One (r407)
  | 835 -> One (r408)
  | 834 -> One (r409)
  | 985 -> One (r411)
  | 984 -> One (r412)
  | 983 -> One (r413)
  | 981 -> One (r414)
  | 1237 -> One (r415)
  | 966 -> One (r416)
  | 559 -> One (r417)
  | 558 -> One (r418)
  | 557 -> One (r419)
  | 556 -> One (r420)
  | 555 -> One (r421)
  | 925 -> One (r422)
  | 965 -> One (r424)
  | 964 -> One (r425)
  | 963 -> One (r426)
  | 562 -> One (r427)
  | 563 -> One (r428)
  | 960 -> One (r429)
  | 945 -> One (r430)
  | 942 -> One (r432)
  | 953 -> One (r434)
  | 959 -> One (r436)
  | 958 -> One (r437)
  | 955 -> One (r438)
  | 568 -> One (r439)
  | 954 -> One (r440)
  | 570 -> One (r441)
  | 573 -> One (r442)
  | 578 -> One (r443)
  | 577 -> One (r444)
  | 576 | 951 -> One (r445)
  | 950 -> One (r446)
  | 657 -> One (r447)
  | 656 -> One (r448)
  | 655 -> One (r449)
  | 654 -> One (r450)
  | 646 -> One (r451)
  | 645 -> One (r452)
  | 644 -> One (r453)
  | 582 -> One (r454)
  | 586 -> One (r455)
  | 643 -> One (r456)
  | 642 -> One (r457)
  | 588 -> One (r458)
  | 641 -> One (r459)
  | 640 -> One (r460)
  | 595 -> One (r461)
  | 593 -> One (r462)
  | 592 -> One (r463)
  | 591 -> One (r464)
  | 628 -> One (r465)
  | 627 -> One (r466)
  | 626 -> One (r467)
  | 625 -> One (r468)
  | 635 -> One (r470)
  | 632 -> One (r472)
  | 623 -> One (r473)
  | 622 -> One (r474)
  | 621 -> One (r475)
  | 608 -> One (r476)
  | 601 -> One (r477)
  | 600 -> One (r478)
  | 602 -> One (r480)
  | 599 -> One (r481)
  | 607 -> One (r483)
  | 604 -> One (r485)
  | 603 -> One (r486)
  | 606 -> One (r487)
  | 611 -> One (r488)
  | 615 -> One (r490)
  | 614 -> One (r491)
  | 613 -> One (r492)
  | 619 -> One (r493)
  | 618 -> One (r494)
  | 617 -> One (r495)
  | 620 -> One (r497)
  | 631 -> One (r498)
  | 630 -> One (r499)
  | 634 -> One (r500)
  | 648 -> One (r501)
  | 650 -> One (r502)
  | 912 -> One (r503)
  | 911 -> One (r504)
  | 910 -> One (r505)
  | 661 -> One (r506)
  | 906 -> One (r507)
  | 664 -> One (r508)
  | 663 -> One (r509)
  | 905 -> One (r510)
  | 904 -> One (r511)
  | 665 -> One (r512)
  | 903 -> One (r513)
  | 902 -> One (r514)
  | 901 -> One (r515)
  | 812 -> One (r516)
  | 886 -> One (r518)
  | 885 -> One (r519)
  | 900 -> One (r521)
  | 899 -> One (r522)
  | 668 -> One (r523)
  | 671 -> One (r524)
  | 677 -> One (r526)
  | 672 -> One (r527)
  | 676 -> One (r529)
  | 675 -> One (r530)
  | 674 -> One (r531)
  | 880 -> One (r532)
  | 879 -> One (r533)
  | 878 -> One (r534)
  | 680 -> One (r535)
  | 877 -> One (r536)
  | 871 -> One (r537)
  | 876 -> One (r539)
  | 875 -> One (r540)
  | 874 -> One (r541)
  | 873 -> One (r542)
  | 872 -> One (r543)
  | 869 -> One (r544)
  | 685 -> One (r545)
  | 684 -> One (r546)
  | 683 -> One (r547)
  | 688 -> One (r548)
  | 687 -> One (r549)
  | 690 -> One (r550)
  | 848 | 867 -> One (r551)
  | 847 | 866 -> One (r552)
  | 846 | 865 -> One (r553)
  | 691 | 709 -> One (r554)
  | 712 | 861 -> One (r555)
  | 711 | 860 -> One (r556)
  | 692 | 710 -> One (r557)
  | 859 -> One (r558)
  | 696 -> One (r559)
  | 697 -> One (r561)
  | 699 -> One (r562)
  | 701 -> One (r563)
  | 703 -> One (r564)
  | 708 -> One (r565)
  | 840 -> One (r566)
  | 718 -> One (r567)
  | 717 -> One (r568)
  | 770 -> One (r569)
  | 721 -> One (r570)
  | 724 -> One (r571)
  | 726 -> One (r572)
  | 728 -> One (r573)
  | 733 -> One (r574)
  | 735 -> One (r575)
  | 737 -> One (r576)
  | 739 -> One (r577)
  | 741 -> One (r578)
  | 743 -> One (r579)
  | 745 -> One (r580)
  | 747 -> One (r581)
  | 749 -> One (r582)
  | 751 -> One (r583)
  | 753 -> One (r584)
  | 755 -> One (r585)
  | 757 -> One (r586)
  | 759 -> One (r587)
  | 761 -> One (r588)
  | 763 -> One (r589)
  | 765 -> One (r590)
  | 767 -> One (r591)
  | 769 -> One (r592)
  | 772 -> One (r593)
  | 774 -> One (r594)
  | 838 -> One (r595)
  | 837 -> One (r596)
  | 779 -> One (r597)
  | 782 -> One (r598)
  | 791 -> One (r600)
  | 789 -> One (r601)
  | 788 -> One (r602)
  | 787 -> One (r603)
  | 786 -> One (r604)
  | 794 -> One (r605)
  | 801 -> One (r607)
  | 800 -> One (r608)
  | 811 -> One (r610)
  | 815 -> One (r612)
  | 797 -> One (r613)
  | 796 -> One (r614)
  | 793 -> One (r615)
  | 799 -> One (r616)
  | 803 -> One (r617)
  | 805 -> One (r618)
  | 804 | 816 -> One (r619)
  | 807 -> One (r620)
  | 810 -> One (r621)
  | 809 -> One (r622)
  | 814 -> One (r623)
  | 822 -> One (r624)
  | 821 -> One (r625)
  | 820 -> One (r626)
  | 819 -> One (r627)
  | 818 -> One (r628)
  | 827 -> One (r629)
  | 826 -> One (r630)
  | 825 -> One (r631)
  | 824 -> One (r632)
  | 833 -> One (r633)
  | 832 -> One (r634)
  | 831 -> One (r635)
  | 843 | 864 -> One (r636)
  | 842 | 863 -> One (r637)
  | 841 | 862 -> One (r638)
  | 845 -> One (r639)
  | 850 -> One (r640)
  | 853 -> One (r641)
  | 898 -> One (r642)
  | 884 -> One (r643)
  | 883 -> One (r644)
  | 897 -> One (r645)
  | 890 -> One (r646)
  | 889 -> One (r647)
  | 894 -> One (r648)
  | 893 -> One (r649)
  | 892 -> One (r650)
  | 896 -> One (r651)
  | 908 -> One (r653)
  | 914 -> One (r654)
  | 917 -> One (r655)
  | 919 -> One (r656)
  | 924 -> One (r657)
  | 923 -> One (r658)
  | 927 -> One (r659)
  | 930 -> One (r660)
  | 929 -> One (r661)
  | 937 -> One (r663)
  | 934 -> One (r664)
  | 933 -> One (r666)
  | 932 -> One (r667)
  | 949 -> One (r668)
  | 948 -> One (r669)
  | 941 -> One (r670)
  | 940 -> One (r671)
  | 947 -> One (r672)
  | 957 -> One (r673)
  | 962 -> One (r674)
  | 978 -> One (r675)
  | 977 -> One (r676)
  | 976 -> One (r677)
  | 969 -> One (r678)
  | 968 -> One (r679)
  | 975 -> One (r680)
  | 974 -> One (r681)
  | 973 -> One (r682)
  | 980 -> One (r683)
  | 990 -> One (r684)
  | 989 -> One (r685)
  | 993 -> One (r686)
  | 992 -> One (r687)
  | 996 -> One (r688)
  | 995 -> One (r689)
  | 1001 -> One (r690)
  | 1000 -> One (r691)
  | 1012 -> One (r692)
  | 1011 -> One (r693)
  | 1317 -> One (r694)
  | 1316 -> One (r695)
  | 1021 -> One (r696)
  | 1023 -> One (r697)
  | 1025 -> One (r698)
  | 1315 -> One (r699)
  | 1314 -> One (r700)
  | 1027 -> One (r701)
  | 1031 -> One (r702)
  | 1030 -> One (r703)
  | 1029 -> One (r704)
  | 1038 -> One (r705)
  | 1041 -> One (r707)
  | 1040 -> One (r708)
  | 1037 -> One (r709)
  | 1036 -> One (r710)
  | 1035 -> One (r711)
  | 1034 -> One (r712)
  | 1033 -> One (r713)
  | 1050 -> One (r714)
  | 1049 -> One (r715)
  | 1048 -> One (r716)
  | 1045 -> One (r717)
  | 1053 -> One (r720)
  | 1052 -> One (r721)
  | 1051 -> One (r722)
  | 1061 -> One (r723)
  | 1060 -> One (r724)
  | 1059 -> One (r725)
  | 1224 -> One (r726)
  | 1058 -> One (r727)
  | 1057 -> One (r728)
  | 1056 -> One (r729)
  | 1112 -> One (r730)
  | 1121 -> One (r732)
  | 1136 -> One (r734)
  | 1135 -> One (r735)
  | 1072 -> One (r736)
  | 1071 -> One (r737)
  | 1070 -> One (r738)
  | 1066 -> One (r739)
  | 1064 -> One (r740)
  | 1063 -> One (r741)
  | 1069 -> One (r742)
  | 1068 -> One (r743)
  | 1080 -> One (r744)
  | 1079 -> One (r745)
  | 1078 -> One (r747)
  | 1077 -> One (r748)
  | 1076 -> One (r749)
  | 1075 -> One (r750)
  | 1096 -> One (r751)
  | 1095 -> One (r752)
  | 1094 -> One (r753)
  | 1093 -> One (r755)
  | 1092 -> One (r756)
  | 1086 -> One (r757)
  | 1085 -> One (r758)
  | 1084 -> One (r759)
  | 1083 -> One (r760)
  | 1091 -> One (r761)
  | 1090 -> One (r762)
  | 1089 -> One (r763)
  | 1088 -> One (r764)
  | 1110 -> One (r765)
  | 1109 -> One (r767)
  | 1108 -> One (r768)
  | 1104 -> One (r769)
  | 1103 -> One (r770)
  | 1102 -> One (r771)
  | 1107 -> One (r772)
  | 1106 -> One (r773)
  | 1123 -> One (r774)
  | 1122 -> One (r775)
  | 1119 -> One (r776)
  | 1115 -> One (r777)
  | 1114 -> One (r778)
  | 1118 -> One (r779)
  | 1117 -> One (r780)
  | 1128 -> One (r781)
  | 1127 -> One (r782)
  | 1126 -> One (r783)
  | 1130 -> One (r785)
  | 1129 -> One (r786)
  | 1132 -> One (r787)
  | 1163 -> One (r788)
  | 1168 -> One (r790)
  | 1167 -> One (r791)
  | 1141 -> One (r792)
  | 1140 -> One (r793)
  | 1139 -> One (r794)
  | 1138 -> One (r795)
  | 1166 -> One (r796)
  | 1146 -> One (r797)
  | 1145 -> One (r798)
  | 1144 -> One (r799)
  | 1143 -> One (r800)
  | 1165 -> One (r801)
  | 1149 -> One (r802)
  | 1148 -> One (r803)
  | 1164 -> One (r804)
  | 1153 -> One (r805)
  | 1152 -> One (r806)
  | 1162 -> One (r807)
  | 1157 -> One (r808)
  | 1177 -> One (r809)
  | 1176 -> One (r810)
  | 1175 -> One (r811)
  | 1174 -> One (r812)
  | 1173 -> One (r813)
  | 1172 -> One (r814)
  | 1181 -> One (r815)
  | 1191 -> One (r816)
  | 1190 -> One (r817)
  | 1189 -> One (r818)
  | 1188 -> One (r819)
  | 1187 -> One (r820)
  | 1200 -> One (r821)
  | 1210 -> One (r822)
  | 1209 -> One (r823)
  | 1208 -> One (r824)
  | 1207 -> One (r825)
  | 1206 -> One (r826)
  | 1205 -> One (r827)
  | 1204 -> One (r828)
  | 1221 -> One (r829)
  | 1220 -> One (r830)
  | 1219 -> One (r831)
  | 1218 -> One (r832)
  | 1217 -> One (r833)
  | 1216 -> One (r834)
  | 1215 -> One (r835)
  | 1251 -> One (r836)
  | 1249 -> One (r838)
  | 1262 -> One (r840)
  | 1231 -> One (r841)
  | 1270 -> One (r843)
  | 1269 -> One (r844)
  | 1230 -> One (r845)
  | 1229 -> One (r846)
  | 1228 -> One (r847)
  | 1235 -> One (r848)
  | 1234 -> One (r849)
  | 1233 -> One (r850)
  | 1258 -> One (r851)
  | 1257 -> One (r852)
  | 1241 -> One (r853)
  | 1240 -> One (r854)
  | 1239 -> One (r855)
  | 1254 -> One (r856)
  | 1245 -> One (r857)
  | 1244 -> One (r858)
  | 1256 -> One (r860)
  | 1243 -> One (r861)
  | 1252 -> One (r862)
  | 1247 -> One (r863)
  | 1261 -> One (r864)
  | 1260 -> One (r865)
  | 1266 -> One (r866)
  | 1265 -> One (r867)
  | 1264 -> One (r868)
  | 1268 -> One (r869)
  | 1308 -> One (r870)
  | 1276 -> One (r871)
  | 1286 -> One (r872)
  | 1285 -> One (r873)
  | 1284 -> One (r874)
  | 1283 -> One (r875)
  | 1295 -> One (r876)
  | 1305 -> One (r877)
  | 1304 -> One (r878)
  | 1303 -> One (r879)
  | 1302 -> One (r880)
  | 1301 -> One (r881)
  | 1300 -> One (r882)
  | 1311 -> One (r883)
  | 1310 -> One (r884)
  | 1327 -> One (r885)
  | 1326 -> One (r886)
  | 1325 -> One (r887)
  | 1339 -> One (r888)
  | 1338 -> One (r889)
  | 1346 -> One (r890)
  | 1345 -> One (r891)
  | 1344 -> One (r892)
  | 1343 -> One (r893)
  | 1342 -> One (r894)
  | 1341 -> One (r895)
  | 1348 -> One (r896)
  | 1350 -> One (r897)
  | 1358 -> One (r898)
  | 1357 -> One (r899)
  | 1386 -> One (r900)
  | 1375 -> One (r901)
  | 1374 -> One (r902)
  | 1373 -> One (r903)
  | 1360 -> One (r904)
  | 1388 -> One (r906)
  | 1387 -> One (r907)
  | 1366 -> One (r908)
  | 1365 -> One (r909)
  | 1364 -> One (r910)
  | 1363 -> One (r911)
  | 1362 -> One (r912)
  | 1372 -> One (r913)
  | 1371 -> One (r914)
  | 1370 -> One (r915)
  | 1369 -> One (r916)
  | 1385 -> One (r917)
  | 1384 -> One (r918)
  | 1383 -> One (r919)
  | 1382 -> One (r920)
  | 1381 -> One (r921)
  | 1380 -> One (r922)
  | 1379 -> One (r923)
  | 1378 -> One (r924)
  | 1392 -> One (r925)
  | 1391 -> One (r926)
  | 1390 -> One (r927)
  | 1400 -> One (r928)
  | 1399 -> One (r929)
  | 1396 -> One (r930)
  | 1395 -> One (r931)
  | 1394 -> One (r932)
  | 1398 -> One (r933)
  | 1404 -> One (r934)
  | 1403 -> One (r935)
  | 1402 -> One (r936)
  | 1406 -> One (r937)
  | 1412 -> One (r938)
  | 1411 -> One (r939)
  | 1434 -> One (r940)
  | 1430 -> One (r942)
  | 1429 -> One (r943)
  | 1423 -> One (r944)
  | 1422 -> One (r945)
  | 1428 -> One (r946)
  | 1427 -> One (r947)
  | 1426 -> One (r948)
  | 1438 -> One (r950)
  | 1437 -> One (r951)
  | 1436 -> One (r952)
  | 1457 -> One (r953)
  | 1475 -> One (r954)
  | 1479 -> One (r955)
  | 1483 -> One (r956)
  | 575 -> Select (function
    | -1 | -1 -> [R 97]
    | _ -> r446)
  | 512 -> Select (function
    | -1 -> S (T T_TYPE) :: r361
    | _ -> R 181 :: r356)
  | 1042 -> Select (function
    | -1 -> r725
    | _ -> R 181 :: r719)
  | 553 -> Select (function
    | -1 | -1 -> [R 673]
    | _ -> r100)
  | 1324 -> Select (function
    | -1 -> S (T T_UIDENT) :: r887
    | _ -> r356)
  | 560 -> Select (function
    | -1 | -1 -> S (T T_RBRACKET) :: r274
    | _ -> Sub (r423) :: r426)
  | 554 -> Select (function
    | -1 | -1 | 59 | 159 | 333 | 494 | 506 | 1021 | 1027 | 1273 -> r415
    | _ -> S (T T_OPEN) :: r421)
  | 156 -> Select (function
    | 816 -> r63
    | _ -> Sub (r61) :: r109)
  | 1424 -> Select (function
    | 1429 -> r379
    | _ -> Sub (r134) :: r949)
  | 157 -> Select (function
    | 816 -> r62
    | _ -> r109)
  | 1425 -> Select (function
    | 1423 -> r949
    | _ -> r378)
  | 1044 -> Select (function
    | -1 -> r723
    | _ -> r718)
  | 1043 -> Select (function
    | -1 -> r724
    | _ -> r719)
  | _ -> raise Not_found
