open Parser_raw

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


let default_value (type a) : a MenhirInterpreter.symbol -> a = function
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
  | MenhirInterpreter.T MenhirInterpreter.T_NATIVEINT -> 0n
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
  | MenhirInterpreter.T MenhirInterpreter.T_LABEL -> "_"
  | MenhirInterpreter.T MenhirInterpreter.T_JSNEW -> ()
  | MenhirInterpreter.T MenhirInterpreter.T_INT64 -> 0L
  | MenhirInterpreter.T MenhirInterpreter.T_INT32 -> 0l
  | MenhirInterpreter.T MenhirInterpreter.T_INT -> 0
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
  | MenhirInterpreter.T MenhirInterpreter.T_FLOAT -> "0."
  | MenhirInterpreter.T MenhirInterpreter.T_FINALLY_LWT -> ()
  | MenhirInterpreter.T MenhirInterpreter.T_FALSE -> ()
  | MenhirInterpreter.T MenhirInterpreter.T_EXTERNAL -> ()
  | MenhirInterpreter.T MenhirInterpreter.T_EXITPOINT -> ()
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
  | MenhirInterpreter.N MenhirInterpreter.N_with_type_binder -> Public
  | MenhirInterpreter.N MenhirInterpreter.N_with_extensions -> []
  | MenhirInterpreter.N MenhirInterpreter.N_with_constraints -> []
  | MenhirInterpreter.N MenhirInterpreter.N_with_constraint -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_virtual_flag -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_value_type -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_value -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_val_longident -> default_longident
  | MenhirInterpreter.N MenhirInterpreter.N_val_ident -> ""
  | MenhirInterpreter.N MenhirInterpreter.N_typevar_list -> []
  | MenhirInterpreter.N MenhirInterpreter.N_type_variance -> Invariant
  | MenhirInterpreter.N MenhirInterpreter.N_type_variable -> default_type
  | MenhirInterpreter.N MenhirInterpreter.N_type_parameters -> []
  | MenhirInterpreter.N MenhirInterpreter.N_type_parameter_list -> []
  | MenhirInterpreter.N MenhirInterpreter.N_type_parameter -> default_type, Invariant
  | MenhirInterpreter.N MenhirInterpreter.N_type_longident -> default_longident
  | MenhirInterpreter.N MenhirInterpreter.N_type_kind -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_type_declarations -> []
  | MenhirInterpreter.N MenhirInterpreter.N_type_declaration -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_type_constraint -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_toplevel_directives -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_tag_field -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_subtractive -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_structure_tail -> []
  | MenhirInterpreter.N MenhirInterpreter.N_structure_item -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_structure_head -> []
  | MenhirInterpreter.N MenhirInterpreter.N_structure -> []
  | MenhirInterpreter.N MenhirInterpreter.N_strict_binding -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_str_type_extension -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_str_extension_constructors -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_str_exception_declaration -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_single_attr_id -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_simple_pattern_not_ident -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_simple_pattern -> default_pattern
  | MenhirInterpreter.N MenhirInterpreter.N_simple_labeled_expr_list -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_simple_expr -> default_expr
  | MenhirInterpreter.N MenhirInterpreter.N_simple_core_type_or_tuple_no_attr -> default_type
  | MenhirInterpreter.N MenhirInterpreter.N_simple_core_type_or_tuple -> default_type
  | MenhirInterpreter.N MenhirInterpreter.N_simple_core_type_no_attr -> default_type
  | MenhirInterpreter.N MenhirInterpreter.N_simple_core_type2 -> default_type
  | MenhirInterpreter.N MenhirInterpreter.N_simple_core_type -> default_type
  | MenhirInterpreter.N MenhirInterpreter.N_signed_constant -> Const_int 0
  | MenhirInterpreter.N MenhirInterpreter.N_signature_item -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_signature -> []
  | MenhirInterpreter.N MenhirInterpreter.N_sig_type_extension -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_sig_extension_constructors -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_sig_exception_declaration -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_seq_expr -> default_expr
  | MenhirInterpreter.N MenhirInterpreter.N_row_field_list -> []
  | MenhirInterpreter.N MenhirInterpreter.N_row_field -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_record_expr -> None, []
  | MenhirInterpreter.N MenhirInterpreter.N_rec_module_declarations -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_rec_flag -> Nonrecursive
  | MenhirInterpreter.N MenhirInterpreter.N_private_virtual_flags -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_private_flag -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_primitive_declaration -> []
  | MenhirInterpreter.N MenhirInterpreter.N_post_item_attributes -> []
  | MenhirInterpreter.N MenhirInterpreter.N_post_item_attribute -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_poly_type_no_attr -> default_type
  | MenhirInterpreter.N MenhirInterpreter.N_poly_type -> default_type
  | MenhirInterpreter.N MenhirInterpreter.N_payload -> Parsetree.PStr []
  | MenhirInterpreter.N MenhirInterpreter.N_pattern_var -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_pattern_semi_list -> []
  | MenhirInterpreter.N MenhirInterpreter.N_pattern_comma_list -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_pattern -> default_pattern
  | MenhirInterpreter.N MenhirInterpreter.N_parse_expression -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_parent_binder -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_package_type_cstrs -> []
  | MenhirInterpreter.N MenhirInterpreter.N_package_type_cstr -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_package_type -> default_longident_loc, []
  | MenhirInterpreter.N MenhirInterpreter.N_override_flag -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_optional_type_variable -> default_type
  | MenhirInterpreter.N MenhirInterpreter.N_optional_type_parameters -> []
  | MenhirInterpreter.N MenhirInterpreter.N_optional_type_parameter_list -> []
  | MenhirInterpreter.N MenhirInterpreter.N_optional_type_parameter -> default_type, Invariant
  | MenhirInterpreter.N MenhirInterpreter.N_option_STRING_ -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_opt_semi -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_opt_default -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_opt_bar -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_opt_ampersand -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_operator -> ""
  | MenhirInterpreter.N MenhirInterpreter.N_open_statement -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_newtype -> ""
  | MenhirInterpreter.N MenhirInterpreter.N_name_tag_list -> []
  | MenhirInterpreter.N MenhirInterpreter.N_name_tag -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_mutable_flag -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_mty_longident -> default_longident
  | MenhirInterpreter.N MenhirInterpreter.N_module_type -> default_module_type
  | MenhirInterpreter.N MenhirInterpreter.N_module_rec_declaration -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_module_expr -> default_module_expr
  | MenhirInterpreter.N MenhirInterpreter.N_module_declaration -> default_module_type
  | MenhirInterpreter.N MenhirInterpreter.N_module_bindings -> []
  | MenhirInterpreter.N MenhirInterpreter.N_module_binding_body -> default_module_expr
  | MenhirInterpreter.N MenhirInterpreter.N_module_binding -> default_module_bind
  | MenhirInterpreter.N MenhirInterpreter.N_mod_longident -> default_longident
  | MenhirInterpreter.N MenhirInterpreter.N_mod_ext_longident -> default_longident
  | MenhirInterpreter.N MenhirInterpreter.N_method_ -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_meth_list -> [], Open
  | MenhirInterpreter.N MenhirInterpreter.N_match_cases -> []
  | MenhirInterpreter.N MenhirInterpreter.N_match_case -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_lident_list -> []
  | MenhirInterpreter.N MenhirInterpreter.N_let_pattern -> default_pattern
  | MenhirInterpreter.N MenhirInterpreter.N_let_operator -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_let_bindings_no_attrs -> []
  | MenhirInterpreter.N MenhirInterpreter.N_let_bindings -> []
  | MenhirInterpreter.N MenhirInterpreter.N_let_binding_ -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_let_binding -> default_value_bind
  | MenhirInterpreter.N MenhirInterpreter.N_lbl_pattern_list -> [], Closed
  | MenhirInterpreter.N MenhirInterpreter.N_lbl_pattern -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_lbl_expr_list -> []
  | MenhirInterpreter.N MenhirInterpreter.N_lbl_expr -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_labeled_simple_pattern -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_labeled_simple_expr -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_label_var -> "", default_pattern
  | MenhirInterpreter.N MenhirInterpreter.N_label_longident -> default_longident
  | MenhirInterpreter.N MenhirInterpreter.N_label_let_pattern -> "", default_pattern
  | MenhirInterpreter.N MenhirInterpreter.N_label_ident -> "", default_expr
  | MenhirInterpreter.N MenhirInterpreter.N_label_expr -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_label_declarations -> []
  | MenhirInterpreter.N MenhirInterpreter.N_label_declaration -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_label -> ""
  | MenhirInterpreter.N MenhirInterpreter.N_item_extension -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_interface -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_implementation -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_ident -> ""
  | MenhirInterpreter.N MenhirInterpreter.N_generalized_constructor_arguments -> ([],None)
  | MenhirInterpreter.N MenhirInterpreter.N_functor_args -> []
  | MenhirInterpreter.N MenhirInterpreter.N_functor_arg_name -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_functor_arg -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_fun_def -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_fun_binding -> default_expr
  | MenhirInterpreter.N MenhirInterpreter.N_floating_attribute -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_field_expr_list -> []
  | MenhirInterpreter.N MenhirInterpreter.N_field -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_rebind -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_declaration -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_extension -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_ext_attributes -> (None, [])
  | MenhirInterpreter.N MenhirInterpreter.N_expr_semi_list -> []
  | MenhirInterpreter.N MenhirInterpreter.N_expr_open -> Fresh, default_longident_loc, (None, [])
  | MenhirInterpreter.N MenhirInterpreter.N_expr_comma_opt_list -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_expr_comma_list -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_expr -> default_expr
  | MenhirInterpreter.N MenhirInterpreter.N_dummy -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_direction_flag -> Upto
  | MenhirInterpreter.N MenhirInterpreter.N_core_type_no_attr -> default_type
  | MenhirInterpreter.N MenhirInterpreter.N_core_type_list_no_attr -> []
  | MenhirInterpreter.N MenhirInterpreter.N_core_type_list -> []
  | MenhirInterpreter.N MenhirInterpreter.N_core_type_comma_list -> []
  | MenhirInterpreter.N MenhirInterpreter.N_core_type2 -> default_type
  | MenhirInterpreter.N MenhirInterpreter.N_core_type -> default_type
  | MenhirInterpreter.N MenhirInterpreter.N_constructor_declarations -> []
  | MenhirInterpreter.N MenhirInterpreter.N_constructor_declaration -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_constraints -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_constrain_field -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_constrain -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_constr_longident -> default_longident
  | MenhirInterpreter.N MenhirInterpreter.N_constr_ident -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_constant -> Const_int 0
  | MenhirInterpreter.N MenhirInterpreter.N_clty_longident -> default_longident
  | MenhirInterpreter.N MenhirInterpreter.N_class_type_parameters -> []
  | MenhirInterpreter.N MenhirInterpreter.N_class_type_declarations -> []
  | MenhirInterpreter.N MenhirInterpreter.N_class_type_declaration -> []
  | MenhirInterpreter.N MenhirInterpreter.N_class_type -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_class_structure -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_class_simple_expr -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_class_signature -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_class_sig_fields -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_class_sig_field -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_class_sig_body -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_class_self_type -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_class_self_pattern -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_class_longident -> default_longident
  | MenhirInterpreter.N MenhirInterpreter.N_class_fun_def -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_class_fun_binding -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_class_fields -> []
  | MenhirInterpreter.N MenhirInterpreter.N_class_field -> []
  | MenhirInterpreter.N MenhirInterpreter.N_class_expr -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_class_descriptions -> []
  | MenhirInterpreter.N MenhirInterpreter.N_class_description -> []
  | MenhirInterpreter.N MenhirInterpreter.N_class_declarations -> []
  | MenhirInterpreter.N MenhirInterpreter.N_class_declaration -> []
  | MenhirInterpreter.N MenhirInterpreter.N_attributes -> []
  | MenhirInterpreter.N MenhirInterpreter.N_attribute -> raise Not_found
  | MenhirInterpreter.N MenhirInterpreter.N_attr_id -> Location.mknoloc ""
  | MenhirInterpreter.N MenhirInterpreter.N_amper_type_list -> []
  | MenhirInterpreter.N MenhirInterpreter.N_additive -> raise Not_found

open MenhirInterpreter

          type action =
  | Shift  : 'a symbol -> action
  | Reduce : int -> action
  | Sub    : action list -> action
  | Pop    : action

type decision =
  | Action of int * action
  | Parent of (int -> int * action)


let rec derive_parse_expression' = 
  Sub [Reduce 3]
and derive_interface' = 
  Sub [Reduce 2]
and derive_implementation' = 
  Sub [Reduce 1]
and derive_dummy' = 
  Sub [Reduce 0]
and derive_with_type_binder =  Shift (N N_with_type_binder)
and derive_with_extensions =  Shift (N N_with_extensions)
and derive_with_constraints =  Shift (N N_with_constraints)
and derive_with_constraint = 
  Sub [Shift (T T_MODULE); derive_mod_longident; Shift (T T_EQUAL); derive_mod_ext_longident; Reduce 724]
and derive_virtual_flag = 
  Sub [Reduce 720]
and derive_value_type = 
  Sub [derive_label; Shift (T T_COLON); derive_core_type; Reduce 719]
and derive_value = 
  Sub [derive_override_flag; derive_mutable_flag; derive_label; Shift (T T_EQUAL); derive_seq_expr; Reduce 715]
and derive_val_longident =  Shift (N N_val_longident)
and derive_val_ident =  Shift (N N_val_ident)
and derive_typevar_list =  Shift (N N_typevar_list)
and derive_type_variance =  Shift (N N_type_variance)
and derive_type_variable =  Shift (N N_type_variable)
and derive_type_parameters =  Shift (N N_type_parameters)
and derive_type_parameter_list =  Shift (N N_type_parameter_list)
and derive_type_parameter =  Shift (N N_type_parameter)
and derive_type_longident =  Shift (N N_type_longident)
and derive_type_kind = 
  Sub [Reduce 684]
and derive_type_declarations =  Shift (N N_type_declarations)
and derive_type_declaration = 
  Sub [derive_optional_type_parameters; Shift (T T_LIDENT); derive_type_kind; derive_constraints; derive_post_item_attributes; Reduce 681]
and derive_type_constraint = 
  Sub [Shift (T T_COLONGREATER); derive_core_type; Reduce 680]
and derive_toplevel_directives = 
  Sub [Reduce 671]
and derive_tag_field = 
  Sub [derive_name_tag; derive_attributes; Reduce 670]
and derive_subtractive = 
  Sub [Shift (T T_MINUSDOT); Reduce 668]
and derive_structure_tail =  Shift (N N_structure_tail)
and derive_structure_item = 
  Sub [Shift (T T_CLASS); derive_class_declarations; Reduce 659]
and derive_structure_head =  Shift (N N_structure_head)
and derive_structure =  Shift (N N_structure)
and derive_strict_binding = 
  Sub [Shift (T T_EQUAL); derive_seq_expr; Reduce 630]
and derive_str_type_extension = 
  Sub [derive_optional_type_parameters; derive_type_longident; Shift (T T_PLUSEQ); derive_private_flag; derive_opt_bar; derive_str_extension_constructors; derive_post_item_attributes; Reduce 629]
and derive_str_extension_constructors = 
  Sub [derive_extension_constructor_declaration; Reduce 625]
and derive_str_exception_declaration = 
  Sub [derive_extension_constructor_declaration; derive_post_item_attributes; Reduce 623]
and derive_single_attr_id = 
  Sub [Shift (T T_WITH); Reduce 622]
and derive_simple_pattern_not_ident = 
  Sub [derive_constr_longident; Reduce 561]
and derive_simple_pattern =  Shift (N N_simple_pattern)
and derive_simple_labeled_expr_list = 
  Sub [derive_labeled_simple_expr; Reduce 554]
and derive_simple_expr =  Shift (N N_simple_expr)
and derive_simple_core_type_or_tuple_no_attr =  Shift (N N_simple_core_type_or_tuple_no_attr)
and derive_simple_core_type_or_tuple =  Shift (N N_simple_core_type_or_tuple)
and derive_simple_core_type_no_attr =  Shift (N N_simple_core_type_no_attr)
and derive_simple_core_type2 =  Shift (N N_simple_core_type2)
and derive_simple_core_type =  Shift (N N_simple_core_type)
and derive_signed_constant =  Shift (N N_signed_constant)
and derive_signature_item = 
  Sub [Shift (T T_CLASS); derive_class_descriptions; Reduce 471]
and derive_signature =  Shift (N N_signature)
and derive_sig_type_extension = 
  Sub [derive_optional_type_parameters; derive_type_longident; Shift (T T_PLUSEQ); derive_private_flag; derive_opt_bar; derive_sig_extension_constructors; derive_post_item_attributes; Reduce 451]
and derive_sig_extension_constructors = 
  Sub [derive_extension_constructor_declaration; Reduce 449]
and derive_sig_exception_declaration = 
  Sub [derive_extension_constructor_declaration; derive_post_item_attributes; Reduce 448]
and derive_seq_expr =  Shift (N N_seq_expr)
and derive_row_field_list =  Shift (N N_row_field_list)
and derive_row_field = 
  Sub [derive_simple_core_type; Reduce 442]
and derive_record_expr =  Shift (N N_record_expr)
and derive_rec_module_declarations = 
  Sub [derive_module_rec_declaration; Reduce 437]
and derive_rec_flag =  Shift (N N_rec_flag)
and derive_private_virtual_flags = 
  Sub [Reduce 430]
and derive_private_flag = 
  Sub [Reduce 428]
and derive_primitive_declaration =  Shift (N N_primitive_declaration)
and derive_post_item_attributes =  Shift (N N_post_item_attributes)
and derive_post_item_attribute = 
  Sub [Shift (T T_LBRACKETATAT); derive_attr_id; derive_payload; Shift (T T_RBRACKET); Reduce 423]
and derive_poly_type_no_attr =  Shift (N N_poly_type_no_attr)
and derive_poly_type =  Shift (N N_poly_type)
and derive_payload =  Shift (N N_payload)
and derive_pattern_var = 
  Sub [Shift (T T_UNDERSCORE); Reduce 414]
and derive_pattern_semi_list =  Shift (N N_pattern_semi_list)
and derive_pattern_comma_list = 
  Sub [derive_pattern; Shift (T T_COMMA); derive_pattern; Reduce 410]
and derive_pattern =  Shift (N N_pattern)
and derive_parse_expression = 
  Sub [derive_seq_expr; Shift (T T_EOF); Reduce 397]
and derive_parent_binder = 
  Sub [Reduce 396]
and derive_package_type_cstrs =  Shift (N N_package_type_cstrs)
and derive_package_type_cstr = 
  Sub [Shift (T T_TYPE); derive_label_longident; Shift (T T_EQUAL); derive_core_type; Reduce 392]
and derive_package_type =  Shift (N N_package_type)
and derive_override_flag = 
  Sub [Reduce 388]
and derive_optional_type_variable =  Shift (N N_optional_type_variable)
and derive_optional_type_parameters =  Shift (N N_optional_type_parameters)
and derive_optional_type_parameter_list =  Shift (N N_optional_type_parameter_list)
and derive_optional_type_parameter =  Shift (N N_optional_type_parameter)
and derive_option_STRING_ = 
  Sub [Reduce 377]
and derive_opt_semi = 
  Sub [Reduce 375]
and derive_opt_default = 
  Sub [Reduce 373]
and derive_opt_bar = 
  Sub [Reduce 371]
and derive_opt_ampersand = 
  Sub [Reduce 370]
and derive_operator =  Shift (N N_operator)
and derive_open_statement = 
  Sub [Shift (T T_OPEN); derive_override_flag; derive_mod_longident; derive_post_item_attributes; Reduce 343]
and derive_newtype =  Shift (N N_newtype)
and derive_name_tag_list =  Shift (N N_name_tag_list)
and derive_name_tag = 
  Sub [Shift (T T_BACKQUOTE); derive_ident; Reduce 339]
and derive_mutable_flag = 
  Sub [Reduce 337]
and derive_mty_longident =  Shift (N N_mty_longident)
and derive_module_type =  Shift (N N_module_type)
and derive_module_rec_declaration = 
  Sub [Shift (T T_UIDENT); Shift (T T_COLON); derive_module_type; derive_post_item_attributes; Reduce 326]
and derive_module_expr =  Shift (N N_module_expr)
and derive_module_declaration =  Shift (N N_module_declaration)
and derive_module_bindings =  Shift (N N_module_bindings)
and derive_module_binding_body =  Shift (N N_module_binding_body)
and derive_module_binding =  Shift (N N_module_binding)
and derive_mod_longident =  Shift (N N_mod_longident)
and derive_mod_ext_longident =  Shift (N N_mod_ext_longident)
and derive_method_ = 
  Sub [derive_override_flag; derive_private_flag; derive_label; derive_strict_binding; Reduce 296]
and derive_meth_list =  Shift (N N_meth_list)
and derive_match_cases =  Shift (N N_match_cases)
and derive_match_case = 
  Sub [derive_pattern; Shift (T T_MINUSGREATER); derive_seq_expr; Reduce 287]
and derive_lident_list =  Shift (N N_lident_list)
and derive_let_pattern =  Shift (N N_let_pattern)
and derive_let_operator = 
  Sub [Shift (T T_LETOP); Reduce 281]
and derive_let_bindings_no_attrs =  Shift (N N_let_bindings_no_attrs)
and derive_let_bindings =  Shift (N N_let_bindings)
and derive_let_binding_ = 
  Sub [derive_val_ident; derive_fun_binding; Reduce 273]
and derive_let_binding =  Shift (N N_let_binding)
and derive_lbl_pattern_list =  Shift (N N_lbl_pattern_list)
and derive_lbl_pattern = 
  Sub [derive_label_longident; Reduce 267]
and derive_lbl_expr_list =  Shift (N N_lbl_expr_list)
and derive_lbl_expr = 
  Sub [derive_label_longident; Reduce 262]
and derive_labeled_simple_pattern = 
  Sub [derive_simple_pattern; Reduce 260]
and derive_labeled_simple_expr = 
  Sub [derive_simple_expr; Reduce 251]
and derive_label_var =  Shift (N N_label_var)
and derive_label_longident =  Shift (N N_label_longident)
and derive_label_let_pattern =  Shift (N N_label_let_pattern)
and derive_label_ident =  Shift (N N_label_ident)
and derive_label_expr = 
  Sub [Shift (T T_QUESTION); derive_label_ident; Reduce 243]
and derive_label_declarations =  Shift (N N_label_declarations)
and derive_label_declaration = 
  Sub [derive_mutable_flag; derive_label; Shift (T T_COLON); derive_poly_type_no_attr; derive_attributes; Reduce 238]
and derive_label =  Shift (N N_label)
and derive_item_extension = 
  Sub [Shift (T T_LBRACKETPERCENTPERCENT); derive_attr_id; derive_payload; Shift (T T_RBRACKET); Reduce 236]
and derive_interface = 
  Sub [derive_signature; Shift (T T_EOF); Reduce 235]
and derive_implementation = 
  Sub [derive_structure; Shift (T T_EOF); Reduce 234]
and derive_ident =  Shift (N N_ident)
and derive_generalized_constructor_arguments =  Shift (N N_generalized_constructor_arguments)
and derive_functor_args =  Shift (N N_functor_args)
and derive_functor_arg_name = 
  Sub [Shift (T T_UNDERSCORE); Reduce 225]
and derive_functor_arg = 
  Sub [Shift (T T_LPAREN); Shift (T T_RPAREN); Reduce 222]
and derive_fun_def = 
  Sub [Shift (T T_MINUSGREATER); derive_seq_expr; Reduce 219]
and derive_fun_binding =  Shift (N N_fun_binding)
and derive_floating_attribute = 
  Sub [Shift (T T_LBRACKETATATAT); derive_attr_id; derive_payload; Shift (T T_RBRACKET); Reduce 216]
and derive_field_expr_list =  Shift (N N_field_expr_list)
and derive_field = 
  Sub [derive_label; Shift (T T_COLON); derive_poly_type_no_attr; derive_attributes; Reduce 213]
and derive_extension_constructor_rebind = 
  Sub [derive_constr_ident; Shift (T T_EQUAL); derive_constr_longident; derive_attributes; Reduce 212]
and derive_extension_constructor_declaration = 
  Sub [derive_constr_ident; derive_generalized_constructor_arguments; derive_attributes; Reduce 211]
and derive_extension = 
  Sub [Shift (T T_LBRACKETPERCENT); derive_attr_id; derive_payload; Shift (T T_RBRACKET); Reduce 210]
and derive_ext_attributes =  Shift (N N_ext_attributes)
and derive_expr_semi_list =  Shift (N N_expr_semi_list)
and derive_expr_open =  Shift (N N_expr_open)
and derive_expr_comma_opt_list = 
  Sub [derive_expr; Reduce 203]
and derive_expr_comma_list = 
  Sub [derive_expr; Shift (T T_COMMA); derive_expr; Reduce 201]
and derive_expr =  Shift (N N_expr)
and derive_dummy = 
  Sub [Shift (T T_OUNIT_BENCH_MODULE); Reduce 137]
and derive_direction_flag =  Shift (N N_direction_flag)
and derive_core_type_no_attr =  Shift (N N_core_type_no_attr)
and derive_core_type_list_no_attr =  Shift (N N_core_type_list_no_attr)
and derive_core_type_list =  Shift (N N_core_type_list)
and derive_core_type_comma_list =  Shift (N N_core_type_comma_list)
and derive_core_type2 =  Shift (N N_core_type2)
and derive_core_type =  Shift (N N_core_type)
and derive_constructor_declarations =  Shift (N N_constructor_declarations)
and derive_constructor_declaration = 
  Sub [derive_constr_ident; derive_generalized_constructor_arguments; derive_attributes; Reduce 99]
and derive_constraints = 
  Sub [Reduce 98]
and derive_constrain_field = 
  Sub [derive_core_type; Shift (T T_EQUAL); derive_core_type; Reduce 96]
and derive_constrain = 
  Sub [derive_core_type; Shift (T T_EQUAL); derive_core_type; Reduce 95]
and derive_constr_longident =  Shift (N N_constr_longident)
and derive_constr_ident = 
  Sub [Shift (T T_TRUE); Reduce 89]
and derive_constant =  Shift (N N_constant)
and derive_clty_longident =  Shift (N N_clty_longident)
and derive_class_type_parameters =  Shift (N N_class_type_parameters)
and derive_class_type_declarations =  Shift (N N_class_type_declarations)
and derive_class_type_declaration =  Shift (N N_class_type_declaration)
and derive_class_type = 
  Sub [derive_class_signature; Reduce 66]
and derive_class_structure = 
  Sub [derive_class_self_pattern; derive_class_fields; Reduce 65]
and derive_class_simple_expr = 
  Sub [derive_class_longident; Reduce 61]
and derive_class_signature = 
  Sub [derive_clty_longident; Reduce 56]
and derive_class_sig_fields = 
  Sub [Reduce 53]
and derive_class_sig_field = 
  Sub [Shift (T T_INHERIT); derive_class_signature; derive_post_item_attributes; Reduce 47]
and derive_class_sig_body = 
  Sub [derive_class_self_type; derive_class_sig_fields; Reduce 46]
and derive_class_self_type = 
  Sub [Reduce 45]
and derive_class_self_pattern = 
  Sub [Reduce 43]
and derive_class_longident =  Shift (N N_class_longident)
and derive_class_fun_def = 
  Sub [derive_labeled_simple_pattern; Shift (T T_MINUSGREATER); derive_class_expr; Reduce 37]
and derive_class_fun_binding = 
  Sub [Shift (T T_EQUAL); derive_class_expr; Reduce 34]
and derive_class_fields =  Shift (N N_class_fields)
and derive_class_field =  Shift (N N_class_field)
and derive_class_expr = 
  Sub [derive_class_simple_expr; Reduce 19]
and derive_class_descriptions =  Shift (N N_class_descriptions)
and derive_class_description =  Shift (N N_class_description)
and derive_class_declarations =  Shift (N N_class_declarations)
and derive_class_declaration =  Shift (N N_class_declaration)
and derive_attributes =  Shift (N N_attributes)
and derive_attribute = 
  Sub [Shift (T T_LBRACKETAT); derive_attr_id; derive_payload; Shift (T T_RBRACKET); Reduce 10]
and derive_attr_id =  Shift (N N_attr_id)
and derive_amper_type_list =  Shift (N N_amper_type_list)
and derive_additive = 
  Sub [Shift (T T_PLUSDOT); Reduce 5]

let decision = function
  | 987 -> Parent (function
     | 988 -> 2, derive_expr
     | 976 | 1016 | 1026 | 1010 | 1012 | 1027 | 1043 | 1046 | 1056 | 1068 | 1117 | 1076 | 1079 | 1091 | 1096 | 1098 | 1101 | 1104 | 1123 | 1124 | 1166 | 1168 | 1173 | 1183 | 1252 | 1276 | 1278 -> 0, Pop
     | _ -> raise Not_found)
  | 564 -> Parent (function
     | 944 -> 1, derive_functor_args
     | 448 | 452 | 563 | 707 | 709 | 1194 | 1198 | 1444 | 1452 | 1492 -> 0, Pop
     | _ -> raise Not_found)
  | 92 -> Parent (function
     | 93 -> 2, derive_mod_ext_longident
     | 91 | 158 | 749 | 752 | 869 | 917 -> 0, Pop
     | _ -> raise Not_found)
  | 898 -> Parent (function
     | 902 -> 1, Shift (T T_LIDENT)
     | 897 | 905 | 909 | 913 | 1354 | 1518 -> 0, Pop
     | _ -> raise Not_found)
  | 702 -> Parent (function
     | 768 -> 1, Shift (T T_RPAREN)
     | 701 | 764 -> 0, Pop
     | _ -> raise Not_found)
  | 717 -> Parent (function
     | 719 -> 1, derive_functor_args
     | 570 | 704 | 716 | 765 | 777 | 783 | 798 | 1196 | 1265 | 1474 -> 0, Pop
     | _ -> raise Not_found)
  | 716 -> Parent (function
     | 716 -> 1, derive_module_type
     | 570 | 704 | 719 | 765 | 777 | 783 | 798 | 1196 | 1265 | 1474 -> 0, Pop
     | _ -> raise Not_found)
  | 695 -> Parent (function
     | 695 -> 1, derive_signature
     | 571 | 932 | 1551 -> 0, Pop
     | _ -> raise Not_found)
  | 533 -> Parent (function
     | 533 -> 1, derive_simple_expr
     | 23 | 382 | 384 | 386 | 388 | 389 | 397 | 398 | 438 | 442 | 444 | 453 | 472 | 480 | 481 | 485 | 486 | 488 | 489 | 490 | 492 | 493 | 495 | 497 | 502 | 524 | 528 | 532 | 953 | 958 | 542 | 539 | 550 | 551 | 556 | 557 | 544 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1022 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1059 | 1067 | 1113 | 1116 | 1078 | 1082 | 1084 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1105 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1344 | 1345 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1456 | 1458 | 1462 | 1466 | 1555 -> 0, Pop
     | _ -> raise Not_found)
  | 529 -> Parent (function
     | 532 | 1128 | 1130 -> 1, derive_ext_attributes
     | 23 | 382 | 384 | 386 | 388 | 396 | 398 | 438 | 442 | 444 | 445 | 453 | 472 | 480 | 481 | 485 | 490 | 495 | 497 | 502 | 524 | 528 | 562 | 539 | 550 | 556 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1067 | 1113 | 1116 | 1078 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1122 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1458 | 1462 | 1466 | 1555 -> 0, Pop
     | _ -> raise Not_found)
  | 525 -> Parent (function
     | 528 | 1134 -> 1, derive_ext_attributes
     | 23 | 382 | 384 | 386 | 388 | 396 | 398 | 438 | 442 | 444 | 445 | 453 | 472 | 480 | 481 | 485 | 490 | 495 | 497 | 502 | 524 | 532 | 562 | 539 | 550 | 556 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1067 | 1113 | 1116 | 1078 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1122 | 1128 | 1130 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1458 | 1462 | 1466 | 1555 -> 0, Pop
     | _ -> raise Not_found)
  | 496 -> Parent (function
     | 497 | 1165 -> 1, derive_ext_attributes
     | 23 | 382 | 384 | 386 | 388 | 396 | 398 | 438 | 442 | 444 | 445 | 453 | 472 | 480 | 481 | 485 | 490 | 495 | 502 | 524 | 528 | 532 | 562 | 539 | 550 | 556 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1067 | 1113 | 1116 | 1078 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1458 | 1462 | 1466 | 1555 -> 0, Pop
     | _ -> raise Not_found)
  | 494 -> Parent (function
     | 495 -> 1, derive_ext_attributes
     | 23 | 382 | 384 | 386 | 388 | 389 | 396 | 397 | 398 | 438 | 442 | 444 | 445 | 453 | 472 | 480 | 481 | 485 | 486 | 488 | 489 | 490 | 492 | 493 | 497 | 502 | 524 | 528 | 532 | 533 | 562 | 953 | 958 | 542 | 539 | 550 | 551 | 556 | 557 | 544 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1022 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1059 | 1067 | 1113 | 1116 | 1078 | 1082 | 1084 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1105 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1344 | 1345 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1456 | 1458 | 1462 | 1466 | 1555 -> 0, Pop
     | _ -> raise Not_found)
  | 493 -> Parent (function
     | 493 -> 1, derive_simple_expr
     | 23 | 382 | 384 | 386 | 388 | 389 | 397 | 398 | 438 | 442 | 444 | 453 | 472 | 480 | 481 | 485 | 486 | 488 | 489 | 490 | 492 | 495 | 497 | 502 | 524 | 528 | 532 | 533 | 953 | 958 | 542 | 539 | 550 | 551 | 556 | 557 | 544 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1022 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1059 | 1067 | 1113 | 1116 | 1078 | 1082 | 1084 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1105 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1344 | 1345 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1456 | 1458 | 1462 | 1466 | 1555 -> 0, Pop
     | _ -> raise Not_found)
  | 490 -> Parent (function
     | 490 -> 1, derive_expr
     | 23 | 382 | 384 | 386 | 388 | 389 | 396 | 397 | 398 | 438 | 442 | 444 | 445 | 453 | 472 | 480 | 481 | 485 | 486 | 488 | 489 | 492 | 493 | 495 | 497 | 502 | 524 | 528 | 532 | 533 | 562 | 953 | 958 | 542 | 539 | 550 | 551 | 556 | 557 | 544 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1022 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1059 | 1067 | 1113 | 1116 | 1078 | 1082 | 1084 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1105 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1344 | 1345 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1456 | 1458 | 1462 | 1466 | 1555 -> 0, Pop
     | _ -> raise Not_found)
  | 489 -> Parent (function
     | 489 -> 1, derive_simple_expr
     | 23 | 382 | 384 | 386 | 388 | 389 | 396 | 397 | 398 | 438 | 442 | 444 | 445 | 453 | 472 | 480 | 481 | 485 | 486 | 488 | 490 | 492 | 493 | 495 | 497 | 502 | 524 | 528 | 532 | 533 | 562 | 953 | 958 | 542 | 539 | 550 | 551 | 556 | 557 | 544 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1022 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1059 | 1067 | 1113 | 1116 | 1078 | 1082 | 1084 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1105 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1344 | 1345 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1456 | 1458 | 1462 | 1466 | 1555 -> 0, Pop
     | _ -> raise Not_found)
  | 488 -> Parent (function
     | 488 | 1177 -> 1, derive_simple_expr
     | 23 | 382 | 384 | 386 | 388 | 389 | 396 | 397 | 398 | 438 | 442 | 444 | 445 | 453 | 472 | 480 | 481 | 485 | 486 | 489 | 490 | 492 | 493 | 495 | 497 | 502 | 524 | 528 | 532 | 533 | 562 | 953 | 958 | 542 | 539 | 550 | 551 | 556 | 557 | 544 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1022 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1059 | 1067 | 1113 | 1116 | 1078 | 1082 | 1084 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1105 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1344 | 1345 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1456 | 1458 | 1462 | 1466 | 1555 -> 0, Pop
     | _ -> raise Not_found)
  | 486 -> Parent (function
     | 486 -> 1, derive_record_expr
     | 23 | 382 | 384 | 386 | 388 | 389 | 396 | 397 | 398 | 438 | 442 | 444 | 445 | 453 | 472 | 480 | 481 | 485 | 488 | 489 | 490 | 492 | 493 | 495 | 497 | 502 | 524 | 528 | 532 | 533 | 562 | 953 | 958 | 542 | 539 | 550 | 551 | 556 | 557 | 544 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1022 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1059 | 1067 | 1113 | 1116 | 1078 | 1082 | 1084 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1105 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1344 | 1345 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1456 | 1458 | 1462 | 1466 | 1555 -> 0, Pop
     | _ -> raise Not_found)
  | 481 -> Parent (function
     | 481 -> 1, Shift (T T_RBRACKET)
     | 23 | 382 | 384 | 386 | 388 | 389 | 396 | 397 | 398 | 438 | 442 | 444 | 445 | 453 | 472 | 480 | 485 | 486 | 488 | 489 | 490 | 492 | 493 | 495 | 497 | 502 | 524 | 528 | 532 | 533 | 562 | 953 | 958 | 542 | 539 | 550 | 551 | 556 | 557 | 544 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1022 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1059 | 1067 | 1113 | 1116 | 1078 | 1082 | 1084 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1105 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1344 | 1345 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1456 | 1458 | 1462 | 1466 | 1555 -> 0, Pop
     | _ -> raise Not_found)
  | 474 -> Parent (function
     | 480 -> 1, Shift (T T_OPEN)
     | 382 | 384 | 386 | 388 | 396 | 398 | 438 | 442 | 444 | 445 | 453 | 472 | 481 | 485 | 490 | 495 | 497 | 502 | 524 | 528 | 532 | 562 | 539 | 550 | 556 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1067 | 1113 | 1116 | 1078 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1458 | 1462 | 1466 | 1555 -> 0, Pop
     | _ -> raise Not_found)
  | 475 -> Parent (function
     | 474 -> 2, derive_expr_open
     | 1488 -> 0, Pop
     | _ -> raise Not_found)
  | 452 -> Parent (function
     | 452 -> 1, derive_module_expr
     | 448 | 563 | 707 | 709 | 944 | 1194 | 1198 | 1444 | 1452 | 1492 -> 0, Pop
     | _ -> raise Not_found)
  | 445 -> Parent (function
     | 445 -> 1, Shift (T T_RPAREN)
     | 23 | 382 | 384 | 386 | 388 | 396 | 398 | 438 | 442 | 444 | 453 | 472 | 480 | 481 | 485 | 490 | 495 | 497 | 502 | 524 | 528 | 532 | 562 | 539 | 550 | 556 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1067 | 1113 | 1116 | 1078 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1458 | 1462 | 1466 | 1555 -> 0, Pop
     | _ -> raise Not_found)
  | 443 -> Parent (function
     | 444 -> 1, derive_ext_attributes
     | 23 | 382 | 384 | 386 | 388 | 396 | 398 | 438 | 442 | 445 | 453 | 472 | 480 | 481 | 485 | 490 | 495 | 497 | 502 | 524 | 528 | 532 | 562 | 539 | 550 | 556 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1067 | 1113 | 1116 | 1078 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1458 | 1462 | 1466 | 1555 -> 0, Pop
     | _ -> raise Not_found)
  | 441 -> Parent (function
     | 442 -> 1, derive_ext_attributes
     | 23 | 382 | 384 | 386 | 388 | 396 | 398 | 438 | 444 | 445 | 453 | 472 | 480 | 481 | 485 | 490 | 495 | 497 | 502 | 524 | 528 | 532 | 562 | 539 | 550 | 556 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1067 | 1113 | 1116 | 1078 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1458 | 1462 | 1466 | 1555 -> 0, Pop
     | _ -> raise Not_found)
  | 398 -> Parent (function
     | 398 -> 1, Shift (T T_BARRBRACKET)
     | 23 | 382 | 384 | 386 | 388 | 389 | 396 | 397 | 438 | 442 | 444 | 445 | 453 | 472 | 480 | 481 | 485 | 486 | 488 | 489 | 490 | 492 | 493 | 495 | 497 | 502 | 524 | 528 | 532 | 533 | 562 | 953 | 958 | 542 | 539 | 550 | 551 | 556 | 557 | 544 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1022 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1059 | 1067 | 1113 | 1116 | 1078 | 1082 | 1084 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1105 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1344 | 1345 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1456 | 1458 | 1462 | 1466 | 1555 -> 0, Pop
     | _ -> raise Not_found)
  | 387 -> Parent (function
     | 388 -> 1, derive_ext_attributes
     | 23 | 382 | 384 | 386 | 396 | 398 | 438 | 442 | 444 | 445 | 453 | 472 | 480 | 481 | 485 | 490 | 495 | 497 | 502 | 524 | 528 | 532 | 562 | 539 | 550 | 556 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1067 | 1113 | 1116 | 1078 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1458 | 1462 | 1466 | 1555 -> 0, Pop
     | _ -> raise Not_found)
  | 385 -> Parent (function
     | 386 -> 1, derive_ext_attributes
     | 23 | 382 | 384 | 388 | 396 | 398 | 438 | 442 | 444 | 445 | 453 | 472 | 480 | 481 | 485 | 490 | 495 | 497 | 502 | 524 | 528 | 532 | 562 | 539 | 550 | 556 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1067 | 1113 | 1116 | 1078 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1458 | 1462 | 1466 | 1555 -> 0, Pop
     | _ -> raise Not_found)
  | 383 -> Parent (function
     | 384 | 1387 -> 1, derive_ext_attributes
     | 23 | 382 | 386 | 388 | 396 | 398 | 438 | 442 | 444 | 445 | 453 | 472 | 480 | 481 | 485 | 490 | 495 | 497 | 502 | 524 | 528 | 532 | 562 | 539 | 550 | 556 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1067 | 1113 | 1116 | 1078 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1382 | 1384 | 1394 | 1396 | 1440 | 1448 | 1458 | 1462 | 1466 | 1555 -> 0, Pop
     | _ -> raise Not_found)
  | 328 -> Parent (function
     | 329 -> 2, derive_pattern
     | 325 | 355 | 327 | 331 | 363 | 366 | 369 | 377 | 379 | 381 | 404 | 501 | 508 | 527 | 531 | 1030 -> 0, Pop
     | _ -> raise Not_found)
  | 314 -> Parent (function
     | 314 -> 1, derive_pattern
     | 82 | 112 | 287 | 288 | 300 | 302 | 324 | 328 | 347 | 352 | 326 | 330 | 365 | 376 | 378 | 403 | 458 | 500 | 507 | 516 | 526 | 530 | 966 | 1036 | 1145 | 1161 | 1206 | 1215 | 1286 | 1290 | 1333 | 1376 | 1380 | 1486 | 1490 | 1534 | 1538 -> 0, Pop
     | _ -> raise Not_found)
  | 289 -> Parent (function
     | 288 | 481 -> 2, Reduce 91
     | 1420 -> 0, Pop
     | _ -> raise Not_found)
  | 288 -> Parent (function
     | 288 -> 1, Shift (T T_RBRACKET)
     | 82 | 112 | 287 | 300 | 301 | 302 | 314 | 324 | 328 | 347 | 352 | 326 | 330 | 365 | 376 | 378 | 403 | 458 | 459 | 500 | 504 | 507 | 516 | 520 | 523 | 526 | 530 | 966 | 1036 | 1145 | 1148 | 1149 | 1153 | 1161 | 1206 | 1215 | 1218 | 1231 | 1286 | 1290 | 1309 | 1333 | 1340 | 1341 | 1376 | 1380 | 1486 | 1490 | 1515 | 1522 | 1534 | 1538 -> 0, Pop
     | _ -> raise Not_found)
  | 287 -> Parent (function
     | 287 -> 1, Shift (T T_BARRBRACKET)
     | 82 | 112 | 288 | 300 | 301 | 302 | 314 | 324 | 328 | 347 | 352 | 326 | 330 | 365 | 376 | 378 | 403 | 458 | 459 | 500 | 504 | 507 | 516 | 520 | 523 | 526 | 530 | 966 | 1036 | 1145 | 1148 | 1149 | 1153 | 1161 | 1206 | 1215 | 1218 | 1231 | 1286 | 1290 | 1309 | 1333 | 1340 | 1341 | 1376 | 1380 | 1486 | 1490 | 1515 | 1522 | 1534 | 1538 -> 0, Pop
     | _ -> raise Not_found)
  | 213 -> Parent (function
     | 214 | 262 | 273 | 276 -> 2, derive_core_type2
     | 212 -> 0, Pop
     | _ -> raise Not_found)
  | 184 -> Parent (function
     | 184 | 185 -> 1, Shift (T T_BAR)
     | 139 | 152 | 153 | 154 | 164 | 172 | 177 | 180 | 183 | 197 | 202 | 205 | 209 | 213 | 219 | 229 | 251 | 261 | 268 | 272 | 275 | 357 | 406 | 418 | 434 | 464 | 509 | 574 | 602 | 605 | 606 | 617 | 618 | 625 | 627 | 630 | 631 | 644 | 669 | 671 | 739 | 743 | 803 | 827 | 838 | 843 | 848 | 857 | 859 | 866 | 878 | 880 | 900 | 903 | 907 | 910 | 915 | 968 | 1221 | 1223 | 1225 | 1234 | 1239 | 1244 | 1301 | 1306 | 1310 | 1313 | 1336 | 1362 | 1497 -> 0, Pop
     | _ -> raise Not_found)
  | 185 -> Parent (function
     | 184 -> 2, derive_row_field_list
     | 910 -> 0, Pop
     | _ -> raise Not_found)
  | 178 -> Parent (function
     | 180 -> 1, derive_opt_bar
     | 139 | 152 | 153 | 154 | 164 | 172 | 177 | 183 | 184 | 185 | 197 | 202 | 205 | 209 | 213 | 219 | 229 | 251 | 261 | 268 | 272 | 275 | 357 | 406 | 418 | 434 | 464 | 509 | 574 | 602 | 605 | 606 | 617 | 618 | 625 | 627 | 630 | 631 | 644 | 669 | 671 | 739 | 743 | 803 | 827 | 838 | 843 | 848 | 857 | 859 | 866 | 878 | 880 | 897 | 900 | 902 | 903 | 905 | 907 | 909 | 910 | 913 | 915 | 968 | 1221 | 1223 | 1225 | 1234 | 1239 | 1244 | 1301 | 1306 | 1310 | 1313 | 1336 | 1354 | 1362 | 1497 | 1518 -> 0, Pop
     | _ -> raise Not_found)
  | 154 -> Parent (function
     | 154 -> 1, derive_core_type_comma_list
     | 139 | 152 | 153 | 164 | 172 | 177 | 180 | 183 | 184 | 185 | 197 | 202 | 205 | 209 | 213 | 219 | 229 | 251 | 261 | 268 | 272 | 275 | 357 | 406 | 418 | 434 | 464 | 509 | 574 | 606 | 618 | 631 | 644 | 669 | 671 | 739 | 743 | 803 | 827 | 838 | 843 | 848 | 857 | 859 | 866 | 878 | 880 | 910 | 968 | 1221 | 1223 | 1225 | 1234 | 1239 | 1244 | 1301 | 1306 | 1310 | 1313 | 1336 | 1362 | 1497 -> 0, Pop
     | _ -> raise Not_found)
  | 150 -> Parent (function
     | 152 -> 1, Shift (T T_LIDENT)
     | 139 | 153 | 154 | 164 | 172 | 177 | 202 | 213 | 219 | 251 | 261 | 268 | 272 | 275 | 357 | 406 | 418 | 434 | 464 | 509 | 574 | 602 | 605 | 606 | 618 | 631 | 644 | 669 | 671 | 739 | 743 | 803 | 827 | 838 | 843 | 848 | 857 | 859 | 866 | 878 | 880 | 910 | 968 | 1221 | 1223 | 1225 | 1234 | 1239 | 1244 | 1301 | 1306 | 1310 | 1313 | 1336 | 1362 | 1497 -> 0, Pop
     | _ -> raise Not_found)
  | 24 -> Parent (function
     | 1394 | 1396 -> 1, derive_ext_attributes
     | 23 | 382 | 384 | 386 | 388 | 396 | 398 | 438 | 442 | 444 | 445 | 453 | 472 | 480 | 481 | 485 | 490 | 495 | 497 | 502 | 524 | 528 | 532 | 562 | 539 | 550 | 556 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1067 | 1113 | 1116 | 1078 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1382 | 1384 | 1387 | 1440 | 1448 | 1458 | 1462 | 1466 | 1555 -> 0, Pop
     | _ -> raise Not_found)
  | 1332 -> Parent (function
     | 1335 -> 1, derive_rec_flag
     | 1327 | 1331 | 1342 | 1516 | 1520 -> 0, Pop
     | _ -> raise Not_found)
  | 1331 -> Parent (function
     | 1331 -> 1, derive_class_expr
     | 1327 | 1335 | 1342 | 1516 | 1520 -> 0, Pop
     | _ -> raise Not_found)
  | 455 -> Parent (function
     | 1250 -> 1, derive_ext_attributes
     | 382 | 384 | 386 | 388 | 396 | 398 | 438 | 442 | 444 | 445 | 453 | 472 | 480 | 481 | 485 | 490 | 495 | 497 | 502 | 524 | 528 | 532 | 562 | 539 | 550 | 556 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1067 | 1113 | 1116 | 1078 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1458 | 1462 | 1466 | 1555 -> 0, Pop
     | _ -> raise Not_found)
  | 1250 -> Parent (function
     | 1249 -> 5, derive_seq_expr
     | 1487 -> 0, Pop
     | _ -> raise Not_found)
  | 115 -> Parent (function
     | 112 | 445 -> 2, Reduce 92
     | 302 | 396 | 516 | 1145 | 1215 | 1419 -> 0, Pop
     | _ -> raise Not_found)
  | 112 -> Parent (function
     | 112 -> 1, Shift (T T_RPAREN)
     | 82 | 287 | 288 | 300 | 302 | 314 | 324 | 328 | 347 | 352 | 326 | 330 | 365 | 376 | 378 | 403 | 458 | 500 | 507 | 516 | 526 | 530 | 966 | 1036 | 1145 | 1161 | 1206 | 1215 | 1286 | 1290 | 1333 | 1376 | 1380 | 1486 | 1490 | 1534 | 1538 -> 0, Pop
     | _ -> raise Not_found)
  | 1019 -> Parent (function
     | 1020 -> 2, derive_expr
     | 976 | 988 | 1016 | 1018 | 1026 | 1010 | 1012 | 1027 | 1043 | 1046 | 1056 | 1068 | 1117 | 1076 | 1079 | 1091 | 1096 | 1098 | 1101 | 1104 | 1123 | 1124 | 1166 | 1168 | 1173 | 1183 | 1252 | 1276 | 1278 -> 0, Pop
     | _ -> raise Not_found)
  | 1017 -> Parent (function
     | 1018 -> 2, derive_expr
     | 976 | 988 | 1016 | 1020 | 1026 | 1010 | 1012 | 1027 | 1043 | 1046 | 1056 | 1068 | 1117 | 1076 | 1079 | 1091 | 1096 | 1098 | 1101 | 1104 | 1123 | 1124 | 1166 | 1168 | 1173 | 1183 | 1252 | 1276 | 1278 -> 0, Pop
     | _ -> raise Not_found)
  | 1015 -> Parent (function
     | 1016 -> 2, derive_expr
     | 976 | 988 | 1026 | 1010 | 1012 | 1027 | 1043 | 1046 | 1056 | 1068 | 1117 | 1076 | 1079 | 1091 | 1096 | 1098 | 1101 | 1104 | 1123 | 1124 | 1166 | 1168 | 1173 | 1183 | 1252 | 1276 | 1278 -> 0, Pop
     | _ -> raise Not_found)
  | 1013 -> Parent (function
     | 1014 -> 2, derive_expr
     | 976 | 988 | 994 | 1002 | 1004 | 1006 | 1008 | 1016 | 1018 | 1020 | 1026 | 1010 | 1012 | 1027 | 1043 | 1046 | 1056 | 1068 | 1117 | 1076 | 1079 | 1091 | 1096 | 1098 | 1101 | 1104 | 1123 | 1124 | 1166 | 1168 | 1173 | 1183 | 1252 | 1276 | 1278 -> 0, Pop
     | _ -> raise Not_found)
  | 1011 -> Parent (function
     | 1012 -> 2, derive_expr
     | 1026 | 1027 | 1043 | 1046 | 1056 | 1068 | 1117 | 1076 | 1079 | 1091 | 1096 | 1098 | 1101 | 1104 | 1123 | 1124 | 1166 | 1168 | 1173 | 1183 | 1252 | 1276 -> 0, Pop
     | _ -> raise Not_found)
  | 1315 -> Action (9, derive_seq_expr)
  | 1142 | 1131 -> Action (9, Shift (T T_DONE))
  | 1141 | 1130 -> Action (8, derive_seq_expr)
  | 1314 -> Action (8, Shift (T T_EQUAL))
  | 663 -> Action (8, Reduce 692)
  | 949 -> Action (8, Reduce 552)
  | 1262 -> Action (8, Reduce 322)
  | 1242 -> Action (8, Reduce 275)
  | 1136 -> Action (8, Reduce 148)
  | 1383 -> Action (8, Reduce 145)
  | 1241 -> Action (7, derive_seq_expr)
  | 1313 -> Action (7, derive_core_type)
  | 1261 | 948 -> Action (7, Shift (T T_RPAREN))
  | 662 -> Action (7, Shift (T T_RBRACE))
  | 1135 -> Action (7, Shift (T T_DONE))
  | 1140 | 1129 -> Action (7, Shift (T T_DO))
  | 1501 -> Action (7, Reduce 650)
  | 1423 -> Action (7, Reduce 629)
  | 809 -> Action (7, Reduce 460)
  | 693 -> Action (7, Reduce 451)
  | 1319 -> Action (7, Reduce 297)
  | 1247 -> Action (7, Reduce 274)
  | 1101 -> Action (7, Reduce 194)
  | 1096 -> Action (7, Reduce 193)
  | 1091 -> Action (7, Reduce 192)
  | 1168 -> Action (7, Reduce 163)
  | 1202 -> Action (7, Reduce 153)
  | 1318 | 1246 | 1201 | 1139 | 1134 | 1128 -> Action (6, derive_seq_expr)
  | 1500 | 1413 | 808 | 687 -> Action (6, derive_post_item_attributes)
  | 947 -> Action (6, derive_package_type)
  | 661 -> Action (6, derive_opt_semi)
  | 1240 -> Action (6, Shift (T T_EQUAL))
  | 1312 -> Action (6, Shift (T T_DOT))
  | 741 -> Action (6, Reduce 722)
  | 1295 -> Action (6, Reduce 716)
  | 435 -> Action (6, Reduce 713)
  | 889 -> Action (6, Reduce 71)
  | 665 -> Action (6, Reduce 691)
  | 651 -> Action (6, Reduce 690)
  | 925 -> Action (6, Reduce 67)
  | 1476 -> Action (6, Reduce 657)
  | 1459 -> Action (6, Reduce 647)
  | 129 -> Action (6, Reduce 571)
  | 1272 -> Action (6, Reduce 551)
  | 1058 -> Action (6, Reduce 547)
  | 1050 -> Action (6, Reduce 541)
  | 1045 -> Action (6, Reduce 539)
  | 1121 -> Action (6, Reduce 519)
  | 238 -> Action (6, Reduce 504)
  | 862 -> Action (6, Reduce 49)
  | 779 -> Action (6, Reduce 468)
  | 1256 -> Action (6, Reduce 323)
  | 1259 -> Action (6, Reduce 321)
  | 767 -> Action (6, Reduce 311)
  | 1302 -> Action (6, Reduce 295)
  | 1307 -> Action (6, Reduce 294)
  | 1389 -> Action (6, Reduce 165)
  | 927 -> Action (6, Reduce 16)
  | 1377 -> Action (6, Reduce 159)
  | 1287 -> Action (6, Reduce 158)
  | 1209 -> Action (6, Reduce 151)
  | 1398 -> Action (6, Reduce 146)
  | 1381 -> Action (6, Reduce 143)
  | 1291 -> Action (6, Reduce 141)
  | 1251 -> Action (6, Reduce 140)
  | 1412 -> Action (5, derive_str_extension_constructors)
  | 686 -> Action (5, derive_sig_extension_constructors)
  | 1458 | 1294 | 1208 -> Action (5, derive_seq_expr)
  | 1499 | 805 -> Action (5, derive_primitive_declaration)
  | 1475 | 926 | 888 | 861 | 778 -> Action (5, derive_post_item_attributes)
  | 1306 | 1301 -> Action (5, derive_poly_type)
  | 764 -> Action (5, derive_module_declaration)
  | 1380 | 1376 | 1290 | 1286 -> Action (5, derive_match_cases)
  | 1138 | 1125 -> Action (5, derive_direction_flag)
  | 1239 | 434 -> Action (5, derive_core_type)
  | 664 -> Action (5, derive_constructor_declarations)
  | 740 -> Action (5, derive_constraints)
  | 902 -> Action (5, derive_class_type)
  | 1271 | 1258 | 1255 | 1120 | 128 -> Action (5, Shift (T T_RPAREN))
  | 1049 | 237 -> Action (5, Shift (T T_RBRACKET))
  | 650 -> Action (5, Shift (T T_RBRACE))
  | 1200 -> Action (5, Shift (T T_IN))
  | 1057 -> Action (5, Shift (T T_GREATERRBRACE))
  | 1317 | 1245 -> Action (5, Shift (T T_EQUAL))
  | 1397 | 1388 -> Action (5, Shift (T T_DONE))
  | 1133 -> Action (5, Shift (T T_DO))
  | 1276 | 377 -> Action (5, Shift (T T_COMMA))
  | 946 -> Action (5, Shift (T T_COLON))
  | 1044 -> Action (5, Shift (T T_BARRBRACKET))
  | 744 -> Action (5, Reduce 723)
  | 844 -> Action (5, Reduce 718)
  | 839 -> Action (5, Reduce 717)
  | 1292 -> Action (5, Reduce 715)
  | 419 -> Action (5, Reduce 714)
  | 923 -> Action (5, Reduce 69)
  | 674 -> Action (5, Reduce 681)
  | 221 -> Action (5, Reduce 669)
  | 1403 -> Action (5, Reduce 640)
  | 1233 -> Action (5, Reduce 632)
  | 1356 -> Action (5, Reduce 63)
  | 359 -> Action (5, Reduce 569)
  | 1070 -> Action (5, Reduce 536)
  | 1118 | 1099 -> Action (5, Reduce 534)
  | 1115 | 1094 -> Action (5, Reduce 533)
  | 1112 | 1089 -> Action (5, Reduce 532)
  | 960 -> Action (5, Reduce 531)
  | 1180 -> Action (5, Reduce 516)
  | 231 -> Action (5, Reduce 500)
  | 266 -> Action (5, Reduce 497)
  | 772 -> Action (5, Reduce 465)
  | 576 -> Action (5, Reduce 459)
  | 597 -> Action (5, Reduce 457)
  | 408 -> Action (5, Reduce 42)
  | 1267 -> Action (5, Reduce 318)
  | 1157 -> Action (5, Reduce 288)
  | 971 -> Action (5, Reduce 277)
  | 513 -> Action (5, Reduce 255)
  | 1213 -> Action (5, Reduce 253)
  | 1361 -> Action (5, Reduce 25)
  | 646 -> Action (5, Reduce 238)
  | 942 -> Action (5, Reduce 223)
  | 1151 -> Action (5, Reduce 221)
  | 1351 -> Action (5, Reduce 22)
  | 1056 -> Action (5, Reduce 215)
  | 1104 -> Action (5, Reduce 191)
  | 1166 -> Action (5, Reduce 164)
  | 1190 -> Action (5, Reduce 154)
  | 1035 -> Action (5, Reduce 152)
  | 1385 -> Action (5, Reduce 144)
  | 1079 -> Action (5, Reduce 138)
  | 1525 -> Action (5, Reduce 13)
  | 1396 | 1387 | 1156 | 1034 | 970 | 532 | 528 | 480 | 438 -> Action (4, derive_seq_expr)
  | 1524 | 1360 | 771 | 668 | 575 -> Action (4, derive_post_item_attributes)
  | 857 -> Action (4, derive_poly_type)
  | 1052 | 1048 | 1041 | 647 -> Action (4, derive_opt_semi)
  | 1411 | 1375 | 1289 | 1285 | 685 | 659 -> Action (4, derive_opt_bar)
  | 1193 -> Action (4, derive_module_binding_body)
  | 1148 -> Action (4, derive_fun_def)
  | 1218 -> Action (4, derive_fun_binding)
  | 1165 | 1055 -> Action (4, derive_expr)
  | 743 | 739 -> Action (4, derive_core_type_no_attr)
  | 1244 | 843 | 838 | 418 -> Action (4, derive_core_type)
  | 909 | 897 -> Action (4, derive_class_type)
  | 825 -> Action (4, derive_class_signature)
  | 1335 -> Action (4, derive_class_expr)
  | 645 | 218 -> Action (4, derive_attributes)
  | 1355 | 1266 | 1212 | 1179 | 1111 | 1088 | 959 | 941 | 763 | 512 | 407 | 358 -> Action (4, Shift (T T_RPAREN))
  | 1114 | 1093 | 230 -> Action (4, Shift (T T_RBRACKET))
  | 1117 | 1098 | 1069 -> Action (4, Shift (T T_RBRACE))
  | 901 | 274 -> Action (4, Shift (T T_MINUSGREATER))
  | 1249 | 1207 -> Action (4, Shift (T T_IN))
  | 1498 | 1457 | 1293 | 804 -> Action (4, Shift (T T_EQUAL))
  | 1238 -> Action (4, Shift (T T_DOT))
  | 1305 | 1300 | 433 -> Action (4, Shift (T T_COLON))
  | 749 -> Action (4, Reduce 725)
  | 752 -> Action (4, Reduce 724)
  | 654 -> Action (4, Reduce 689)
  | 924 -> Action (4, Reduce 68)
  | 1226 -> Action (4, Reduce 679)
  | 1432 -> Action (4, Reduce 675)
  | 1430 -> Action (4, Reduce 674)
  | 1429 -> Action (4, Reduce 673)
  | 1477 -> Action (4, Reduce 656)
  | 1539 | 1491 -> Action (4, Reduce 649)
  | 1453 -> Action (4, Reduce 648)
  | 1463 -> Action (4, Reduce 646)
  | 1467 -> Action (4, Reduce 645)
  | 1445 -> Action (4, Reduce 644)
  | 1441 -> Action (4, Reduce 643)
  | 1449 -> Action (4, Reduce 642)
  | 1505 -> Action (4, Reduce 641)
  | 1406 -> Action (4, Reduce 639)
  | 1535 | 1487 -> Action (4, Reduce 637)
  | 1547 -> Action (4, Reduce 635)
  | 1339 -> Action (4, Reduce 60)
  | 124 -> Action (4, Reduce 570)
  | 373 -> Action (4, Reduce 566)
  | 368 -> Action (4, Reduce 565)
  | 1269 -> Action (4, Reduce 550)
  | 872 -> Action (4, Reduce 55)
  | 1186 -> Action (4, Reduce 545)
  | 1189 -> Action (4, Reduce 540)
  | 1372 -> Action (4, Reduce 537)
  | 1283 -> Action (4, Reduce 529)
  | 1171 -> Action (4, Reduce 527)
  | 1178 -> Action (4, Reduce 515)
  | 157 -> Action (4, Reduce 505)
  | 235 -> Action (4, Reduce 503)
  | 233 -> Action (4, Reduce 501)
  | 196 -> Action (4, Reduce 499)
  | 267 -> Action (4, Reduce 492)
  | 780 -> Action (4, Reduce 467)
  | 774 -> Action (4, Reduce 464)
  | 813 -> Action (4, Reduce 458)
  | 679 -> Action (4, Reduce 456)
  | 426 -> Action (4, Reduce 423)
  | 1390 -> Action (4, Reduce 418)
  | 277 -> Action (4, Reduce 392)
  | 1521 -> Action (4, Reduce 35)
  | 699 -> Action (4, Reduce 343)
  | 519 -> Action (4, Reduce 342)
  | 708 -> Action (4, Reduce 331)
  | 721 -> Action (4, Reduce 329)
  | 785 -> Action (4, Reduce 326)
  | 1253 -> Action (4, Reduce 320)
  | 712 -> Action (4, Reduce 316)
  | 945 -> Action (4, Reduce 315)
  | 1199 -> Action (4, Reduce 306)
  | 94 -> Action (4, Reduce 301)
  | 1320 -> Action (4, Reduce 296)
  | 297 -> Action (4, Reduce 270)
  | 467 -> Action (4, Reduce 257)
  | 793 -> Action (4, Reduce 236)
  | 634 -> Action (4, Reduce 230)
  | 797 -> Action (4, Reduce 216)
  | 254 -> Action (4, Reduce 213)
  | 1422 -> Action (4, Reduce 212)
  | 247 -> Action (4, Reduce 210)
  | 410 -> Action (4, Reduce 198)
  | 1152 -> Action (4, Reduce 157)
  | 1154 -> Action (4, Reduce 156)
  | 1160 -> Action (4, Reduce 155)
  | 1076 -> Action (4, Reduce 139)
  | 217 -> Action (4, Reduce 115)
  | 1392 -> Action (4, Reduce 10)
  | 736 -> Action (3, derive_with_type_binder)
  | 1546 -> Action (3, derive_structure_tail)
  | 1309 -> Action (3, derive_strict_binding)
  | 1456 -> Action (3, derive_simple_expr)
  | 900 -> Action (3, derive_simple_core_type_or_tuple_no_attr)
  | 630 -> Action (3, derive_simple_core_type_no_attr)
  | 1466 | 1462 | 1448 | 1440 -> Action (3, derive_seq_expr)
  | 229 -> Action (3, derive_row_field_list)
  | 1410 | 684 -> Action (3, derive_private_flag)
  | 1473 | 784 | 776 | 773 | 698 -> Action (3, derive_post_item_attributes)
  | 644 -> Action (3, derive_poly_type_no_attr)
  | 1357 -> Action (3, derive_parent_binder)
  | 511 | 471 -> Action (3, derive_opt_default)
  | 719 | 704 | 570 -> Action (3, derive_module_type)
  | 1452 | 1444 | 1198 | 944 -> Action (3, derive_module_expr)
  | 751 | 748 -> Action (3, derive_mod_ext_longident)
  | 500 -> Action (3, derive_match_cases)
  | 1206 -> Action (3, derive_let_bindings_no_attrs)
  | 1538 | 1534 | 1490 | 1486 | 458 -> Action (3, derive_let_bindings)
  | 1299 -> Action (3, derive_label)
  | 1153 | 523 -> Action (3, derive_fun_def)
  | 152 -> Action (3, derive_core_type2)
  | 1497 | 803 | 574 | 139 -> Action (3, derive_core_type)
  | 653 -> Action (3, derive_constructor_declarations)
  | 667 -> Action (3, derive_constraints)
  | 868 -> Action (3, derive_clty_longident)
  | 905 -> Action (3, derive_class_type)
  | 1338 -> Action (3, derive_class_longident)
  | 1515 -> Action (3, derive_class_fun_binding)
  | 1520 -> Action (3, derive_class_expr)
  | 1421 | 253 -> Action (3, derive_attributes)
  | 202 -> Action (3, derive_amper_type_list)
  | 1374 | 1288 | 1284 -> Action (3, Shift (T T_WITH))
  | 1192 -> Action (3, Shift (T T_UIDENT))
  | 1164 -> Action (3, Shift (T T_THEN))
  | 1282 | 1268 | 1252 | 1217 | 1177 | 1147 | 711 | 518 | 466 | 156 | 123 | 93 -> Action (3, Shift (T T_RPAREN))
  | 1391 | 1188 | 796 | 792 | 425 | 367 | 246 | 234 | 232 | 195 -> Action (3, Shift (T T_RBRACKET))
  | 1155 | 908 | 260 -> Action (3, Shift (T T_MINUSGREATER))
  | 1334 | 1033 | 527 | 479 -> Action (3, Shift (T T_IN))
  | 1185 -> Action (3, Shift (T T_GREATERRBRACE))
  | 1054 | 969 | 824 | 531 | 437 -> Action (3, Shift (T T_EQUAL))
  | 1170 | 409 -> Action (3, Shift (T T_END))
  | 1243 -> Action (3, Shift (T T_DOT))
  | 1395 | 1386 -> Action (3, Shift (T T_DO))
  | 742 -> Action (3, Shift (T T_COLONEQUAL))
  | 896 | 856 | 842 | 837 | 417 -> Action (3, Shift (T T_COLON))
  | 1371 | 372 -> Action (3, Shift (T T_BARRBRACKET))
  | 637 -> Action (3, Reduce 99)
  | 673 -> Action (3, Reduce 97)
  | 881 -> Action (3, Reduce 96)
  | 672 -> Action (3, Reduce 95)
  | 78 -> Action (3, Reduce 9)
  | 871 -> Action (3, Reduce 77)
  | 822 -> Action (3, Reduce 75)
  | 596 -> Action (3, Reduce 728)
  | 755 -> Action (3, Reduce 727)
  | 892 -> Action (3, Reduce 72)
  | 849 -> Action (3, Reduce 719)
  | 1074 -> Action (3, Reduce 712)
  | 344 -> Action (3, Reduce 710)
  | 250 -> Action (3, Reduce 708)
  | 730 -> Action (3, Reduce 702)
  | 921 -> Action (3, Reduce 70)
  | 220 -> Action (3, Reduce 7)
  | 732 -> Action (3, Reduce 699)
  | 919 | 98 -> Action (3, Reduce 696)
  | 611 -> Action (3, Reduce 688)
  | 610 -> Action (3, Reduce 686)
  | 599 -> Action (3, Reduce 683)
  | 1427 -> Action (3, Reduce 672)
  | 1494 -> Action (3, Reduce 661)
  | 1512 -> Action (3, Reduce 660)
  | 1479 -> Action (3, Reduce 655)
  | 1353 -> Action (3, Reduce 64)
  | 1401 -> Action (3, Reduce 638)
  | 1415 -> Action (3, Reduce 628)
  | 1416 -> Action (3, Reduce 627)
  | 1330 -> Action (3, Reduce 62)
  | 831 -> Action (3, Reduce 57)
  | 356 -> Action (3, Reduce 568)
  | 292 -> Action (3, Reduce 564)
  | 321 -> Action (3, Reduce 560)
  | 545 -> Action (3, Reduce 549)
  | 547 -> Action (3, Reduce 548)
  | 393 -> Action (3, Reduce 544)
  | 1182 -> Action (3, Reduce 535)
  | 1119 | 1102 -> Action (3, Reduce 530)
  | 1169 -> Action (3, Reduce 528)
  | 1174 -> Action (3, Reduce 525)
  | 1281 -> Action (3, Reduce 524)
  | 1077 | 538 -> Action (3, Reduce 517)
  | 916 -> Action (3, Reduce 512)
  | 208 -> Action (3, Reduce 510)
  | 633 | 620 -> Action (3, Reduce 508)
  | 883 -> Action (3, Reduce 50)
  | 227 -> Action (3, Reduce 498)
  | 192 -> Action (3, Reduce 496)
  | 170 -> Action (3, Reduce 493)
  | 264 -> Action (3, Reduce 487)
  | 846 -> Action (3, Reduce 48)
  | 890 -> Action (3, Reduce 472)
  | 800 -> Action (3, Reduce 470)
  | 876 -> Action (3, Reduce 47)
  | 786 -> Action (3, Reduce 466)
  | 592 -> Action (3, Reduce 455)
  | 689 -> Action (3, Reduce 450)
  | 1029 -> Action (3, Reduce 447)
  | 198 -> Action (3, Reduce 444)
  | 829 -> Action (3, Reduce 44)
  | 1062 -> Action (3, Reduce 439)
  | 788 -> Action (3, Reduce 438)
  | 252 -> Action (3, Reduce 422)
  | 860 -> Action (3, Reduce 420)
  | 366 -> Action (3, Reduce 412)
  | 379 | 327 -> Action (3, Reduce 410)
  | 405 -> Action (3, Reduce 41)
  | 325 -> Action (3, Reduce 409)
  | 331 -> Action (3, Reduce 405)
  | 329 -> Action (3, Reduce 403)
  | 145 -> Action (3, Reduce 40)
  | 345 -> Action (3, Reduce 399)
  | 281 -> Action (3, Reduce 394)
  | 278 -> Action (3, Reduce 391)
  | 588 -> Action (3, Reduce 384)
  | 590 -> Action (3, Reduce 381)
  | 1347 -> Action (3, Reduce 37)
  | 161 -> Action (3, Reduce 336)
  | 762 -> Action (3, Reduce 332)
  | 753 -> Action (3, Reduce 330)
  | 940 -> Action (3, Reduce 328)
  | 1264 -> Action (3, Reduce 319)
  | 451 -> Action (3, Reduce 314)
  | 769 -> Action (3, Reduce 312)
  | 1481 -> Action (3, Reduce 309)
  | 1471 -> Action (3, Reduce 304)
  | 136 -> Action (3, Reduce 303)
  | 160 | 96 -> Action (3, Reduce 300)
  | 258 -> Action (3, Reduce 291)
  | 1162 -> Action (3, Reduce 290)
  | 1325 -> Action (3, Reduce 29)
  | 1159 -> Action (3, Reduce 287)
  | 510 -> Action (3, Reduce 284)
  | 963 -> Action (3, Reduce 282)
  | 1364 -> Action (3, Reduce 28)
  | 1039 -> Action (3, Reduce 279)
  | 1032 -> Action (3, Reduce 276)
  | 298 -> Action (3, Reduce 271)
  | 1322 -> Action (3, Reduce 27)
  | 363 -> Action (3, Reduce 266)
  | 1065 -> Action (3, Reduce 264)
  | 1068 -> Action (3, Reduce 261)
  | 427 -> Action (3, Reduce 26)
  | 1073 | 137 -> Action (3, Reduce 249)
  | 465 -> Action (3, Reduce 247)
  | 649 -> Action (3, Reduce 240)
  | 1229 -> Action (3, Reduce 218)
  | 1183 -> Action (3, Reduce 214)
  | 692 -> Action (3, Reduce 211)
  | 1393 -> Action (3, Reduce 209)
  | 1043 -> Action (3, Reduce 206)
  | 478 -> Action (3, Reduce 204)
  | 1123 -> Action (3, Reduce 202)
  | 1278 | 1010 -> Action (3, Reduce 201)
  | 976 -> Action (3, Reduce 200)
  | 1172 -> Action (3, Reduce 197)
  | 543 -> Action (3, Reduce 196)
  | 1026 -> Action (3, Reduce 195)
  | 1012 -> Action (3, Reduce 188)
  | 1020 -> Action (3, Reduce 187)
  | 1018 -> Action (3, Reduce 186)
  | 1016 -> Action (3, Reduce 185)
  | 988 -> Action (3, Reduce 184)
  | 1006 -> Action (3, Reduce 183)
  | 994 -> Action (3, Reduce 182)
  | 1008 -> Action (3, Reduce 181)
  | 986 -> Action (3, Reduce 180)
  | 978 -> Action (3, Reduce 179)
  | 990 -> Action (3, Reduce 178)
  | 992 -> Action (3, Reduce 177)
  | 980 -> Action (3, Reduce 176)
  | 982 -> Action (3, Reduce 175)
  | 984 -> Action (3, Reduce 174)
  | 996 -> Action (3, Reduce 173)
  | 998 -> Action (3, Reduce 172)
  | 1000 -> Action (3, Reduce 171)
  | 1002 -> Action (3, Reduce 170)
  | 930 -> Action (3, Reduce 17)
  | 1004 -> Action (3, Reduce 169)
  | 1014 -> Action (3, Reduce 167)
  | 1378 -> Action (3, Reduce 142)
  | 1528 -> Action (3, Reduce 14)
  | 626 -> Action (3, Reduce 113)
  | 210 -> Action (3, Reduce 111)
  | 269 -> Action (3, Reduce 109)
  | 276 | 273 | 262 | 214 -> Action (3, Reduce 107)
  | 615 -> Action (3, Reduce 101)
  | 1455 | 1434 -> Action (2, derive_val_ident)
  | 682 | 601 -> Action (2, derive_type_kind)
  | 544 | 542 | 492 -> Action (2, derive_simple_expr)
  | 625 -> Action (2, derive_simple_core_type_no_attr)
  | 1394 | 1228 | 1158 | 1031 | 497 | 444 | 442 | 388 | 386 | 384 -> Action (2, derive_seq_expr)
  | 183 | 180 -> Action (2, derive_row_field_list)
  | 1537 | 1533 | 1489 | 1485 | 1205 | 456 -> Action (2, derive_rec_flag)
  | 1545 | 1493 | 1470 | 1363 | 1324 | 1321 | 882 | 875 | 845 | 799 | 422 -> Action (2, derive_post_item_attributes)
  | 172 -> Action (2, derive_poly_type_no_attr)
  | 795 | 791 | 424 | 176 | 81 -> Action (2, derive_payload)
  | 530 | 526 -> Action (2, derive_pattern)
  | 1370 | 1187 | 1184 | 371 | 364 -> Action (2, derive_opt_semi)
  | 499 -> Action (2, derive_opt_bar)
  | 504 -> Action (2, derive_newtype)
  | 783 -> Action (2, derive_module_type)
  | 768 | 701 -> Action (2, derive_module_declaration)
  | 697 | 477 -> Action (2, derive_mod_longident)
  | 1333 -> Action (2, derive_let_bindings_no_attrs)
  | 966 -> Action (2, derive_let_bindings)
  | 1061 -> Action (2, derive_lbl_expr_list)
  | 1308 | 855 | 841 | 836 | 734 | 436 | 416 -> Action (2, derive_label)
  | 159 -> Action (2, derive_ident)
  | 1003 | 1001 | 999 | 997 | 995 | 973 | 485 -> Action (2, derive_expr)
  | 251 -> Action (2, derive_core_type_no_attr)
  | 880 | 859 | 848 | 671 -> Action (2, derive_core_type)
  | 913 -> Action (2, derive_class_type)
  | 402 -> Action (2, derive_class_structure)
  | 392 -> Action (2, derive_class_longident)
  | 1342 | 1327 -> Action (2, derive_class_expr)
  | 691 | 636 | 79 -> Action (2, derive_attributes)
  | 1352 | 1280 | 1263 | 828 | 761 | 729 | 632 | 619 | 587 | 404 | 355 | 343 | 263 -> Action (2, Shift (T T_RPAREN))
  | 1337 | 867 | 821 | 226 -> Action (2, Shift (T T_RBRACKET))
  | 1181 | 291 -> Action (2, Shift (T T_RBRACE))
  | 1409 | 683 -> Action (2, Shift (T T_PLUSEQ))
  | 943 | 904 | 718 | 629 | 271 -> Action (2, Shift (T T_MINUSGREATER))
  | 1176 -> Action (2, Shift (T T_LPAREN))
  | 1514 | 895 | 823 | 97 -> Action (2, Shift (T T_LIDENT))
  | 1173 -> Action (2, Shift (T T_GREATERDOT))
  | 169 -> Action (2, Shift (T T_GREATER))
  | 1519 | 1465 | 1461 | 1451 | 1447 | 1443 | 1439 | 1197 | 750 | 138 -> Action (2, Shift (T T_EQUAL))
  | 1329 | 939 | 830 | 495 | 450 -> Action (2, Shift (T T_END))
  | 747 -> Action (2, Shift (T T_COLONEQUAL))
  | 1496 | 899 | 802 | 703 | 643 | 573 | 569 | 151 -> Action (2, Shift (T T_COLON))
  | 640 | 228 -> Action (2, Shift (T T_BAR))
  | 174 -> Action (2, Reduce 707)
  | 727 -> Action (2, Reduce 703)
  | 728 -> Action (2, Reduce 697)
  | 666 -> Action (2, Reduce 687)
  | 655 -> Action (2, Reduce 685)
  | 1222 -> Action (2, Reduce 680)
  | 1224 -> Action (2, Reduce 678)
  | 224 -> Action (2, Reduce 670)
  | 1540 -> Action (2, Reduce 666)
  | 1436 -> Action (2, Reduce 665)
  | 1543 -> Action (2, Reduce 662)
  | 1526 -> Action (2, Reduce 659)
  | 1483 -> Action (2, Reduce 654)
  | 1503 -> Action (2, Reduce 653)
  | 1407 -> Action (2, Reduce 652)
  | 1404 -> Action (2, Reduce 651)
  | 412 -> Action (2, Reduce 65)
  | 1530 -> Action (2, Reduce 636)
  | 1232 -> Action (2, Reduce 631)
  | 1220 -> Action (2, Reduce 630)
  | 1507 -> Action (2, Reduce 624)
  | 1509 -> Action (2, Reduce 623)
  | 877 -> Action (2, Reduce 58)
  | 370 -> Action (2, Reduce 567)
  | 90 -> Action (2, Reduce 563)
  | 1107 -> Action (2, Reduce 555)
  | 536 -> Action (2, Reduce 543)
  | 1373 -> Action (2, Reduce 542)
  | 887 -> Action (2, Reduce 54)
  | 1369 -> Action (2, Reduce 538)
  | 1175 -> Action (2, Reduce 526)
  | 954 -> Action (2, Reduce 513)
  | 885 -> Action (2, Reduce 51)
  | 146 -> Action (2, Reduce 495)
  | 193 -> Action (2, Reduce 491)
  | 149 -> Action (2, Reduce 488)
  | 100 -> Action (2, Reduce 485)
  | 101 -> Action (2, Reduce 484)
  | 102 -> Action (2, Reduce 483)
  | 104 -> Action (2, Reduce 482)
  | 103 -> Action (2, Reduce 481)
  | 107 -> Action (2, Reduce 480)
  | 108 -> Action (2, Reduce 479)
  | 109 -> Action (2, Reduce 478)
  | 111 -> Action (2, Reduce 477)
  | 110 -> Action (2, Reduce 476)
  | 936 -> Action (2, Reduce 473)
  | 928 -> Action (2, Reduce 471)
  | 811 -> Action (2, Reduce 463)
  | 680 -> Action (2, Reduce 462)
  | 677 -> Action (2, Reduce 461)
  | 833 -> Action (2, Reduce 46)
  | 933 -> Action (2, Reduce 454)
  | 938 -> Action (2, Reduce 453)
  | 815 -> Action (2, Reduce 448)
  | 807 -> Action (2, Reduce 427)
  | 429 -> Action (2, Reduce 425)
  | 381 -> Action (2, Reduce 417)
  | 242 -> Action (2, Reduce 416)
  | 346 -> Action (2, Reduce 408)
  | 354 -> Action (2, Reduce 407)
  | 360 -> Action (2, Reduce 406)
  | 348 -> Action (2, Reduce 402)
  | 353 -> Action (2, Reduce 401)
  | 1557 -> Action (2, Reduce 397)
  | 1359 -> Action (2, Reduce 395)
  | 585 -> Action (2, Reduce 385)
  | 1349 -> Action (2, Reduce 38)
  | 586 -> Action (2, Reduce 379)
  | 1211 -> Action (2, Reduce 374)
  | 1523 -> Action (2, Reduce 36)
  | 239 -> Action (2, Reduce 341)
  | 1517 -> Action (2, Reduce 34)
  | 187 -> Action (2, Reduce 339)
  | 757 -> Action (2, Reduce 334)
  | 1368 -> Action (2, Reduce 33)
  | 713 -> Action (2, Reduce 324)
  | 766 -> Action (2, Reduce 310)
  | 1204 -> Action (2, Reduce 307)
  | 1195 -> Action (2, Reduce 305)
  | 1366 -> Action (2, Reduce 30)
  | 259 -> Action (2, Reduce 292)
  | 1237 -> Action (2, Reduce 286)
  | 1248 -> Action (2, Reduce 273)
  | 1038 -> Action (2, Reduce 272)
  | 521 -> Action (2, Reduce 259)
  | 468 -> Action (2, Reduce 258)
  | 515 -> Action (2, Reduce 256)
  | 1214 -> Action (2, Reduce 254)
  | 1083 -> Action (2, Reduce 244)
  | 1081 -> Action (2, Reduce 243)
  | 554 -> Action (2, Reduce 242)
  | 1085 -> Action (2, Reduce 241)
  | 1553 -> Action (2, Reduce 235)
  | 1549 -> Action (2, Reduce 234)
  | 628 -> Action (2, Reduce 231)
  | 1348 -> Action (2, Reduce 23)
  | 623 -> Action (2, Reduce 229)
  | 759 -> Action (2, Reduce 226)
  | 1150 -> Action (2, Reduce 220)
  | 1144 -> Action (2, Reduce 219)
  | 1345 -> Action (2, Reduce 21)
  | 395 -> Action (2, Reduce 208)
  | 1350 -> Action (2, Reduce 20)
  | 1021 -> Action (2, Reduce 199)
  | 1025 -> Action (2, Reduce 190)
  | 1110 -> Action (2, Reduce 189)
  | 558 -> Action (2, Reduce 162)
  | 1023 -> Action (2, Reduce 161)
  | 1105 -> Action (2, Reduce 150)
  | 223 -> Action (2, Reduce 12)
  | 1316 -> Action (10, Reduce 298)
  | 1132 -> Action (10, Reduce 166)
  | 1143 -> Action (10, Reduce 147)
  | 725 -> Action (1, derive_type_variable)
  | 1531 | 23 -> Action (1, derive_structure_tail)
  | 520 -> Action (1, derive_simple_pattern)
  | 1084 | 1082 | 389 -> Action (1, derive_simple_expr)
  | 903 -> Action (1, derive_simple_core_type_or_tuple_no_attr)
  | 932 -> Action (1, derive_signature)
  | 1297 -> Action (1, derive_private_flag)
  | 1542 | 1508 | 1506 | 1365 | 1037 | 935 | 884 | 814 | 428 -> Action (1, derive_post_item_attributes)
  | 505 -> Action (1, derive_pattern_var)
  | 582 -> Action (1, derive_optional_type_variable)
  | 256 -> Action (1, derive_opt_semi)
  | 430 -> Action (1, derive_mutable_flag)
  | 1469 | 1203 -> Action (1, derive_module_binding_body)
  | 642 -> Action (1, derive_label)
  | 1417 | 690 | 616 -> Action (1, derive_generalized_constructor_arguments)
  | 1149 -> Action (1, derive_fun_def)
  | 1231 | 459 -> Action (1, derive_fun_binding)
  | 965 | 476 -> Action (1, derive_ext_attributes)
  | 1024 | 550 -> Action (1, derive_expr)
  | 153 -> Action (1, derive_core_type2)
  | 1513 | 894 | 819 -> Action (1, derive_class_type_parameters)
  | 832 -> Action (1, derive_class_sig_fields)
  | 1522 -> Action (1, derive_class_fun_binding)
  | 411 -> Action (1, derive_class_fields)
  | 394 | 222 | 199 -> Action (1, derive_attributes)
  | 1060 -> Action (1, Shift (T T_WITH))
  | 624 -> Action (1, Shift (T T_STAR))
  | 1341 | 912 | 501 -> Action (1, Shift (T T_MINUSGREATER))
  | 1408 | 681 | 600 -> Action (1, Shift (T T_LIDENT))
  | 972 -> Action (1, Shift (T T_LESSMINUS))
  | 1227 | 1030 | 879 | 670 | 484 -> Action (1, Shift (T T_EQUAL))
  | 1556 | 1552 | 1548 -> Action (1, Shift (T T_EOF))
  | 1433 | 917 | 869 | 858 | 248 | 158 | 143 | 134 | 91 -> Action (1, Shift (T T_DOT))
  | 847 | 782 | 171 -> Action (1, Shift (T T_COLON))
  | 1071 | 961 | 560 | 349 -> Action (1, Reduce 90)
  | 613 | 603 -> Action (1, Reduce 85)
  | 105 -> Action (1, Reduce 84)
  | 303 -> Action (1, Reduce 83)
  | 304 -> Action (1, Reduce 82)
  | 312 -> Action (1, Reduce 81)
  | 86 -> Action (1, Reduce 80)
  | 76 -> Action (1, Reduce 8)
  | 315 -> Action (1, Reduce 79)
  | 305 -> Action (1, Reduce 78)
  | 906 | 865 -> Action (1, Reduce 76)
  | 893 -> Action (1, Reduce 73)
  | 594 -> Action (1, Reduce 729)
  | 756 -> Action (1, Reduce 726)
  | 535 -> Action (1, Reduce 711)
  | 454 | 284 -> Action (1, Reduce 709)
  | 745 -> Action (1, Reduce 701)
  | 733 -> Action (1, Reduce 698)
  | 163 | 89 -> Action (1, Reduce 695)
  | 676 -> Action (1, Reduce 682)
  | 1544 -> Action (1, Reduce 663)
  | 922 -> Action (1, Reduce 66)
  | 1541 -> Action (1, Reduce 658)
  | 243 -> Action (1, Reduce 633)
  | 1424 -> Action (1, Reduce 626)
  | 1425 -> Action (1, Reduce 625)
  | 1346 -> Action (1, Reduce 61)
  | 211 -> Action (1, Reduce 6)
  | 920 | 873 -> Action (1, Reduce 59)
  | 31 -> Action (1, Reduce 574)
  | 50 -> Action (1, Reduce 573)
  | 351 -> Action (1, Reduce 572)
  | 361 | 347 -> Action (1, Reduce 562)
  | 362 | 352 -> Action (1, Reduce 561)
  | 874 -> Action (1, Reduce 56)
  | 319 -> Action (1, Reduce 559)
  | 967 | 317 -> Action (1, Reduce 557)
  | 316 -> Action (1, Reduce 556)
  | 1109 -> Action (1, Reduce 554)
  | 955 -> Action (1, Reduce 553)
  | 559 | 557 -> Action (1, Reduce 523)
  | 1022 | 956 -> Action (1, Reduce 522)
  | 957 -> Action (1, Reduce 521)
  | 534 -> Action (1, Reduce 520)
  | 886 -> Action (1, Reduce 52)
  | 914 -> Action (1, Reduce 511)
  | 911 | 204 -> Action (1, Reduce 509)
  | 635 | 622 -> Action (1, Reduce 507)
  | 207 -> Action (1, Reduce 506)
  | 188 -> Action (1, Reduce 490)
  | 190 -> Action (1, Reduce 486)
  | 322 -> Action (1, Reduce 475)
  | 937 -> Action (1, Reduce 474)
  | 934 -> Action (1, Reduce 469)
  | 694 -> Action (1, Reduce 449)
  | 1027 -> Action (1, Reduce 445)
  | 225 -> Action (1, Reduce 443)
  | 194 -> Action (1, Reduce 442)
  | 189 -> Action (1, Reduce 441)
  | 1075 -> Action (1, Reduce 440)
  | 789 -> Action (1, Reduce 437)
  | 806 -> Action (1, Reduce 426)
  | 255 -> Action (1, Reduce 421)
  | 863 -> Action (1, Reduce 419)
  | 245 -> Action (1, Reduce 415)
  | 514 -> Action (1, Reduce 413)
  | 369 -> Action (1, Reduce 411)
  | 323 -> Action (1, Reduce 400)
  | 318 -> Action (1, Reduce 398)
  | 279 -> Action (1, Reduce 393)
  | 130 -> Action (1, Reduce 390)
  | 142 -> Action (1, Reduce 39)
  | 675 -> Action (1, Reduce 383)
  | 591 -> Action (1, Reduce 380)
  | 1438 -> Action (1, Reduce 378)
  | 114 -> Action (1, Reduce 352)
  | 952 | 285 -> Action (1, Reduce 351)
  | 306 -> Action (1, Reduce 350)
  | 307 -> Action (1, Reduce 349)
  | 308 -> Action (1, Reduce 348)
  | 309 -> Action (1, Reduce 347)
  | 310 -> Action (1, Reduce 346)
  | 397 | 116 -> Action (1, Reduce 345)
  | 240 -> Action (1, Reduce 340)
  | 162 -> Action (1, Reduce 335)
  | 758 -> Action (1, Reduce 333)
  | 720 -> Action (1, Reduce 327)
  | 715 -> Action (1, Reduce 325)
  | 714 -> Action (1, Reduce 313)
  | 1367 -> Action (1, Reduce 31)
  | 1482 -> Action (1, Reduce 308)
  | 84 -> Action (1, Reduce 302)
  | 88 -> Action (1, Reduce 299)
  | 1163 -> Action (1, Reduce 289)
  | 1236 -> Action (1, Reduce 285)
  | 508 -> Action (1, Reduce 283)
  | 473 -> Action (1, Reduce 281)
  | 1210 -> Action (1, Reduce 280)
  | 1040 -> Action (1, Reduce 278)
  | 293 -> Action (1, Reduce 268)
  | 299 -> Action (1, Reduce 267)
  | 1063 -> Action (1, Reduce 263)
  | 1066 -> Action (1, Reduce 262)
  | 522 -> Action (1, Reduce 260)
  | 1108 -> Action (1, Reduce 252)
  | 1106 -> Action (1, Reduce 251)
  | 462 -> Action (1, Reduce 250)
  | 487 | 133 -> Action (1, Reduce 248)
  | 463 -> Action (1, Reduce 246)
  | 553 -> Action (1, Reduce 245)
  | 1343 -> Action (1, Reduce 24)
  | 652 -> Action (1, Reduce 239)
  | 735 | 166 -> Action (1, Reduce 237)
  | 127 -> Action (1, Reduce 233)
  | 148 | 126 -> Action (1, Reduce 232)
  | 760 -> Action (1, Reduce 227)
  | 567 -> Action (1, Reduce 224)
  | 1230 -> Action (1, Reduce 217)
  | 1046 -> Action (1, Reduce 205)
  | 1124 -> Action (1, Reduce 203)
  | 1344 -> Action (1, Reduce 19)
  | 931 -> Action (1, Reduce 18)
  | 974 -> Action (1, Reduce 160)
  | 1529 -> Action (1, Reduce 15)
  | 551 -> Action (1, Reduce 149)
  | 20 -> Action (1, Reduce 121)
  | 3 -> Action (1, Reduce 118)
  | 212 -> Action (1, Reduce 114)
  | 621 -> Action (1, Reduce 112)
  | 206 -> Action (1, Reduce 110)
  | 270 -> Action (1, Reduce 108)
  | 203 -> Action (1, Reduce 103)
  | 241 -> Action (1, Reduce 102)
  | 638 -> Action (1, Reduce 100)
  | 1536 | 1532 | 1527 | 1518 | 1516 | 1511 | 1510 | 1504 | 1502 | 1495 | 1492 | 1488 | 1484 | 1480 | 1478 | 1474 | 1472 | 1468 | 1464 | 1460 | 1454 | 1450 | 1446 | 1442 | 1437 | 1435 | 1431 | 1428 | 1426 | 1420 | 1419 | 1418 | 1414 | 1405 | 1402 | 1400 | 1399 | 1384 | 1382 | 1379 | 1362 | 1358 | 1354 | 1340 | 1336 | 1328 | 1326 | 1323 | 1311 | 1310 | 1304 | 1303 | 1298 | 1296 | 1279 | 1277 | 1275 | 1274 | 1273 | 1270 | 1265 | 1260 | 1257 | 1254 | 1235 | 1234 | 1225 | 1223 | 1221 | 1219 | 1216 | 1215 | 1196 | 1194 | 1191 | 1167 | 1161 | 1146 | 1145 | 1137 | 1127 | 1126 | 1122 | 1116 | 1113 | 1103 | 1100 | 1097 | 1095 | 1092 | 1090 | 1087 | 1086 | 1080 | 1078 | 1072 | 1067 | 1064 | 1059 | 1053 | 1051 | 1047 | 1042 | 1036 | 1028 | 1009 | 1007 | 1005 | 993 | 991 | 989 | 985 | 983 | 981 | 979 | 977 | 975 | 968 | 964 | 962 | 958 | 953 | 951 | 950 | 929 | 918 | 915 | 910 | 907 | 891 | 878 | 870 | 866 | 864 | 854 | 853 | 852 | 851 | 850 | 840 | 835 | 834 | 827 | 826 | 820 | 818 | 817 | 816 | 812 | 810 | 801 | 798 | 794 | 790 | 787 | 781 | 777 | 775 | 770 | 765 | 754 | 746 | 738 | 737 | 731 | 726 | 724 | 723 | 722 | 710 | 709 | 707 | 706 | 705 | 700 | 696 | 688 | 678 | 669 | 660 | 658 | 657 | 656 | 648 | 641 | 639 | 631 | 627 | 618 | 617 | 614 | 612 | 609 | 608 | 607 | 606 | 605 | 604 | 602 | 598 | 595 | 593 | 589 | 584 | 583 | 581 | 580 | 579 | 578 | 577 | 572 | 571 | 568 | 566 | 565 | 563 | 562 | 561 | 556 | 555 | 552 | 549 | 548 | 546 | 541 | 540 | 539 | 537 | 524 | 517 | 516 | 509 | 507 | 506 | 503 | 502 | 498 | 491 | 483 | 482 | 472 | 470 | 469 | 464 | 461 | 460 | 457 | 453 | 449 | 448 | 447 | 446 | 440 | 439 | 432 | 431 | 423 | 421 | 420 | 415 | 414 | 413 | 406 | 403 | 401 | 400 | 399 | 396 | 391 | 390 | 382 | 380 | 378 | 376 | 375 | 374 | 365 | 357 | 350 | 342 | 341 | 340 | 339 | 338 | 337 | 336 | 335 | 334 | 333 | 332 | 330 | 326 | 324 | 320 | 313 | 311 | 302 | 301 | 300 | 296 | 295 | 294 | 290 | 286 | 283 | 282 | 280 | 275 | 272 | 268 | 265 | 261 | 257 | 249 | 244 | 236 | 219 | 216 | 215 | 209 | 205 | 201 | 200 | 197 | 191 | 186 | 182 | 181 | 179 | 177 | 175 | 173 | 168 | 167 | 165 | 164 | 155 | 147 | 144 | 141 | 140 | 135 | 132 | 131 | 125 | 122 | 121 | 120 | 119 | 118 | 117 | 113 | 106 | 99 | 95 | 87 | 85 | 83 | 82 | 80 | 77 | 75 | 74 | 73 | 72 | 71 | 70 | 69 | 68 | 67 | 66 | 65 | 64 | 63 | 62 | 61 | 60 | 59 | 58 | 57 | 56 | 55 | 54 | 53 | 52 | 51 | 49 | 48 | 47 | 46 | 45 | 44 | 43 | 42 | 41 | 40 | 39 | 38 | 37 | 36 | 35 | 34 | 33 | 32 | 30 | 29 | 28 | 27 | 26 | 25 | 19 | 18 | 17 | 16 | 15 | 14 | 13 | 12 | 11 | 10 | 9 | 8 | 7 | 6 | 5 | 4 | 2 | 1 -> Action (0, Pop)
  | _ -> raise Not_found

let lr1_to_lr0 =
  [|0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25;26;27;28;29;30;31;32;33;34;35;36;37;38;39;40;41;42;43;44;45;46;47;48;49;50;51;52;53;54;55;56;57;58;59;60;61;62;63;64;65;66;67;68;69;70;71;72;73;74;75;76;77;78;79;80;81;82;83;84;85;86;87;88;89;90;91;92;93;94;95;96;97;98;99;100;101;102;103;104;105;106;107;108;109;110;111;112;113;114;115;116;117;118;119;120;121;122;123;124;125;126;127;128;129;130;131;132;133;134;135;136;137;138;139;140;141;142;143;144;145;146;147;148;149;150;151;152;153;154;155;156;157;158;159;160;161;162;163;164;165;166;167;168;169;170;171;172;173;174;175;176;177;178;179;180;181;182;183;184;185;186;187;188;189;190;191;192;193;194;195;196;197;198;199;200;201;202;203;204;205;206;207;208;209;210;211;212;213;214;215;216;217;218;219;220;221;222;223;224;225;226;227;228;229;230;231;232;233;234;235;236;237;238;239;240;241;242;243;244;245;246;247;248;249;250;251;252;253;254;255;256;257;258;259;260;261;262;263;264;265;266;267;268;269;270;271;272;273;274;275;276;277;278;279;280;281;282;283;284;285;286;287;288;289;290;291;292;293;294;295;296;297;298;299;300;301;302;303;304;305;306;307;308;309;310;311;312;313;314;315;316;317;318;319;320;321;322;323;324;325;328;329;346;347;348;349;350;351;352;353;354;336;337;338;339;340;341;342;355;356;326;327;357;358;359;330;331;332;333;334;335;343;344;345;360;361;362;363;364;365;366;367;368;369;370;371;372;373;374;375;376;377;378;379;380;381;382;383;384;385;386;387;388;389;390;391;392;393;394;395;396;397;398;399;400;401;402;403;404;405;406;407;408;409;410;411;412;413;414;415;416;417;418;419;420;421;422;423;424;425;426;427;428;429;430;431;432;433;434;435;436;437;438;439;440;441;442;443;444;445;446;447;448;449;450;451;452;453;454;455;456;457;458;459;460;461;462;463;464;465;466;467;468;469;470;471;472;473;474;475;476;477;478;479;480;481;482;483;484;485;486;487;488;489;490;491;492;493;494;495;496;497;498;499;500;501;502;503;504;505;506;507;508;509;510;511;512;513;514;515;516;517;518;519;520;521;522;523;524;525;526;527;528;529;530;531;532;533;534;535;536;559;560;561;562;563;564;565;566;567;568;569;570;571;572;573;574;575;576;577;578;579;580;581;582;583;584;585;586;587;588;589;590;591;592;593;594;595;596;597;598;599;600;601;602;603;604;605;606;607;608;609;610;611;612;613;614;615;616;617;618;619;620;621;622;623;624;625;626;627;628;629;630;631;632;633;634;635;636;637;638;639;640;641;642;643;644;645;646;647;648;649;650;651;652;653;654;655;656;657;658;659;660;661;662;663;664;665;666;667;668;669;670;671;672;673;674;675;676;677;678;679;680;681;682;683;684;685;686;687;688;689;690;691;692;693;694;695;696;697;698;699;700;701;702;703;704;705;706;707;708;709;710;711;712;713;714;715;716;717;718;719;720;721;722;723;724;725;726;727;728;729;730;731;732;733;734;735;736;737;738;739;740;741;742;743;744;745;746;747;748;749;750;751;752;753;754;755;756;757;758;759;760;761;762;763;764;765;766;767;768;769;770;771;772;773;774;775;776;777;778;779;780;781;782;783;784;785;786;787;788;789;790;791;792;793;794;795;796;797;798;799;800;801;802;803;804;805;806;807;808;809;810;811;812;813;814;815;816;817;818;819;820;821;822;823;824;825;826;827;828;829;830;831;832;833;834;835;836;837;838;839;840;841;842;843;844;845;846;847;848;849;850;851;852;853;854;855;856;857;858;859;860;861;862;863;864;865;866;867;868;869;870;871;872;873;874;875;876;877;878;879;880;881;882;883;884;885;886;887;888;889;890;891;892;893;894;895;896;897;898;899;900;901;902;903;904;905;906;907;908;909;910;911;912;913;914;915;916;917;918;919;920;921;922;923;924;925;926;927;928;929;930;931;932;933;934;935;936;937;938;939;940;941;942;943;944;945;946;947;948;949;950;951;952;953;954;955;956;957;958;541;542;543;537;538;539;540;550;551;552;553;554;555;556;557;558;544;545;548;549;1111;1112;961;962;963;964;965;966;967;968;969;970;971;972;973;974;975;976;977;978;995;996;1021;1022;1023;546;547;1024;1025;979;980;985;986;997;998;981;982;983;984;987;988;989;990;991;992;993;994;999;1000;1001;1002;1013;1014;1003;1004;1005;1006;1007;1008;1015;1016;1017;1018;1019;1020;1026;1009;1010;1011;1012;1027;1028;1029;1030;1031;1032;1033;1034;1035;1036;1037;1038;1039;1040;1041;1042;1043;1044;1045;1046;1047;1048;1049;1050;1051;1052;1053;1054;1055;1056;1057;1058;1059;1060;1061;1062;1063;1064;1065;1066;1067;1068;1069;1070;1071;1072;1073;1074;1075;1113;1114;1115;1116;1117;1118;1119;1076;1077;1078;1079;1080;1081;1082;1083;1084;1085;1086;1087;1088;1089;1090;1091;1092;1093;1094;1095;1096;1097;1098;1099;1100;1101;1102;1103;1104;1105;1106;1107;1108;1109;1110;1120;1121;1122;1123;1124;959;960;1125;1126;1127;1128;1129;1130;1131;1132;1133;1134;1135;1136;1137;1138;1139;1140;1141;1142;1143;1144;1145;1146;1147;1148;1149;1150;1151;1152;1153;1154;1155;1156;1157;1158;1159;1160;1161;1162;1163;1164;1165;1166;1167;1168;1169;1170;1171;1172;1173;1174;1175;1176;1177;1178;1179;1180;1181;1182;1183;1184;1185;1186;1187;1188;1189;1190;1191;1192;1193;1194;1195;1196;1197;1198;1199;1200;1201;1202;1203;1204;1205;1206;1207;1208;1209;1210;1211;1212;1213;1214;1215;1216;1217;1218;1219;1220;1221;1222;1223;1224;1225;1226;1227;1228;1229;1230;1231;1232;1233;1234;1235;1236;1237;1238;1239;1240;1241;1242;1243;1244;1245;1246;1247;1248;1249;1250;1251;1252;1253;1254;1255;1256;1257;1258;1259;1260;1261;1262;1263;1264;1265;1266;1267;1268;1269;1270;1271;1272;1273;1274;1275;1276;1277;1278;1279;1280;1281;1282;1283;1284;1285;1286;1287;1288;1289;1290;1291;1292;1293;1294;1295;1296;1297;1298;1299;1300;1301;1302;1303;1304;1305;1306;1307;1308;1309;1310;1311;1312;1313;1314;1315;1316;1317;1318;1319;1320;1321;1322;1323;1324;1325;1326;1327;1328;1329;1330;1331;1332;1333;1334;1335;1336;1337;1338;1339;1340;1341;1342;1343;1344;1345;1346;1347;1348;1349;1350;1351;1352;1353;1354;1355;1356;1357;1358;1359;1360;1361;1362;1363;1364;1365;1366;1367;1368;1369;1370;1371;1372;1373;1374;1375;1376;1377;1378;1379;1380;1381;1382;1383;1384;1385;1386;1387;1388;1389;1390;1391;1392;1393;1394;1395;1396;1397;1398;1399;1400;1401;1402;1403;1404;1405;1406;1407;1408;1409;1410;1411;1412;1413;1414;1415;1416;1417;1418;1419;1420;1421;1422;1423;1424;1425;1426;1427;1428;1429;1430;1431;1432;1433;1434;1435;1436;1437;1438;1439;1440;1441;1442;1443;1444;1445;1446;1447;1448;1449;1450;1451;1452;1453;1454;1455;1456;1457;1458;1459;1460;1461;1462;1463;1464;1465;1466;1467;1468;1469;1470;1471;1472;1473;1474;1475;1476;1477;1478;1479;1480;1481;1482;1483;1484;1485;1486;1487;1488;1489;1490;1491;1492;1493;1494;1495;1496;1497;1498;1499;1500;1501;1502;1503;1504;1505;1506;1507;1508;1509;1510;1511;1512;1513;1514;1515;1516;1517;1518;1519;1520;1521;1522;1523;1524;1525;1526;1527;1528;1529;1530;1531;1532;1533;1534;1535;1536;1537;1538;1539;1540;1541;1542;1543;1544;1545;1546;1547;1548;1549;1550;1551;1552;1553;1554;1555;1556;1557;1558;
|]

let decision i = decision lr1_to_lr0.(i)
