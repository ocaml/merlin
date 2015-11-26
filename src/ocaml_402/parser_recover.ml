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
  | Action : action -> decision
  | Parent : (int -> action) -> decision


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
     | 988 -> derive_expr
     | 976 | 1016 | 1026 | 1010 | 1012 | 1027 | 1043 | 1046 | 1056 | 1068 | 1117 | 1076 | 1079 | 1091 | 1096 | 1098 | 1101 | 1104 | 1123 | 1124 | 1166 | 1168 | 1173 | 1183 | 1252 | 1276 | 1278 -> Pop
     | _ -> raise Not_found)
  | 564 -> Parent (function
     | 944 -> derive_functor_args
     | 448 | 452 | 563 | 707 | 709 | 1194 | 1198 | 1444 | 1452 | 1492 -> Pop
     | _ -> raise Not_found)
  | 92 -> Parent (function
     | 93 -> derive_mod_ext_longident
     | 91 | 158 | 749 | 752 | 869 | 917 -> Pop
     | _ -> raise Not_found)
  | 898 -> Parent (function
     | 902 -> Shift (T T_LIDENT)
     | 897 | 905 | 909 | 913 | 1354 | 1518 -> Pop
     | _ -> raise Not_found)
  | 702 -> Parent (function
     | 768 -> Shift (T T_RPAREN)
     | 701 | 764 -> Pop
     | _ -> raise Not_found)
  | 717 -> Parent (function
     | 719 -> derive_functor_args
     | 570 | 704 | 716 | 765 | 777 | 783 | 798 | 1196 | 1265 | 1474 -> Pop
     | _ -> raise Not_found)
  | 716 -> Parent (function
     | 716 -> derive_module_type
     | 570 | 704 | 719 | 765 | 777 | 783 | 798 | 1196 | 1265 | 1474 -> Pop
     | _ -> raise Not_found)
  | 695 -> Parent (function
     | 695 -> derive_signature
     | 571 | 932 | 1551 -> Pop
     | _ -> raise Not_found)
  | 533 -> Parent (function
     | 533 -> derive_simple_expr
     | 23 | 382 | 384 | 386 | 388 | 389 | 397 | 398 | 438 | 442 | 444 | 453 | 472 | 480 | 481 | 485 | 486 | 488 | 489 | 490 | 492 | 493 | 495 | 497 | 502 | 524 | 528 | 532 | 953 | 958 | 542 | 539 | 550 | 551 | 556 | 557 | 544 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1022 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1059 | 1067 | 1113 | 1116 | 1078 | 1082 | 1084 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1105 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1344 | 1345 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1456 | 1458 | 1462 | 1466 | 1555 -> Pop
     | _ -> raise Not_found)
  | 529 -> Parent (function
     | 532 | 1128 | 1130 -> derive_ext_attributes
     | 23 | 382 | 384 | 386 | 388 | 396 | 398 | 438 | 442 | 444 | 445 | 453 | 472 | 480 | 481 | 485 | 490 | 495 | 497 | 502 | 524 | 528 | 562 | 539 | 550 | 556 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1067 | 1113 | 1116 | 1078 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1122 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1458 | 1462 | 1466 | 1555 -> Pop
     | _ -> raise Not_found)
  | 525 -> Parent (function
     | 528 | 1134 -> derive_ext_attributes
     | 23 | 382 | 384 | 386 | 388 | 396 | 398 | 438 | 442 | 444 | 445 | 453 | 472 | 480 | 481 | 485 | 490 | 495 | 497 | 502 | 524 | 532 | 562 | 539 | 550 | 556 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1067 | 1113 | 1116 | 1078 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1122 | 1128 | 1130 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1458 | 1462 | 1466 | 1555 -> Pop
     | _ -> raise Not_found)
  | 496 -> Parent (function
     | 497 | 1165 -> derive_ext_attributes
     | 23 | 382 | 384 | 386 | 388 | 396 | 398 | 438 | 442 | 444 | 445 | 453 | 472 | 480 | 481 | 485 | 490 | 495 | 502 | 524 | 528 | 532 | 562 | 539 | 550 | 556 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1067 | 1113 | 1116 | 1078 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1458 | 1462 | 1466 | 1555 -> Pop
     | _ -> raise Not_found)
  | 494 -> Parent (function
     | 495 -> derive_ext_attributes
     | 23 | 382 | 384 | 386 | 388 | 389 | 396 | 397 | 398 | 438 | 442 | 444 | 445 | 453 | 472 | 480 | 481 | 485 | 486 | 488 | 489 | 490 | 492 | 493 | 497 | 502 | 524 | 528 | 532 | 533 | 562 | 953 | 958 | 542 | 539 | 550 | 551 | 556 | 557 | 544 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1022 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1059 | 1067 | 1113 | 1116 | 1078 | 1082 | 1084 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1105 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1344 | 1345 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1456 | 1458 | 1462 | 1466 | 1555 -> Pop
     | _ -> raise Not_found)
  | 493 -> Parent (function
     | 493 -> derive_simple_expr
     | 23 | 382 | 384 | 386 | 388 | 389 | 397 | 398 | 438 | 442 | 444 | 453 | 472 | 480 | 481 | 485 | 486 | 488 | 489 | 490 | 492 | 495 | 497 | 502 | 524 | 528 | 532 | 533 | 953 | 958 | 542 | 539 | 550 | 551 | 556 | 557 | 544 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1022 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1059 | 1067 | 1113 | 1116 | 1078 | 1082 | 1084 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1105 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1344 | 1345 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1456 | 1458 | 1462 | 1466 | 1555 -> Pop
     | _ -> raise Not_found)
  | 490 -> Parent (function
     | 490 -> derive_expr
     | 23 | 382 | 384 | 386 | 388 | 389 | 396 | 397 | 398 | 438 | 442 | 444 | 445 | 453 | 472 | 480 | 481 | 485 | 486 | 488 | 489 | 492 | 493 | 495 | 497 | 502 | 524 | 528 | 532 | 533 | 562 | 953 | 958 | 542 | 539 | 550 | 551 | 556 | 557 | 544 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1022 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1059 | 1067 | 1113 | 1116 | 1078 | 1082 | 1084 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1105 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1344 | 1345 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1456 | 1458 | 1462 | 1466 | 1555 -> Pop
     | _ -> raise Not_found)
  | 489 -> Parent (function
     | 489 -> derive_simple_expr
     | 23 | 382 | 384 | 386 | 388 | 389 | 396 | 397 | 398 | 438 | 442 | 444 | 445 | 453 | 472 | 480 | 481 | 485 | 486 | 488 | 490 | 492 | 493 | 495 | 497 | 502 | 524 | 528 | 532 | 533 | 562 | 953 | 958 | 542 | 539 | 550 | 551 | 556 | 557 | 544 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1022 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1059 | 1067 | 1113 | 1116 | 1078 | 1082 | 1084 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1105 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1344 | 1345 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1456 | 1458 | 1462 | 1466 | 1555 -> Pop
     | _ -> raise Not_found)
  | 488 -> Parent (function
     | 488 | 1177 -> derive_simple_expr
     | 23 | 382 | 384 | 386 | 388 | 389 | 396 | 397 | 398 | 438 | 442 | 444 | 445 | 453 | 472 | 480 | 481 | 485 | 486 | 489 | 490 | 492 | 493 | 495 | 497 | 502 | 524 | 528 | 532 | 533 | 562 | 953 | 958 | 542 | 539 | 550 | 551 | 556 | 557 | 544 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1022 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1059 | 1067 | 1113 | 1116 | 1078 | 1082 | 1084 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1105 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1344 | 1345 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1456 | 1458 | 1462 | 1466 | 1555 -> Pop
     | _ -> raise Not_found)
  | 486 -> Parent (function
     | 486 -> derive_record_expr
     | 23 | 382 | 384 | 386 | 388 | 389 | 396 | 397 | 398 | 438 | 442 | 444 | 445 | 453 | 472 | 480 | 481 | 485 | 488 | 489 | 490 | 492 | 493 | 495 | 497 | 502 | 524 | 528 | 532 | 533 | 562 | 953 | 958 | 542 | 539 | 550 | 551 | 556 | 557 | 544 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1022 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1059 | 1067 | 1113 | 1116 | 1078 | 1082 | 1084 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1105 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1344 | 1345 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1456 | 1458 | 1462 | 1466 | 1555 -> Pop
     | _ -> raise Not_found)
  | 481 -> Parent (function
     | 481 -> Shift (T T_RBRACKET)
     | 23 | 382 | 384 | 386 | 388 | 389 | 396 | 397 | 398 | 438 | 442 | 444 | 445 | 453 | 472 | 480 | 485 | 486 | 488 | 489 | 490 | 492 | 493 | 495 | 497 | 502 | 524 | 528 | 532 | 533 | 562 | 953 | 958 | 542 | 539 | 550 | 551 | 556 | 557 | 544 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1022 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1059 | 1067 | 1113 | 1116 | 1078 | 1082 | 1084 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1105 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1344 | 1345 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1456 | 1458 | 1462 | 1466 | 1555 -> Pop
     | _ -> raise Not_found)
  | 474 -> Parent (function
     | 480 -> Shift (T T_OPEN)
     | 382 | 384 | 386 | 388 | 396 | 398 | 438 | 442 | 444 | 445 | 453 | 472 | 481 | 485 | 490 | 495 | 497 | 502 | 524 | 528 | 532 | 562 | 539 | 550 | 556 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1067 | 1113 | 1116 | 1078 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1458 | 1462 | 1466 | 1555 -> Pop
     | _ -> raise Not_found)
  | 475 -> Parent (function
     | 474 -> derive_expr_open
     | 1488 -> Pop
     | _ -> raise Not_found)
  | 452 -> Parent (function
     | 452 -> derive_module_expr
     | 448 | 563 | 707 | 709 | 944 | 1194 | 1198 | 1444 | 1452 | 1492 -> Pop
     | _ -> raise Not_found)
  | 445 -> Parent (function
     | 445 -> Shift (T T_RPAREN)
     | 23 | 382 | 384 | 386 | 388 | 396 | 398 | 438 | 442 | 444 | 453 | 472 | 480 | 481 | 485 | 490 | 495 | 497 | 502 | 524 | 528 | 532 | 562 | 539 | 550 | 556 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1067 | 1113 | 1116 | 1078 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1458 | 1462 | 1466 | 1555 -> Pop
     | _ -> raise Not_found)
  | 443 -> Parent (function
     | 444 -> derive_ext_attributes
     | 23 | 382 | 384 | 386 | 388 | 396 | 398 | 438 | 442 | 445 | 453 | 472 | 480 | 481 | 485 | 490 | 495 | 497 | 502 | 524 | 528 | 532 | 562 | 539 | 550 | 556 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1067 | 1113 | 1116 | 1078 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1458 | 1462 | 1466 | 1555 -> Pop
     | _ -> raise Not_found)
  | 441 -> Parent (function
     | 442 -> derive_ext_attributes
     | 23 | 382 | 384 | 386 | 388 | 396 | 398 | 438 | 444 | 445 | 453 | 472 | 480 | 481 | 485 | 490 | 495 | 497 | 502 | 524 | 528 | 532 | 562 | 539 | 550 | 556 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1067 | 1113 | 1116 | 1078 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1458 | 1462 | 1466 | 1555 -> Pop
     | _ -> raise Not_found)
  | 398 -> Parent (function
     | 398 -> Shift (T T_BARRBRACKET)
     | 23 | 382 | 384 | 386 | 388 | 389 | 396 | 397 | 438 | 442 | 444 | 445 | 453 | 472 | 480 | 481 | 485 | 486 | 488 | 489 | 490 | 492 | 493 | 495 | 497 | 502 | 524 | 528 | 532 | 533 | 562 | 953 | 958 | 542 | 539 | 550 | 551 | 556 | 557 | 544 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1022 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1059 | 1067 | 1113 | 1116 | 1078 | 1082 | 1084 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1105 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1344 | 1345 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1456 | 1458 | 1462 | 1466 | 1555 -> Pop
     | _ -> raise Not_found)
  | 387 -> Parent (function
     | 388 -> derive_ext_attributes
     | 23 | 382 | 384 | 386 | 396 | 398 | 438 | 442 | 444 | 445 | 453 | 472 | 480 | 481 | 485 | 490 | 495 | 497 | 502 | 524 | 528 | 532 | 562 | 539 | 550 | 556 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1067 | 1113 | 1116 | 1078 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1458 | 1462 | 1466 | 1555 -> Pop
     | _ -> raise Not_found)
  | 385 -> Parent (function
     | 386 -> derive_ext_attributes
     | 23 | 382 | 384 | 388 | 396 | 398 | 438 | 442 | 444 | 445 | 453 | 472 | 480 | 481 | 485 | 490 | 495 | 497 | 502 | 524 | 528 | 532 | 562 | 539 | 550 | 556 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1067 | 1113 | 1116 | 1078 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1458 | 1462 | 1466 | 1555 -> Pop
     | _ -> raise Not_found)
  | 383 -> Parent (function
     | 384 | 1387 -> derive_ext_attributes
     | 23 | 382 | 386 | 388 | 396 | 398 | 438 | 442 | 444 | 445 | 453 | 472 | 480 | 481 | 485 | 490 | 495 | 497 | 502 | 524 | 528 | 532 | 562 | 539 | 550 | 556 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1067 | 1113 | 1116 | 1078 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1382 | 1384 | 1394 | 1396 | 1440 | 1448 | 1458 | 1462 | 1466 | 1555 -> Pop
     | _ -> raise Not_found)
  | 328 -> Parent (function
     | 329 -> derive_pattern
     | 325 | 355 | 327 | 331 | 363 | 366 | 369 | 377 | 379 | 381 | 404 | 501 | 508 | 527 | 531 | 1030 -> Pop
     | _ -> raise Not_found)
  | 314 -> Parent (function
     | 314 -> derive_pattern
     | 82 | 112 | 287 | 288 | 300 | 302 | 324 | 328 | 347 | 352 | 326 | 330 | 365 | 376 | 378 | 403 | 458 | 500 | 507 | 516 | 526 | 530 | 966 | 1036 | 1145 | 1161 | 1206 | 1215 | 1286 | 1290 | 1333 | 1376 | 1380 | 1486 | 1490 | 1534 | 1538 -> Pop
     | _ -> raise Not_found)
  | 289 -> Parent (function
     | 288 | 481 -> Reduce 91
     | 1420 -> Pop
     | _ -> raise Not_found)
  | 288 -> Parent (function
     | 288 -> Shift (T T_RBRACKET)
     | 82 | 112 | 287 | 300 | 301 | 302 | 314 | 324 | 328 | 347 | 352 | 326 | 330 | 365 | 376 | 378 | 403 | 458 | 459 | 500 | 504 | 507 | 516 | 520 | 523 | 526 | 530 | 966 | 1036 | 1145 | 1148 | 1149 | 1153 | 1161 | 1206 | 1215 | 1218 | 1231 | 1286 | 1290 | 1309 | 1333 | 1340 | 1341 | 1376 | 1380 | 1486 | 1490 | 1515 | 1522 | 1534 | 1538 -> Pop
     | _ -> raise Not_found)
  | 287 -> Parent (function
     | 287 -> Shift (T T_BARRBRACKET)
     | 82 | 112 | 288 | 300 | 301 | 302 | 314 | 324 | 328 | 347 | 352 | 326 | 330 | 365 | 376 | 378 | 403 | 458 | 459 | 500 | 504 | 507 | 516 | 520 | 523 | 526 | 530 | 966 | 1036 | 1145 | 1148 | 1149 | 1153 | 1161 | 1206 | 1215 | 1218 | 1231 | 1286 | 1290 | 1309 | 1333 | 1340 | 1341 | 1376 | 1380 | 1486 | 1490 | 1515 | 1522 | 1534 | 1538 -> Pop
     | _ -> raise Not_found)
  | 213 -> Parent (function
     | 214 | 262 | 273 | 276 -> derive_core_type2
     | 212 -> Pop
     | _ -> raise Not_found)
  | 184 -> Parent (function
     | 184 | 185 -> Shift (T T_BAR)
     | 139 | 152 | 153 | 154 | 164 | 172 | 177 | 180 | 183 | 197 | 202 | 205 | 209 | 213 | 219 | 229 | 251 | 261 | 268 | 272 | 275 | 357 | 406 | 418 | 434 | 464 | 509 | 574 | 602 | 605 | 606 | 617 | 618 | 625 | 627 | 630 | 631 | 644 | 669 | 671 | 739 | 743 | 803 | 827 | 838 | 843 | 848 | 857 | 859 | 866 | 878 | 880 | 900 | 903 | 907 | 910 | 915 | 968 | 1221 | 1223 | 1225 | 1234 | 1239 | 1244 | 1301 | 1306 | 1310 | 1313 | 1336 | 1362 | 1497 -> Pop
     | _ -> raise Not_found)
  | 185 -> Parent (function
     | 184 -> derive_row_field_list
     | 910 -> Pop
     | _ -> raise Not_found)
  | 178 -> Parent (function
     | 180 -> derive_opt_bar
     | 139 | 152 | 153 | 154 | 164 | 172 | 177 | 183 | 184 | 185 | 197 | 202 | 205 | 209 | 213 | 219 | 229 | 251 | 261 | 268 | 272 | 275 | 357 | 406 | 418 | 434 | 464 | 509 | 574 | 602 | 605 | 606 | 617 | 618 | 625 | 627 | 630 | 631 | 644 | 669 | 671 | 739 | 743 | 803 | 827 | 838 | 843 | 848 | 857 | 859 | 866 | 878 | 880 | 897 | 900 | 902 | 903 | 905 | 907 | 909 | 910 | 913 | 915 | 968 | 1221 | 1223 | 1225 | 1234 | 1239 | 1244 | 1301 | 1306 | 1310 | 1313 | 1336 | 1354 | 1362 | 1497 | 1518 -> Pop
     | _ -> raise Not_found)
  | 154 -> Parent (function
     | 154 -> derive_core_type_comma_list
     | 139 | 152 | 153 | 164 | 172 | 177 | 180 | 183 | 184 | 185 | 197 | 202 | 205 | 209 | 213 | 219 | 229 | 251 | 261 | 268 | 272 | 275 | 357 | 406 | 418 | 434 | 464 | 509 | 574 | 606 | 618 | 631 | 644 | 669 | 671 | 739 | 743 | 803 | 827 | 838 | 843 | 848 | 857 | 859 | 866 | 878 | 880 | 910 | 968 | 1221 | 1223 | 1225 | 1234 | 1239 | 1244 | 1301 | 1306 | 1310 | 1313 | 1336 | 1362 | 1497 -> Pop
     | _ -> raise Not_found)
  | 150 -> Parent (function
     | 152 -> Shift (T T_LIDENT)
     | 139 | 153 | 154 | 164 | 172 | 177 | 202 | 213 | 219 | 251 | 261 | 268 | 272 | 275 | 357 | 406 | 418 | 434 | 464 | 509 | 574 | 602 | 605 | 606 | 618 | 631 | 644 | 669 | 671 | 739 | 743 | 803 | 827 | 838 | 843 | 848 | 857 | 859 | 866 | 878 | 880 | 910 | 968 | 1221 | 1223 | 1225 | 1234 | 1239 | 1244 | 1301 | 1306 | 1310 | 1313 | 1336 | 1362 | 1497 -> Pop
     | _ -> raise Not_found)
  | 24 -> Parent (function
     | 1394 | 1396 -> derive_ext_attributes
     | 23 | 382 | 384 | 386 | 388 | 396 | 398 | 438 | 442 | 444 | 445 | 453 | 472 | 480 | 481 | 485 | 490 | 495 | 497 | 502 | 524 | 528 | 532 | 562 | 539 | 550 | 556 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1067 | 1113 | 1116 | 1078 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1250 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1382 | 1384 | 1387 | 1440 | 1448 | 1458 | 1462 | 1466 | 1555 -> Pop
     | _ -> raise Not_found)
  | 1332 -> Parent (function
     | 1335 -> derive_rec_flag
     | 1327 | 1331 | 1342 | 1516 | 1520 -> Pop
     | _ -> raise Not_found)
  | 1331 -> Parent (function
     | 1331 -> derive_class_expr
     | 1327 | 1335 | 1342 | 1516 | 1520 -> Pop
     | _ -> raise Not_found)
  | 455 -> Parent (function
     | 1250 -> derive_ext_attributes
     | 382 | 384 | 386 | 388 | 396 | 398 | 438 | 442 | 444 | 445 | 453 | 472 | 480 | 481 | 485 | 490 | 495 | 497 | 502 | 524 | 528 | 532 | 562 | 539 | 550 | 556 | 549 | 964 | 970 | 973 | 975 | 977 | 995 | 1024 | 979 | 985 | 997 | 981 | 983 | 987 | 989 | 991 | 993 | 999 | 1001 | 1013 | 1003 | 1005 | 1007 | 1015 | 1017 | 1019 | 1009 | 1011 | 1028 | 1031 | 1034 | 1042 | 1047 | 1055 | 1067 | 1113 | 1116 | 1078 | 1087 | 1090 | 1092 | 1095 | 1097 | 1100 | 1103 | 1122 | 1128 | 1130 | 1134 | 1137 | 1139 | 1141 | 1156 | 1158 | 1165 | 1167 | 1177 | 1201 | 1208 | 1219 | 1228 | 1241 | 1246 | 1275 | 1277 | 1294 | 1315 | 1318 | 1323 | 1382 | 1384 | 1387 | 1394 | 1396 | 1440 | 1448 | 1458 | 1462 | 1466 | 1555 -> Pop
     | _ -> raise Not_found)
  | 1250 -> Parent (function
     | 1249 -> derive_seq_expr
     | 1487 -> Pop
     | _ -> raise Not_found)
  | 115 -> Parent (function
     | 112 | 445 -> Reduce 92
     | 302 | 396 | 516 | 1145 | 1215 | 1419 -> Pop
     | _ -> raise Not_found)
  | 112 -> Parent (function
     | 112 -> Shift (T T_RPAREN)
     | 82 | 287 | 288 | 300 | 302 | 314 | 324 | 328 | 347 | 352 | 326 | 330 | 365 | 376 | 378 | 403 | 458 | 500 | 507 | 516 | 526 | 530 | 966 | 1036 | 1145 | 1161 | 1206 | 1215 | 1286 | 1290 | 1333 | 1376 | 1380 | 1486 | 1490 | 1534 | 1538 -> Pop
     | _ -> raise Not_found)
  | 1019 -> Parent (function
     | 1020 -> derive_expr
     | 976 | 988 | 1016 | 1018 | 1026 | 1010 | 1012 | 1027 | 1043 | 1046 | 1056 | 1068 | 1117 | 1076 | 1079 | 1091 | 1096 | 1098 | 1101 | 1104 | 1123 | 1124 | 1166 | 1168 | 1173 | 1183 | 1252 | 1276 | 1278 -> Pop
     | _ -> raise Not_found)
  | 1017 -> Parent (function
     | 1018 -> derive_expr
     | 976 | 988 | 1016 | 1020 | 1026 | 1010 | 1012 | 1027 | 1043 | 1046 | 1056 | 1068 | 1117 | 1076 | 1079 | 1091 | 1096 | 1098 | 1101 | 1104 | 1123 | 1124 | 1166 | 1168 | 1173 | 1183 | 1252 | 1276 | 1278 -> Pop
     | _ -> raise Not_found)
  | 1015 -> Parent (function
     | 1016 -> derive_expr
     | 976 | 988 | 1026 | 1010 | 1012 | 1027 | 1043 | 1046 | 1056 | 1068 | 1117 | 1076 | 1079 | 1091 | 1096 | 1098 | 1101 | 1104 | 1123 | 1124 | 1166 | 1168 | 1173 | 1183 | 1252 | 1276 | 1278 -> Pop
     | _ -> raise Not_found)
  | 1013 -> Parent (function
     | 1014 -> derive_expr
     | 976 | 988 | 994 | 1002 | 1004 | 1006 | 1008 | 1016 | 1018 | 1020 | 1026 | 1010 | 1012 | 1027 | 1043 | 1046 | 1056 | 1068 | 1117 | 1076 | 1079 | 1091 | 1096 | 1098 | 1101 | 1104 | 1123 | 1124 | 1166 | 1168 | 1173 | 1183 | 1252 | 1276 | 1278 -> Pop
     | _ -> raise Not_found)
  | 1011 -> Parent (function
     | 1012 -> derive_expr
     | 1026 | 1027 | 1043 | 1046 | 1056 | 1068 | 1117 | 1076 | 1079 | 1091 | 1096 | 1098 | 1101 | 1104 | 1123 | 1124 | 1166 | 1168 | 1173 | 1183 | 1252 | 1276 -> Pop
     | _ -> raise Not_found)
  | 736 -> Action (derive_with_type_binder)
  | 1455 | 1434 -> Action (derive_val_ident)
  | 725 -> Action (derive_type_variable)
  | 682 | 601 -> Action (derive_type_kind)
  | 1546 | 1531 | 23 -> Action (derive_structure_tail)
  | 1309 -> Action (derive_strict_binding)
  | 1412 -> Action (derive_str_extension_constructors)
  | 520 -> Action (derive_simple_pattern)
  | 1456 | 1084 | 1082 | 544 | 542 | 492 | 389 -> Action (derive_simple_expr)
  | 903 | 900 -> Action (derive_simple_core_type_or_tuple_no_attr)
  | 630 | 625 -> Action (derive_simple_core_type_no_attr)
  | 932 -> Action (derive_signature)
  | 686 -> Action (derive_sig_extension_constructors)
  | 1466 | 1462 | 1458 | 1448 | 1440 | 1396 | 1394 | 1387 | 1318 | 1315 | 1294 | 1246 | 1241 | 1228 | 1208 | 1201 | 1158 | 1156 | 1141 | 1139 | 1134 | 1130 | 1128 | 1034 | 1031 | 970 | 532 | 528 | 497 | 480 | 444 | 442 | 438 | 388 | 386 | 384 -> Action (derive_seq_expr)
  | 229 | 183 | 180 -> Action (derive_row_field_list)
  | 1537 | 1533 | 1489 | 1485 | 1205 | 456 -> Action (derive_rec_flag)
  | 1410 | 1297 | 684 -> Action (derive_private_flag)
  | 1499 | 805 -> Action (derive_primitive_declaration)
  | 1545 | 1542 | 1524 | 1508 | 1506 | 1500 | 1493 | 1475 | 1473 | 1470 | 1413 | 1365 | 1363 | 1360 | 1324 | 1321 | 1037 | 935 | 926 | 888 | 884 | 882 | 875 | 861 | 845 | 814 | 808 | 799 | 784 | 778 | 776 | 773 | 771 | 698 | 687 | 668 | 575 | 428 | 422 -> Action (derive_post_item_attributes)
  | 644 | 172 -> Action (derive_poly_type_no_attr)
  | 1306 | 1301 | 857 -> Action (derive_poly_type)
  | 795 | 791 | 424 | 176 | 81 -> Action (derive_payload)
  | 505 -> Action (derive_pattern_var)
  | 530 | 526 -> Action (derive_pattern)
  | 1357 -> Action (derive_parent_binder)
  | 947 -> Action (derive_package_type)
  | 582 -> Action (derive_optional_type_variable)
  | 1370 | 1187 | 1184 | 1052 | 1048 | 1041 | 661 | 647 | 371 | 364 | 256 -> Action (derive_opt_semi)
  | 511 | 471 -> Action (derive_opt_default)
  | 1411 | 1375 | 1289 | 1285 | 685 | 659 | 499 -> Action (derive_opt_bar)
  | 504 -> Action (derive_newtype)
  | 430 -> Action (derive_mutable_flag)
  | 783 | 719 | 704 | 570 -> Action (derive_module_type)
  | 1452 | 1444 | 1198 | 944 -> Action (derive_module_expr)
  | 768 | 764 | 701 -> Action (derive_module_declaration)
  | 1469 | 1203 | 1193 -> Action (derive_module_binding_body)
  | 697 | 477 -> Action (derive_mod_longident)
  | 751 | 748 -> Action (derive_mod_ext_longident)
  | 1380 | 1376 | 1290 | 1286 | 500 -> Action (derive_match_cases)
  | 1333 | 1206 -> Action (derive_let_bindings_no_attrs)
  | 1538 | 1534 | 1490 | 1486 | 966 | 458 -> Action (derive_let_bindings)
  | 1061 -> Action (derive_lbl_expr_list)
  | 1308 | 1299 | 855 | 841 | 836 | 734 | 642 | 436 | 416 -> Action (derive_label)
  | 159 -> Action (derive_ident)
  | 1417 | 690 | 616 -> Action (derive_generalized_constructor_arguments)
  | 1153 | 1149 | 1148 | 523 -> Action (derive_fun_def)
  | 1231 | 1218 | 459 -> Action (derive_fun_binding)
  | 965 | 476 -> Action (derive_ext_attributes)
  | 1165 | 1055 | 1024 | 1003 | 1001 | 999 | 997 | 995 | 973 | 550 | 485 -> Action (derive_expr)
  | 1138 | 1125 -> Action (derive_direction_flag)
  | 743 | 739 | 251 -> Action (derive_core_type_no_attr)
  | 153 | 152 -> Action (derive_core_type2)
  | 1497 | 1313 | 1244 | 1239 | 880 | 859 | 848 | 843 | 838 | 803 | 671 | 574 | 434 | 418 | 139 -> Action (derive_core_type)
  | 664 | 653 -> Action (derive_constructor_declarations)
  | 740 | 667 -> Action (derive_constraints)
  | 868 -> Action (derive_clty_longident)
  | 1513 | 894 | 819 -> Action (derive_class_type_parameters)
  | 913 | 909 | 905 | 902 | 897 -> Action (derive_class_type)
  | 402 -> Action (derive_class_structure)
  | 825 -> Action (derive_class_signature)
  | 832 -> Action (derive_class_sig_fields)
  | 1338 | 392 -> Action (derive_class_longident)
  | 1522 | 1515 -> Action (derive_class_fun_binding)
  | 411 -> Action (derive_class_fields)
  | 1520 | 1342 | 1335 | 1327 -> Action (derive_class_expr)
  | 1421 | 691 | 645 | 636 | 394 | 253 | 222 | 218 | 199 | 79 -> Action (derive_attributes)
  | 202 -> Action (derive_amper_type_list)
  | 1374 | 1288 | 1284 | 1060 -> Action (Shift (T T_WITH))
  | 1192 -> Action (Shift (T T_UIDENT))
  | 1164 -> Action (Shift (T T_THEN))
  | 624 -> Action (Shift (T T_STAR))
  | 1355 | 1352 | 1282 | 1280 | 1271 | 1268 | 1266 | 1263 | 1261 | 1258 | 1255 | 1252 | 1217 | 1212 | 1179 | 1177 | 1147 | 1120 | 1111 | 1088 | 959 | 948 | 941 | 828 | 763 | 761 | 729 | 711 | 632 | 619 | 587 | 518 | 512 | 466 | 407 | 404 | 358 | 355 | 343 | 263 | 156 | 128 | 123 | 93 -> Action (Shift (T T_RPAREN))
  | 1391 | 1337 | 1188 | 1114 | 1093 | 1049 | 867 | 821 | 796 | 792 | 425 | 367 | 246 | 237 | 234 | 232 | 230 | 226 | 195 -> Action (Shift (T T_RBRACKET))
  | 1181 | 1117 | 1098 | 1069 | 662 | 650 | 291 -> Action (Shift (T T_RBRACE))
  | 1409 | 683 -> Action (Shift (T T_PLUSEQ))
  | 1341 | 1155 | 943 | 912 | 908 | 904 | 901 | 718 | 629 | 501 | 274 | 271 | 260 -> Action (Shift (T T_MINUSGREATER))
  | 1176 -> Action (Shift (T T_LPAREN))
  | 1514 | 1408 | 895 | 823 | 681 | 600 | 97 -> Action (Shift (T T_LIDENT))
  | 972 -> Action (Shift (T T_LESSMINUS))
  | 1334 | 1249 | 1207 | 1200 | 1033 | 527 | 479 -> Action (Shift (T T_IN))
  | 1185 | 1057 -> Action (Shift (T T_GREATERRBRACE))
  | 1173 -> Action (Shift (T T_GREATERDOT))
  | 169 -> Action (Shift (T T_GREATER))
  | 1519 | 1498 | 1465 | 1461 | 1457 | 1451 | 1447 | 1443 | 1439 | 1317 | 1314 | 1293 | 1245 | 1240 | 1227 | 1197 | 1054 | 1030 | 969 | 879 | 824 | 804 | 750 | 670 | 531 | 484 | 437 | 138 -> Action (Shift (T T_EQUAL))
  | 1556 | 1552 | 1548 -> Action (Shift (T T_EOF))
  | 1329 | 1170 | 939 | 830 | 495 | 450 | 409 -> Action (Shift (T T_END))
  | 1433 | 1312 | 1243 | 1238 | 917 | 869 | 858 | 248 | 158 | 143 | 134 | 91 -> Action (Shift (T T_DOT))
  | 1397 | 1388 | 1142 | 1135 | 1131 -> Action (Shift (T T_DONE))
  | 1395 | 1386 | 1140 | 1133 | 1129 -> Action (Shift (T T_DO))
  | 1276 | 377 -> Action (Shift (T T_COMMA))
  | 747 | 742 -> Action (Shift (T T_COLONEQUAL))
  | 1496 | 1305 | 1300 | 946 | 899 | 896 | 856 | 847 | 842 | 837 | 802 | 782 | 703 | 643 | 573 | 569 | 433 | 417 | 171 | 151 -> Action (Shift (T T_COLON))
  | 1371 | 1044 | 372 -> Action (Shift (T T_BARRBRACKET))
  | 640 | 228 -> Action (Shift (T T_BAR))
  | 637 -> Action (Reduce 99)
  | 673 -> Action (Reduce 97)
  | 881 -> Action (Reduce 96)
  | 672 -> Action (Reduce 95)
  | 1071 | 961 | 560 | 349 -> Action (Reduce 90)
  | 78 -> Action (Reduce 9)
  | 613 | 603 -> Action (Reduce 85)
  | 105 -> Action (Reduce 84)
  | 303 -> Action (Reduce 83)
  | 304 -> Action (Reduce 82)
  | 312 -> Action (Reduce 81)
  | 86 -> Action (Reduce 80)
  | 76 -> Action (Reduce 8)
  | 315 -> Action (Reduce 79)
  | 305 -> Action (Reduce 78)
  | 871 -> Action (Reduce 77)
  | 906 | 865 -> Action (Reduce 76)
  | 822 -> Action (Reduce 75)
  | 893 -> Action (Reduce 73)
  | 594 -> Action (Reduce 729)
  | 596 -> Action (Reduce 728)
  | 755 -> Action (Reduce 727)
  | 756 -> Action (Reduce 726)
  | 749 -> Action (Reduce 725)
  | 752 -> Action (Reduce 724)
  | 744 -> Action (Reduce 723)
  | 741 -> Action (Reduce 722)
  | 892 -> Action (Reduce 72)
  | 849 -> Action (Reduce 719)
  | 844 -> Action (Reduce 718)
  | 839 -> Action (Reduce 717)
  | 1295 -> Action (Reduce 716)
  | 1292 -> Action (Reduce 715)
  | 419 -> Action (Reduce 714)
  | 435 -> Action (Reduce 713)
  | 1074 -> Action (Reduce 712)
  | 535 -> Action (Reduce 711)
  | 344 -> Action (Reduce 710)
  | 889 -> Action (Reduce 71)
  | 454 | 284 -> Action (Reduce 709)
  | 250 -> Action (Reduce 708)
  | 174 -> Action (Reduce 707)
  | 727 -> Action (Reduce 703)
  | 730 -> Action (Reduce 702)
  | 745 -> Action (Reduce 701)
  | 921 -> Action (Reduce 70)
  | 220 -> Action (Reduce 7)
  | 732 -> Action (Reduce 699)
  | 733 -> Action (Reduce 698)
  | 728 -> Action (Reduce 697)
  | 919 | 98 -> Action (Reduce 696)
  | 163 | 89 -> Action (Reduce 695)
  | 663 -> Action (Reduce 692)
  | 665 -> Action (Reduce 691)
  | 651 -> Action (Reduce 690)
  | 923 -> Action (Reduce 69)
  | 654 -> Action (Reduce 689)
  | 611 -> Action (Reduce 688)
  | 666 -> Action (Reduce 687)
  | 610 -> Action (Reduce 686)
  | 655 -> Action (Reduce 685)
  | 599 -> Action (Reduce 683)
  | 676 -> Action (Reduce 682)
  | 674 -> Action (Reduce 681)
  | 1222 -> Action (Reduce 680)
  | 924 -> Action (Reduce 68)
  | 1226 -> Action (Reduce 679)
  | 1224 -> Action (Reduce 678)
  | 1432 -> Action (Reduce 675)
  | 1430 -> Action (Reduce 674)
  | 1429 -> Action (Reduce 673)
  | 1427 -> Action (Reduce 672)
  | 224 -> Action (Reduce 670)
  | 925 -> Action (Reduce 67)
  | 221 -> Action (Reduce 669)
  | 1540 -> Action (Reduce 666)
  | 1436 -> Action (Reduce 665)
  | 1544 -> Action (Reduce 663)
  | 1543 -> Action (Reduce 662)
  | 1494 -> Action (Reduce 661)
  | 1512 -> Action (Reduce 660)
  | 922 -> Action (Reduce 66)
  | 1526 -> Action (Reduce 659)
  | 1541 -> Action (Reduce 658)
  | 1476 -> Action (Reduce 657)
  | 1477 -> Action (Reduce 656)
  | 1479 -> Action (Reduce 655)
  | 1483 -> Action (Reduce 654)
  | 1503 -> Action (Reduce 653)
  | 1407 -> Action (Reduce 652)
  | 1404 -> Action (Reduce 651)
  | 1501 -> Action (Reduce 650)
  | 412 -> Action (Reduce 65)
  | 1539 | 1491 -> Action (Reduce 649)
  | 1453 -> Action (Reduce 648)
  | 1459 -> Action (Reduce 647)
  | 1463 -> Action (Reduce 646)
  | 1467 -> Action (Reduce 645)
  | 1445 -> Action (Reduce 644)
  | 1441 -> Action (Reduce 643)
  | 1449 -> Action (Reduce 642)
  | 1505 -> Action (Reduce 641)
  | 1403 -> Action (Reduce 640)
  | 1353 -> Action (Reduce 64)
  | 1406 -> Action (Reduce 639)
  | 1401 -> Action (Reduce 638)
  | 1535 | 1487 -> Action (Reduce 637)
  | 1530 -> Action (Reduce 636)
  | 1547 -> Action (Reduce 635)
  | 243 -> Action (Reduce 633)
  | 1233 -> Action (Reduce 632)
  | 1232 -> Action (Reduce 631)
  | 1220 -> Action (Reduce 630)
  | 1356 -> Action (Reduce 63)
  | 1423 -> Action (Reduce 629)
  | 1415 -> Action (Reduce 628)
  | 1416 -> Action (Reduce 627)
  | 1424 -> Action (Reduce 626)
  | 1425 -> Action (Reduce 625)
  | 1507 -> Action (Reduce 624)
  | 1509 -> Action (Reduce 623)
  | 1330 -> Action (Reduce 62)
  | 1346 -> Action (Reduce 61)
  | 1339 -> Action (Reduce 60)
  | 211 -> Action (Reduce 6)
  | 920 | 873 -> Action (Reduce 59)
  | 877 -> Action (Reduce 58)
  | 31 -> Action (Reduce 574)
  | 50 -> Action (Reduce 573)
  | 351 -> Action (Reduce 572)
  | 129 -> Action (Reduce 571)
  | 124 -> Action (Reduce 570)
  | 831 -> Action (Reduce 57)
  | 359 -> Action (Reduce 569)
  | 356 -> Action (Reduce 568)
  | 370 -> Action (Reduce 567)
  | 373 -> Action (Reduce 566)
  | 368 -> Action (Reduce 565)
  | 292 -> Action (Reduce 564)
  | 90 -> Action (Reduce 563)
  | 361 | 347 -> Action (Reduce 562)
  | 362 | 352 -> Action (Reduce 561)
  | 321 -> Action (Reduce 560)
  | 874 -> Action (Reduce 56)
  | 319 -> Action (Reduce 559)
  | 967 | 317 -> Action (Reduce 557)
  | 316 -> Action (Reduce 556)
  | 1107 -> Action (Reduce 555)
  | 1109 -> Action (Reduce 554)
  | 955 -> Action (Reduce 553)
  | 949 -> Action (Reduce 552)
  | 1272 -> Action (Reduce 551)
  | 1269 -> Action (Reduce 550)
  | 872 -> Action (Reduce 55)
  | 545 -> Action (Reduce 549)
  | 547 -> Action (Reduce 548)
  | 1058 -> Action (Reduce 547)
  | 1186 -> Action (Reduce 545)
  | 393 -> Action (Reduce 544)
  | 536 -> Action (Reduce 543)
  | 1373 -> Action (Reduce 542)
  | 1050 -> Action (Reduce 541)
  | 1189 -> Action (Reduce 540)
  | 887 -> Action (Reduce 54)
  | 1045 -> Action (Reduce 539)
  | 1369 -> Action (Reduce 538)
  | 1372 -> Action (Reduce 537)
  | 1070 -> Action (Reduce 536)
  | 1182 -> Action (Reduce 535)
  | 1118 | 1099 -> Action (Reduce 534)
  | 1115 | 1094 -> Action (Reduce 533)
  | 1112 | 1089 -> Action (Reduce 532)
  | 960 -> Action (Reduce 531)
  | 1119 | 1102 -> Action (Reduce 530)
  | 1283 -> Action (Reduce 529)
  | 1169 -> Action (Reduce 528)
  | 1171 -> Action (Reduce 527)
  | 1175 -> Action (Reduce 526)
  | 1174 -> Action (Reduce 525)
  | 1281 -> Action (Reduce 524)
  | 559 | 557 -> Action (Reduce 523)
  | 1022 | 956 -> Action (Reduce 522)
  | 957 -> Action (Reduce 521)
  | 534 -> Action (Reduce 520)
  | 886 -> Action (Reduce 52)
  | 1121 -> Action (Reduce 519)
  | 1077 | 538 -> Action (Reduce 517)
  | 1180 -> Action (Reduce 516)
  | 1178 -> Action (Reduce 515)
  | 954 -> Action (Reduce 513)
  | 916 -> Action (Reduce 512)
  | 914 -> Action (Reduce 511)
  | 208 -> Action (Reduce 510)
  | 885 -> Action (Reduce 51)
  | 911 | 204 -> Action (Reduce 509)
  | 633 | 620 -> Action (Reduce 508)
  | 635 | 622 -> Action (Reduce 507)
  | 207 -> Action (Reduce 506)
  | 157 -> Action (Reduce 505)
  | 238 -> Action (Reduce 504)
  | 235 -> Action (Reduce 503)
  | 233 -> Action (Reduce 501)
  | 231 -> Action (Reduce 500)
  | 883 -> Action (Reduce 50)
  | 196 -> Action (Reduce 499)
  | 227 -> Action (Reduce 498)
  | 266 -> Action (Reduce 497)
  | 192 -> Action (Reduce 496)
  | 146 -> Action (Reduce 495)
  | 170 -> Action (Reduce 493)
  | 267 -> Action (Reduce 492)
  | 193 -> Action (Reduce 491)
  | 188 -> Action (Reduce 490)
  | 862 -> Action (Reduce 49)
  | 149 -> Action (Reduce 488)
  | 264 -> Action (Reduce 487)
  | 190 -> Action (Reduce 486)
  | 100 -> Action (Reduce 485)
  | 101 -> Action (Reduce 484)
  | 102 -> Action (Reduce 483)
  | 104 -> Action (Reduce 482)
  | 103 -> Action (Reduce 481)
  | 107 -> Action (Reduce 480)
  | 846 -> Action (Reduce 48)
  | 108 -> Action (Reduce 479)
  | 109 -> Action (Reduce 478)
  | 111 -> Action (Reduce 477)
  | 110 -> Action (Reduce 476)
  | 322 -> Action (Reduce 475)
  | 937 -> Action (Reduce 474)
  | 936 -> Action (Reduce 473)
  | 890 -> Action (Reduce 472)
  | 928 -> Action (Reduce 471)
  | 800 -> Action (Reduce 470)
  | 876 -> Action (Reduce 47)
  | 934 -> Action (Reduce 469)
  | 779 -> Action (Reduce 468)
  | 780 -> Action (Reduce 467)
  | 786 -> Action (Reduce 466)
  | 772 -> Action (Reduce 465)
  | 774 -> Action (Reduce 464)
  | 811 -> Action (Reduce 463)
  | 680 -> Action (Reduce 462)
  | 677 -> Action (Reduce 461)
  | 809 -> Action (Reduce 460)
  | 833 -> Action (Reduce 46)
  | 576 -> Action (Reduce 459)
  | 813 -> Action (Reduce 458)
  | 597 -> Action (Reduce 457)
  | 679 -> Action (Reduce 456)
  | 592 -> Action (Reduce 455)
  | 933 -> Action (Reduce 454)
  | 938 -> Action (Reduce 453)
  | 693 -> Action (Reduce 451)
  | 689 -> Action (Reduce 450)
  | 694 -> Action (Reduce 449)
  | 815 -> Action (Reduce 448)
  | 1029 -> Action (Reduce 447)
  | 1027 -> Action (Reduce 445)
  | 198 -> Action (Reduce 444)
  | 225 -> Action (Reduce 443)
  | 194 -> Action (Reduce 442)
  | 189 -> Action (Reduce 441)
  | 1075 -> Action (Reduce 440)
  | 829 -> Action (Reduce 44)
  | 1062 -> Action (Reduce 439)
  | 788 -> Action (Reduce 438)
  | 789 -> Action (Reduce 437)
  | 807 -> Action (Reduce 427)
  | 806 -> Action (Reduce 426)
  | 429 -> Action (Reduce 425)
  | 426 -> Action (Reduce 423)
  | 252 -> Action (Reduce 422)
  | 255 -> Action (Reduce 421)
  | 860 -> Action (Reduce 420)
  | 408 -> Action (Reduce 42)
  | 863 -> Action (Reduce 419)
  | 1390 -> Action (Reduce 418)
  | 381 -> Action (Reduce 417)
  | 242 -> Action (Reduce 416)
  | 245 -> Action (Reduce 415)
  | 514 -> Action (Reduce 413)
  | 366 -> Action (Reduce 412)
  | 369 -> Action (Reduce 411)
  | 379 | 327 -> Action (Reduce 410)
  | 405 -> Action (Reduce 41)
  | 325 -> Action (Reduce 409)
  | 346 -> Action (Reduce 408)
  | 354 -> Action (Reduce 407)
  | 360 -> Action (Reduce 406)
  | 331 -> Action (Reduce 405)
  | 329 -> Action (Reduce 403)
  | 348 -> Action (Reduce 402)
  | 353 -> Action (Reduce 401)
  | 323 -> Action (Reduce 400)
  | 145 -> Action (Reduce 40)
  | 345 -> Action (Reduce 399)
  | 318 -> Action (Reduce 398)
  | 1557 -> Action (Reduce 397)
  | 1359 -> Action (Reduce 395)
  | 281 -> Action (Reduce 394)
  | 279 -> Action (Reduce 393)
  | 277 -> Action (Reduce 392)
  | 278 -> Action (Reduce 391)
  | 130 -> Action (Reduce 390)
  | 142 -> Action (Reduce 39)
  | 585 -> Action (Reduce 385)
  | 588 -> Action (Reduce 384)
  | 675 -> Action (Reduce 383)
  | 590 -> Action (Reduce 381)
  | 591 -> Action (Reduce 380)
  | 1349 -> Action (Reduce 38)
  | 586 -> Action (Reduce 379)
  | 1438 -> Action (Reduce 378)
  | 1211 -> Action (Reduce 374)
  | 1347 -> Action (Reduce 37)
  | 1523 -> Action (Reduce 36)
  | 114 -> Action (Reduce 352)
  | 952 | 285 -> Action (Reduce 351)
  | 306 -> Action (Reduce 350)
  | 1521 -> Action (Reduce 35)
  | 307 -> Action (Reduce 349)
  | 308 -> Action (Reduce 348)
  | 309 -> Action (Reduce 347)
  | 310 -> Action (Reduce 346)
  | 397 | 116 -> Action (Reduce 345)
  | 699 -> Action (Reduce 343)
  | 519 -> Action (Reduce 342)
  | 239 -> Action (Reduce 341)
  | 240 -> Action (Reduce 340)
  | 1517 -> Action (Reduce 34)
  | 187 -> Action (Reduce 339)
  | 161 -> Action (Reduce 336)
  | 162 -> Action (Reduce 335)
  | 757 -> Action (Reduce 334)
  | 758 -> Action (Reduce 333)
  | 762 -> Action (Reduce 332)
  | 708 -> Action (Reduce 331)
  | 753 -> Action (Reduce 330)
  | 1368 -> Action (Reduce 33)
  | 721 -> Action (Reduce 329)
  | 940 -> Action (Reduce 328)
  | 720 -> Action (Reduce 327)
  | 785 -> Action (Reduce 326)
  | 715 -> Action (Reduce 325)
  | 713 -> Action (Reduce 324)
  | 1256 -> Action (Reduce 323)
  | 1262 -> Action (Reduce 322)
  | 1259 -> Action (Reduce 321)
  | 1253 -> Action (Reduce 320)
  | 1264 -> Action (Reduce 319)
  | 1267 -> Action (Reduce 318)
  | 712 -> Action (Reduce 316)
  | 945 -> Action (Reduce 315)
  | 451 -> Action (Reduce 314)
  | 714 -> Action (Reduce 313)
  | 769 -> Action (Reduce 312)
  | 767 -> Action (Reduce 311)
  | 766 -> Action (Reduce 310)
  | 1367 -> Action (Reduce 31)
  | 1481 -> Action (Reduce 309)
  | 1482 -> Action (Reduce 308)
  | 1204 -> Action (Reduce 307)
  | 1199 -> Action (Reduce 306)
  | 1195 -> Action (Reduce 305)
  | 1471 -> Action (Reduce 304)
  | 136 -> Action (Reduce 303)
  | 84 -> Action (Reduce 302)
  | 94 -> Action (Reduce 301)
  | 160 | 96 -> Action (Reduce 300)
  | 1366 -> Action (Reduce 30)
  | 88 -> Action (Reduce 299)
  | 1316 -> Action (Reduce 298)
  | 1319 -> Action (Reduce 297)
  | 1320 -> Action (Reduce 296)
  | 1302 -> Action (Reduce 295)
  | 1307 -> Action (Reduce 294)
  | 259 -> Action (Reduce 292)
  | 258 -> Action (Reduce 291)
  | 1162 -> Action (Reduce 290)
  | 1325 -> Action (Reduce 29)
  | 1163 -> Action (Reduce 289)
  | 1157 -> Action (Reduce 288)
  | 1159 -> Action (Reduce 287)
  | 1237 -> Action (Reduce 286)
  | 1236 -> Action (Reduce 285)
  | 510 -> Action (Reduce 284)
  | 508 -> Action (Reduce 283)
  | 963 -> Action (Reduce 282)
  | 473 -> Action (Reduce 281)
  | 1210 -> Action (Reduce 280)
  | 1364 -> Action (Reduce 28)
  | 1039 -> Action (Reduce 279)
  | 1040 -> Action (Reduce 278)
  | 971 -> Action (Reduce 277)
  | 1032 -> Action (Reduce 276)
  | 1242 -> Action (Reduce 275)
  | 1247 -> Action (Reduce 274)
  | 1248 -> Action (Reduce 273)
  | 1038 -> Action (Reduce 272)
  | 298 -> Action (Reduce 271)
  | 297 -> Action (Reduce 270)
  | 1322 -> Action (Reduce 27)
  | 293 -> Action (Reduce 268)
  | 299 -> Action (Reduce 267)
  | 363 -> Action (Reduce 266)
  | 1065 -> Action (Reduce 264)
  | 1063 -> Action (Reduce 263)
  | 1066 -> Action (Reduce 262)
  | 1068 -> Action (Reduce 261)
  | 522 -> Action (Reduce 260)
  | 427 -> Action (Reduce 26)
  | 521 -> Action (Reduce 259)
  | 468 -> Action (Reduce 258)
  | 467 -> Action (Reduce 257)
  | 515 -> Action (Reduce 256)
  | 513 -> Action (Reduce 255)
  | 1214 -> Action (Reduce 254)
  | 1213 -> Action (Reduce 253)
  | 1108 -> Action (Reduce 252)
  | 1106 -> Action (Reduce 251)
  | 462 -> Action (Reduce 250)
  | 1361 -> Action (Reduce 25)
  | 1073 | 137 -> Action (Reduce 249)
  | 487 | 133 -> Action (Reduce 248)
  | 465 -> Action (Reduce 247)
  | 463 -> Action (Reduce 246)
  | 553 -> Action (Reduce 245)
  | 1083 -> Action (Reduce 244)
  | 1081 -> Action (Reduce 243)
  | 554 -> Action (Reduce 242)
  | 1085 -> Action (Reduce 241)
  | 649 -> Action (Reduce 240)
  | 1343 -> Action (Reduce 24)
  | 652 -> Action (Reduce 239)
  | 646 -> Action (Reduce 238)
  | 735 | 166 -> Action (Reduce 237)
  | 793 -> Action (Reduce 236)
  | 1553 -> Action (Reduce 235)
  | 1549 -> Action (Reduce 234)
  | 127 -> Action (Reduce 233)
  | 148 | 126 -> Action (Reduce 232)
  | 628 -> Action (Reduce 231)
  | 634 -> Action (Reduce 230)
  | 1348 -> Action (Reduce 23)
  | 623 -> Action (Reduce 229)
  | 760 -> Action (Reduce 227)
  | 759 -> Action (Reduce 226)
  | 567 -> Action (Reduce 224)
  | 942 -> Action (Reduce 223)
  | 1151 -> Action (Reduce 221)
  | 1150 -> Action (Reduce 220)
  | 1351 -> Action (Reduce 22)
  | 1144 -> Action (Reduce 219)
  | 1229 -> Action (Reduce 218)
  | 1230 -> Action (Reduce 217)
  | 797 -> Action (Reduce 216)
  | 1056 -> Action (Reduce 215)
  | 1183 -> Action (Reduce 214)
  | 254 -> Action (Reduce 213)
  | 1422 -> Action (Reduce 212)
  | 692 -> Action (Reduce 211)
  | 247 -> Action (Reduce 210)
  | 1345 -> Action (Reduce 21)
  | 1393 -> Action (Reduce 209)
  | 395 -> Action (Reduce 208)
  | 1043 -> Action (Reduce 206)
  | 1046 -> Action (Reduce 205)
  | 478 -> Action (Reduce 204)
  | 1124 -> Action (Reduce 203)
  | 1123 -> Action (Reduce 202)
  | 1278 | 1010 -> Action (Reduce 201)
  | 976 -> Action (Reduce 200)
  | 1350 -> Action (Reduce 20)
  | 1021 -> Action (Reduce 199)
  | 410 -> Action (Reduce 198)
  | 1172 -> Action (Reduce 197)
  | 543 -> Action (Reduce 196)
  | 1026 -> Action (Reduce 195)
  | 1101 -> Action (Reduce 194)
  | 1096 -> Action (Reduce 193)
  | 1091 -> Action (Reduce 192)
  | 1104 -> Action (Reduce 191)
  | 1025 -> Action (Reduce 190)
  | 1344 -> Action (Reduce 19)
  | 1110 -> Action (Reduce 189)
  | 1012 -> Action (Reduce 188)
  | 1020 -> Action (Reduce 187)
  | 1018 -> Action (Reduce 186)
  | 1016 -> Action (Reduce 185)
  | 988 -> Action (Reduce 184)
  | 1006 -> Action (Reduce 183)
  | 994 -> Action (Reduce 182)
  | 1008 -> Action (Reduce 181)
  | 986 -> Action (Reduce 180)
  | 931 -> Action (Reduce 18)
  | 978 -> Action (Reduce 179)
  | 990 -> Action (Reduce 178)
  | 992 -> Action (Reduce 177)
  | 980 -> Action (Reduce 176)
  | 982 -> Action (Reduce 175)
  | 984 -> Action (Reduce 174)
  | 996 -> Action (Reduce 173)
  | 998 -> Action (Reduce 172)
  | 1000 -> Action (Reduce 171)
  | 1002 -> Action (Reduce 170)
  | 930 -> Action (Reduce 17)
  | 1004 -> Action (Reduce 169)
  | 1014 -> Action (Reduce 167)
  | 1132 -> Action (Reduce 166)
  | 1389 -> Action (Reduce 165)
  | 1166 -> Action (Reduce 164)
  | 1168 -> Action (Reduce 163)
  | 558 -> Action (Reduce 162)
  | 1023 -> Action (Reduce 161)
  | 974 -> Action (Reduce 160)
  | 927 -> Action (Reduce 16)
  | 1377 -> Action (Reduce 159)
  | 1287 -> Action (Reduce 158)
  | 1152 -> Action (Reduce 157)
  | 1154 -> Action (Reduce 156)
  | 1160 -> Action (Reduce 155)
  | 1190 -> Action (Reduce 154)
  | 1202 -> Action (Reduce 153)
  | 1035 -> Action (Reduce 152)
  | 1209 -> Action (Reduce 151)
  | 1105 -> Action (Reduce 150)
  | 1529 -> Action (Reduce 15)
  | 551 -> Action (Reduce 149)
  | 1136 -> Action (Reduce 148)
  | 1143 -> Action (Reduce 147)
  | 1398 -> Action (Reduce 146)
  | 1383 -> Action (Reduce 145)
  | 1385 -> Action (Reduce 144)
  | 1381 -> Action (Reduce 143)
  | 1378 -> Action (Reduce 142)
  | 1291 -> Action (Reduce 141)
  | 1251 -> Action (Reduce 140)
  | 1528 -> Action (Reduce 14)
  | 1076 -> Action (Reduce 139)
  | 1079 -> Action (Reduce 138)
  | 1525 -> Action (Reduce 13)
  | 20 -> Action (Reduce 121)
  | 223 -> Action (Reduce 12)
  | 3 -> Action (Reduce 118)
  | 217 -> Action (Reduce 115)
  | 212 -> Action (Reduce 114)
  | 626 -> Action (Reduce 113)
  | 621 -> Action (Reduce 112)
  | 210 -> Action (Reduce 111)
  | 206 -> Action (Reduce 110)
  | 269 -> Action (Reduce 109)
  | 270 -> Action (Reduce 108)
  | 276 | 273 | 262 | 214 -> Action (Reduce 107)
  | 203 -> Action (Reduce 103)
  | 241 -> Action (Reduce 102)
  | 615 -> Action (Reduce 101)
  | 638 -> Action (Reduce 100)
  | 1392 -> Action (Reduce 10)
  | 1536 | 1532 | 1527 | 1518 | 1516 | 1511 | 1510 | 1504 | 1502 | 1495 | 1492 | 1488 | 1484 | 1480 | 1478 | 1474 | 1472 | 1468 | 1464 | 1460 | 1454 | 1450 | 1446 | 1442 | 1437 | 1435 | 1431 | 1428 | 1426 | 1420 | 1419 | 1418 | 1414 | 1405 | 1402 | 1400 | 1399 | 1384 | 1382 | 1379 | 1362 | 1358 | 1354 | 1340 | 1336 | 1328 | 1326 | 1323 | 1311 | 1310 | 1304 | 1303 | 1298 | 1296 | 1279 | 1277 | 1275 | 1274 | 1273 | 1270 | 1265 | 1260 | 1257 | 1254 | 1235 | 1234 | 1225 | 1223 | 1221 | 1219 | 1216 | 1215 | 1196 | 1194 | 1191 | 1167 | 1161 | 1146 | 1145 | 1137 | 1127 | 1126 | 1122 | 1116 | 1113 | 1103 | 1100 | 1097 | 1095 | 1092 | 1090 | 1087 | 1086 | 1080 | 1078 | 1072 | 1067 | 1064 | 1059 | 1053 | 1051 | 1047 | 1042 | 1036 | 1028 | 1009 | 1007 | 1005 | 993 | 991 | 989 | 985 | 983 | 981 | 979 | 977 | 975 | 968 | 964 | 962 | 958 | 953 | 951 | 950 | 929 | 918 | 915 | 910 | 907 | 891 | 878 | 870 | 866 | 864 | 854 | 853 | 852 | 851 | 850 | 840 | 835 | 834 | 827 | 826 | 820 | 818 | 817 | 816 | 812 | 810 | 801 | 798 | 794 | 790 | 787 | 781 | 777 | 775 | 770 | 765 | 754 | 746 | 738 | 737 | 731 | 726 | 724 | 723 | 722 | 710 | 709 | 707 | 706 | 705 | 700 | 696 | 688 | 678 | 669 | 660 | 658 | 657 | 656 | 648 | 641 | 639 | 631 | 627 | 618 | 617 | 614 | 612 | 609 | 608 | 607 | 606 | 605 | 604 | 602 | 598 | 595 | 593 | 589 | 584 | 583 | 581 | 580 | 579 | 578 | 577 | 572 | 571 | 568 | 566 | 565 | 563 | 562 | 561 | 556 | 555 | 552 | 549 | 548 | 546 | 541 | 540 | 539 | 537 | 524 | 517 | 516 | 509 | 507 | 506 | 503 | 502 | 498 | 491 | 483 | 482 | 472 | 470 | 469 | 464 | 461 | 460 | 457 | 453 | 449 | 448 | 447 | 446 | 440 | 439 | 432 | 431 | 423 | 421 | 420 | 415 | 414 | 413 | 406 | 403 | 401 | 400 | 399 | 396 | 391 | 390 | 382 | 380 | 378 | 376 | 375 | 374 | 365 | 357 | 350 | 342 | 341 | 340 | 339 | 338 | 337 | 336 | 335 | 334 | 333 | 332 | 330 | 326 | 324 | 320 | 313 | 311 | 302 | 301 | 300 | 296 | 295 | 294 | 290 | 286 | 283 | 282 | 280 | 275 | 272 | 268 | 265 | 261 | 257 | 249 | 244 | 236 | 219 | 216 | 215 | 209 | 205 | 201 | 200 | 197 | 191 | 186 | 182 | 181 | 179 | 177 | 175 | 173 | 168 | 167 | 165 | 164 | 155 | 147 | 144 | 141 | 140 | 135 | 132 | 131 | 125 | 122 | 121 | 120 | 119 | 118 | 117 | 113 | 106 | 99 | 95 | 87 | 85 | 83 | 82 | 80 | 77 | 75 | 74 | 73 | 72 | 71 | 70 | 69 | 68 | 67 | 66 | 65 | 64 | 63 | 62 | 61 | 60 | 59 | 58 | 57 | 56 | 55 | 54 | 53 | 52 | 51 | 49 | 48 | 47 | 46 | 45 | 44 | 43 | 42 | 41 | 40 | 39 | 38 | 37 | 36 | 35 | 34 | 33 | 32 | 30 | 29 | 28 | 27 | 26 | 25 | 19 | 18 | 17 | 16 | 15 | 14 | 13 | 12 | 11 | 10 | 9 | 8 | 7 | 6 | 5 | 4 | 2 | 1 -> Action (Pop)
  | _ -> raise Not_found

let lr1_to_lr0 =
  [|0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25;26;27;28;29;30;31;32;33;34;35;36;37;38;39;40;41;42;43;44;45;46;47;48;49;50;51;52;53;54;55;56;57;58;59;60;61;62;63;64;65;66;67;68;69;70;71;72;73;74;75;76;77;78;79;80;81;82;83;84;85;86;87;88;89;90;91;92;93;94;95;96;97;98;99;100;101;102;103;104;105;106;107;108;109;110;111;112;113;114;115;116;117;118;119;120;121;122;123;124;125;126;127;128;129;130;131;132;133;134;135;136;137;138;139;140;141;142;143;144;145;146;147;148;149;150;151;152;153;154;155;156;157;158;159;160;161;162;163;164;165;166;167;168;169;170;171;172;173;174;175;176;177;178;179;180;181;182;183;184;185;186;187;188;189;190;191;192;193;194;195;196;197;198;199;200;201;202;203;204;205;206;207;208;209;210;211;212;213;214;215;216;217;218;219;220;221;222;223;224;225;226;227;228;229;230;231;232;233;234;235;236;237;238;239;240;241;242;243;244;245;246;247;248;249;250;251;252;253;254;255;256;257;258;259;260;261;262;263;264;265;266;267;268;269;270;271;272;273;274;275;276;277;278;279;280;281;282;283;284;285;286;287;288;289;290;291;292;293;294;295;296;297;298;299;300;301;302;303;304;305;306;307;308;309;310;311;312;313;314;315;316;317;318;319;320;321;322;323;324;325;328;329;346;347;348;349;350;351;352;353;354;336;337;338;339;340;341;342;355;356;326;327;357;358;359;330;331;332;333;334;335;343;344;345;360;361;362;363;364;365;366;367;368;369;370;371;372;373;374;375;376;377;378;379;380;381;382;383;384;385;386;387;388;389;390;391;392;393;394;395;396;397;398;399;400;401;402;403;404;405;406;407;408;409;410;411;412;413;414;415;416;417;418;419;420;421;422;423;424;425;426;427;428;429;430;431;432;433;434;435;436;437;438;439;440;441;442;443;444;445;446;447;448;449;450;451;452;453;454;455;456;457;458;459;460;461;462;463;464;465;466;467;468;469;470;471;472;473;474;475;476;477;478;479;480;481;482;483;484;485;486;487;488;489;490;491;492;493;494;495;496;497;498;499;500;501;502;503;504;505;506;507;508;509;510;511;512;513;514;515;516;517;518;519;520;521;522;523;524;525;526;527;528;529;530;531;532;533;534;535;536;559;560;561;562;563;564;565;566;567;568;569;570;571;572;573;574;575;576;577;578;579;580;581;582;583;584;585;586;587;588;589;590;591;592;593;594;595;596;597;598;599;600;601;602;603;604;605;606;607;608;609;610;611;612;613;614;615;616;617;618;619;620;621;622;623;624;625;626;627;628;629;630;631;632;633;634;635;636;637;638;639;640;641;642;643;644;645;646;647;648;649;650;651;652;653;654;655;656;657;658;659;660;661;662;663;664;665;666;667;668;669;670;671;672;673;674;675;676;677;678;679;680;681;682;683;684;685;686;687;688;689;690;691;692;693;694;695;696;697;698;699;700;701;702;703;704;705;706;707;708;709;710;711;712;713;714;715;716;717;718;719;720;721;722;723;724;725;726;727;728;729;730;731;732;733;734;735;736;737;738;739;740;741;742;743;744;745;746;747;748;749;750;751;752;753;754;755;756;757;758;759;760;761;762;763;764;765;766;767;768;769;770;771;772;773;774;775;776;777;778;779;780;781;782;783;784;785;786;787;788;789;790;791;792;793;794;795;796;797;798;799;800;801;802;803;804;805;806;807;808;809;810;811;812;813;814;815;816;817;818;819;820;821;822;823;824;825;826;827;828;829;830;831;832;833;834;835;836;837;838;839;840;841;842;843;844;845;846;847;848;849;850;851;852;853;854;855;856;857;858;859;860;861;862;863;864;865;866;867;868;869;870;871;872;873;874;875;876;877;878;879;880;881;882;883;884;885;886;887;888;889;890;891;892;893;894;895;896;897;898;899;900;901;902;903;904;905;906;907;908;909;910;911;912;913;914;915;916;917;918;919;920;921;922;923;924;925;926;927;928;929;930;931;932;933;934;935;936;937;938;939;940;941;942;943;944;945;946;947;948;949;950;951;952;953;954;955;956;957;958;541;542;543;537;538;539;540;550;551;552;553;554;555;556;557;558;544;545;548;549;1111;1112;961;962;963;964;965;966;967;968;969;970;971;972;973;974;975;976;977;978;995;996;1021;1022;1023;546;547;1024;1025;979;980;985;986;997;998;981;982;983;984;987;988;989;990;991;992;993;994;999;1000;1001;1002;1013;1014;1003;1004;1005;1006;1007;1008;1015;1016;1017;1018;1019;1020;1026;1009;1010;1011;1012;1027;1028;1029;1030;1031;1032;1033;1034;1035;1036;1037;1038;1039;1040;1041;1042;1043;1044;1045;1046;1047;1048;1049;1050;1051;1052;1053;1054;1055;1056;1057;1058;1059;1060;1061;1062;1063;1064;1065;1066;1067;1068;1069;1070;1071;1072;1073;1074;1075;1113;1114;1115;1116;1117;1118;1119;1076;1077;1078;1079;1080;1081;1082;1083;1084;1085;1086;1087;1088;1089;1090;1091;1092;1093;1094;1095;1096;1097;1098;1099;1100;1101;1102;1103;1104;1105;1106;1107;1108;1109;1110;1120;1121;1122;1123;1124;959;960;1125;1126;1127;1128;1129;1130;1131;1132;1133;1134;1135;1136;1137;1138;1139;1140;1141;1142;1143;1144;1145;1146;1147;1148;1149;1150;1151;1152;1153;1154;1155;1156;1157;1158;1159;1160;1161;1162;1163;1164;1165;1166;1167;1168;1169;1170;1171;1172;1173;1174;1175;1176;1177;1178;1179;1180;1181;1182;1183;1184;1185;1186;1187;1188;1189;1190;1191;1192;1193;1194;1195;1196;1197;1198;1199;1200;1201;1202;1203;1204;1205;1206;1207;1208;1209;1210;1211;1212;1213;1214;1215;1216;1217;1218;1219;1220;1221;1222;1223;1224;1225;1226;1227;1228;1229;1230;1231;1232;1233;1234;1235;1236;1237;1238;1239;1240;1241;1242;1243;1244;1245;1246;1247;1248;1249;1250;1251;1252;1253;1254;1255;1256;1257;1258;1259;1260;1261;1262;1263;1264;1265;1266;1267;1268;1269;1270;1271;1272;1273;1274;1275;1276;1277;1278;1279;1280;1281;1282;1283;1284;1285;1286;1287;1288;1289;1290;1291;1292;1293;1294;1295;1296;1297;1298;1299;1300;1301;1302;1303;1304;1305;1306;1307;1308;1309;1310;1311;1312;1313;1314;1315;1316;1317;1318;1319;1320;1321;1322;1323;1324;1325;1326;1327;1328;1329;1330;1331;1332;1333;1334;1335;1336;1337;1338;1339;1340;1341;1342;1343;1344;1345;1346;1347;1348;1349;1350;1351;1352;1353;1354;1355;1356;1357;1358;1359;1360;1361;1362;1363;1364;1365;1366;1367;1368;1369;1370;1371;1372;1373;1374;1375;1376;1377;1378;1379;1380;1381;1382;1383;1384;1385;1386;1387;1388;1389;1390;1391;1392;1393;1394;1395;1396;1397;1398;1399;1400;1401;1402;1403;1404;1405;1406;1407;1408;1409;1410;1411;1412;1413;1414;1415;1416;1417;1418;1419;1420;1421;1422;1423;1424;1425;1426;1427;1428;1429;1430;1431;1432;1433;1434;1435;1436;1437;1438;1439;1440;1441;1442;1443;1444;1445;1446;1447;1448;1449;1450;1451;1452;1453;1454;1455;1456;1457;1458;1459;1460;1461;1462;1463;1464;1465;1466;1467;1468;1469;1470;1471;1472;1473;1474;1475;1476;1477;1478;1479;1480;1481;1482;1483;1484;1485;1486;1487;1488;1489;1490;1491;1492;1493;1494;1495;1496;1497;1498;1499;1500;1501;1502;1503;1504;1505;1506;1507;1508;1509;1510;1511;1512;1513;1514;1515;1516;1517;1518;1519;1520;1521;1522;1523;1524;1525;1526;1527;1528;1529;1530;1531;1532;1533;1534;1535;1536;1537;1538;1539;1540;1541;1542;1543;1544;1545;1546;1547;1548;1549;1550;1551;1552;1553;1554;1555;1556;1557;1558;
|]

let decision i = decision lr1_to_lr0.(i)
