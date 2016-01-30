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
  Sub [derive_parse_expression; Reduce 3]
and derive_interface' = 
  Sub [derive_interface; Reduce 2]
and derive_implementation' = 
  Sub [derive_implementation; Reduce 1]
and derive_dummy' = 
  Sub [derive_dummy; Reduce 0]
and derive_with_type_binder =  Shift (N N_with_type_binder)
and derive_with_extensions =  Shift (N N_with_extensions)
and derive_with_constraints =  Shift (N N_with_constraints)
and derive_with_constraint = 
  Sub [Shift (T T_MODULE); derive_mod_longident; Shift (T T_EQUAL); derive_mod_ext_longident; Reduce 725]
and derive_virtual_flag = 
  Sub [Reduce 721]
and derive_value_type = 
  Sub [derive_label; Shift (T T_COLON); derive_core_type; Reduce 720]
and derive_value = 
  Sub [derive_override_flag; derive_mutable_flag; derive_label; Shift (T T_EQUAL); derive_seq_expr; Reduce 716]
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
  Sub [Reduce 685]
and derive_type_declarations =  Shift (N N_type_declarations)
and derive_type_declaration = 
  Sub [derive_optional_type_parameters; Shift (T T_LIDENT); derive_type_kind; derive_constraints; derive_post_item_attributes; Reduce 682]
and derive_type_constraint = 
  Sub [Shift (T T_COLONGREATER); derive_core_type; Reduce 681]
and derive_toplevel_directives = 
  Sub [Reduce 672]
and derive_tag_field = 
  Sub [derive_name_tag; derive_attributes; Reduce 671]
and derive_subtractive = 
  Sub [Shift (T T_MINUSDOT); Reduce 669]
and derive_structure_tail =  Shift (N N_structure_tail)
and derive_structure_item = 
  Sub [Shift (T T_CLASS); derive_class_declarations; Reduce 660]
and derive_structure_head =  Shift (N N_structure_head)
and derive_structure =  Shift (N N_structure)
and derive_strict_binding = 
  Sub [Shift (T T_EQUAL); derive_seq_expr; Reduce 631]
and derive_str_type_extension = 
  Sub [derive_optional_type_parameters; derive_type_longident; Shift (T T_PLUSEQ); derive_private_flag; derive_opt_bar; derive_str_extension_constructors; derive_post_item_attributes; Reduce 630]
and derive_str_extension_constructors = 
  Sub [derive_extension_constructor_declaration; Reduce 626]
and derive_str_exception_declaration = 
  Sub [derive_extension_constructor_declaration; derive_post_item_attributes; Reduce 624]
and derive_single_attr_id = 
  Sub [Shift (T T_WITH); Reduce 623]
and derive_simple_pattern_not_ident = 
  Sub [derive_constr_longident; Reduce 562]
and derive_simple_pattern =  Shift (N N_simple_pattern)
and derive_simple_labeled_expr_list = 
  Sub [derive_labeled_simple_expr; Reduce 555]
and derive_simple_expr =  Shift (N N_simple_expr)
and derive_simple_core_type_or_tuple_no_attr =  Shift (N N_simple_core_type_or_tuple_no_attr)
and derive_simple_core_type_or_tuple =  Shift (N N_simple_core_type_or_tuple)
and derive_simple_core_type_no_attr =  Shift (N N_simple_core_type_no_attr)
and derive_simple_core_type2 =  Shift (N N_simple_core_type2)
and derive_simple_core_type =  Shift (N N_simple_core_type)
and derive_signed_constant =  Shift (N N_signed_constant)
and derive_signature_item = 
  Sub [Shift (T T_CLASS); derive_class_descriptions; Reduce 472]
and derive_signature =  Shift (N N_signature)
and derive_sig_type_extension = 
  Sub [derive_optional_type_parameters; derive_type_longident; Shift (T T_PLUSEQ); derive_private_flag; derive_opt_bar; derive_sig_extension_constructors; derive_post_item_attributes; Reduce 452]
and derive_sig_extension_constructors = 
  Sub [derive_extension_constructor_declaration; Reduce 450]
and derive_sig_exception_declaration = 
  Sub [derive_extension_constructor_declaration; derive_post_item_attributes; Reduce 449]
and derive_seq_expr =  Shift (N N_seq_expr)
and derive_row_field_list =  Shift (N N_row_field_list)
and derive_row_field = 
  Sub [derive_simple_core_type; Reduce 443]
and derive_record_expr =  Shift (N N_record_expr)
and derive_rec_module_declarations = 
  Sub [derive_module_rec_declaration; Reduce 438]
and derive_rec_flag =  Shift (N N_rec_flag)
and derive_private_virtual_flags = 
  Sub [Reduce 431]
and derive_private_flag = 
  Sub [Reduce 429]
and derive_primitive_declaration =  Shift (N N_primitive_declaration)
and derive_post_item_attributes =  Shift (N N_post_item_attributes)
and derive_post_item_attribute = 
  Sub [Shift (T T_LBRACKETATAT); derive_attr_id; derive_payload; Shift (T T_RBRACKET); Reduce 424]
and derive_poly_type_no_attr =  Shift (N N_poly_type_no_attr)
and derive_poly_type =  Shift (N N_poly_type)
and derive_payload =  Shift (N N_payload)
and derive_pattern_var = 
  Sub [Shift (T T_UNDERSCORE); Reduce 415]
and derive_pattern_semi_list =  Shift (N N_pattern_semi_list)
and derive_pattern_comma_list = 
  Sub [derive_pattern; Shift (T T_COMMA); derive_pattern; Reduce 411]
and derive_pattern =  Shift (N N_pattern)
and derive_parse_expression = 
  Sub [derive_seq_expr; Shift (T T_EOF); Reduce 398]
and derive_parent_binder = 
  Sub [Reduce 397]
and derive_package_type_cstrs =  Shift (N N_package_type_cstrs)
and derive_package_type_cstr = 
  Sub [Shift (T T_TYPE); derive_label_longident; Shift (T T_EQUAL); derive_core_type; Reduce 393]
and derive_package_type =  Shift (N N_package_type)
and derive_override_flag = 
  Sub [Reduce 389]
and derive_optional_type_variable =  Shift (N N_optional_type_variable)
and derive_optional_type_parameters =  Shift (N N_optional_type_parameters)
and derive_optional_type_parameter_list =  Shift (N N_optional_type_parameter_list)
and derive_optional_type_parameter =  Shift (N N_optional_type_parameter)
and derive_option_STRING_ = 
  Sub [Reduce 378]
and derive_opt_semi = 
  Sub [Reduce 376]
and derive_opt_default = 
  Sub [Reduce 374]
and derive_opt_bar = 
  Sub [Reduce 372]
and derive_opt_ampersand = 
  Sub [Reduce 371]
and derive_operator =  Shift (N N_operator)
and derive_open_statement = 
  Sub [Shift (T T_OPEN); derive_override_flag; derive_mod_longident; derive_post_item_attributes; Reduce 344]
and derive_newtype =  Shift (N N_newtype)
and derive_name_tag_list =  Shift (N N_name_tag_list)
and derive_name_tag = 
  Sub [Shift (T T_BACKQUOTE); derive_ident; Reduce 340]
and derive_mutable_flag = 
  Sub [Reduce 338]
and derive_mty_longident =  Shift (N N_mty_longident)
and derive_module_type =  Shift (N N_module_type)
and derive_module_rec_declaration = 
  Sub [Shift (T T_UIDENT); Shift (T T_COLON); derive_module_type; derive_post_item_attributes; Reduce 327]
and derive_module_expr =  Shift (N N_module_expr)
and derive_module_declaration =  Shift (N N_module_declaration)
and derive_module_bindings =  Shift (N N_module_bindings)
and derive_module_binding_body =  Shift (N N_module_binding_body)
and derive_module_binding =  Shift (N N_module_binding)
and derive_mod_longident =  Shift (N N_mod_longident)
and derive_mod_ext_longident =  Shift (N N_mod_ext_longident)
and derive_method_ = 
  Sub [derive_override_flag; derive_private_flag; derive_label; derive_strict_binding; Reduce 297]
and derive_meth_list =  Shift (N N_meth_list)
and derive_match_cases =  Shift (N N_match_cases)
and derive_match_case = 
  Sub [derive_pattern; Shift (T T_MINUSGREATER); derive_seq_expr; Reduce 288]
and derive_lident_list =  Shift (N N_lident_list)
and derive_let_pattern =  Shift (N N_let_pattern)
and derive_let_operator = 
  Sub [Shift (T T_LETOP); Reduce 282]
and derive_let_bindings_no_attrs =  Shift (N N_let_bindings_no_attrs)
and derive_let_bindings =  Shift (N N_let_bindings)
and derive_let_binding_ = 
  Sub [derive_val_ident; derive_fun_binding; Reduce 274]
and derive_let_binding =  Shift (N N_let_binding)
and derive_lbl_pattern_list =  Shift (N N_lbl_pattern_list)
and derive_lbl_pattern = 
  Sub [derive_label_longident; Reduce 268]
and derive_lbl_expr_list =  Shift (N N_lbl_expr_list)
and derive_lbl_expr = 
  Sub [derive_label_longident; Reduce 263]
and derive_labeled_simple_pattern = 
  Sub [derive_simple_pattern; Reduce 261]
and derive_labeled_simple_expr = 
  Sub [derive_simple_expr; Reduce 252]
and derive_label_var =  Shift (N N_label_var)
and derive_label_longident =  Shift (N N_label_longident)
and derive_label_let_pattern =  Shift (N N_label_let_pattern)
and derive_label_ident =  Shift (N N_label_ident)
and derive_label_expr = 
  Sub [Shift (T T_QUESTION); derive_label_ident; Reduce 244]
and derive_label_declarations =  Shift (N N_label_declarations)
and derive_label_declaration = 
  Sub [derive_mutable_flag; derive_label; Shift (T T_COLON); derive_poly_type_no_attr; derive_attributes; Reduce 239]
and derive_label =  Shift (N N_label)
and derive_item_extension = 
  Sub [Shift (T T_LBRACKETPERCENTPERCENT); derive_attr_id; derive_payload; Shift (T T_RBRACKET); Reduce 237]
and derive_interface = 
  Sub [derive_signature; Shift (T T_EOF); Reduce 236]
and derive_implementation = 
  Sub [derive_structure; Shift (T T_EOF); Reduce 235]
and derive_ident =  Shift (N N_ident)
and derive_generalized_constructor_arguments =  Shift (N N_generalized_constructor_arguments)
and derive_functor_args =  Shift (N N_functor_args)
and derive_functor_arg_name = 
  Sub [Shift (T T_UNDERSCORE); Reduce 226]
and derive_functor_arg = 
  Sub [Shift (T T_LPAREN); Shift (T T_RPAREN); Reduce 223]
and derive_fun_def = 
  Sub [Shift (T T_MINUSGREATER); derive_seq_expr; Reduce 220]
and derive_fun_binding =  Shift (N N_fun_binding)
and derive_floating_attribute = 
  Sub [Shift (T T_LBRACKETATATAT); derive_attr_id; derive_payload; Shift (T T_RBRACKET); Reduce 217]
and derive_field_expr_list =  Shift (N N_field_expr_list)
and derive_field = 
  Sub [derive_label; Shift (T T_COLON); derive_poly_type_no_attr; derive_attributes; Reduce 214]
and derive_extension_constructor_rebind = 
  Sub [derive_constr_ident; Shift (T T_EQUAL); derive_constr_longident; derive_attributes; Reduce 213]
and derive_extension_constructor_declaration = 
  Sub [derive_constr_ident; derive_generalized_constructor_arguments; derive_attributes; Reduce 212]
and derive_extension = 
  Sub [Shift (T T_LBRACKETPERCENT); derive_attr_id; derive_payload; Shift (T T_RBRACKET); Reduce 211]
and derive_ext_attributes =  Shift (N N_ext_attributes)
and derive_expr_semi_list =  Shift (N N_expr_semi_list)
and derive_expr_open =  Shift (N N_expr_open)
and derive_expr_comma_opt_list = 
  Sub [derive_expr; Reduce 204]
and derive_expr_comma_list = 
  Sub [derive_expr; Shift (T T_COMMA); derive_expr; Reduce 202]
and derive_expr =  Shift (N N_expr)
and derive_dummy = 
  Sub [Shift (T T_OUNIT_BENCH_MODULE); Reduce 138]
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

let lr1_to_lr0 =
  [|0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25;26;27;28;29;30;31;32;33;34;35;36;37;38;39;40;41;42;43;44;45;46;47;48;49;50;51;52;53;54;55;56;57;58;59;60;61;62;63;64;65;66;67;68;69;70;71;72;73;74;75;76;77;78;79;80;81;82;83;84;85;86;87;88;89;90;91;92;93;94;95;96;97;98;99;100;101;102;103;104;105;106;107;108;109;110;111;112;113;114;115;116;117;118;119;120;121;122;123;124;125;126;127;128;129;130;131;132;133;134;135;136;137;138;139;140;141;142;143;144;145;146;147;148;149;150;151;152;153;154;155;156;157;158;159;160;161;162;163;164;165;166;167;168;169;170;171;172;173;174;175;176;177;178;179;180;181;182;183;184;185;186;187;188;189;190;191;192;193;194;195;196;197;198;199;200;201;202;203;204;205;206;207;208;209;210;211;212;213;214;215;216;217;218;219;220;221;222;223;224;225;226;227;228;229;230;231;232;233;234;235;236;237;238;239;240;241;242;243;244;245;246;247;248;249;250;251;252;253;254;255;256;257;258;259;260;261;262;263;264;265;266;267;268;269;270;271;272;273;274;275;276;277;278;279;280;281;282;283;284;285;286;287;288;289;290;291;292;293;294;295;296;297;298;299;300;301;302;303;304;305;306;307;308;309;310;311;312;313;314;315;316;317;318;319;320;321;322;323;324;325;326;329;330;347;348;349;350;351;352;353;354;355;337;338;339;340;341;342;343;356;357;327;328;358;359;360;331;332;333;334;335;336;344;345;346;361;362;363;364;365;366;367;368;369;370;371;372;373;374;375;376;377;378;379;380;381;382;383;384;385;386;387;388;389;390;391;392;393;394;395;396;397;398;399;400;401;402;403;404;405;406;407;408;409;410;411;412;413;414;415;416;417;418;419;420;421;422;423;424;425;426;427;428;429;430;431;432;433;434;435;436;437;438;439;440;441;442;443;444;445;446;447;448;449;450;451;452;453;454;455;456;457;458;459;460;461;462;463;464;465;466;467;468;469;470;471;472;473;474;475;476;477;478;479;480;481;482;483;484;485;486;487;488;489;490;491;492;493;494;495;496;497;498;499;500;501;502;503;504;505;506;507;508;509;510;511;512;513;514;515;516;517;518;519;520;521;522;523;524;525;526;527;528;529;530;531;532;533;534;535;536;537;560;561;562;563;564;565;566;567;568;569;570;571;572;573;574;575;576;577;578;579;580;581;582;583;584;585;586;587;588;589;590;591;592;593;594;595;596;597;598;599;600;601;602;603;604;605;606;607;608;609;610;611;612;613;614;615;616;617;618;619;620;621;622;623;624;625;626;627;628;629;630;631;632;633;634;635;636;637;638;639;640;641;642;643;644;645;646;647;648;649;650;651;652;653;654;655;656;657;658;659;660;661;662;663;664;665;666;667;668;669;670;671;672;673;674;675;676;677;678;679;680;681;682;683;684;685;686;687;688;689;690;691;692;693;694;695;696;697;698;699;700;701;702;703;704;705;706;707;708;709;710;711;712;713;714;715;716;717;718;719;720;721;722;723;724;725;726;727;728;729;730;731;732;733;734;735;736;737;738;739;740;741;742;743;744;745;746;747;748;749;750;751;752;753;754;755;756;757;758;759;760;761;762;763;764;765;766;767;768;769;770;771;772;773;774;775;776;777;778;779;780;781;782;783;784;785;786;787;788;789;790;791;792;793;794;795;796;797;798;799;800;801;802;803;804;805;806;807;808;809;810;811;812;813;814;815;816;817;818;819;820;821;822;823;824;825;826;827;828;829;830;831;832;833;834;835;836;837;838;839;840;841;842;843;844;845;846;847;848;849;850;851;852;853;854;855;856;857;858;859;860;861;862;863;864;865;866;867;868;869;870;871;872;873;874;875;876;877;878;879;880;881;882;883;884;885;886;887;888;889;890;891;892;893;894;895;896;897;898;899;900;901;902;903;904;905;906;907;908;909;910;911;912;913;914;915;916;917;918;919;920;921;922;923;924;925;926;927;928;929;930;931;932;933;934;935;936;937;938;939;940;941;942;943;944;945;946;947;948;949;950;951;952;953;954;955;956;957;958;959;542;543;544;538;539;540;541;551;552;553;554;555;556;557;558;559;545;546;549;550;1112;1113;962;963;964;965;966;967;968;969;970;971;972;973;974;975;976;977;978;979;996;997;1022;1023;1024;547;548;1025;1026;980;981;986;987;998;999;982;983;984;985;988;989;990;991;992;993;994;995;1000;1001;1002;1003;1014;1015;1004;1005;1006;1007;1008;1009;1016;1017;1018;1019;1020;1021;1027;1010;1011;1012;1013;1028;1029;1030;1031;1032;1033;1034;1035;1036;1037;1038;1039;1040;1041;1042;1043;1044;1045;1046;1047;1048;1049;1050;1051;1052;1053;1054;1055;1056;1057;1058;1059;1060;1061;1062;1063;1064;1065;1066;1067;1068;1069;1070;1071;1072;1073;1074;1075;1076;1114;1115;1116;1117;1118;1119;1120;1077;1078;1079;1080;1081;1082;1083;1084;1085;1086;1087;1088;1089;1090;1091;1092;1093;1094;1095;1096;1097;1098;1099;1100;1101;1102;1103;1104;1105;1106;1107;1108;1109;1110;1111;1121;1122;1123;1124;1125;960;961;1126;1127;1128;1129;1130;1131;1132;1133;1134;1135;1136;1137;1138;1139;1140;1141;1142;1143;1144;1145;1146;1147;1148;1149;1150;1151;1152;1153;1154;1155;1156;1157;1158;1159;1160;1161;1162;1163;1164;1165;1166;1167;1168;1169;1170;1171;1172;1173;1174;1175;1176;1177;1178;1179;1180;1181;1182;1183;1184;1185;1186;1187;1188;1189;1190;1191;1192;1193;1194;1195;1196;1197;1198;1199;1200;1201;1202;1203;1204;1205;1206;1207;1208;1209;1210;1211;1212;1213;1214;1215;1216;1217;1218;1219;1220;1221;1222;1223;1224;1225;1226;1227;1228;1229;1230;1231;1232;1233;1234;1235;1236;1237;1238;1239;1240;1241;1242;1243;1244;1245;1246;1247;1248;1249;1250;1251;1252;1253;1254;1255;1256;1257;1258;1259;1260;1261;1262;1263;1264;1265;1266;1267;1268;1269;1270;1271;1272;1273;1274;1275;1276;1277;1278;1279;1280;1281;1282;1283;1284;1285;1286;1287;1288;1289;1290;1291;1292;1293;1294;1295;1296;1297;1298;1299;1300;1301;1302;1303;1304;1305;1306;1307;1308;1309;1310;1311;1312;1313;1314;1315;1316;1317;1318;1319;1320;1321;1322;1323;1324;1325;1326;1327;1328;1329;1330;1331;1332;1333;1334;1335;1336;1337;1338;1339;1340;1341;1342;1343;1344;1345;1346;1347;1348;1349;1350;1351;1352;1353;1354;1355;1356;1357;1358;1359;1360;1361;1362;1363;1364;1365;1366;1367;1368;1369;1370;1371;1372;1373;1374;1375;1376;1377;1378;1379;1380;1381;1382;1383;1384;1385;1386;1387;1388;1389;1390;1391;1392;1393;1394;1395;1396;1397;1398;1399;1400;1401;1402;1403;1404;1405;1406;1407;1408;1409;1410;1411;1412;1413;1414;1415;1416;1417;1418;1419;1420;1421;1422;1423;1424;1425;1426;1427;1428;1429;1430;1431;1432;1433;1434;1435;1436;1437;1438;1439;1440;1441;1442;1443;1444;1445;1446;1447;1448;1449;1450;1451;1452;1453;1454;1455;1456;1457;1458;1459;1460;1461;1462;1463;1464;1465;1466;1467;1468;1469;1470;1471;1472;1473;1474;1475;1476;1477;1478;1479;1480;1481;1482;1483;1484;1485;1486;1487;1488;1489;1490;1491;1492;1493;1494;1495;1496;1497;1498;1499;1500;1501;1502;1503;1504;1505;1506;1507;1508;1509;1510;1511;1512;1513;1514;1515;1516;1517;1518;1519;1520;1521;1522;1523;1524;1525;1526;1527;1528;1529;1530;1531;1532;1533;1534;1535;1536;1537;1538;1539;1540;1541;1542;1543;1544;1545;1546;1547;1548;1549;1550;1551;1552;1553;1554;1555;1556;1557;1558;1559;
|]

let decision x = match lr1_to_lr0.(x) with
  | 623 -> Parent (fun x -> match lr1_to_lr0.(x) with
     | 618 | 628 | 916 -> 1, Reduce 508
     | 626 -> 1, Reduce 487
     | _ -> raise Not_found)
  | 163 -> Parent (fun x -> match lr1_to_lr0.(x) with
     | 139 | 152 | 153 | 154 | 164 | 172 | 177 | 202 | 213 | 219 | 252 | 262 | 269 | 273 | 276 | 358 | 407 | 419 | 435 | 465 | 510 | 575 | 603 | 606 | 607 | 619 | 632 | 645 | 670 | 672 | 740 | 744 | 804 | 828 | 839 | 844 | 849 | 858 | 860 | 867 | 879 | 881 | 969 | 1222 | 1224 | 1226 | 1235 | 1240 | 1245 | 1302 | 1307 | 1311 | 1314 | 1337 | 1363 | 1498 -> 1, Shift (T T_COLON)
     | 911 -> 1, Reduce 696
     | _ -> raise Not_found)
  | 1251 -> Parent (fun x -> match lr1_to_lr0.(x) with
     | 1250 -> 5, derive_seq_expr
     | 1488 -> 0, Pop
     | _ -> raise Not_found)
  | 1316 -> Action (9, derive_seq_expr)
  | 1143 | 1132 -> Action (9, Shift (T T_DONE))
  | 1142 | 1131 -> Action (8, derive_seq_expr)
  | 1315 -> Action (8, Shift (T T_EQUAL))
  | 664 -> Action (8, Reduce 693)
  | 950 -> Action (8, Reduce 553)
  | 1263 -> Action (8, Reduce 323)
  | 1243 -> Action (8, Reduce 276)
  | 1137 -> Action (8, Reduce 149)
  | 1384 -> Action (8, Reduce 146)
  | 1242 -> Action (7, derive_seq_expr)
  | 1314 -> Action (7, derive_core_type)
  | 1262 | 949 -> Action (7, Shift (T T_RPAREN))
  | 663 -> Action (7, Shift (T T_RBRACE))
  | 1136 -> Action (7, Shift (T T_DONE))
  | 1141 | 1130 -> Action (7, Shift (T T_DO))
  | 1502 -> Action (7, Reduce 651)
  | 1424 -> Action (7, Reduce 630)
  | 810 -> Action (7, Reduce 461)
  | 694 -> Action (7, Reduce 452)
  | 1320 -> Action (7, Reduce 298)
  | 1248 -> Action (7, Reduce 275)
  | 1102 -> Action (7, Reduce 195)
  | 1097 -> Action (7, Reduce 194)
  | 1092 -> Action (7, Reduce 193)
  | 1169 -> Action (7, Reduce 164)
  | 1203 -> Action (7, Reduce 154)
  | 1319 | 1247 | 1202 | 1140 | 1135 | 1129 -> Action (6, derive_seq_expr)
  | 1501 | 1414 | 809 | 688 -> Action (6, derive_post_item_attributes)
  | 948 -> Action (6, derive_package_type)
  | 662 -> Action (6, derive_opt_semi)
  | 1241 -> Action (6, Shift (T T_EQUAL))
  | 1313 -> Action (6, Shift (T T_DOT))
  | 742 -> Action (6, Reduce 723)
  | 1296 -> Action (6, Reduce 717)
  | 436 -> Action (6, Reduce 714)
  | 890 -> Action (6, Reduce 71)
  | 666 -> Action (6, Reduce 692)
  | 652 -> Action (6, Reduce 691)
  | 926 -> Action (6, Reduce 67)
  | 1477 -> Action (6, Reduce 658)
  | 1460 -> Action (6, Reduce 648)
  | 129 -> Action (6, Reduce 572)
  | 1273 -> Action (6, Reduce 552)
  | 1059 -> Action (6, Reduce 548)
  | 1051 -> Action (6, Reduce 542)
  | 1046 -> Action (6, Reduce 540)
  | 1122 -> Action (6, Reduce 520)
  | 238 -> Action (6, Reduce 505)
  | 863 -> Action (6, Reduce 49)
  | 780 -> Action (6, Reduce 469)
  | 1257 -> Action (6, Reduce 324)
  | 1260 -> Action (6, Reduce 322)
  | 768 -> Action (6, Reduce 312)
  | 1303 -> Action (6, Reduce 296)
  | 1308 -> Action (6, Reduce 295)
  | 1390 -> Action (6, Reduce 166)
  | 1378 -> Action (6, Reduce 160)
  | 928 -> Action (6, Reduce 16)
  | 1288 -> Action (6, Reduce 159)
  | 1210 -> Action (6, Reduce 152)
  | 1399 -> Action (6, Reduce 147)
  | 1382 -> Action (6, Reduce 144)
  | 1292 -> Action (6, Reduce 142)
  | 1252 -> Action (6, Reduce 141)
  | 277 -> Action (6, Reduce 105)
  | 1413 -> Action (5, derive_str_extension_constructors)
  | 687 -> Action (5, derive_sig_extension_constructors)
  | 1459 | 1295 | 1209 -> Action (5, derive_seq_expr)
  | 1500 | 806 -> Action (5, derive_primitive_declaration)
  | 1476 | 927 | 889 | 862 | 779 -> Action (5, derive_post_item_attributes)
  | 1307 | 1302 -> Action (5, derive_poly_type)
  | 765 -> Action (5, derive_module_declaration)
  | 1381 | 1377 | 1291 | 1287 -> Action (5, derive_match_cases)
  | 1139 | 1126 -> Action (5, derive_direction_flag)
  | 276 -> Action (5, derive_core_type2)
  | 1240 | 435 -> Action (5, derive_core_type)
  | 665 -> Action (5, derive_constructor_declarations)
  | 741 -> Action (5, derive_constraints)
  | 903 -> Action (5, derive_class_type)
  | 1272 | 1259 | 1256 | 1121 | 128 -> Action (5, Shift (T T_RPAREN))
  | 1050 | 237 -> Action (5, Shift (T T_RBRACKET))
  | 651 -> Action (5, Shift (T T_RBRACE))
  | 1201 -> Action (5, Shift (T T_IN))
  | 1058 -> Action (5, Shift (T T_GREATERRBRACE))
  | 1318 | 1246 -> Action (5, Shift (T T_EQUAL))
  | 1398 | 1389 -> Action (5, Shift (T T_DONE))
  | 1134 -> Action (5, Shift (T T_DO))
  | 1277 | 378 -> Action (5, Shift (T T_COMMA))
  | 947 -> Action (5, Shift (T T_COLON))
  | 1045 -> Action (5, Shift (T T_BARRBRACKET))
  | 745 -> Action (5, Reduce 724)
  | 845 -> Action (5, Reduce 719)
  | 840 -> Action (5, Reduce 718)
  | 1293 -> Action (5, Reduce 716)
  | 420 -> Action (5, Reduce 715)
  | 924 -> Action (5, Reduce 69)
  | 675 -> Action (5, Reduce 682)
  | 221 -> Action (5, Reduce 670)
  | 1404 -> Action (5, Reduce 641)
  | 1234 -> Action (5, Reduce 633)
  | 1357 -> Action (5, Reduce 63)
  | 360 -> Action (5, Reduce 570)
  | 1071 -> Action (5, Reduce 537)
  | 1119 | 1100 -> Action (5, Reduce 535)
  | 1116 | 1095 -> Action (5, Reduce 534)
  | 1113 | 1090 -> Action (5, Reduce 533)
  | 961 -> Action (5, Reduce 532)
  | 1181 -> Action (5, Reduce 517)
  | 231 -> Action (5, Reduce 501)
  | 267 -> Action (5, Reduce 498)
  | 773 -> Action (5, Reduce 466)
  | 577 -> Action (5, Reduce 460)
  | 598 -> Action (5, Reduce 458)
  | 409 -> Action (5, Reduce 42)
  | 1268 -> Action (5, Reduce 319)
  | 1158 -> Action (5, Reduce 289)
  | 972 -> Action (5, Reduce 278)
  | 514 -> Action (5, Reduce 256)
  | 1214 -> Action (5, Reduce 254)
  | 1362 -> Action (5, Reduce 25)
  | 647 -> Action (5, Reduce 239)
  | 943 -> Action (5, Reduce 224)
  | 1152 -> Action (5, Reduce 222)
  | 1352 -> Action (5, Reduce 22)
  | 1057 -> Action (5, Reduce 216)
  | 1105 -> Action (5, Reduce 192)
  | 1167 -> Action (5, Reduce 165)
  | 1191 -> Action (5, Reduce 155)
  | 1036 -> Action (5, Reduce 153)
  | 1386 -> Action (5, Reduce 145)
  | 1080 -> Action (5, Reduce 139)
  | 1526 -> Action (5, Reduce 13)
  | 263 -> Action (5, Reduce 107)
  | 1397 | 1388 | 1157 | 1035 | 971 | 533 | 529 | 481 | 439 -> Action (4, derive_seq_expr)
  | 1525 | 1361 | 772 | 669 | 576 -> Action (4, derive_post_item_attributes)
  | 858 -> Action (4, derive_poly_type)
  | 1053 | 1049 | 1042 | 648 -> Action (4, derive_opt_semi)
  | 1412 | 1376 | 1290 | 1286 | 686 | 660 -> Action (4, derive_opt_bar)
  | 1194 -> Action (4, derive_module_binding_body)
  | 1149 -> Action (4, derive_fun_def)
  | 1219 -> Action (4, derive_fun_binding)
  | 1166 | 1056 -> Action (4, derive_expr)
  | 744 | 740 -> Action (4, derive_core_type_no_attr)
  | 262 -> Action (4, derive_core_type2)
  | 1245 | 844 | 839 | 419 -> Action (4, derive_core_type)
  | 910 | 898 -> Action (4, derive_class_type)
  | 826 -> Action (4, derive_class_signature)
  | 1336 -> Action (4, derive_class_expr)
  | 646 | 218 -> Action (4, derive_attributes)
  | 1356 | 1267 | 1213 | 1180 | 1112 | 1089 | 960 | 942 | 764 | 513 | 408 | 359 -> Action (4, Shift (T T_RPAREN))
  | 1115 | 1094 | 230 -> Action (4, Shift (T T_RBRACKET))
  | 1118 | 1099 | 1070 -> Action (4, Shift (T T_RBRACE))
  | 902 | 275 -> Action (4, Shift (T T_MINUSGREATER))
  | 1250 | 1208 -> Action (4, Shift (T T_IN))
  | 1499 | 1458 | 1294 | 805 -> Action (4, Shift (T T_EQUAL))
  | 1239 -> Action (4, Shift (T T_DOT))
  | 1306 | 1301 | 434 -> Action (4, Shift (T T_COLON))
  | 750 -> Action (4, Reduce 726)
  | 753 -> Action (4, Reduce 725)
  | 655 -> Action (4, Reduce 690)
  | 1227 -> Action (4, Reduce 680)
  | 925 -> Action (4, Reduce 68)
  | 1433 -> Action (4, Reduce 676)
  | 1431 -> Action (4, Reduce 675)
  | 1430 -> Action (4, Reduce 674)
  | 1478 -> Action (4, Reduce 657)
  | 1540 | 1492 -> Action (4, Reduce 650)
  | 1454 -> Action (4, Reduce 649)
  | 1464 -> Action (4, Reduce 647)
  | 1468 -> Action (4, Reduce 646)
  | 1446 -> Action (4, Reduce 645)
  | 1442 -> Action (4, Reduce 644)
  | 1450 -> Action (4, Reduce 643)
  | 1506 -> Action (4, Reduce 642)
  | 1407 -> Action (4, Reduce 640)
  | 1536 | 1488 -> Action (4, Reduce 638)
  | 1548 -> Action (4, Reduce 636)
  | 1340 -> Action (4, Reduce 60)
  | 124 -> Action (4, Reduce 571)
  | 374 -> Action (4, Reduce 567)
  | 369 -> Action (4, Reduce 566)
  | 1270 -> Action (4, Reduce 551)
  | 873 -> Action (4, Reduce 55)
  | 1187 -> Action (4, Reduce 546)
  | 1190 -> Action (4, Reduce 541)
  | 1373 -> Action (4, Reduce 538)
  | 1284 -> Action (4, Reduce 530)
  | 1172 -> Action (4, Reduce 528)
  | 1179 -> Action (4, Reduce 516)
  | 157 -> Action (4, Reduce 506)
  | 235 -> Action (4, Reduce 504)
  | 233 -> Action (4, Reduce 502)
  | 196 -> Action (4, Reduce 500)
  | 268 -> Action (4, Reduce 493)
  | 781 -> Action (4, Reduce 468)
  | 775 -> Action (4, Reduce 465)
  | 814 -> Action (4, Reduce 459)
  | 680 -> Action (4, Reduce 457)
  | 427 -> Action (4, Reduce 424)
  | 1391 -> Action (4, Reduce 419)
  | 278 -> Action (4, Reduce 393)
  | 1522 -> Action (4, Reduce 35)
  | 700 -> Action (4, Reduce 344)
  | 520 -> Action (4, Reduce 343)
  | 709 -> Action (4, Reduce 332)
  | 722 -> Action (4, Reduce 330)
  | 786 -> Action (4, Reduce 327)
  | 1254 -> Action (4, Reduce 321)
  | 713 -> Action (4, Reduce 317)
  | 946 -> Action (4, Reduce 316)
  | 1200 -> Action (4, Reduce 307)
  | 94 -> Action (4, Reduce 302)
  | 1321 -> Action (4, Reduce 297)
  | 298 -> Action (4, Reduce 271)
  | 468 -> Action (4, Reduce 258)
  | 794 -> Action (4, Reduce 237)
  | 635 -> Action (4, Reduce 231)
  | 798 -> Action (4, Reduce 217)
  | 255 -> Action (4, Reduce 214)
  | 1423 -> Action (4, Reduce 213)
  | 248 -> Action (4, Reduce 211)
  | 411 -> Action (4, Reduce 199)
  | 1153 -> Action (4, Reduce 158)
  | 1155 -> Action (4, Reduce 157)
  | 1161 -> Action (4, Reduce 156)
  | 1077 -> Action (4, Reduce 140)
  | 217 -> Action (4, Reduce 116)
  | 274 -> Action (4, Reduce 106)
  | 1393 -> Action (4, Reduce 10)
  | 737 -> Action (3, derive_with_type_binder)
  | 1547 -> Action (3, derive_structure_tail)
  | 1310 -> Action (3, derive_strict_binding)
  | 1457 -> Action (3, derive_simple_expr)
  | 901 -> Action (3, derive_simple_core_type_or_tuple_no_attr)
  | 631 -> Action (3, derive_simple_core_type_no_attr)
  | 1467 | 1463 | 1449 | 1441 -> Action (3, derive_seq_expr)
  | 229 -> Action (3, derive_row_field_list)
  | 1411 | 685 -> Action (3, derive_private_flag)
  | 1474 | 785 | 777 | 774 | 699 -> Action (3, derive_post_item_attributes)
  | 645 -> Action (3, derive_poly_type_no_attr)
  | 1358 -> Action (3, derive_parent_binder)
  | 512 | 472 -> Action (3, derive_opt_default)
  | 720 | 705 | 571 -> Action (3, derive_module_type)
  | 1453 | 1445 | 1199 | 945 -> Action (3, derive_module_expr)
  | 752 | 749 -> Action (3, derive_mod_ext_longident)
  | 501 -> Action (3, derive_match_cases)
  | 1207 -> Action (3, derive_let_bindings_no_attrs)
  | 1539 | 1535 | 1491 | 1487 | 459 -> Action (3, derive_let_bindings)
  | 1300 -> Action (3, derive_label)
  | 1154 | 524 -> Action (3, derive_fun_def)
  | 273 | 152 -> Action (3, derive_core_type2)
  | 1498 | 804 | 575 | 139 -> Action (3, derive_core_type)
  | 654 -> Action (3, derive_constructor_declarations)
  | 668 -> Action (3, derive_constraints)
  | 869 -> Action (3, derive_clty_longident)
  | 906 -> Action (3, derive_class_type)
  | 1339 -> Action (3, derive_class_longident)
  | 1516 -> Action (3, derive_class_fun_binding)
  | 1521 -> Action (3, derive_class_expr)
  | 1422 | 254 -> Action (3, derive_attributes)
  | 202 -> Action (3, derive_amper_type_list)
  | 1375 | 1289 | 1285 -> Action (3, Shift (T T_WITH))
  | 1193 -> Action (3, Shift (T T_UIDENT))
  | 1165 -> Action (3, Shift (T T_THEN))
  | 1283 | 1269 | 1253 | 1218 | 1178 | 1148 | 712 | 519 | 467 | 156 | 123 | 93 -> Action (3, Shift (T T_RPAREN))
  | 1392 | 1189 | 797 | 793 | 426 | 368 | 247 | 234 | 232 | 195 -> Action (3, Shift (T T_RBRACKET))
  | 1156 | 909 | 261 -> Action (3, Shift (T T_MINUSGREATER))
  | 1335 | 1034 | 528 | 480 -> Action (3, Shift (T T_IN))
  | 1186 -> Action (3, Shift (T T_GREATERRBRACE))
  | 1055 | 970 | 825 | 532 | 438 -> Action (3, Shift (T T_EQUAL))
  | 1171 | 410 -> Action (3, Shift (T T_END))
  | 1244 -> Action (3, Shift (T T_DOT))
  | 1396 | 1387 -> Action (3, Shift (T T_DO))
  | 743 -> Action (3, Shift (T T_COLONEQUAL))
  | 897 | 857 | 843 | 838 | 418 -> Action (3, Shift (T T_COLON))
  | 1372 | 373 -> Action (3, Shift (T T_BARRBRACKET))
  | 638 -> Action (3, Reduce 99)
  | 674 -> Action (3, Reduce 97)
  | 882 -> Action (3, Reduce 96)
  | 673 -> Action (3, Reduce 95)
  | 78 -> Action (3, Reduce 9)
  | 872 -> Action (3, Reduce 77)
  | 823 -> Action (3, Reduce 75)
  | 597 -> Action (3, Reduce 729)
  | 756 -> Action (3, Reduce 728)
  | 850 -> Action (3, Reduce 720)
  | 893 -> Action (3, Reduce 72)
  | 1075 -> Action (3, Reduce 713)
  | 345 -> Action (3, Reduce 711)
  | 251 -> Action (3, Reduce 709)
  | 731 -> Action (3, Reduce 703)
  | 733 -> Action (3, Reduce 700)
  | 922 -> Action (3, Reduce 70)
  | 220 -> Action (3, Reduce 7)
  | 920 | 98 -> Action (3, Reduce 697)
  | 612 -> Action (3, Reduce 689)
  | 611 -> Action (3, Reduce 687)
  | 600 -> Action (3, Reduce 684)
  | 1428 -> Action (3, Reduce 673)
  | 1495 -> Action (3, Reduce 662)
  | 1513 -> Action (3, Reduce 661)
  | 1480 -> Action (3, Reduce 656)
  | 1354 -> Action (3, Reduce 64)
  | 1402 -> Action (3, Reduce 639)
  | 1416 -> Action (3, Reduce 629)
  | 1417 -> Action (3, Reduce 628)
  | 1331 -> Action (3, Reduce 62)
  | 832 -> Action (3, Reduce 57)
  | 357 -> Action (3, Reduce 569)
  | 293 -> Action (3, Reduce 565)
  | 322 -> Action (3, Reduce 561)
  | 546 -> Action (3, Reduce 550)
  | 548 -> Action (3, Reduce 549)
  | 394 -> Action (3, Reduce 545)
  | 1183 -> Action (3, Reduce 536)
  | 1120 | 1103 -> Action (3, Reduce 531)
  | 1170 -> Action (3, Reduce 529)
  | 1175 -> Action (3, Reduce 526)
  | 1282 -> Action (3, Reduce 525)
  | 1078 | 539 -> Action (3, Reduce 518)
  | 917 -> Action (3, Reduce 513)
  | 208 -> Action (3, Reduce 511)
  | 634 | 621 -> Action (3, Reduce 509)
  | 884 -> Action (3, Reduce 50)
  | 227 -> Action (3, Reduce 499)
  | 192 -> Action (3, Reduce 497)
  | 170 -> Action (3, Reduce 494)
  | 265 -> Action (3, Reduce 488)
  | 847 -> Action (3, Reduce 48)
  | 891 -> Action (3, Reduce 473)
  | 801 -> Action (3, Reduce 471)
  | 877 -> Action (3, Reduce 47)
  | 787 -> Action (3, Reduce 467)
  | 593 -> Action (3, Reduce 456)
  | 690 -> Action (3, Reduce 451)
  | 1030 -> Action (3, Reduce 448)
  | 198 -> Action (3, Reduce 445)
  | 1063 -> Action (3, Reduce 440)
  | 830 -> Action (3, Reduce 44)
  | 789 -> Action (3, Reduce 439)
  | 253 -> Action (3, Reduce 423)
  | 861 -> Action (3, Reduce 421)
  | 367 -> Action (3, Reduce 413)
  | 380 | 328 -> Action (3, Reduce 411)
  | 326 -> Action (3, Reduce 410)
  | 406 -> Action (3, Reduce 41)
  | 332 -> Action (3, Reduce 406)
  | 330 -> Action (3, Reduce 404)
  | 346 -> Action (3, Reduce 400)
  | 145 -> Action (3, Reduce 40)
  | 282 -> Action (3, Reduce 395)
  | 279 -> Action (3, Reduce 392)
  | 589 -> Action (3, Reduce 385)
  | 591 -> Action (3, Reduce 382)
  | 1348 -> Action (3, Reduce 37)
  | 161 -> Action (3, Reduce 337)
  | 763 -> Action (3, Reduce 333)
  | 754 -> Action (3, Reduce 331)
  | 941 -> Action (3, Reduce 329)
  | 1265 -> Action (3, Reduce 320)
  | 452 -> Action (3, Reduce 315)
  | 770 -> Action (3, Reduce 313)
  | 1482 -> Action (3, Reduce 310)
  | 1472 -> Action (3, Reduce 305)
  | 136 -> Action (3, Reduce 304)
  | 160 | 96 -> Action (3, Reduce 301)
  | 259 -> Action (3, Reduce 292)
  | 1163 -> Action (3, Reduce 291)
  | 1326 -> Action (3, Reduce 29)
  | 1160 -> Action (3, Reduce 288)
  | 511 -> Action (3, Reduce 285)
  | 964 -> Action (3, Reduce 283)
  | 1040 -> Action (3, Reduce 280)
  | 1365 -> Action (3, Reduce 28)
  | 1033 -> Action (3, Reduce 277)
  | 299 -> Action (3, Reduce 272)
  | 1323 -> Action (3, Reduce 27)
  | 364 -> Action (3, Reduce 267)
  | 1066 -> Action (3, Reduce 265)
  | 1069 -> Action (3, Reduce 262)
  | 428 -> Action (3, Reduce 26)
  | 1074 | 137 -> Action (3, Reduce 250)
  | 466 -> Action (3, Reduce 248)
  | 650 -> Action (3, Reduce 241)
  | 1230 -> Action (3, Reduce 219)
  | 1184 -> Action (3, Reduce 215)
  | 693 -> Action (3, Reduce 212)
  | 1394 -> Action (3, Reduce 210)
  | 1044 -> Action (3, Reduce 207)
  | 479 -> Action (3, Reduce 205)
  | 1124 -> Action (3, Reduce 203)
  | 1279 | 1011 -> Action (3, Reduce 202)
  | 977 -> Action (3, Reduce 201)
  | 1173 -> Action (3, Reduce 198)
  | 544 -> Action (3, Reduce 197)
  | 1027 -> Action (3, Reduce 196)
  | 1013 -> Action (3, Reduce 189)
  | 1021 -> Action (3, Reduce 188)
  | 1019 -> Action (3, Reduce 187)
  | 1017 -> Action (3, Reduce 186)
  | 989 -> Action (3, Reduce 185)
  | 1007 -> Action (3, Reduce 184)
  | 995 -> Action (3, Reduce 183)
  | 1009 -> Action (3, Reduce 182)
  | 987 -> Action (3, Reduce 181)
  | 979 -> Action (3, Reduce 180)
  | 991 -> Action (3, Reduce 179)
  | 993 -> Action (3, Reduce 178)
  | 981 -> Action (3, Reduce 177)
  | 983 -> Action (3, Reduce 176)
  | 985 -> Action (3, Reduce 175)
  | 997 -> Action (3, Reduce 174)
  | 999 -> Action (3, Reduce 173)
  | 1001 -> Action (3, Reduce 172)
  | 1003 -> Action (3, Reduce 171)
  | 1005 -> Action (3, Reduce 170)
  | 931 -> Action (3, Reduce 17)
  | 1015 -> Action (3, Reduce 168)
  | 1379 -> Action (3, Reduce 143)
  | 1529 -> Action (3, Reduce 14)
  | 627 -> Action (3, Reduce 114)
  | 210 -> Action (3, Reduce 112)
  | 270 -> Action (3, Reduce 110)
  | 214 -> Action (3, Reduce 108)
  | 616 -> Action (3, Reduce 101)
  | 1456 | 1435 -> Action (2, derive_val_ident)
  | 683 | 602 -> Action (2, derive_type_kind)
  | 545 | 543 | 493 -> Action (2, derive_simple_expr)
  | 626 -> Action (2, derive_simple_core_type_no_attr)
  | 1395 | 1229 | 1159 | 1032 | 498 | 445 | 443 | 389 | 387 | 385 -> Action (2, derive_seq_expr)
  | 183 | 180 -> Action (2, derive_row_field_list)
  | 1538 | 1534 | 1490 | 1486 | 1206 | 457 -> Action (2, derive_rec_flag)
  | 1546 | 1494 | 1471 | 1364 | 1325 | 1322 | 883 | 876 | 846 | 800 | 423 -> Action (2, derive_post_item_attributes)
  | 172 -> Action (2, derive_poly_type_no_attr)
  | 796 | 792 | 425 | 176 | 81 -> Action (2, derive_payload)
  | 531 | 527 | 379 -> Action (2, derive_pattern)
  | 1371 | 1188 | 1185 | 372 | 365 -> Action (2, derive_opt_semi)
  | 500 -> Action (2, derive_opt_bar)
  | 505 -> Action (2, derive_newtype)
  | 784 -> Action (2, derive_module_type)
  | 702 -> Action (2, derive_module_declaration)
  | 698 | 478 -> Action (2, derive_mod_longident)
  | 1334 -> Action (2, derive_let_bindings_no_attrs)
  | 967 -> Action (2, derive_let_bindings)
  | 1062 -> Action (2, derive_lbl_expr_list)
  | 1309 | 856 | 842 | 837 | 735 | 437 | 417 -> Action (2, derive_label)
  | 159 -> Action (2, derive_ident)
  | 1278 | 1004 | 1002 | 1000 | 998 | 996 | 974 | 486 -> Action (2, derive_expr)
  | 252 -> Action (2, derive_core_type_no_attr)
  | 164 -> Action (2, derive_core_type2)
  | 881 | 860 | 849 | 672 -> Action (2, derive_core_type)
  | 914 -> Action (2, derive_class_type)
  | 403 -> Action (2, derive_class_structure)
  | 393 -> Action (2, derive_class_longident)
  | 1343 | 1328 -> Action (2, derive_class_expr)
  | 692 | 637 | 79 -> Action (2, derive_attributes)
  | 871 | 144 | 135 -> Action (2, Shift (T T_UIDENT))
  | 1353 | 1281 | 1264 | 829 | 762 | 730 | 633 | 620 | 588 | 405 | 356 | 344 | 264 -> Action (2, Shift (T T_RPAREN))
  | 1338 | 868 | 822 | 226 -> Action (2, Shift (T T_RBRACKET))
  | 1182 | 292 -> Action (2, Shift (T T_RBRACE))
  | 1410 | 684 -> Action (2, Shift (T T_PLUSEQ))
  | 944 | 905 | 719 | 630 | 272 -> Action (2, Shift (T T_MINUSGREATER))
  | 1177 -> Action (2, Shift (T T_LPAREN))
  | 1515 | 919 | 896 | 824 | 97 -> Action (2, Shift (T T_LIDENT))
  | 1174 -> Action (2, Shift (T T_GREATERDOT))
  | 169 -> Action (2, Shift (T T_GREATER))
  | 1520 | 1466 | 1462 | 1452 | 1448 | 1444 | 1440 | 1198 | 751 | 138 -> Action (2, Shift (T T_EQUAL))
  | 1330 | 940 | 831 | 496 | 451 -> Action (2, Shift (T T_END))
  | 748 -> Action (2, Shift (T T_COLONEQUAL))
  | 1497 | 900 | 803 | 704 | 644 | 574 | 570 | 151 -> Action (2, Shift (T T_COLON))
  | 641 | 228 -> Action (2, Shift (T T_BAR))
  | 174 -> Action (2, Reduce 708)
  | 728 -> Action (2, Reduce 704)
  | 729 -> Action (2, Reduce 698)
  | 667 -> Action (2, Reduce 688)
  | 656 -> Action (2, Reduce 686)
  | 1223 -> Action (2, Reduce 681)
  | 1225 -> Action (2, Reduce 679)
  | 224 -> Action (2, Reduce 671)
  | 1541 -> Action (2, Reduce 667)
  | 1437 -> Action (2, Reduce 666)
  | 1544 -> Action (2, Reduce 663)
  | 1527 -> Action (2, Reduce 660)
  | 1484 -> Action (2, Reduce 655)
  | 1504 -> Action (2, Reduce 654)
  | 1408 -> Action (2, Reduce 653)
  | 1405 -> Action (2, Reduce 652)
  | 413 -> Action (2, Reduce 65)
  | 1531 -> Action (2, Reduce 637)
  | 1233 -> Action (2, Reduce 632)
  | 1221 -> Action (2, Reduce 631)
  | 1508 -> Action (2, Reduce 625)
  | 1510 -> Action (2, Reduce 624)
  | 878 -> Action (2, Reduce 58)
  | 90 -> Action (2, Reduce 564)
  | 1108 -> Action (2, Reduce 556)
  | 537 -> Action (2, Reduce 544)
  | 1374 -> Action (2, Reduce 543)
  | 888 -> Action (2, Reduce 54)
  | 1176 -> Action (2, Reduce 527)
  | 955 -> Action (2, Reduce 514)
  | 886 -> Action (2, Reduce 51)
  | 146 -> Action (2, Reduce 496)
  | 193 -> Action (2, Reduce 492)
  | 149 -> Action (2, Reduce 489)
  | 100 -> Action (2, Reduce 486)
  | 101 -> Action (2, Reduce 485)
  | 102 -> Action (2, Reduce 484)
  | 104 -> Action (2, Reduce 483)
  | 103 -> Action (2, Reduce 482)
  | 107 -> Action (2, Reduce 481)
  | 108 -> Action (2, Reduce 480)
  | 109 -> Action (2, Reduce 479)
  | 111 -> Action (2, Reduce 478)
  | 110 -> Action (2, Reduce 477)
  | 937 -> Action (2, Reduce 474)
  | 929 -> Action (2, Reduce 472)
  | 812 -> Action (2, Reduce 464)
  | 681 -> Action (2, Reduce 463)
  | 678 -> Action (2, Reduce 462)
  | 834 -> Action (2, Reduce 46)
  | 934 -> Action (2, Reduce 455)
  | 939 -> Action (2, Reduce 454)
  | 816 -> Action (2, Reduce 449)
  | 808 -> Action (2, Reduce 428)
  | 430 -> Action (2, Reduce 426)
  | 382 -> Action (2, Reduce 418)
  | 242 -> Action (2, Reduce 417)
  | 347 -> Action (2, Reduce 409)
  | 355 -> Action (2, Reduce 408)
  | 361 -> Action (2, Reduce 407)
  | 349 -> Action (2, Reduce 403)
  | 354 -> Action (2, Reduce 402)
  | 1558 -> Action (2, Reduce 398)
  | 1360 -> Action (2, Reduce 396)
  | 586 -> Action (2, Reduce 386)
  | 587 -> Action (2, Reduce 380)
  | 1350 -> Action (2, Reduce 38)
  | 1212 -> Action (2, Reduce 375)
  | 1524 -> Action (2, Reduce 36)
  | 239 -> Action (2, Reduce 342)
  | 187 -> Action (2, Reduce 340)
  | 1518 -> Action (2, Reduce 34)
  | 758 -> Action (2, Reduce 335)
  | 1369 -> Action (2, Reduce 33)
  | 714 -> Action (2, Reduce 325)
  | 767 -> Action (2, Reduce 311)
  | 1205 -> Action (2, Reduce 308)
  | 1196 -> Action (2, Reduce 306)
  | 1367 -> Action (2, Reduce 30)
  | 260 -> Action (2, Reduce 293)
  | 1238 -> Action (2, Reduce 287)
  | 1249 -> Action (2, Reduce 274)
  | 1039 -> Action (2, Reduce 273)
  | 522 -> Action (2, Reduce 260)
  | 469 -> Action (2, Reduce 259)
  | 516 -> Action (2, Reduce 257)
  | 1215 -> Action (2, Reduce 255)
  | 1084 -> Action (2, Reduce 245)
  | 1082 -> Action (2, Reduce 244)
  | 555 -> Action (2, Reduce 243)
  | 1086 -> Action (2, Reduce 242)
  | 1554 -> Action (2, Reduce 236)
  | 1550 -> Action (2, Reduce 235)
  | 629 -> Action (2, Reduce 232)
  | 624 -> Action (2, Reduce 230)
  | 1349 -> Action (2, Reduce 23)
  | 760 -> Action (2, Reduce 227)
  | 1151 -> Action (2, Reduce 221)
  | 1145 -> Action (2, Reduce 220)
  | 1346 -> Action (2, Reduce 21)
  | 396 -> Action (2, Reduce 209)
  | 1022 -> Action (2, Reduce 200)
  | 1351 -> Action (2, Reduce 20)
  | 1026 -> Action (2, Reduce 191)
  | 1111 -> Action (2, Reduce 190)
  | 559 -> Action (2, Reduce 163)
  | 1024 -> Action (2, Reduce 162)
  | 1106 -> Action (2, Reduce 151)
  | 223 -> Action (2, Reduce 12)
  | 243 -> Action (2, Reduce 103)
  | 1317 -> Action (10, Reduce 299)
  | 1133 -> Action (10, Reduce 167)
  | 1144 -> Action (10, Reduce 148)
  | 726 -> Action (1, derive_type_variable)
  | 1532 | 23 -> Action (1, derive_structure_tail)
  | 521 -> Action (1, derive_simple_pattern)
  | 1085 | 1083 | 390 -> Action (1, derive_simple_expr)
  | 904 -> Action (1, derive_simple_core_type_or_tuple_no_attr)
  | 933 -> Action (1, derive_signature)
  | 1298 -> Action (1, derive_private_flag)
  | 1543 | 1509 | 1507 | 1366 | 1038 | 936 | 885 | 815 | 429 -> Action (1, derive_post_item_attributes)
  | 506 -> Action (1, derive_pattern_var)
  | 353 | 348 -> Action (1, derive_pattern)
  | 583 -> Action (1, derive_optional_type_variable)
  | 257 -> Action (1, derive_opt_semi)
  | 431 -> Action (1, derive_mutable_flag)
  | 1470 | 1204 -> Action (1, derive_module_binding_body)
  | 643 -> Action (1, derive_label)
  | 1418 | 691 | 617 -> Action (1, derive_generalized_constructor_arguments)
  | 1150 -> Action (1, derive_fun_def)
  | 1232 | 460 -> Action (1, derive_fun_binding)
  | 966 | 477 -> Action (1, derive_ext_attributes)
  | 1025 | 551 -> Action (1, derive_expr)
  | 153 -> Action (1, derive_core_type2)
  | 1514 | 895 | 820 -> Action (1, derive_class_type_parameters)
  | 833 -> Action (1, derive_class_sig_fields)
  | 1523 -> Action (1, derive_class_fun_binding)
  | 412 -> Action (1, derive_class_fields)
  | 395 | 222 | 199 -> Action (1, derive_attributes)
  | 1061 -> Action (1, Shift (T T_WITH))
  | 625 -> Action (1, Shift (T T_STAR))
  | 1342 | 913 | 502 -> Action (1, Shift (T T_MINUSGREATER))
  | 1409 | 682 | 601 -> Action (1, Shift (T T_LIDENT))
  | 973 -> Action (1, Shift (T T_LESSMINUS))
  | 1228 | 1031 | 880 | 671 | 485 -> Action (1, Shift (T T_EQUAL))
  | 1557 | 1553 | 1549 -> Action (1, Shift (T T_EOF))
  | 1434 | 918 | 870 | 859 | 249 | 158 | 143 | 134 | 91 -> Action (1, Shift (T T_DOT))
  | 848 | 783 | 171 -> Action (1, Shift (T T_COLON))
  | 1072 | 962 | 561 | 350 -> Action (1, Reduce 90)
  | 614 | 604 -> Action (1, Reduce 85)
  | 105 -> Action (1, Reduce 84)
  | 304 -> Action (1, Reduce 83)
  | 305 -> Action (1, Reduce 82)
  | 313 -> Action (1, Reduce 81)
  | 86 -> Action (1, Reduce 80)
  | 76 -> Action (1, Reduce 8)
  | 316 -> Action (1, Reduce 79)
  | 306 -> Action (1, Reduce 78)
  | 907 | 866 -> Action (1, Reduce 76)
  | 595 -> Action (1, Reduce 730)
  | 894 -> Action (1, Reduce 73)
  | 757 -> Action (1, Reduce 727)
  | 536 -> Action (1, Reduce 712)
  | 285 -> Action (1, Reduce 710)
  | 746 -> Action (1, Reduce 702)
  | 734 -> Action (1, Reduce 699)
  | 89 -> Action (1, Reduce 696)
  | 677 -> Action (1, Reduce 683)
  | 1545 -> Action (1, Reduce 664)
  | 923 -> Action (1, Reduce 66)
  | 1542 -> Action (1, Reduce 659)
  | 244 -> Action (1, Reduce 634)
  | 1425 -> Action (1, Reduce 627)
  | 1426 -> Action (1, Reduce 626)
  | 1347 -> Action (1, Reduce 61)
  | 211 -> Action (1, Reduce 6)
  | 921 | 874 -> Action (1, Reduce 59)
  | 31 -> Action (1, Reduce 575)
  | 50 -> Action (1, Reduce 574)
  | 352 -> Action (1, Reduce 573)
  | 362 -> Action (1, Reduce 563)
  | 363 -> Action (1, Reduce 562)
  | 320 -> Action (1, Reduce 560)
  | 875 -> Action (1, Reduce 56)
  | 968 | 318 -> Action (1, Reduce 558)
  | 317 -> Action (1, Reduce 557)
  | 1110 -> Action (1, Reduce 555)
  | 956 -> Action (1, Reduce 554)
  | 560 | 558 -> Action (1, Reduce 524)
  | 1023 | 957 -> Action (1, Reduce 523)
  | 958 -> Action (1, Reduce 522)
  | 535 -> Action (1, Reduce 521)
  | 887 -> Action (1, Reduce 52)
  | 915 -> Action (1, Reduce 512)
  | 204 -> Action (1, Reduce 510)
  | 636 -> Action (1, Reduce 508)
  | 207 -> Action (1, Reduce 507)
  | 188 -> Action (1, Reduce 491)
  | 190 -> Action (1, Reduce 487)
  | 323 -> Action (1, Reduce 476)
  | 938 -> Action (1, Reduce 475)
  | 935 -> Action (1, Reduce 470)
  | 695 -> Action (1, Reduce 450)
  | 1028 -> Action (1, Reduce 446)
  | 225 -> Action (1, Reduce 444)
  | 912 | 194 -> Action (1, Reduce 443)
  | 189 -> Action (1, Reduce 442)
  | 1076 -> Action (1, Reduce 441)
  | 790 -> Action (1, Reduce 438)
  | 807 -> Action (1, Reduce 427)
  | 256 -> Action (1, Reduce 422)
  | 864 -> Action (1, Reduce 420)
  | 246 -> Action (1, Reduce 416)
  | 515 -> Action (1, Reduce 414)
  | 370 -> Action (1, Reduce 412)
  | 324 -> Action (1, Reduce 401)
  | 319 -> Action (1, Reduce 399)
  | 280 -> Action (1, Reduce 394)
  | 130 -> Action (1, Reduce 391)
  | 142 -> Action (1, Reduce 39)
  | 676 -> Action (1, Reduce 384)
  | 592 -> Action (1, Reduce 381)
  | 1439 -> Action (1, Reduce 379)
  | 114 -> Action (1, Reduce 353)
  | 953 | 286 -> Action (1, Reduce 352)
  | 307 -> Action (1, Reduce 351)
  | 308 -> Action (1, Reduce 350)
  | 309 -> Action (1, Reduce 349)
  | 310 -> Action (1, Reduce 348)
  | 311 -> Action (1, Reduce 347)
  | 398 | 116 -> Action (1, Reduce 346)
  | 240 -> Action (1, Reduce 341)
  | 162 -> Action (1, Reduce 336)
  | 759 -> Action (1, Reduce 334)
  | 721 -> Action (1, Reduce 328)
  | 716 -> Action (1, Reduce 326)
  | 715 -> Action (1, Reduce 314)
  | 1368 -> Action (1, Reduce 31)
  | 1483 -> Action (1, Reduce 309)
  | 84 -> Action (1, Reduce 303)
  | 88 -> Action (1, Reduce 300)
  | 1559 -> Action (1, Reduce 3)
  | 1164 -> Action (1, Reduce 290)
  | 1237 -> Action (1, Reduce 286)
  | 509 -> Action (1, Reduce 284)
  | 474 -> Action (1, Reduce 282)
  | 1211 -> Action (1, Reduce 281)
  | 1041 -> Action (1, Reduce 279)
  | 294 -> Action (1, Reduce 269)
  | 300 -> Action (1, Reduce 268)
  | 1064 -> Action (1, Reduce 264)
  | 1067 -> Action (1, Reduce 263)
  | 523 -> Action (1, Reduce 261)
  | 1109 -> Action (1, Reduce 253)
  | 1107 -> Action (1, Reduce 252)
  | 463 -> Action (1, Reduce 251)
  | 488 | 133 -> Action (1, Reduce 249)
  | 464 -> Action (1, Reduce 247)
  | 554 -> Action (1, Reduce 246)
  | 653 -> Action (1, Reduce 240)
  | 1344 -> Action (1, Reduce 24)
  | 736 | 455 | 166 -> Action (1, Reduce 238)
  | 127 -> Action (1, Reduce 234)
  | 148 | 126 -> Action (1, Reduce 233)
  | 761 -> Action (1, Reduce 228)
  | 568 -> Action (1, Reduce 225)
  | 1231 -> Action (1, Reduce 218)
  | 1047 -> Action (1, Reduce 206)
  | 1125 -> Action (1, Reduce 204)
  | 1555 -> Action (1, Reduce 2)
  | 1345 -> Action (1, Reduce 19)
  | 932 -> Action (1, Reduce 18)
  | 975 -> Action (1, Reduce 161)
  | 552 -> Action (1, Reduce 150)
  | 1530 -> Action (1, Reduce 15)
  | 20 -> Action (1, Reduce 122)
  | 3 -> Action (1, Reduce 119)
  | 212 -> Action (1, Reduce 115)
  | 622 -> Action (1, Reduce 113)
  | 206 -> Action (1, Reduce 111)
  | 271 -> Action (1, Reduce 109)
  | 203 -> Action (1, Reduce 104)
  | 241 -> Action (1, Reduce 102)
  | 639 -> Action (1, Reduce 100)
  | 1551 -> Action (1, Reduce 1)
  | 21 -> Action (1, Reduce 0)
  | 1537 | 1533 | 1528 | 1519 | 1517 | 1512 | 1511 | 1505 | 1503 | 1496 | 1493 | 1489 | 1485 | 1481 | 1479 | 1475 | 1473 | 1469 | 1465 | 1461 | 1455 | 1451 | 1447 | 1443 | 1438 | 1436 | 1432 | 1429 | 1427 | 1421 | 1420 | 1419 | 1415 | 1406 | 1403 | 1401 | 1400 | 1385 | 1383 | 1380 | 1370 | 1363 | 1359 | 1355 | 1341 | 1337 | 1333 | 1332 | 1329 | 1327 | 1324 | 1312 | 1311 | 1305 | 1304 | 1299 | 1297 | 1280 | 1276 | 1275 | 1274 | 1271 | 1266 | 1261 | 1258 | 1255 | 1236 | 1235 | 1226 | 1224 | 1222 | 1220 | 1217 | 1216 | 1197 | 1195 | 1192 | 1168 | 1162 | 1147 | 1146 | 1138 | 1128 | 1127 | 1123 | 1117 | 1114 | 1104 | 1101 | 1098 | 1096 | 1093 | 1091 | 1088 | 1087 | 1081 | 1079 | 1073 | 1068 | 1065 | 1060 | 1054 | 1052 | 1048 | 1043 | 1037 | 1029 | 1020 | 1018 | 1016 | 1014 | 1012 | 1010 | 1008 | 1006 | 994 | 992 | 990 | 988 | 986 | 984 | 982 | 980 | 978 | 976 | 969 | 965 | 963 | 959 | 954 | 952 | 951 | 930 | 916 | 911 | 908 | 899 | 892 | 879 | 867 | 865 | 855 | 854 | 853 | 852 | 851 | 841 | 836 | 835 | 828 | 827 | 821 | 819 | 818 | 817 | 813 | 811 | 802 | 799 | 795 | 791 | 788 | 782 | 778 | 776 | 771 | 769 | 766 | 755 | 747 | 739 | 738 | 732 | 727 | 725 | 724 | 723 | 718 | 717 | 711 | 710 | 708 | 707 | 706 | 703 | 701 | 697 | 696 | 689 | 679 | 670 | 661 | 659 | 658 | 657 | 649 | 642 | 640 | 632 | 628 | 619 | 618 | 615 | 613 | 610 | 609 | 608 | 607 | 606 | 605 | 603 | 599 | 596 | 594 | 590 | 585 | 584 | 582 | 581 | 580 | 579 | 578 | 573 | 572 | 569 | 567 | 566 | 565 | 564 | 563 | 562 | 557 | 556 | 553 | 550 | 549 | 547 | 542 | 541 | 540 | 538 | 534 | 530 | 526 | 525 | 518 | 517 | 510 | 508 | 507 | 504 | 503 | 499 | 497 | 495 | 494 | 492 | 491 | 490 | 489 | 487 | 484 | 483 | 482 | 476 | 475 | 473 | 471 | 470 | 465 | 462 | 461 | 458 | 456 | 454 | 453 | 450 | 449 | 448 | 447 | 446 | 444 | 442 | 441 | 440 | 433 | 432 | 424 | 422 | 421 | 416 | 415 | 414 | 407 | 404 | 402 | 401 | 400 | 399 | 397 | 392 | 391 | 388 | 386 | 384 | 383 | 381 | 377 | 376 | 375 | 371 | 366 | 358 | 351 | 343 | 342 | 341 | 340 | 339 | 338 | 337 | 336 | 335 | 334 | 333 | 331 | 329 | 327 | 325 | 321 | 315 | 314 | 312 | 303 | 302 | 301 | 297 | 296 | 295 | 291 | 290 | 289 | 288 | 287 | 284 | 283 | 281 | 269 | 266 | 258 | 250 | 245 | 236 | 219 | 216 | 215 | 213 | 209 | 205 | 201 | 200 | 197 | 191 | 186 | 185 | 184 | 182 | 181 | 179 | 178 | 177 | 175 | 173 | 168 | 167 | 165 | 155 | 154 | 150 | 147 | 141 | 140 | 132 | 131 | 125 | 122 | 121 | 120 | 119 | 118 | 117 | 115 | 113 | 112 | 106 | 99 | 95 | 92 | 87 | 85 | 83 | 82 | 80 | 77 | 75 | 74 | 73 | 72 | 71 | 70 | 69 | 68 | 67 | 66 | 65 | 64 | 63 | 62 | 61 | 60 | 59 | 58 | 57 | 56 | 55 | 54 | 53 | 52 | 51 | 49 | 48 | 47 | 46 | 45 | 44 | 43 | 42 | 41 | 40 | 39 | 38 | 37 | 36 | 35 | 34 | 33 | 32 | 30 | 29 | 28 | 27 | 26 | 25 | 24 | 19 | 18 | 17 | 16 | 15 | 14 | 13 | 12 | 11 | 10 | 9 | 8 | 7 | 6 | 5 | 4 | 2 | 1 -> Action (0, Pop)
  | _ -> raise Not_found
