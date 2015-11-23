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
  | MenhirInterpreter.N MenhirInterpreter.N_fun_def -> default_expr
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

type decision =
  | Shift  : 'a MenhirInterpreter.symbol * 'a -> decision
  | Reduce : int -> decision
  | Parent : (int -> decision) -> decision
  | Pop    : decision

let decision =
  let open MenhirInterpreter in function
  | 1374 | 1288 | 1284 | 1060 -> Shift (T T_WITH, default_value (T T_WITH))
  | 1303 | 1297 | 431 -> Shift (T T_VIRTUAL, default_value (T T_VIRTUAL))
  | 1192 | 870 | 350 | 144 | 135 | 122 | 95 -> Shift (T T_UIDENT, default_value (T T_UIDENT))
  | 705 | 700 -> Shift (T T_TYPE, default_value (T T_TYPE))
  | 1164 -> Shift (T T_THEN, default_value (T T_THEN))
  | 1464 | 1460 | 1454 | 1450 -> Shift (T T_STRING, default_value (T T_STRING))
  | 624 -> Shift (T T_STAR, default_value (T T_STAR))
  | 1419 | 1355 | 1352 | 1282 | 1280 | 1273 | 1271 | 1268 | 1266 | 1263 | 1261 | 1258 | 1255 | 1252 | 1217 | 1215 | 1212 | 1179 | 1177 | 1147 | 1145 | 1120 | 1111 | 1088 | 959 | 948 | 941 | 828 | 763 | 761 | 729 | 711 | 709 | 702 | 632 | 619 | 614 | 587 | 565 | 539 | 518 | 516 | 512 | 466 | 445 | 407 | 404 | 396 | 374 | 358 | 355 | 343 | 302 | 263 | 156 | 128 | 123 | 93 -> Shift (T T_RPAREN, default_value (T T_RPAREN))
  | 1420 | 1391 | 1337 | 1188 | 1114 | 1093 | 1049 | 867 | 821 | 796 | 792 | 481 | 425 | 367 | 288 | 246 | 237 | 234 | 232 | 230 | 226 | 195 | 181 -> Shift (T T_RBRACKET, default_value (T T_RBRACKET))
  | 1181 | 1117 | 1098 | 1069 | 662 | 650 | 291 -> Shift (T T_RBRACE, default_value (T T_RBRACE))
  | 215 -> Shift (T T_QUOTE, default_value (T T_QUOTE))
  | 1409 | 683 -> Shift (T T_PLUSEQ, default_value (T T_PLUSEQ))
  | 474 -> Shift (T T_OPEN, default_value (T T_OPEN))
  | 706 -> Shift (T T_OF, default_value (T T_OF))
  | 106 | 99 -> Shift (T T_NATIVEINT, default_value (T T_NATIVEINT))
  | 1341 | 1155 | 943 | 912 | 908 | 904 | 901 | 718 | 629 | 501 | 274 | 271 | 260 -> Shift (T T_MINUSGREATER, default_value (T T_MINUSGREATER))
  | 1274 | 1176 | 505 | 375 -> Shift (T T_LPAREN, default_value (T T_LPAREN))
  | 1514 | 1408 | 1358 | 1216 | 1146 | 918 | 898 | 895 | 823 | 681 | 600 | 517 | 150 | 97 -> Shift (T T_LIDENT, default_value (T T_LIDENT))
  | 972 -> Shift (T T_LESSMINUS, default_value (T T_LESSMINUS))
  | 1334 | 1249 | 1207 | 1200 | 1033 | 527 | 479 -> Shift (T T_IN, default_value (T T_IN))
  | 1185 | 1057 | 482 -> Shift (T T_GREATERRBRACE, default_value (T T_GREATERRBRACE))
  | 1173 -> Shift (T T_GREATERDOT, default_value (T T_GREATERDOT))
  | 169 | 165 -> Shift (T T_GREATER, default_value (T T_GREATER))
  | 1519 | 1498 | 1465 | 1461 | 1457 | 1451 | 1447 | 1443 | 1439 | 1317 | 1314 | 1293 | 1245 | 1240 | 1227 | 1197 | 1054 | 1030 | 969 | 879 | 824 | 804 | 750 | 670 | 531 | 484 | 437 | 138 -> Shift (T T_EQUAL, default_value (T T_EQUAL))
  | 1556 | 1552 | 1548 -> Shift (T T_EOF, default_value (T T_EOF))
  | 1329 | 1170 | 939 | 830 | 495 | 450 | 409 -> Shift (T T_END, default_value (T T_END))
  | 656 | 602 -> Shift (T T_DOTDOT, default_value (T T_DOTDOT))
  | 1433 | 1312 | 1243 | 1238 | 917 | 869 | 858 | 248 | 158 | 143 | 134 | 91 -> Shift (T T_DOT, default_value (T T_DOT))
  | 1397 | 1388 | 1142 | 1135 | 1131 -> Shift (T T_DONE, default_value (T T_DONE))
  | 1395 | 1386 | 1140 | 1133 | 1129 -> Shift (T T_DO, default_value (T T_DO))
  | 1276 | 377 -> Shift (T T_COMMA, default_value (T T_COMMA))
  | 747 | 742 -> Shift (T T_COLONEQUAL, default_value (T T_COLONEQUAL))
  | 1496 | 1309 | 1305 | 1300 | 946 | 899 | 896 | 856 | 847 | 842 | 837 | 802 | 782 | 703 | 643 | 573 | 569 | 433 | 417 | 171 | 151 -> Shift (T T_COLON, default_value (T T_COLON))
  | 1371 | 1044 | 398 | 372 | 287 -> Shift (T T_BARRBRACKET, default_value (T T_BARRBRACKET))
  | 640 | 228 | 184 -> Shift (T T_BAR, default_value (T T_BAR))
  | 736 -> Shift (N N_with_type_binder, default_value (N N_with_type_binder))
  | 1504 | 1405 | 1402 | 812 | 678 | 595 | 593 -> Shift (N N_with_extensions, default_value (N N_with_extensions))
  | 722 -> Shift (N N_with_constraints, default_value (N N_with_constraints))
  | 840 -> Shift (N N_virtual_flag, default_value (N N_virtual_flag))
  | 1495 | 1455 | 1434 | 1072 | 962 | 801 | 572 | 561 | 332 -> Shift (N N_val_ident, default_value (N N_val_ident))
  | 1234 -> Shift (N N_typevar_list, default_value (N N_typevar_list))
  | 725 -> Shift (N N_type_variable, default_value (N N_type_variable))
  | 723 -> Shift (N N_type_parameters, default_value (N N_type_parameters))
  | 820 | 724 -> Shift (N N_type_parameter_list, default_value (N N_type_parameter_list))
  | 731 -> Shift (N N_type_parameter, default_value (N N_type_parameter))
  | 87 -> Shift (N N_type_longident, default_value (N N_type_longident))
  | 682 | 601 -> Shift (N N_type_kind, default_value (N N_type_kind))
  | 1400 | 1399 | 579 | 577 -> Shift (N N_type_declarations, default_value (N N_type_declarations))
  | 1546 | 1531 | 23 -> Shift (N N_structure_tail, default_value (N N_structure_tail))
  | 1435 -> Shift (N N_structure_head, default_value (N N_structure_head))
  | 449 -> Shift (N N_structure, default_value (N N_structure))
  | 1412 -> Shift (N N_str_extension_constructors, default_value (N N_str_extension_constructors))
  | 520 | 301 -> Shift (N N_simple_pattern, default_value (N N_simple_pattern))
  | 1456 | 1084 | 1082 | 544 | 542 | 533 | 493 | 492 | 489 | 488 | 389 -> Shift (N N_simple_expr, default_value (N N_simple_expr))
  | 907 | 903 | 900 -> Shift (N N_simple_core_type_or_tuple_no_attr, default_value (N N_simple_core_type_or_tuple_no_attr))
  | 630 | 627 | 625 -> Shift (N N_simple_core_type_no_attr, default_value (N N_simple_core_type_no_attr))
  | 209 -> Shift (N N_simple_core_type, default_value (N N_simple_core_type))
  | 320 -> Shift (N N_signed_constant, default_value (N N_signed_constant))
  | 932 | 695 | 571 -> Shift (N N_signature, default_value (N N_signature))
  | 686 -> Shift (N N_sig_extension_constructors, default_value (N N_sig_extension_constructors))
  | 1466 | 1462 | 1458 | 1448 | 1440 | 1396 | 1394 | 1387 | 1384 | 1382 | 1323 | 1318 | 1315 | 1294 | 1250 | 1246 | 1241 | 1228 | 1219 | 1208 | 1201 | 1158 | 1156 | 1141 | 1139 | 1137 | 1134 | 1130 | 1128 | 1113 | 1092 | 1087 | 1034 | 1031 | 970 | 562 | 549 | 532 | 528 | 524 | 502 | 497 | 480 | 472 | 444 | 442 | 438 | 388 | 386 | 384 | 382 -> Shift (N N_seq_expr, default_value (N N_seq_expr))
  | 229 | 185 | 183 | 180 -> Shift (N N_row_field_list, default_value (N N_row_field_list))
  | 1059 | 486 -> Shift (N N_record_expr, default_value (N N_record_expr))
  | 1537 | 1533 | 1489 | 1485 | 1332 | 1205 | 456 -> Shift (N N_rec_flag, default_value (N N_rec_flag))
  | 850 -> Shift (N N_private_virtual_flags, default_value (N N_private_virtual_flags))
  | 1410 | 1298 | 684 -> Shift (N N_private_flag, default_value (N N_private_flag))
  | 1499 | 805 -> Shift (N N_primitive_declaration, default_value (N N_primitive_declaration))
  | 1545 | 1542 | 1524 | 1508 | 1506 | 1500 | 1493 | 1475 | 1473 | 1470 | 1413 | 1365 | 1363 | 1360 | 1324 | 1321 | 1037 | 935 | 926 | 888 | 884 | 882 | 875 | 861 | 845 | 814 | 808 | 799 | 784 | 778 | 776 | 773 | 771 | 698 | 687 | 668 | 575 | 428 | 422 -> Shift (N N_post_item_attributes, default_value (N N_post_item_attributes))
  | 644 | 172 -> Shift (N N_poly_type_no_attr, default_value (N N_poly_type_no_attr))
  | 1310 | 1306 | 1301 | 857 -> Shift (N N_poly_type, default_value (N N_poly_type))
  | 795 | 791 | 424 | 176 | 81 -> Shift (N N_payload, default_value (N N_payload))
  | 530 | 526 | 403 | 378 | 376 | 365 | 330 | 328 | 326 | 324 | 314 | 300 | 82 -> Shift (N N_pattern, default_value (N N_pattern))
  | 1357 -> Shift (N N_parent_binder, default_value (N N_parent_binder))
  | 280 | 131 -> Shift (N N_package_type_cstrs, default_value (N N_package_type_cstrs))
  | 1270 | 1260 | 1257 | 1254 | 947 | 155 | 125 -> Shift (N N_package_type, default_value (N N_package_type))
  | 1326 | 696 -> Shift (N N_override_flag, default_value (N N_override_flag))
  | 582 -> Shift (N N_optional_type_variable, default_value (N N_optional_type_variable))
  | 581 -> Shift (N N_optional_type_parameter_list, default_value (N N_optional_type_parameter_list))
  | 589 -> Shift (N N_optional_type_parameter, default_value (N N_optional_type_parameter))
  | 1446 | 1442 | 1437 -> Shift (N N_option_STRING_, default_value (N N_option_STRING_))
  | 1370 | 1187 | 1184 | 1052 | 1048 | 1041 | 661 | 647 | 371 | 364 | 295 | 256 -> Shift (N N_opt_semi, default_value (N N_opt_semi))
  | 511 | 471 -> Shift (N N_opt_default, default_value (N N_opt_default))
  | 1411 | 1379 | 1375 | 1289 | 1285 | 685 | 659 | 499 | 178 -> Shift (N N_opt_bar, default_value (N N_opt_bar))
  | 200 -> Shift (N N_opt_ampersand, default_value (N N_opt_ampersand))
  | 333 -> Shift (N N_operator, default_value (N N_operator))
  | 504 -> Shift (N N_newtype, default_value (N N_newtype))
  | 236 -> Shift (N N_name_tag_list, default_value (N N_name_tag_list))
  | 835 | 430 | 414 -> Shift (N N_mutable_flag, default_value (N N_mutable_flag))
  | 1474 | 1265 | 1196 | 798 | 783 | 777 | 765 | 719 | 716 | 704 | 570 -> Shift (N N_module_type, default_value (N N_module_type))
  | 1492 | 1452 | 1444 | 1198 | 1194 | 944 | 707 | 563 | 452 | 448 -> Shift (N N_module_expr, default_value (N N_module_expr))
  | 768 | 764 | 701 -> Shift (N N_module_declaration, default_value (N N_module_declaration))
  | 1478 -> Shift (N N_module_bindings, default_value (N N_module_bindings))
  | 1469 | 1203 | 1193 -> Shift (N N_module_binding_body, default_value (N N_module_binding_body))
  | 1480 | 1468 -> Shift (N N_module_binding, default_value (N N_module_binding))
  | 770 | 746 | 697 | 477 -> Shift (N N_mod_longident, default_value (N N_mod_longident))
  | 751 | 748 | 92 -> Shift (N N_mod_ext_longident, default_value (N N_mod_ext_longident))
  | 257 -> Shift (N N_meth_list, default_value (N N_meth_list))
  | 1380 | 1376 | 1290 | 1286 | 500 -> Shift (N N_match_cases, default_value (N N_match_cases))
  | 1311 | 1235 -> Shift (N N_lident_list, default_value (N N_lident_list))
  | 507 -> Shift (N N_let_pattern, default_value (N N_let_pattern))
  | 1333 | 1206 -> Shift (N N_let_bindings_no_attrs, default_value (N N_let_bindings_no_attrs))
  | 1538 | 1534 | 1490 | 1486 | 966 | 458 -> Shift (N N_let_bindings, default_value (N N_let_bindings))
  | 1036 -> Shift (N N_let_binding, default_value (N N_let_binding))
  | 290 -> Shift (N N_lbl_pattern_list, default_value (N N_lbl_pattern_list))
  | 1061 -> Shift (N N_lbl_expr_list, default_value (N N_lbl_expr_list))
  | 469 | 460 -> Shift (N N_label_var, default_value (N N_label_var))
  | 1086 | 548 | 132 -> Shift (N N_label_longident, default_value (N N_label_longident))
  | 470 | 461 -> Shift (N N_label_let_pattern, default_value (N N_label_let_pattern))
  | 1080 | 552 -> Shift (N N_label_ident, default_value (N N_label_ident))
  | 660 | 641 -> Shift (N N_label_declarations, default_value (N N_label_declarations))
  | 1308 | 1304 | 1299 | 1053 | 855 | 841 | 836 | 734 | 642 | 555 | 546 | 537 | 436 | 432 | 416 -> Shift (N N_label, default_value (N N_label))
  | 1472 | 1426 | 775 | 726 | 584 | 249 | 216 | 186 | 173 | 159 | 147 -> Shift (N N_ident, default_value (N N_ident))
  | 1417 | 690 | 616 -> Shift (N N_generalized_constructor_arguments, default_value (N N_generalized_constructor_arguments))
  | 717 | 564 -> Shift (N N_functor_args, default_value (N N_functor_args))
  | 1153 | 1149 | 1148 | 523 -> Shift (N N_fun_def, default_value (N N_fun_def))
  | 1231 | 1218 | 459 -> Shift (N N_fun_binding, default_value (N N_fun_binding))
  | 1051 -> Shift (N N_field_expr_list, default_value (N N_field_expr_list))
  | 1536 | 1532 | 1488 | 1484 | 1191 | 965 | 541 | 529 | 525 | 503 | 498 | 496 | 494 | 491 | 476 | 455 | 443 | 441 | 401 | 391 | 387 | 385 | 383 | 24 -> Shift (N N_ext_attributes, default_value (N N_ext_attributes))
  | 1047 | 964 -> Shift (N N_expr_semi_list, default_value (N N_expr_semi_list))
  | 475 -> Shift (N N_expr_open, default_value (N N_expr_open))
  | 1277 | 1275 | 1167 | 1165 | 1122 | 1116 | 1103 | 1100 | 1097 | 1095 | 1090 | 1078 | 1067 | 1055 | 1042 | 1024 | 1019 | 1017 | 1015 | 1013 | 1011 | 1009 | 1007 | 1005 | 1003 | 1001 | 999 | 997 | 995 | 993 | 991 | 989 | 987 | 985 | 983 | 981 | 979 | 977 | 975 | 973 | 556 | 550 | 490 | 485 | 453 -> Shift (N N_expr, default_value (N N_expr))
  | 1138 | 1125 -> Shift (N N_direction_flag, default_value (N N_direction_flag))
  | 743 | 739 | 251 | 219 -> Shift (N N_core_type_no_attr, default_value (N N_core_type_no_attr))
  | 915 | 617 -> Shift (N N_core_type_list_no_attr, default_value (N N_core_type_list_no_attr))
  | 205 -> Shift (N N_core_type_list, default_value (N N_core_type_list))
  | 1336 | 910 | 866 | 631 | 618 | 606 | 154 -> Shift (N N_core_type_comma_list, default_value (N N_core_type_comma_list))
  | 275 | 272 | 261 | 213 | 164 | 153 | 152 -> Shift (N N_core_type2, default_value (N N_core_type2))
  | 1497 | 1313 | 1244 | 1239 | 1225 | 1223 | 1221 | 968 | 880 | 859 | 848 | 843 | 838 | 827 | 803 | 671 | 574 | 509 | 464 | 434 | 418 | 406 | 357 | 268 | 177 | 139 -> Shift (N N_core_type, default_value (N N_core_type))
  | 664 | 653 | 605 -> Shift (N N_constructor_declarations, default_value (N N_constructor_declarations))
  | 740 | 667 -> Shift (N N_constraints, default_value (N N_constraints))
  | 1418 -> Shift (N N_constr_longident, default_value (N N_constr_longident))
  | 868 -> Shift (N N_clty_longident, default_value (N N_clty_longident))
  | 1513 | 894 | 819 -> Shift (N N_class_type_parameters, default_value (N N_class_type_parameters))
  | 1511 | 818 -> Shift (N N_class_type_declarations, default_value (N N_class_type_declarations))
  | 891 -> Shift (N N_class_type_declaration, default_value (N N_class_type_declaration))
  | 1518 | 1354 | 913 | 909 | 905 | 902 | 897 -> Shift (N N_class_type, default_value (N N_class_type))
  | 1328 | 402 -> Shift (N N_class_structure, default_value (N N_class_structure))
  | 864 | 825 -> Shift (N N_class_signature, default_value (N N_class_signature))
  | 832 -> Shift (N N_class_sig_fields, default_value (N N_class_sig_fields))
  | 1338 | 392 | 265 | 191 | 141 -> Shift (N N_class_longident, default_value (N N_class_longident))
  | 1522 | 1515 -> Shift (N N_class_fun_binding, default_value (N N_class_fun_binding))
  | 411 -> Shift (N N_class_fields, default_value (N N_class_fields))
  | 1520 | 1516 | 1342 | 1335 | 1331 | 1327 -> Shift (N N_class_expr, default_value (N N_class_expr))
  | 816 -> Shift (N N_class_descriptions, default_value (N N_class_descriptions))
  | 929 -> Shift (N N_class_description, default_value (N N_class_description))
  | 1510 -> Shift (N N_class_declarations, default_value (N N_class_declarations))
  | 1527 -> Shift (N N_class_declaration, default_value (N N_class_declaration))
  | 1421 | 691 | 645 | 636 | 394 | 253 | 222 | 218 | 199 | 79 -> Shift (N N_attributes, default_value (N N_attributes))
  | 794 | 790 | 423 | 175 | 80 | 77 | 25 -> Shift (N N_attr_id, default_value (N N_attr_id))
  | 202 -> Shift (N N_amper_type_list, default_value (N N_amper_type_list))
  | 637 -> Reduce 99
  | 673 -> Reduce 97
  | 881 -> Reduce 96
  | 672 -> Reduce 95
  | 85 -> Reduce 94
  | 313 -> Reduce 93
  | 115 -> Reduce 92
  | 289 -> Reduce 91
  | 1071 | 961 | 560 | 349 -> Reduce 90
  | 78 -> Reduce 9
  | 604 -> Reduce 89
  | 608 -> Reduce 88
  | 609 -> Reduce 87
  | 607 -> Reduce 86
  | 613 | 603 -> Reduce 85
  | 105 -> Reduce 84
  | 303 -> Reduce 83
  | 304 -> Reduce 82
  | 312 -> Reduce 81
  | 86 -> Reduce 80
  | 76 -> Reduce 8
  | 315 -> Reduce 79
  | 305 -> Reduce 78
  | 871 -> Reduce 77
  | 906 | 865 -> Reduce 76
  | 822 -> Reduce 75
  | 738 -> Reduce 731
  | 737 -> Reduce 730
  | 893 -> Reduce 73
  | 594 -> Reduce 729
  | 596 -> Reduce 728
  | 755 -> Reduce 727
  | 756 -> Reduce 726
  | 749 -> Reduce 725
  | 752 -> Reduce 724
  | 744 -> Reduce 723
  | 741 -> Reduce 722
  | 817 -> Reduce 721
  | 892 -> Reduce 72
  | 849 -> Reduce 719
  | 844 -> Reduce 718
  | 839 -> Reduce 717
  | 1295 -> Reduce 716
  | 1292 -> Reduce 715
  | 419 -> Reduce 714
  | 435 -> Reduce 713
  | 1074 -> Reduce 712
  | 535 -> Reduce 711
  | 344 -> Reduce 710
  | 889 -> Reduce 71
  | 454 | 284 -> Reduce 709
  | 250 -> Reduce 708
  | 174 -> Reduce 707
  | 580 -> Reduce 706
  | 578 -> Reduce 705
  | 727 -> Reduce 703
  | 730 -> Reduce 702
  | 745 -> Reduce 701
  | 921 -> Reduce 70
  | 220 -> Reduce 7
  | 732 -> Reduce 699
  | 733 -> Reduce 698
  | 728 -> Reduce 697
  | 919 | 98 -> Reduce 696
  | 163 | 89 -> Reduce 695
  | 658 -> Reduce 694
  | 639 -> Reduce 693
  | 663 -> Reduce 692
  | 665 -> Reduce 691
  | 651 -> Reduce 690
  | 923 -> Reduce 69
  | 654 -> Reduce 689
  | 611 -> Reduce 688
  | 666 -> Reduce 687
  | 610 -> Reduce 686
  | 655 -> Reduce 685
  | 599 -> Reduce 683
  | 676 -> Reduce 682
  | 674 -> Reduce 681
  | 1222 -> Reduce 680
  | 924 -> Reduce 68
  | 1226 -> Reduce 679
  | 1224 -> Reduce 678
  | 1428 -> Reduce 677
  | 1431 -> Reduce 676
  | 1432 -> Reduce 675
  | 1430 -> Reduce 674
  | 1429 -> Reduce 673
  | 1427 -> Reduce 672
  | 224 -> Reduce 670
  | 925 -> Reduce 67
  | 221 -> Reduce 669
  | 439 -> Reduce 668
  | 440 -> Reduce 667
  | 1540 -> Reduce 666
  | 1436 -> Reduce 665
  | 1544 -> Reduce 663
  | 1543 -> Reduce 662
  | 1494 -> Reduce 661
  | 1512 -> Reduce 660
  | 922 -> Reduce 66
  | 1526 -> Reduce 659
  | 1541 -> Reduce 658
  | 1476 -> Reduce 657
  | 1477 -> Reduce 656
  | 1479 -> Reduce 655
  | 1483 -> Reduce 654
  | 1503 -> Reduce 653
  | 1407 -> Reduce 652
  | 1404 -> Reduce 651
  | 1501 -> Reduce 650
  | 412 -> Reduce 65
  | 1539 | 1491 -> Reduce 649
  | 1453 -> Reduce 648
  | 1459 -> Reduce 647
  | 1463 -> Reduce 646
  | 1467 -> Reduce 645
  | 1445 -> Reduce 644
  | 1441 -> Reduce 643
  | 1449 -> Reduce 642
  | 1505 -> Reduce 641
  | 1403 -> Reduce 640
  | 1353 -> Reduce 64
  | 1406 -> Reduce 639
  | 1401 -> Reduce 638
  | 1535 | 1487 -> Reduce 637
  | 1530 -> Reduce 636
  | 1547 -> Reduce 635
  | 244 -> Reduce 634
  | 243 -> Reduce 633
  | 1233 -> Reduce 632
  | 1232 -> Reduce 631
  | 1220 -> Reduce 630
  | 1356 -> Reduce 63
  | 1423 -> Reduce 629
  | 1415 -> Reduce 628
  | 1416 -> Reduce 627
  | 1424 -> Reduce 626
  | 1425 -> Reduce 625
  | 1507 -> Reduce 624
  | 1509 -> Reduce 623
  | 26 -> Reduce 622
  | 27 -> Reduce 621
  | 28 -> Reduce 620
  | 1330 -> Reduce 62
  | 29 -> Reduce 619
  | 30 -> Reduce 618
  | 32 -> Reduce 617
  | 33 -> Reduce 616
  | 34 -> Reduce 615
  | 35 -> Reduce 614
  | 36 -> Reduce 613
  | 37 -> Reduce 612
  | 38 -> Reduce 611
  | 39 -> Reduce 610
  | 1346 -> Reduce 61
  | 40 -> Reduce 609
  | 41 -> Reduce 608
  | 42 -> Reduce 607
  | 43 -> Reduce 606
  | 44 -> Reduce 605
  | 45 -> Reduce 604
  | 46 -> Reduce 603
  | 47 -> Reduce 602
  | 48 -> Reduce 601
  | 49 -> Reduce 600
  | 1339 -> Reduce 60
  | 211 -> Reduce 6
  | 51 -> Reduce 599
  | 52 -> Reduce 598
  | 53 -> Reduce 597
  | 54 -> Reduce 596
  | 55 -> Reduce 595
  | 56 -> Reduce 594
  | 57 -> Reduce 593
  | 58 -> Reduce 592
  | 59 -> Reduce 591
  | 60 -> Reduce 590
  | 920 | 873 -> Reduce 59
  | 61 -> Reduce 589
  | 62 -> Reduce 588
  | 63 -> Reduce 587
  | 64 -> Reduce 586
  | 65 -> Reduce 585
  | 66 -> Reduce 584
  | 67 -> Reduce 583
  | 68 -> Reduce 582
  | 69 -> Reduce 581
  | 70 -> Reduce 580
  | 877 -> Reduce 58
  | 71 -> Reduce 579
  | 72 -> Reduce 578
  | 73 -> Reduce 577
  | 74 -> Reduce 576
  | 75 -> Reduce 575
  | 31 -> Reduce 574
  | 50 -> Reduce 573
  | 351 -> Reduce 572
  | 129 -> Reduce 571
  | 124 -> Reduce 570
  | 831 -> Reduce 57
  | 359 -> Reduce 569
  | 356 -> Reduce 568
  | 370 -> Reduce 567
  | 373 -> Reduce 566
  | 368 -> Reduce 565
  | 292 -> Reduce 564
  | 90 -> Reduce 563
  | 361 | 347 -> Reduce 562
  | 362 | 352 -> Reduce 561
  | 321 -> Reduce 560
  | 874 -> Reduce 56
  | 319 -> Reduce 559
  | 83 -> Reduce 558
  | 967 | 317 -> Reduce 557
  | 316 -> Reduce 556
  | 1107 -> Reduce 555
  | 1109 -> Reduce 554
  | 955 -> Reduce 553
  | 949 -> Reduce 552
  | 1272 -> Reduce 551
  | 1269 -> Reduce 550
  | 872 -> Reduce 55
  | 545 -> Reduce 549
  | 547 -> Reduce 548
  | 1058 -> Reduce 547
  | 483 -> Reduce 546
  | 1186 -> Reduce 545
  | 393 -> Reduce 544
  | 536 -> Reduce 543
  | 1373 -> Reduce 542
  | 1050 -> Reduce 541
  | 1189 -> Reduce 540
  | 887 -> Reduce 54
  | 1045 -> Reduce 539
  | 1369 -> Reduce 538
  | 1372 -> Reduce 537
  | 1070 -> Reduce 536
  | 1182 -> Reduce 535
  | 1118 | 1099 -> Reduce 534
  | 1115 | 1094 -> Reduce 533
  | 1112 | 1089 -> Reduce 532
  | 960 -> Reduce 531
  | 1119 | 1102 -> Reduce 530
  | 1283 -> Reduce 529
  | 1169 -> Reduce 528
  | 1171 -> Reduce 527
  | 1175 -> Reduce 526
  | 1174 -> Reduce 525
  | 1281 -> Reduce 524
  | 559 | 557 -> Reduce 523
  | 1022 | 956 -> Reduce 522
  | 957 -> Reduce 521
  | 534 -> Reduce 520
  | 886 -> Reduce 52
  | 1121 -> Reduce 519
  | 540 -> Reduce 518
  | 1077 | 538 -> Reduce 517
  | 1180 -> Reduce 516
  | 1178 -> Reduce 515
  | 390 -> Reduce 514
  | 954 -> Reduce 513
  | 916 -> Reduce 512
  | 914 -> Reduce 511
  | 208 -> Reduce 510
  | 885 -> Reduce 51
  | 911 | 204 -> Reduce 509
  | 633 | 620 -> Reduce 508
  | 635 | 622 -> Reduce 507
  | 207 -> Reduce 506
  | 157 -> Reduce 505
  | 238 -> Reduce 504
  | 235 -> Reduce 503
  | 182 -> Reduce 502
  | 233 -> Reduce 501
  | 231 -> Reduce 500
  | 883 -> Reduce 50
  | 399 -> Reduce 5
  | 196 -> Reduce 499
  | 227 -> Reduce 498
  | 266 -> Reduce 497
  | 192 -> Reduce 496
  | 146 -> Reduce 495
  | 167 -> Reduce 494
  | 170 -> Reduce 493
  | 267 -> Reduce 492
  | 193 -> Reduce 491
  | 188 -> Reduce 490
  | 862 -> Reduce 49
  | 140 -> Reduce 489
  | 149 -> Reduce 488
  | 264 -> Reduce 487
  | 190 -> Reduce 486
  | 100 -> Reduce 485
  | 101 -> Reduce 484
  | 102 -> Reduce 483
  | 104 -> Reduce 482
  | 103 -> Reduce 481
  | 107 -> Reduce 480
  | 846 -> Reduce 48
  | 108 -> Reduce 479
  | 109 -> Reduce 478
  | 111 -> Reduce 477
  | 110 -> Reduce 476
  | 322 -> Reduce 475
  | 937 -> Reduce 474
  | 936 -> Reduce 473
  | 890 -> Reduce 472
  | 928 -> Reduce 471
  | 800 -> Reduce 470
  | 876 -> Reduce 47
  | 934 -> Reduce 469
  | 779 -> Reduce 468
  | 780 -> Reduce 467
  | 786 -> Reduce 466
  | 772 -> Reduce 465
  | 774 -> Reduce 464
  | 811 -> Reduce 463
  | 680 -> Reduce 462
  | 677 -> Reduce 461
  | 809 -> Reduce 460
  | 833 -> Reduce 46
  | 576 -> Reduce 459
  | 813 -> Reduce 458
  | 597 -> Reduce 457
  | 679 -> Reduce 456
  | 592 -> Reduce 455
  | 933 -> Reduce 454
  | 938 -> Reduce 453
  | 693 -> Reduce 451
  | 689 -> Reduce 450
  | 694 -> Reduce 449
  | 815 -> Reduce 448
  | 1029 -> Reduce 447
  | 1028 -> Reduce 446
  | 1027 -> Reduce 445
  | 198 -> Reduce 444
  | 225 -> Reduce 443
  | 194 -> Reduce 442
  | 189 -> Reduce 441
  | 1075 -> Reduce 440
  | 829 -> Reduce 44
  | 1062 -> Reduce 439
  | 788 -> Reduce 438
  | 789 -> Reduce 437
  | 457 -> Reduce 436
  | 852 -> Reduce 434
  | 854 -> Reduce 433
  | 851 -> Reduce 432
  | 853 -> Reduce 431
  | 657 -> Reduce 429
  | 807 -> Reduce 427
  | 806 -> Reduce 426
  | 429 -> Reduce 425
  | 426 -> Reduce 423
  | 252 -> Reduce 422
  | 255 -> Reduce 421
  | 860 -> Reduce 420
  | 408 -> Reduce 42
  | 863 -> Reduce 419
  | 1390 -> Reduce 418
  | 381 -> Reduce 417
  | 242 -> Reduce 416
  | 245 -> Reduce 415
  | 506 -> Reduce 414
  | 514 -> Reduce 413
  | 366 -> Reduce 412
  | 369 -> Reduce 411
  | 379 | 327 -> Reduce 410
  | 405 -> Reduce 41
  | 325 -> Reduce 409
  | 346 -> Reduce 408
  | 354 -> Reduce 407
  | 360 -> Reduce 406
  | 331 -> Reduce 405
  | 380 -> Reduce 404
  | 329 -> Reduce 403
  | 348 -> Reduce 402
  | 353 -> Reduce 401
  | 323 -> Reduce 400
  | 145 -> Reduce 40
  | 400 -> Reduce 4
  | 345 -> Reduce 399
  | 318 -> Reduce 398
  | 1557 -> Reduce 397
  | 1359 -> Reduce 395
  | 281 -> Reduce 394
  | 279 -> Reduce 393
  | 277 -> Reduce 392
  | 278 -> Reduce 391
  | 130 -> Reduce 390
  | 142 -> Reduce 39
  | 421 -> Reduce 389
  | 420 -> Reduce 387
  | 583 -> Reduce 386
  | 585 -> Reduce 385
  | 588 -> Reduce 384
  | 675 -> Reduce 383
  | 590 -> Reduce 381
  | 591 -> Reduce 380
  | 1349 -> Reduce 38
  | 586 -> Reduce 379
  | 1438 -> Reduce 378
  | 648 | 296 -> Reduce 376
  | 1211 -> Reduce 374
  | 179 -> Reduce 372
  | 1347 -> Reduce 37
  | 201 -> Reduce 369
  | 120 -> Reduce 368
  | 117 -> Reduce 367
  | 338 -> Reduce 366
  | 342 -> Reduce 365
  | 341 -> Reduce 364
  | 339 -> Reduce 363
  | 121 -> Reduce 362
  | 311 -> Reduce 361
  | 286 -> Reduce 360
  | 1523 -> Reduce 36
  | 336 -> Reduce 359
  | 113 -> Reduce 358
  | 950 | 282 -> Reduce 357
  | 951 | 335 | 283 -> Reduce 356
  | 446 | 118 -> Reduce 355
  | 447 | 334 | 119 -> Reduce 354
  | 958 | 340 -> Reduce 353
  | 114 -> Reduce 352
  | 952 | 285 -> Reduce 351
  | 306 -> Reduce 350
  | 1521 -> Reduce 35
  | 307 -> Reduce 349
  | 308 -> Reduce 348
  | 309 -> Reduce 347
  | 310 -> Reduce 346
  | 397 | 116 -> Reduce 345
  | 953 | 337 -> Reduce 344
  | 699 -> Reduce 343
  | 519 -> Reduce 342
  | 239 -> Reduce 341
  | 240 -> Reduce 340
  | 1517 -> Reduce 34
  | 187 -> Reduce 339
  | 415 -> Reduce 338
  | 161 -> Reduce 336
  | 162 -> Reduce 335
  | 757 -> Reduce 334
  | 758 -> Reduce 333
  | 762 -> Reduce 332
  | 708 -> Reduce 331
  | 753 -> Reduce 330
  | 1368 -> Reduce 33
  | 721 -> Reduce 329
  | 940 -> Reduce 328
  | 720 -> Reduce 327
  | 785 -> Reduce 326
  | 715 -> Reduce 325
  | 713 -> Reduce 324
  | 1256 -> Reduce 323
  | 1262 -> Reduce 322
  | 1259 -> Reduce 321
  | 1253 -> Reduce 320
  | 1264 -> Reduce 319
  | 1267 -> Reduce 318
  | 710 -> Reduce 317
  | 712 -> Reduce 316
  | 945 -> Reduce 315
  | 451 -> Reduce 314
  | 714 -> Reduce 313
  | 769 -> Reduce 312
  | 767 -> Reduce 311
  | 766 -> Reduce 310
  | 1367 -> Reduce 31
  | 1481 -> Reduce 309
  | 1482 -> Reduce 308
  | 1204 -> Reduce 307
  | 1199 -> Reduce 306
  | 1195 -> Reduce 305
  | 1471 -> Reduce 304
  | 136 -> Reduce 303
  | 84 -> Reduce 302
  | 94 -> Reduce 301
  | 160 | 96 -> Reduce 300
  | 1366 -> Reduce 30
  | 88 -> Reduce 299
  | 1316 -> Reduce 298
  | 1319 -> Reduce 297
  | 1320 -> Reduce 296
  | 1302 -> Reduce 295
  | 1307 -> Reduce 294
  | 168 -> Reduce 293
  | 259 -> Reduce 292
  | 258 -> Reduce 291
  | 1162 -> Reduce 290
  | 1325 -> Reduce 29
  | 1163 -> Reduce 289
  | 1157 -> Reduce 288
  | 1159 -> Reduce 287
  | 1237 -> Reduce 286
  | 1236 -> Reduce 285
  | 510 -> Reduce 284
  | 508 -> Reduce 283
  | 963 -> Reduce 282
  | 473 -> Reduce 281
  | 1210 -> Reduce 280
  | 1364 -> Reduce 28
  | 1039 -> Reduce 279
  | 1040 -> Reduce 278
  | 971 -> Reduce 277
  | 1032 -> Reduce 276
  | 1242 -> Reduce 275
  | 1247 -> Reduce 274
  | 1248 -> Reduce 273
  | 1038 -> Reduce 272
  | 298 -> Reduce 271
  | 297 -> Reduce 270
  | 1322 -> Reduce 27
  | 294 -> Reduce 269
  | 293 -> Reduce 268
  | 299 -> Reduce 267
  | 363 -> Reduce 266
  | 1064 -> Reduce 265
  | 1065 -> Reduce 264
  | 1063 -> Reduce 263
  | 1066 -> Reduce 262
  | 1068 -> Reduce 261
  | 522 -> Reduce 260
  | 427 -> Reduce 26
  | 521 -> Reduce 259
  | 468 -> Reduce 258
  | 467 -> Reduce 257
  | 515 -> Reduce 256
  | 513 -> Reduce 255
  | 1214 -> Reduce 254
  | 1213 -> Reduce 253
  | 1108 -> Reduce 252
  | 1106 -> Reduce 251
  | 462 -> Reduce 250
  | 1361 -> Reduce 25
  | 1073 | 137 -> Reduce 249
  | 487 | 133 -> Reduce 248
  | 465 -> Reduce 247
  | 463 -> Reduce 246
  | 553 -> Reduce 245
  | 1083 -> Reduce 244
  | 1081 -> Reduce 243
  | 554 -> Reduce 242
  | 1085 -> Reduce 241
  | 649 -> Reduce 240
  | 1343 -> Reduce 24
  | 652 -> Reduce 239
  | 646 -> Reduce 238
  | 735 | 166 -> Reduce 237
  | 793 -> Reduce 236
  | 1553 -> Reduce 235
  | 1549 -> Reduce 234
  | 127 -> Reduce 233
  | 148 | 126 -> Reduce 232
  | 628 -> Reduce 231
  | 634 -> Reduce 230
  | 1348 -> Reduce 23
  | 623 -> Reduce 229
  | 760 -> Reduce 227
  | 759 -> Reduce 226
  | 566 -> Reduce 225
  | 567 -> Reduce 224
  | 942 -> Reduce 223
  | 568 -> Reduce 222
  | 1151 -> Reduce 221
  | 1150 -> Reduce 220
  | 1351 -> Reduce 22
  | 1144 -> Reduce 219
  | 1229 -> Reduce 218
  | 1230 -> Reduce 217
  | 797 -> Reduce 216
  | 1056 -> Reduce 215
  | 1183 -> Reduce 214
  | 254 -> Reduce 213
  | 1422 -> Reduce 212
  | 692 -> Reduce 211
  | 247 -> Reduce 210
  | 1345 -> Reduce 21
  | 1393 -> Reduce 209
  | 395 -> Reduce 208
  | 1043 -> Reduce 206
  | 1046 -> Reduce 205
  | 478 -> Reduce 204
  | 1124 -> Reduce 203
  | 1123 -> Reduce 202
  | 1278 | 1010 -> Reduce 201
  | 976 -> Reduce 200
  | 1350 -> Reduce 20
  | 1021 -> Reduce 199
  | 410 -> Reduce 198
  | 1172 -> Reduce 197
  | 543 -> Reduce 196
  | 1026 -> Reduce 195
  | 1101 -> Reduce 194
  | 1096 -> Reduce 193
  | 1091 -> Reduce 192
  | 1104 -> Reduce 191
  | 1025 -> Reduce 190
  | 1344 -> Reduce 19
  | 1110 -> Reduce 189
  | 1012 -> Reduce 188
  | 1020 -> Reduce 187
  | 1018 -> Reduce 186
  | 1016 -> Reduce 185
  | 988 -> Reduce 184
  | 1006 -> Reduce 183
  | 994 -> Reduce 182
  | 1008 -> Reduce 181
  | 986 -> Reduce 180
  | 931 -> Reduce 18
  | 978 -> Reduce 179
  | 990 -> Reduce 178
  | 992 -> Reduce 177
  | 980 -> Reduce 176
  | 982 -> Reduce 175
  | 984 -> Reduce 174
  | 996 -> Reduce 173
  | 998 -> Reduce 172
  | 1000 -> Reduce 171
  | 1002 -> Reduce 170
  | 930 -> Reduce 17
  | 1004 -> Reduce 169
  | 1279 -> Reduce 168
  | 1014 -> Reduce 167
  | 1132 -> Reduce 166
  | 1389 -> Reduce 165
  | 1166 -> Reduce 164
  | 1168 -> Reduce 163
  | 558 -> Reduce 162
  | 1023 -> Reduce 161
  | 974 -> Reduce 160
  | 927 -> Reduce 16
  | 1377 -> Reduce 159
  | 1287 -> Reduce 158
  | 1152 -> Reduce 157
  | 1154 -> Reduce 156
  | 1160 -> Reduce 155
  | 1190 -> Reduce 154
  | 1202 -> Reduce 153
  | 1035 -> Reduce 152
  | 1209 -> Reduce 151
  | 1105 -> Reduce 150
  | 1529 -> Reduce 15
  | 551 -> Reduce 149
  | 1136 -> Reduce 148
  | 1143 -> Reduce 147
  | 1398 -> Reduce 146
  | 1383 -> Reduce 145
  | 1385 -> Reduce 144
  | 1381 -> Reduce 143
  | 1378 -> Reduce 142
  | 1291 -> Reduce 141
  | 1251 -> Reduce 140
  | 1528 -> Reduce 14
  | 1076 -> Reduce 139
  | 1079 -> Reduce 138
  | 8 -> Reduce 137
  | 9 -> Reduce 136
  | 10 -> Reduce 135
  | 11 -> Reduce 134
  | 6 -> Reduce 133
  | 5 -> Reduce 132
  | 7 -> Reduce 131
  | 4 -> Reduce 130
  | 1525 -> Reduce 13
  | 15 -> Reduce 129
  | 1 -> Reduce 128
  | 17 -> Reduce 127
  | 18 -> Reduce 126
  | 13 -> Reduce 125
  | 2 -> Reduce 124
  | 14 -> Reduce 123
  | 16 -> Reduce 122
  | 20 -> Reduce 121
  | 12 -> Reduce 120
  | 223 -> Reduce 12
  | 19 -> Reduce 119
  | 3 -> Reduce 118
  | 1127 -> Reduce 117
  | 1126 -> Reduce 116
  | 217 -> Reduce 115
  | 212 -> Reduce 114
  | 626 -> Reduce 113
  | 621 -> Reduce 112
  | 210 -> Reduce 111
  | 206 -> Reduce 110
  | 269 -> Reduce 109
  | 270 -> Reduce 108
  | 276 | 273 | 262 | 214 -> Reduce 107
  | 203 -> Reduce 103
  | 241 -> Reduce 102
  | 615 -> Reduce 101
  | 638 -> Reduce 100
  | 1392 -> Reduce 10
  | 1502 | 1414 | 1362 | 1340 | 1296 | 1161 | 878 | 834 | 826 | 810 | 787 | 781 | 754 | 688 | 669 | 612 | 598 | 413 | 197 -> Pop
  | 112 -> Parent (function
     | 82 | 112 | 287 | 288 | 300 | 302 | 314 | 324 | 328 | 347 | 352 | 326 | 330 | 365 | 376 | 378 | 403 | 500 | 507 | 516 | 526 | 530 | 1145 | 1161 | 1215 | 1286 | 1290 | 1376 | 1380 -> Shift (T T_RPAREN, default_value (T T_RPAREN))
     | 458 | 966 | 1036 | 1206 | 1333 | 1486 | 1490 | 1534 | 1538 -> Shift (N N_operator, default_value (N N_operator))
     | _ -> raise Not_found)
  | _ -> raise Not_found

let lr1_to_lr0 =
  [|0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25;26;27;28;29;30;31;32;33;34;35;36;37;38;39;40;41;42;43;44;45;46;47;48;49;50;51;52;53;54;55;56;57;58;59;60;61;62;63;64;65;66;67;68;69;70;71;72;73;74;75;76;77;78;79;80;81;82;83;84;85;86;87;88;89;90;91;92;93;94;95;96;97;98;99;100;101;102;103;104;105;106;107;108;109;110;111;112;113;114;115;116;117;118;119;120;121;122;123;124;125;126;127;128;129;130;131;132;133;134;135;136;137;138;139;140;141;142;143;144;145;146;147;148;149;150;151;152;153;154;155;156;157;158;159;160;161;162;163;164;165;166;167;168;169;170;171;172;173;174;175;176;177;178;179;180;181;182;183;184;185;186;187;188;189;190;191;192;193;194;195;196;197;198;199;200;201;202;203;204;205;206;207;208;209;210;211;212;213;214;215;216;217;218;219;220;221;222;223;224;225;226;227;228;229;230;231;232;233;234;235;236;237;238;239;240;241;242;243;244;245;246;247;248;249;250;251;252;253;254;255;256;257;258;259;260;261;262;263;264;265;266;267;268;269;270;271;272;273;274;275;276;277;278;279;280;281;282;283;284;285;286;287;288;289;290;291;292;293;294;295;296;297;298;299;300;301;302;303;304;305;306;307;308;309;310;311;312;313;314;315;316;317;318;319;320;321;322;323;324;325;328;329;346;347;348;349;350;351;352;353;354;336;337;338;339;340;341;342;355;356;326;327;357;358;359;330;331;332;333;334;335;343;344;345;360;361;362;363;364;365;366;367;368;369;370;371;372;373;374;375;376;377;378;379;380;381;382;383;384;385;386;387;388;389;390;391;392;393;394;395;396;397;398;399;400;401;402;403;404;405;406;407;408;409;410;411;412;413;414;415;416;417;418;419;420;421;422;423;424;425;426;427;428;429;430;431;432;433;434;435;436;437;438;439;440;441;442;443;444;445;446;447;448;449;450;451;452;453;454;455;456;457;458;459;460;461;462;463;464;465;466;467;468;469;470;471;472;473;474;475;476;477;478;479;480;481;482;483;484;485;486;487;488;489;490;491;492;493;494;495;496;497;498;499;500;501;502;503;504;505;506;507;508;509;510;511;512;513;514;515;516;517;518;519;520;521;522;523;524;525;526;527;528;529;530;531;532;533;534;535;536;559;560;561;562;563;564;565;566;567;568;569;570;571;572;573;574;575;576;577;578;579;580;581;582;583;584;585;586;587;588;589;590;591;592;593;594;595;596;597;598;599;600;601;602;603;604;605;606;607;608;609;610;611;612;613;614;615;616;617;618;619;620;621;622;623;624;625;626;627;628;629;630;631;632;633;634;635;636;637;638;639;640;641;642;643;644;645;646;647;648;649;650;651;652;653;654;655;656;657;658;659;660;661;662;663;664;665;666;667;668;669;670;671;672;673;674;675;676;677;678;679;680;681;682;683;684;685;686;687;688;689;690;691;692;693;694;695;696;697;698;699;700;701;702;703;704;705;706;707;708;709;710;711;712;713;714;715;716;717;718;719;720;721;722;723;724;725;726;727;728;729;730;731;732;733;734;735;736;737;738;739;740;741;742;743;744;745;746;747;748;749;750;751;752;753;754;755;756;757;758;759;760;761;762;763;764;765;766;767;768;769;770;771;772;773;774;775;776;777;778;779;780;781;782;783;784;785;786;787;788;789;790;791;792;793;794;795;796;797;798;799;800;801;802;803;804;805;806;807;808;809;810;811;812;813;814;815;816;817;818;819;820;821;822;823;824;825;826;827;828;829;830;831;832;833;834;835;836;837;838;839;840;841;842;843;844;845;846;847;848;849;850;851;852;853;854;855;856;857;858;859;860;861;862;863;864;865;866;867;868;869;870;871;872;873;874;875;876;877;878;879;880;881;882;883;884;885;886;887;888;889;890;891;892;893;894;895;896;897;898;899;900;901;902;903;904;905;906;907;908;909;910;911;912;913;914;915;916;917;918;919;920;921;922;923;924;925;926;927;928;929;930;931;932;933;934;935;936;937;938;939;940;941;942;943;944;945;946;947;948;949;950;951;952;953;954;955;956;957;958;541;542;543;537;538;539;540;550;551;552;553;554;555;556;557;558;544;545;548;549;1111;1112;961;962;963;964;965;966;967;968;969;970;971;972;973;974;975;976;977;978;995;996;1021;1022;1023;546;547;1024;1025;979;980;985;986;997;998;981;982;983;984;987;988;989;990;991;992;993;994;999;1000;1001;1002;1013;1014;1003;1004;1005;1006;1007;1008;1015;1016;1017;1018;1019;1020;1026;1009;1010;1011;1012;1027;1028;1029;1030;1031;1032;1033;1034;1035;1036;1037;1038;1039;1040;1041;1042;1043;1044;1045;1046;1047;1048;1049;1050;1051;1052;1053;1054;1055;1056;1057;1058;1059;1060;1061;1062;1063;1064;1065;1066;1067;1068;1069;1070;1071;1072;1073;1074;1075;1113;1114;1115;1116;1117;1118;1119;1076;1077;1078;1079;1080;1081;1082;1083;1084;1085;1086;1087;1088;1089;1090;1091;1092;1093;1094;1095;1096;1097;1098;1099;1100;1101;1102;1103;1104;1105;1106;1107;1108;1109;1110;1120;1121;1122;1123;1124;959;960;1125;1126;1127;1128;1129;1130;1131;1132;1133;1134;1135;1136;1137;1138;1139;1140;1141;1142;1143;1144;1145;1146;1147;1148;1149;1150;1151;1152;1153;1154;1155;1156;1157;1158;1159;1160;1161;1162;1163;1164;1165;1166;1167;1168;1169;1170;1171;1172;1173;1174;1175;1176;1177;1178;1179;1180;1181;1182;1183;1184;1185;1186;1187;1188;1189;1190;1191;1192;1193;1194;1195;1196;1197;1198;1199;1200;1201;1202;1203;1204;1205;1206;1207;1208;1209;1210;1211;1212;1213;1214;1215;1216;1217;1218;1219;1220;1221;1222;1223;1224;1225;1226;1227;1228;1229;1230;1231;1232;1233;1234;1235;1236;1237;1238;1239;1240;1241;1242;1243;1244;1245;1246;1247;1248;1249;1250;1251;1252;1253;1254;1255;1256;1257;1258;1259;1260;1261;1262;1263;1264;1265;1266;1267;1268;1269;1270;1271;1272;1273;1274;1275;1276;1277;1278;1279;1280;1281;1282;1283;1284;1285;1286;1287;1288;1289;1290;1291;1292;1293;1294;1295;1296;1297;1298;1299;1300;1301;1302;1303;1304;1305;1306;1307;1308;1309;1310;1311;1312;1313;1314;1315;1316;1317;1318;1319;1320;1321;1322;1323;1324;1325;1326;1327;1328;1329;1330;1331;1332;1333;1334;1335;1336;1337;1338;1339;1340;1341;1342;1343;1344;1345;1346;1347;1348;1349;1350;1351;1352;1353;1354;1355;1356;1357;1358;1359;1360;1361;1362;1363;1364;1365;1366;1367;1368;1369;1370;1371;1372;1373;1374;1375;1376;1377;1378;1379;1380;1381;1382;1383;1384;1385;1386;1387;1388;1389;1390;1391;1392;1393;1394;1395;1396;1397;1398;1399;1400;1401;1402;1403;1404;1405;1406;1407;1408;1409;1410;1411;1412;1413;1414;1415;1416;1417;1418;1419;1420;1421;1422;1423;1424;1425;1426;1427;1428;1429;1430;1431;1432;1433;1434;1435;1436;1437;1438;1439;1440;1441;1442;1443;1444;1445;1446;1447;1448;1449;1450;1451;1452;1453;1454;1455;1456;1457;1458;1459;1460;1461;1462;1463;1464;1465;1466;1467;1468;1469;1470;1471;1472;1473;1474;1475;1476;1477;1478;1479;1480;1481;1482;1483;1484;1485;1486;1487;1488;1489;1490;1491;1492;1493;1494;1495;1496;1497;1498;1499;1500;1501;1502;1503;1504;1505;1506;1507;1508;1509;1510;1511;1512;1513;1514;1515;1516;1517;1518;1519;1520;1521;1522;1523;1524;1525;1526;1527;1528;1529;1530;1531;1532;1533;1534;1535;1536;1537;1538;1539;1540;1541;1542;1543;1544;1545;1546;1547;1548;1549;1550;1551;1552;1553;1554;1555;1556;1557;1558;
|]

let decision i = decision lr1_to_lr0.(i)
