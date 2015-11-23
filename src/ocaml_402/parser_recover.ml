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
  | MenhirInterpreter.T MenhirInterpreter.T_ENTRYPOINT -> ()
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
  | 1376 | 1290 | 1286 | 1062 -> Shift (T T_WITH, default_value (T T_WITH))
  | 1305 | 1299 | 433 -> Shift (T T_VIRTUAL, default_value (T T_VIRTUAL))
  | 1194 | 872 | 352 | 146 | 137 | 124 | 97 -> Shift (T T_UIDENT, default_value (T T_UIDENT))
  | 707 | 702 -> Shift (T T_TYPE, default_value (T T_TYPE))
  | 1166 -> Shift (T T_THEN, default_value (T T_THEN))
  | 1466 | 1462 | 1456 | 1452 -> Shift (T T_STRING, default_value (T T_STRING))
  | 626 -> Shift (T T_STAR, default_value (T T_STAR))
  | 1421 | 1357 | 1354 | 1284 | 1282 | 1275 | 1273 | 1270 | 1268 | 1265 | 1263 | 1260 | 1257 | 1254 | 1219 | 1217 | 1214 | 1181 | 1179 | 1149 | 1147 | 1122 | 1113 | 1090 | 961 | 950 | 943 | 830 | 765 | 763 | 731 | 713 | 711 | 704 | 634 | 621 | 616 | 589 | 567 | 541 | 520 | 518 | 514 | 468 | 447 | 409 | 406 | 398 | 376 | 360 | 357 | 345 | 304 | 265 | 158 | 130 | 125 | 95 -> Shift (T T_RPAREN, default_value (T T_RPAREN))
  | 1422 | 1393 | 1339 | 1190 | 1116 | 1095 | 1051 | 869 | 823 | 798 | 794 | 483 | 427 | 369 | 290 | 248 | 239 | 236 | 234 | 232 | 228 | 197 | 183 -> Shift (T T_RBRACKET, default_value (T T_RBRACKET))
  | 1183 | 1119 | 1100 | 1071 | 664 | 652 | 293 -> Shift (T T_RBRACE, default_value (T T_RBRACE))
  | 217 -> Shift (T T_QUOTE, default_value (T T_QUOTE))
  | 1411 | 685 -> Shift (T T_PLUSEQ, default_value (T T_PLUSEQ))
  | 476 -> Shift (T T_OPEN, default_value (T T_OPEN))
  | 708 -> Shift (T T_OF, default_value (T T_OF))
  | 108 | 101 -> Shift (T T_NATIVEINT, default_value (T T_NATIVEINT))
  | 1343 | 1157 | 945 | 914 | 910 | 906 | 903 | 720 | 631 | 503 | 276 | 273 | 262 -> Shift (T T_MINUSGREATER, default_value (T T_MINUSGREATER))
  | 1276 | 1178 | 507 | 377 -> Shift (T T_LPAREN, default_value (T T_LPAREN))
  | 1516 | 1410 | 1360 | 1218 | 1148 | 920 | 900 | 897 | 825 | 683 | 602 | 519 | 152 | 99 -> Shift (T T_LIDENT, default_value (T T_LIDENT))
  | 974 -> Shift (T T_LESSMINUS, default_value (T T_LESSMINUS))
  | 1336 | 1251 | 1209 | 1202 | 1035 | 529 | 481 -> Shift (T T_IN, default_value (T T_IN))
  | 1187 | 1059 | 484 -> Shift (T T_GREATERRBRACE, default_value (T T_GREATERRBRACE))
  | 1175 -> Shift (T T_GREATERDOT, default_value (T T_GREATERDOT))
  | 171 | 167 -> Shift (T T_GREATER, default_value (T T_GREATER))
  | 1521 | 1500 | 1467 | 1463 | 1459 | 1453 | 1449 | 1445 | 1441 | 1319 | 1316 | 1295 | 1247 | 1242 | 1229 | 1199 | 1056 | 1032 | 971 | 881 | 826 | 806 | 752 | 672 | 533 | 486 | 439 | 140 -> Shift (T T_EQUAL, default_value (T T_EQUAL))
  | 1560 | 1555 | 1550 -> Shift (T T_EOF, default_value (T T_EOF))
  | 1331 | 1172 | 941 | 832 | 497 | 452 | 411 -> Shift (T T_END, default_value (T T_END))
  | 658 | 604 -> Shift (T T_DOTDOT, default_value (T T_DOTDOT))
  | 1435 | 1314 | 1245 | 1240 | 919 | 871 | 860 | 250 | 160 | 145 | 136 | 93 -> Shift (T T_DOT, default_value (T T_DOT))
  | 1399 | 1390 | 1144 | 1137 | 1133 -> Shift (T T_DONE, default_value (T T_DONE))
  | 1397 | 1388 | 1142 | 1135 | 1131 -> Shift (T T_DO, default_value (T T_DO))
  | 1278 | 379 -> Shift (T T_COMMA, default_value (T T_COMMA))
  | 749 | 744 -> Shift (T T_COLONEQUAL, default_value (T T_COLONEQUAL))
  | 1498 | 1311 | 1307 | 1302 | 948 | 901 | 898 | 858 | 849 | 844 | 839 | 804 | 784 | 705 | 645 | 575 | 571 | 435 | 419 | 173 | 153 -> Shift (T T_COLON, default_value (T T_COLON))
  | 1373 | 1046 | 400 | 374 | 289 -> Shift (T T_BARRBRACKET, default_value (T T_BARRBRACKET))
  | 642 | 230 | 186 -> Shift (T T_BAR, default_value (T T_BAR))
  | 738 -> Shift (N N_with_type_binder, default_value (N N_with_type_binder))
  | 1506 | 1407 | 1404 | 814 | 680 | 597 | 595 -> Shift (N N_with_extensions, default_value (N N_with_extensions))
  | 724 -> Shift (N N_with_constraints, default_value (N N_with_constraints))
  | 842 -> Shift (N N_virtual_flag, default_value (N N_virtual_flag))
  | 1497 | 1457 | 1436 | 1074 | 964 | 803 | 574 | 563 | 334 -> Shift (N N_val_ident, default_value (N N_val_ident))
  | 1236 -> Shift (N N_typevar_list, default_value (N N_typevar_list))
  | 727 -> Shift (N N_type_variable, default_value (N N_type_variable))
  | 725 -> Shift (N N_type_parameters, default_value (N N_type_parameters))
  | 822 | 726 -> Shift (N N_type_parameter_list, default_value (N N_type_parameter_list))
  | 733 -> Shift (N N_type_parameter, default_value (N N_type_parameter))
  | 89 -> Shift (N N_type_longident, default_value (N N_type_longident))
  | 684 | 603 -> Shift (N N_type_kind, default_value (N N_type_kind))
  | 1402 | 1401 | 581 | 579 -> Shift (N N_type_declarations, default_value (N N_type_declarations))
  | 1548 | 1533 | 25 -> Shift (N N_structure_tail, default_value (N N_structure_tail))
  | 1437 -> Shift (N N_structure_head, default_value (N N_structure_head))
  | 451 | 24 -> Shift (N N_structure, default_value (N N_structure))
  | 1414 -> Shift (N N_str_extension_constructors, default_value (N N_str_extension_constructors))
  | 522 | 303 -> Shift (N N_simple_pattern, default_value (N N_simple_pattern))
  | 1458 | 1086 | 1084 | 546 | 544 | 535 | 495 | 494 | 491 | 490 | 391 -> Shift (N N_simple_expr, default_value (N N_simple_expr))
  | 909 | 905 | 902 -> Shift (N N_simple_core_type_or_tuple_no_attr, default_value (N N_simple_core_type_or_tuple_no_attr))
  | 632 | 629 | 627 -> Shift (N N_simple_core_type_no_attr, default_value (N N_simple_core_type_no_attr))
  | 211 -> Shift (N N_simple_core_type, default_value (N N_simple_core_type))
  | 322 -> Shift (N N_signed_constant, default_value (N N_signed_constant))
  | 1554 | 934 | 697 | 573 -> Shift (N N_signature, default_value (N N_signature))
  | 688 -> Shift (N N_sig_extension_constructors, default_value (N N_sig_extension_constructors))
  | 1559 | 1468 | 1464 | 1460 | 1450 | 1442 | 1398 | 1396 | 1389 | 1386 | 1384 | 1325 | 1320 | 1317 | 1296 | 1252 | 1248 | 1243 | 1230 | 1221 | 1210 | 1203 | 1160 | 1158 | 1143 | 1141 | 1139 | 1136 | 1132 | 1130 | 1115 | 1094 | 1089 | 1036 | 1033 | 972 | 564 | 551 | 534 | 530 | 526 | 504 | 499 | 482 | 474 | 446 | 444 | 440 | 390 | 388 | 386 | 384 -> Shift (N N_seq_expr, default_value (N N_seq_expr))
  | 231 | 187 | 185 | 182 -> Shift (N N_row_field_list, default_value (N N_row_field_list))
  | 1061 | 488 -> Shift (N N_record_expr, default_value (N N_record_expr))
  | 1539 | 1535 | 1491 | 1487 | 1334 | 1207 | 458 -> Shift (N N_rec_flag, default_value (N N_rec_flag))
  | 852 -> Shift (N N_private_virtual_flags, default_value (N N_private_virtual_flags))
  | 1412 | 1300 | 686 -> Shift (N N_private_flag, default_value (N N_private_flag))
  | 1501 | 807 -> Shift (N N_primitive_declaration, default_value (N N_primitive_declaration))
  | 1547 | 1544 | 1526 | 1510 | 1508 | 1502 | 1495 | 1477 | 1475 | 1472 | 1415 | 1367 | 1365 | 1362 | 1326 | 1323 | 1039 | 937 | 928 | 890 | 886 | 884 | 877 | 863 | 847 | 816 | 810 | 801 | 786 | 780 | 778 | 775 | 773 | 700 | 689 | 670 | 577 | 430 | 424 -> Shift (N N_post_item_attributes, default_value (N N_post_item_attributes))
  | 646 | 174 -> Shift (N N_poly_type_no_attr, default_value (N N_poly_type_no_attr))
  | 1312 | 1308 | 1303 | 859 -> Shift (N N_poly_type, default_value (N N_poly_type))
  | 797 | 793 | 426 | 178 | 83 -> Shift (N N_payload, default_value (N N_payload))
  | 532 | 528 | 405 | 380 | 378 | 367 | 332 | 330 | 328 | 326 | 316 | 302 | 84 -> Shift (N N_pattern, default_value (N N_pattern))
  | 1359 -> Shift (N N_parent_binder, default_value (N N_parent_binder))
  | 282 | 133 -> Shift (N N_package_type_cstrs, default_value (N N_package_type_cstrs))
  | 1272 | 1262 | 1259 | 1256 | 949 | 157 | 127 -> Shift (N N_package_type, default_value (N N_package_type))
  | 1328 | 698 -> Shift (N N_override_flag, default_value (N N_override_flag))
  | 584 -> Shift (N N_optional_type_variable, default_value (N N_optional_type_variable))
  | 583 -> Shift (N N_optional_type_parameter_list, default_value (N N_optional_type_parameter_list))
  | 591 -> Shift (N N_optional_type_parameter, default_value (N N_optional_type_parameter))
  | 1448 | 1444 | 1439 -> Shift (N N_option_STRING_, default_value (N N_option_STRING_))
  | 1372 | 1189 | 1186 | 1054 | 1050 | 1043 | 663 | 649 | 373 | 366 | 297 | 258 -> Shift (N N_opt_semi, default_value (N N_opt_semi))
  | 513 | 473 -> Shift (N N_opt_default, default_value (N N_opt_default))
  | 1413 | 1381 | 1377 | 1291 | 1287 | 687 | 661 | 501 | 180 -> Shift (N N_opt_bar, default_value (N N_opt_bar))
  | 202 -> Shift (N N_opt_ampersand, default_value (N N_opt_ampersand))
  | 335 -> Shift (N N_operator, default_value (N N_operator))
  | 506 -> Shift (N N_newtype, default_value (N N_newtype))
  | 238 -> Shift (N N_name_tag_list, default_value (N N_name_tag_list))
  | 837 | 432 | 416 -> Shift (N N_mutable_flag, default_value (N N_mutable_flag))
  | 1476 | 1267 | 1198 | 800 | 785 | 779 | 767 | 721 | 718 | 706 | 572 -> Shift (N N_module_type, default_value (N N_module_type))
  | 1494 | 1454 | 1446 | 1200 | 1196 | 946 | 709 | 565 | 454 | 450 -> Shift (N N_module_expr, default_value (N N_module_expr))
  | 770 | 766 | 703 -> Shift (N N_module_declaration, default_value (N N_module_declaration))
  | 1480 -> Shift (N N_module_bindings, default_value (N N_module_bindings))
  | 1471 | 1205 | 1195 -> Shift (N N_module_binding_body, default_value (N N_module_binding_body))
  | 1482 | 1470 -> Shift (N N_module_binding, default_value (N N_module_binding))
  | 772 | 748 | 699 | 479 -> Shift (N N_mod_longident, default_value (N N_mod_longident))
  | 753 | 750 | 94 -> Shift (N N_mod_ext_longident, default_value (N N_mod_ext_longident))
  | 259 -> Shift (N N_meth_list, default_value (N N_meth_list))
  | 1382 | 1378 | 1292 | 1288 | 502 -> Shift (N N_match_cases, default_value (N N_match_cases))
  | 1313 | 1237 -> Shift (N N_lident_list, default_value (N N_lident_list))
  | 509 -> Shift (N N_let_pattern, default_value (N N_let_pattern))
  | 1335 | 1208 -> Shift (N N_let_bindings_no_attrs, default_value (N N_let_bindings_no_attrs))
  | 1540 | 1536 | 1492 | 1488 | 968 | 460 -> Shift (N N_let_bindings, default_value (N N_let_bindings))
  | 1038 -> Shift (N N_let_binding, default_value (N N_let_binding))
  | 292 -> Shift (N N_lbl_pattern_list, default_value (N N_lbl_pattern_list))
  | 1063 -> Shift (N N_lbl_expr_list, default_value (N N_lbl_expr_list))
  | 471 | 462 -> Shift (N N_label_var, default_value (N N_label_var))
  | 1088 | 550 | 134 -> Shift (N N_label_longident, default_value (N N_label_longident))
  | 472 | 463 -> Shift (N N_label_let_pattern, default_value (N N_label_let_pattern))
  | 1082 | 554 -> Shift (N N_label_ident, default_value (N N_label_ident))
  | 662 | 643 -> Shift (N N_label_declarations, default_value (N N_label_declarations))
  | 1310 | 1306 | 1301 | 1055 | 857 | 843 | 838 | 736 | 644 | 557 | 548 | 539 | 438 | 434 | 418 -> Shift (N N_label, default_value (N N_label))
  | 1474 | 1428 | 777 | 728 | 586 | 251 | 218 | 188 | 175 | 161 | 149 -> Shift (N N_ident, default_value (N N_ident))
  | 1419 | 692 | 618 -> Shift (N N_generalized_constructor_arguments, default_value (N N_generalized_constructor_arguments))
  | 719 | 566 -> Shift (N N_functor_args, default_value (N N_functor_args))
  | 1155 | 1151 | 1150 | 525 -> Shift (N N_fun_def, default_value (N N_fun_def))
  | 1233 | 1220 | 461 -> Shift (N N_fun_binding, default_value (N N_fun_binding))
  | 1053 -> Shift (N N_field_expr_list, default_value (N N_field_expr_list))
  | 1538 | 1534 | 1490 | 1486 | 1193 | 967 | 543 | 531 | 527 | 505 | 500 | 498 | 496 | 493 | 478 | 457 | 445 | 443 | 403 | 393 | 389 | 387 | 385 | 26 -> Shift (N N_ext_attributes, default_value (N N_ext_attributes))
  | 1049 | 966 -> Shift (N N_expr_semi_list, default_value (N N_expr_semi_list))
  | 477 -> Shift (N N_expr_open, default_value (N N_expr_open))
  | 1279 | 1277 | 1169 | 1167 | 1124 | 1118 | 1105 | 1102 | 1099 | 1097 | 1092 | 1080 | 1069 | 1057 | 1044 | 1026 | 1021 | 1019 | 1017 | 1015 | 1013 | 1011 | 1009 | 1007 | 1005 | 1003 | 1001 | 999 | 997 | 995 | 993 | 991 | 989 | 987 | 985 | 983 | 981 | 979 | 977 | 975 | 558 | 552 | 492 | 487 | 455 -> Shift (N N_expr, default_value (N N_expr))
  | 1140 | 1127 -> Shift (N N_direction_flag, default_value (N N_direction_flag))
  | 745 | 741 | 253 | 221 -> Shift (N N_core_type_no_attr, default_value (N N_core_type_no_attr))
  | 917 | 619 -> Shift (N N_core_type_list_no_attr, default_value (N N_core_type_list_no_attr))
  | 207 -> Shift (N N_core_type_list, default_value (N N_core_type_list))
  | 1338 | 912 | 868 | 633 | 620 | 608 | 156 -> Shift (N N_core_type_comma_list, default_value (N N_core_type_comma_list))
  | 277 | 274 | 263 | 215 | 166 | 155 | 154 -> Shift (N N_core_type2, default_value (N N_core_type2))
  | 1499 | 1315 | 1246 | 1241 | 1227 | 1225 | 1223 | 970 | 882 | 861 | 850 | 845 | 840 | 829 | 805 | 673 | 576 | 511 | 466 | 436 | 420 | 408 | 359 | 270 | 179 | 141 -> Shift (N N_core_type, default_value (N N_core_type))
  | 666 | 655 | 607 -> Shift (N N_constructor_declarations, default_value (N N_constructor_declarations))
  | 742 | 669 -> Shift (N N_constraints, default_value (N N_constraints))
  | 1420 -> Shift (N N_constr_longident, default_value (N N_constr_longident))
  | 870 -> Shift (N N_clty_longident, default_value (N N_clty_longident))
  | 1515 | 896 | 821 -> Shift (N N_class_type_parameters, default_value (N N_class_type_parameters))
  | 1513 | 820 -> Shift (N N_class_type_declarations, default_value (N N_class_type_declarations))
  | 893 -> Shift (N N_class_type_declaration, default_value (N N_class_type_declaration))
  | 1520 | 1356 | 915 | 911 | 907 | 904 | 899 -> Shift (N N_class_type, default_value (N N_class_type))
  | 1330 | 404 -> Shift (N N_class_structure, default_value (N N_class_structure))
  | 866 | 827 -> Shift (N N_class_signature, default_value (N N_class_signature))
  | 834 -> Shift (N N_class_sig_fields, default_value (N N_class_sig_fields))
  | 1340 | 394 | 267 | 193 | 143 -> Shift (N N_class_longident, default_value (N N_class_longident))
  | 1524 | 1517 -> Shift (N N_class_fun_binding, default_value (N N_class_fun_binding))
  | 413 -> Shift (N N_class_fields, default_value (N N_class_fields))
  | 1522 | 1518 | 1344 | 1337 | 1333 | 1329 -> Shift (N N_class_expr, default_value (N N_class_expr))
  | 818 -> Shift (N N_class_descriptions, default_value (N N_class_descriptions))
  | 931 -> Shift (N N_class_description, default_value (N N_class_description))
  | 1512 -> Shift (N N_class_declarations, default_value (N N_class_declarations))
  | 1529 -> Shift (N N_class_declaration, default_value (N N_class_declaration))
  | 1423 | 693 | 647 | 638 | 396 | 255 | 224 | 220 | 201 | 81 -> Shift (N N_attributes, default_value (N N_attributes))
  | 796 | 792 | 425 | 177 | 82 | 79 | 27 -> Shift (N N_attr_id, default_value (N N_attr_id))
  | 204 -> Shift (N N_amper_type_list, default_value (N N_amper_type_list))
  | 639 -> Reduce 99
  | 675 -> Reduce 97
  | 883 -> Reduce 96
  | 674 -> Reduce 95
  | 87 -> Reduce 94
  | 315 -> Reduce 93
  | 117 -> Reduce 92
  | 291 -> Reduce 91
  | 1073 | 963 | 562 | 351 -> Reduce 90
  | 80 -> Reduce 9
  | 606 -> Reduce 89
  | 610 -> Reduce 88
  | 611 -> Reduce 87
  | 609 -> Reduce 86
  | 615 | 605 -> Reduce 85
  | 107 -> Reduce 84
  | 305 -> Reduce 83
  | 306 -> Reduce 82
  | 314 -> Reduce 81
  | 88 -> Reduce 80
  | 78 -> Reduce 8
  | 317 -> Reduce 79
  | 307 -> Reduce 78
  | 873 -> Reduce 77
  | 908 | 867 -> Reduce 76
  | 824 -> Reduce 75
  | 740 -> Reduce 732
  | 739 -> Reduce 731
  | 596 -> Reduce 730
  | 895 -> Reduce 73
  | 598 -> Reduce 729
  | 757 -> Reduce 728
  | 758 -> Reduce 727
  | 751 -> Reduce 726
  | 754 -> Reduce 725
  | 746 -> Reduce 724
  | 743 -> Reduce 723
  | 819 -> Reduce 722
  | 851 -> Reduce 720
  | 894 -> Reduce 72
  | 846 -> Reduce 719
  | 841 -> Reduce 718
  | 1297 -> Reduce 717
  | 1294 -> Reduce 716
  | 421 -> Reduce 715
  | 437 -> Reduce 714
  | 1076 -> Reduce 713
  | 537 -> Reduce 712
  | 346 -> Reduce 711
  | 456 | 286 -> Reduce 710
  | 891 -> Reduce 71
  | 252 -> Reduce 709
  | 176 -> Reduce 708
  | 582 -> Reduce 707
  | 580 -> Reduce 706
  | 729 -> Reduce 704
  | 732 -> Reduce 703
  | 747 -> Reduce 702
  | 734 -> Reduce 700
  | 923 -> Reduce 70
  | 222 -> Reduce 7
  | 735 -> Reduce 699
  | 730 -> Reduce 698
  | 921 | 100 -> Reduce 697
  | 165 | 91 -> Reduce 696
  | 660 -> Reduce 695
  | 641 -> Reduce 694
  | 665 -> Reduce 693
  | 667 -> Reduce 692
  | 653 -> Reduce 691
  | 656 -> Reduce 690
  | 925 -> Reduce 69
  | 613 -> Reduce 689
  | 668 -> Reduce 688
  | 612 -> Reduce 687
  | 657 -> Reduce 686
  | 601 -> Reduce 684
  | 678 -> Reduce 683
  | 676 -> Reduce 682
  | 1224 -> Reduce 681
  | 1228 -> Reduce 680
  | 926 -> Reduce 68
  | 1226 -> Reduce 679
  | 1430 -> Reduce 678
  | 1433 -> Reduce 677
  | 1434 -> Reduce 676
  | 1432 -> Reduce 675
  | 1431 -> Reduce 674
  | 1429 -> Reduce 673
  | 226 -> Reduce 671
  | 223 -> Reduce 670
  | 927 -> Reduce 67
  | 441 -> Reduce 669
  | 442 -> Reduce 668
  | 1542 -> Reduce 667
  | 1438 -> Reduce 666
  | 1546 -> Reduce 664
  | 1545 -> Reduce 663
  | 1496 -> Reduce 662
  | 1514 -> Reduce 661
  | 1528 -> Reduce 660
  | 924 -> Reduce 66
  | 1543 -> Reduce 659
  | 1478 -> Reduce 658
  | 1479 -> Reduce 657
  | 1481 -> Reduce 656
  | 1485 -> Reduce 655
  | 1505 -> Reduce 654
  | 1409 -> Reduce 653
  | 1406 -> Reduce 652
  | 1503 -> Reduce 651
  | 1541 | 1493 -> Reduce 650
  | 414 -> Reduce 65
  | 1455 -> Reduce 649
  | 1461 -> Reduce 648
  | 1465 -> Reduce 647
  | 1469 -> Reduce 646
  | 1447 -> Reduce 645
  | 1443 -> Reduce 644
  | 1451 -> Reduce 643
  | 1507 -> Reduce 642
  | 1405 -> Reduce 641
  | 1408 -> Reduce 640
  | 1355 -> Reduce 64
  | 1403 -> Reduce 639
  | 1537 | 1489 -> Reduce 638
  | 1532 -> Reduce 637
  | 1549 -> Reduce 636
  | 246 -> Reduce 635
  | 245 -> Reduce 634
  | 1235 -> Reduce 633
  | 1234 -> Reduce 632
  | 1222 -> Reduce 631
  | 1425 -> Reduce 630
  | 1358 -> Reduce 63
  | 1417 -> Reduce 629
  | 1418 -> Reduce 628
  | 1426 -> Reduce 627
  | 1427 -> Reduce 626
  | 1509 -> Reduce 625
  | 1511 -> Reduce 624
  | 28 -> Reduce 623
  | 29 -> Reduce 622
  | 30 -> Reduce 621
  | 31 -> Reduce 620
  | 1332 -> Reduce 62
  | 32 -> Reduce 619
  | 34 -> Reduce 618
  | 35 -> Reduce 617
  | 36 -> Reduce 616
  | 37 -> Reduce 615
  | 38 -> Reduce 614
  | 39 -> Reduce 613
  | 40 -> Reduce 612
  | 41 -> Reduce 611
  | 42 -> Reduce 610
  | 1348 -> Reduce 61
  | 43 -> Reduce 609
  | 44 -> Reduce 608
  | 45 -> Reduce 607
  | 46 -> Reduce 606
  | 47 -> Reduce 605
  | 48 -> Reduce 604
  | 49 -> Reduce 603
  | 50 -> Reduce 602
  | 51 -> Reduce 601
  | 53 -> Reduce 600
  | 1341 -> Reduce 60
  | 213 -> Reduce 6
  | 54 -> Reduce 599
  | 55 -> Reduce 598
  | 56 -> Reduce 597
  | 57 -> Reduce 596
  | 58 -> Reduce 595
  | 59 -> Reduce 594
  | 60 -> Reduce 593
  | 61 -> Reduce 592
  | 62 -> Reduce 591
  | 63 -> Reduce 590
  | 922 | 875 -> Reduce 59
  | 64 -> Reduce 589
  | 65 -> Reduce 588
  | 66 -> Reduce 587
  | 67 -> Reduce 586
  | 68 -> Reduce 585
  | 69 -> Reduce 584
  | 70 -> Reduce 583
  | 71 -> Reduce 582
  | 72 -> Reduce 581
  | 73 -> Reduce 580
  | 879 -> Reduce 58
  | 74 -> Reduce 579
  | 75 -> Reduce 578
  | 76 -> Reduce 577
  | 77 -> Reduce 576
  | 33 -> Reduce 575
  | 52 -> Reduce 574
  | 353 -> Reduce 573
  | 131 -> Reduce 572
  | 126 -> Reduce 571
  | 361 -> Reduce 570
  | 833 -> Reduce 57
  | 358 -> Reduce 569
  | 372 -> Reduce 568
  | 375 -> Reduce 567
  | 370 -> Reduce 566
  | 294 -> Reduce 565
  | 92 -> Reduce 564
  | 363 | 349 -> Reduce 563
  | 364 | 354 -> Reduce 562
  | 323 -> Reduce 561
  | 321 -> Reduce 560
  | 876 -> Reduce 56
  | 85 -> Reduce 559
  | 969 | 319 -> Reduce 558
  | 318 -> Reduce 557
  | 1109 -> Reduce 556
  | 1111 -> Reduce 555
  | 957 -> Reduce 554
  | 951 -> Reduce 553
  | 1274 -> Reduce 552
  | 1271 -> Reduce 551
  | 547 -> Reduce 550
  | 874 -> Reduce 55
  | 549 -> Reduce 549
  | 1060 -> Reduce 548
  | 485 -> Reduce 547
  | 1188 -> Reduce 546
  | 395 -> Reduce 545
  | 538 -> Reduce 544
  | 1375 -> Reduce 543
  | 1052 -> Reduce 542
  | 1191 -> Reduce 541
  | 1047 -> Reduce 540
  | 889 -> Reduce 54
  | 1371 -> Reduce 539
  | 1374 -> Reduce 538
  | 1072 -> Reduce 537
  | 1184 -> Reduce 536
  | 1120 | 1101 -> Reduce 535
  | 1117 | 1096 -> Reduce 534
  | 1114 | 1091 -> Reduce 533
  | 962 -> Reduce 532
  | 1121 | 1104 -> Reduce 531
  | 1285 -> Reduce 530
  | 1171 -> Reduce 529
  | 1173 -> Reduce 528
  | 1177 -> Reduce 527
  | 1176 -> Reduce 526
  | 1283 -> Reduce 525
  | 561 | 559 -> Reduce 524
  | 1024 | 958 -> Reduce 523
  | 959 -> Reduce 522
  | 536 -> Reduce 521
  | 1123 -> Reduce 520
  | 888 -> Reduce 52
  | 542 -> Reduce 519
  | 1079 | 540 -> Reduce 518
  | 1182 -> Reduce 517
  | 1180 -> Reduce 516
  | 392 -> Reduce 515
  | 956 -> Reduce 514
  | 918 -> Reduce 513
  | 916 -> Reduce 512
  | 210 -> Reduce 511
  | 913 | 206 -> Reduce 510
  | 887 -> Reduce 51
  | 635 | 622 -> Reduce 509
  | 637 | 624 -> Reduce 508
  | 209 -> Reduce 507
  | 159 -> Reduce 506
  | 240 -> Reduce 505
  | 237 -> Reduce 504
  | 184 -> Reduce 503
  | 235 -> Reduce 502
  | 233 -> Reduce 501
  | 198 -> Reduce 500
  | 885 -> Reduce 50
  | 401 -> Reduce 5
  | 229 -> Reduce 499
  | 268 -> Reduce 498
  | 194 -> Reduce 497
  | 148 -> Reduce 496
  | 169 -> Reduce 495
  | 172 -> Reduce 494
  | 269 -> Reduce 493
  | 195 -> Reduce 492
  | 190 -> Reduce 491
  | 142 -> Reduce 490
  | 864 -> Reduce 49
  | 151 -> Reduce 489
  | 266 -> Reduce 488
  | 192 -> Reduce 487
  | 102 -> Reduce 486
  | 103 -> Reduce 485
  | 104 -> Reduce 484
  | 106 -> Reduce 483
  | 105 -> Reduce 482
  | 109 -> Reduce 481
  | 110 -> Reduce 480
  | 848 -> Reduce 48
  | 111 -> Reduce 479
  | 113 -> Reduce 478
  | 112 -> Reduce 477
  | 324 -> Reduce 476
  | 939 -> Reduce 475
  | 938 -> Reduce 474
  | 892 -> Reduce 473
  | 930 -> Reduce 472
  | 802 -> Reduce 471
  | 936 -> Reduce 470
  | 878 -> Reduce 47
  | 781 -> Reduce 469
  | 782 -> Reduce 468
  | 788 -> Reduce 467
  | 774 -> Reduce 466
  | 776 -> Reduce 465
  | 813 -> Reduce 464
  | 682 -> Reduce 463
  | 679 -> Reduce 462
  | 811 -> Reduce 461
  | 578 -> Reduce 460
  | 835 -> Reduce 46
  | 815 -> Reduce 459
  | 599 -> Reduce 458
  | 681 -> Reduce 457
  | 594 -> Reduce 456
  | 935 -> Reduce 455
  | 940 -> Reduce 454
  | 695 -> Reduce 452
  | 691 -> Reduce 451
  | 696 -> Reduce 450
  | 817 -> Reduce 449
  | 1031 -> Reduce 448
  | 1030 -> Reduce 447
  | 1029 -> Reduce 446
  | 200 -> Reduce 445
  | 227 -> Reduce 444
  | 196 -> Reduce 443
  | 191 -> Reduce 442
  | 1077 -> Reduce 441
  | 1064 -> Reduce 440
  | 831 -> Reduce 44
  | 790 -> Reduce 439
  | 791 -> Reduce 438
  | 459 -> Reduce 437
  | 854 -> Reduce 435
  | 856 -> Reduce 434
  | 853 -> Reduce 433
  | 855 -> Reduce 432
  | 659 -> Reduce 430
  | 809 -> Reduce 428
  | 808 -> Reduce 427
  | 431 -> Reduce 426
  | 428 -> Reduce 424
  | 254 -> Reduce 423
  | 257 -> Reduce 422
  | 862 -> Reduce 421
  | 865 -> Reduce 420
  | 410 -> Reduce 42
  | 1392 -> Reduce 419
  | 383 -> Reduce 418
  | 244 -> Reduce 417
  | 247 -> Reduce 416
  | 508 -> Reduce 415
  | 516 -> Reduce 414
  | 368 -> Reduce 413
  | 371 -> Reduce 412
  | 381 | 329 -> Reduce 411
  | 327 -> Reduce 410
  | 407 -> Reduce 41
  | 348 -> Reduce 409
  | 356 -> Reduce 408
  | 362 -> Reduce 407
  | 333 -> Reduce 406
  | 382 -> Reduce 405
  | 331 -> Reduce 404
  | 350 -> Reduce 403
  | 355 -> Reduce 402
  | 325 -> Reduce 401
  | 347 -> Reduce 400
  | 147 -> Reduce 40
  | 402 -> Reduce 4
  | 320 -> Reduce 399
  | 1561 -> Reduce 398
  | 1361 -> Reduce 396
  | 283 -> Reduce 395
  | 281 -> Reduce 394
  | 279 -> Reduce 393
  | 280 -> Reduce 392
  | 132 -> Reduce 391
  | 423 -> Reduce 390
  | 144 -> Reduce 39
  | 422 -> Reduce 388
  | 585 -> Reduce 387
  | 587 -> Reduce 386
  | 590 -> Reduce 385
  | 677 -> Reduce 384
  | 592 -> Reduce 382
  | 593 -> Reduce 381
  | 588 -> Reduce 380
  | 1351 -> Reduce 38
  | 1440 -> Reduce 379
  | 650 | 298 -> Reduce 377
  | 1213 -> Reduce 375
  | 181 -> Reduce 373
  | 203 -> Reduce 370
  | 1349 -> Reduce 37
  | 122 -> Reduce 369
  | 119 -> Reduce 368
  | 340 -> Reduce 367
  | 344 -> Reduce 366
  | 343 -> Reduce 365
  | 341 -> Reduce 364
  | 123 -> Reduce 363
  | 313 -> Reduce 362
  | 288 -> Reduce 361
  | 338 -> Reduce 360
  | 1525 -> Reduce 36
  | 115 -> Reduce 359
  | 952 | 284 -> Reduce 358
  | 953 | 337 | 285 -> Reduce 357
  | 448 | 120 -> Reduce 356
  | 449 | 336 | 121 -> Reduce 355
  | 960 | 342 -> Reduce 354
  | 116 -> Reduce 353
  | 954 | 287 -> Reduce 352
  | 308 -> Reduce 351
  | 309 -> Reduce 350
  | 1523 -> Reduce 35
  | 310 -> Reduce 349
  | 311 -> Reduce 348
  | 312 -> Reduce 347
  | 399 | 118 -> Reduce 346
  | 955 | 339 -> Reduce 345
  | 701 -> Reduce 344
  | 521 -> Reduce 343
  | 241 -> Reduce 342
  | 242 -> Reduce 341
  | 189 -> Reduce 340
  | 1519 -> Reduce 34
  | 417 -> Reduce 339
  | 163 -> Reduce 337
  | 164 -> Reduce 336
  | 759 -> Reduce 335
  | 760 -> Reduce 334
  | 764 -> Reduce 333
  | 710 -> Reduce 332
  | 755 -> Reduce 331
  | 723 -> Reduce 330
  | 1370 -> Reduce 33
  | 942 -> Reduce 329
  | 722 -> Reduce 328
  | 787 -> Reduce 327
  | 717 -> Reduce 326
  | 715 -> Reduce 325
  | 1258 -> Reduce 324
  | 1264 -> Reduce 323
  | 1261 -> Reduce 322
  | 1255 -> Reduce 321
  | 1266 -> Reduce 320
  | 1269 -> Reduce 319
  | 712 -> Reduce 318
  | 714 -> Reduce 317
  | 947 -> Reduce 316
  | 453 -> Reduce 315
  | 716 -> Reduce 314
  | 771 -> Reduce 313
  | 769 -> Reduce 312
  | 768 -> Reduce 311
  | 1483 -> Reduce 310
  | 1369 -> Reduce 31
  | 1484 -> Reduce 309
  | 1206 -> Reduce 308
  | 1201 -> Reduce 307
  | 1197 -> Reduce 306
  | 1473 -> Reduce 305
  | 138 -> Reduce 304
  | 86 -> Reduce 303
  | 96 -> Reduce 302
  | 162 | 98 -> Reduce 301
  | 90 -> Reduce 300
  | 1368 -> Reduce 30
  | 1318 -> Reduce 299
  | 1321 -> Reduce 298
  | 1322 -> Reduce 297
  | 1304 -> Reduce 296
  | 1309 -> Reduce 295
  | 170 -> Reduce 294
  | 261 -> Reduce 293
  | 260 -> Reduce 292
  | 1164 -> Reduce 291
  | 1165 -> Reduce 290
  | 1327 -> Reduce 29
  | 1159 -> Reduce 289
  | 1161 -> Reduce 288
  | 1239 -> Reduce 287
  | 1238 -> Reduce 286
  | 512 -> Reduce 285
  | 510 -> Reduce 284
  | 965 -> Reduce 283
  | 475 -> Reduce 282
  | 1212 -> Reduce 281
  | 1041 -> Reduce 280
  | 1366 -> Reduce 28
  | 1042 -> Reduce 279
  | 973 -> Reduce 278
  | 1034 -> Reduce 277
  | 1244 -> Reduce 276
  | 1249 -> Reduce 275
  | 1250 -> Reduce 274
  | 1040 -> Reduce 273
  | 300 -> Reduce 272
  | 299 -> Reduce 271
  | 296 -> Reduce 270
  | 1324 -> Reduce 27
  | 295 -> Reduce 269
  | 301 -> Reduce 268
  | 365 -> Reduce 267
  | 1066 -> Reduce 266
  | 1067 -> Reduce 265
  | 1065 -> Reduce 264
  | 1068 -> Reduce 263
  | 1070 -> Reduce 262
  | 524 -> Reduce 261
  | 523 -> Reduce 260
  | 429 -> Reduce 26
  | 470 -> Reduce 259
  | 469 -> Reduce 258
  | 517 -> Reduce 257
  | 515 -> Reduce 256
  | 1216 -> Reduce 255
  | 1215 -> Reduce 254
  | 1110 -> Reduce 253
  | 1108 -> Reduce 252
  | 464 -> Reduce 251
  | 1075 | 139 -> Reduce 250
  | 1363 -> Reduce 25
  | 489 | 135 -> Reduce 249
  | 467 -> Reduce 248
  | 465 -> Reduce 247
  | 555 -> Reduce 246
  | 1085 -> Reduce 245
  | 1083 -> Reduce 244
  | 556 -> Reduce 243
  | 1087 -> Reduce 242
  | 651 -> Reduce 241
  | 654 -> Reduce 240
  | 1345 -> Reduce 24
  | 648 -> Reduce 239
  | 737 | 168 -> Reduce 238
  | 795 -> Reduce 237
  | 1556 -> Reduce 236
  | 1551 -> Reduce 235
  | 129 -> Reduce 234
  | 150 | 128 -> Reduce 233
  | 630 -> Reduce 232
  | 636 -> Reduce 231
  | 625 -> Reduce 230
  | 1350 -> Reduce 23
  | 762 -> Reduce 228
  | 761 -> Reduce 227
  | 568 -> Reduce 226
  | 569 -> Reduce 225
  | 944 -> Reduce 224
  | 570 -> Reduce 223
  | 1153 -> Reduce 222
  | 1152 -> Reduce 221
  | 1146 -> Reduce 220
  | 1353 -> Reduce 22
  | 1231 -> Reduce 219
  | 1232 -> Reduce 218
  | 799 -> Reduce 217
  | 1058 -> Reduce 216
  | 1185 -> Reduce 215
  | 256 -> Reduce 214
  | 1424 -> Reduce 213
  | 694 -> Reduce 212
  | 249 -> Reduce 211
  | 1395 -> Reduce 210
  | 1347 -> Reduce 21
  | 397 -> Reduce 209
  | 1045 -> Reduce 207
  | 1048 -> Reduce 206
  | 480 -> Reduce 205
  | 1126 -> Reduce 204
  | 1125 -> Reduce 203
  | 1280 | 1012 -> Reduce 202
  | 978 -> Reduce 201
  | 1023 -> Reduce 200
  | 1352 -> Reduce 20
  | 412 -> Reduce 199
  | 1174 -> Reduce 198
  | 545 -> Reduce 197
  | 1028 -> Reduce 196
  | 1103 -> Reduce 195
  | 1098 -> Reduce 194
  | 1093 -> Reduce 193
  | 1106 -> Reduce 192
  | 1027 -> Reduce 191
  | 1112 -> Reduce 190
  | 1346 -> Reduce 19
  | 1014 -> Reduce 189
  | 1022 -> Reduce 188
  | 1020 -> Reduce 187
  | 1018 -> Reduce 186
  | 990 -> Reduce 185
  | 1008 -> Reduce 184
  | 996 -> Reduce 183
  | 1010 -> Reduce 182
  | 988 -> Reduce 181
  | 980 -> Reduce 180
  | 933 -> Reduce 18
  | 992 -> Reduce 179
  | 994 -> Reduce 178
  | 982 -> Reduce 177
  | 984 -> Reduce 176
  | 986 -> Reduce 175
  | 998 -> Reduce 174
  | 1000 -> Reduce 173
  | 1002 -> Reduce 172
  | 1004 -> Reduce 171
  | 1006 -> Reduce 170
  | 932 -> Reduce 17
  | 1281 -> Reduce 169
  | 1016 -> Reduce 168
  | 1134 -> Reduce 167
  | 1391 -> Reduce 166
  | 1168 -> Reduce 165
  | 1170 -> Reduce 164
  | 560 -> Reduce 163
  | 1025 -> Reduce 162
  | 976 -> Reduce 161
  | 1379 -> Reduce 160
  | 929 -> Reduce 16
  | 1289 -> Reduce 159
  | 1154 -> Reduce 158
  | 1156 -> Reduce 157
  | 1162 -> Reduce 156
  | 1192 -> Reduce 155
  | 1204 -> Reduce 154
  | 1037 -> Reduce 153
  | 1211 -> Reduce 152
  | 1107 -> Reduce 151
  | 553 -> Reduce 150
  | 1531 -> Reduce 15
  | 1138 -> Reduce 149
  | 1145 -> Reduce 148
  | 1400 -> Reduce 147
  | 1385 -> Reduce 146
  | 1387 -> Reduce 145
  | 1383 -> Reduce 144
  | 1380 -> Reduce 143
  | 1293 -> Reduce 142
  | 1253 -> Reduce 141
  | 1078 -> Reduce 140
  | 1530 -> Reduce 14
  | 1081 -> Reduce 139
  | 8 -> Reduce 138
  | 9 -> Reduce 137
  | 10 -> Reduce 136
  | 11 -> Reduce 135
  | 6 -> Reduce 134
  | 5 -> Reduce 133
  | 7 -> Reduce 132
  | 4 -> Reduce 131
  | 15 -> Reduce 130
  | 1527 -> Reduce 13
  | 1 -> Reduce 129
  | 17 -> Reduce 128
  | 18 -> Reduce 127
  | 13 -> Reduce 126
  | 2 -> Reduce 125
  | 14 -> Reduce 124
  | 20 -> Reduce 123
  | 16 -> Reduce 122
  | 21 -> Reduce 121
  | 12 -> Reduce 120
  | 225 -> Reduce 12
  | 19 -> Reduce 119
  | 3 -> Reduce 118
  | 1129 -> Reduce 117
  | 1128 -> Reduce 116
  | 219 -> Reduce 115
  | 214 -> Reduce 114
  | 628 -> Reduce 113
  | 623 -> Reduce 112
  | 212 -> Reduce 111
  | 208 -> Reduce 110
  | 271 -> Reduce 109
  | 272 -> Reduce 108
  | 278 | 275 | 264 | 216 -> Reduce 107
  | 205 -> Reduce 103
  | 243 -> Reduce 102
  | 617 -> Reduce 101
  | 640 -> Reduce 100
  | 1394 -> Reduce 10
  | 1504 | 1416 | 1364 | 1342 | 1298 | 1163 | 880 | 836 | 828 | 812 | 789 | 783 | 756 | 690 | 671 | 614 | 600 | 415 | 199 -> Pop
  | 114 -> Parent (function
     | 84 | 114 | 289 | 290 | 302 | 304 | 316 | 326 | 330 | 349 | 354 | 328 | 332 | 367 | 378 | 380 | 405 | 502 | 509 | 518 | 528 | 532 | 1147 | 1163 | 1217 | 1288 | 1292 | 1378 | 1382 -> Shift (T T_RPAREN, default_value (T T_RPAREN))
     | 460 | 968 | 1038 | 1208 | 1335 | 1488 | 1492 | 1536 | 1540 -> Shift (N N_operator, default_value (N N_operator))
     | _ -> raise Not_found)
  | _ -> raise Not_found
