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
  [|0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;2;1;2;1;1;1;1;1;1;1;1;2;1;2;3;4;2;3;2;3;1;2;2;2;2;2;1;1;2;2;2;2;2;1;1;1;2;1;1;1;1;1;1;2;3;4;4;1;1;5;6;1;2;1;1;1;2;3;3;2;3;1;1;1;1;2;3;2;1;1;2;1;2;3;1;1;2;3;4;1;2;3;3;1;1;2;1;1;2;1;2;3;1;2;1;2;1;2;1;1;1;2;1;2;2;1;2;1;2;1;1;1;2;3;2;1;3;4;2;3;1;2;1;3;1;1;2;1;1;3;2;3;1;1;2;3;2;3;4;4;2;3;5;1;2;2;1;2;3;2;3;4;5;3;4;3;4;4;5;6;2;1;1;2;2;1;1;3;4;1;2;3;2;3;3;4;1;1;2;3;2;3;4;5;2;3;4;5;4;2;3;1;2;3;4;4;5;6;4;3;1;2;3;1;1;1;1;1;1;1;2;1;2;3;1;2;3;1;4;3;1;2;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;1;1;2;3;2;3;2;1;2;1;2;1;1;2;2;1;1;1;1;1;1;1;2;3;2;3;3;4;5;2;3;2;1;1;1;2;3;3;2;1;1;3;2;2;3;3;4;1;2;2;3;4;2;3;4;5;6;7;8;2;3;1;2;1;2;1;2;1;1;1;2;3;1;2;1;1;1;1;1;1;2;1;2;3;3;4;5;3;4;1;2;1;1;1;2;3;4;5;1;1;2;1;2;3;4;3;1;2;1;2;3;4;5;6;2;3;4;1;1;1;2;1;2;1;1;1;2;1;2;3;1;2;1;1;2;1;3;1;1;2;1;1;2;3;3;4;2;1;2;3;1;1;1;2;1;2;3;3;4;1;1;2;1;2;1;1;1;1;1;1;2;1;1;2;1;2;1;2;3;1;2;1;2;1;1;2;1;2;3;3;4;5;1;2;1;2;3;4;1;2;1;3;1;1;2;3;4;1;2;3;4;1;1;1;2;1;1;2;3;4;1;1;1;1;2;2;3;1;1;2;3;4;5;1;1;2;1;1;1;1;1;2;2;2;3;2;3;1;3;4;1;2;3;5;2;3;1;2;1;1;1;2;1;2;1;1;3;3;2;1;1;3;1;1;1;2;3;1;1;2;1;2;3;1;2;2;3;1;2;3;4;1;2;3;1;2;2;3;1;2;3;4;5;4;2;3;5;6;1;3;4;2;3;1;4;4;5;6;7;8;5;6;2;3;4;2;1;2;3;3;5;1;1;2;3;4;2;1;2;2;3;4;5;6;2;3;1;2;3;7;1;1;1;2;3;4;1;2;1;2;3;1;2;3;4;2;3;3;4;2;1;1;1;1;2;3;1;4;2;1;1;1;1;2;2;2;3;2;3;1;2;1;3;1;2;4;5;6;3;4;5;1;1;2;3;4;2;3;4;3;2;3;1;2;1;2;1;2;3;4;5;1;2;6;2;3;3;4;5;3;4;2;3;4;5;6;4;2;1;2;3;4;3;2;3;1;1;2;3;4;1;2;3;4;1;2;3;1;2;3;4;5;1;2;6;7;1;2;3;4;1;2;1;1;2;1;1;2;3;2;3;4;1;1;2;3;2;3;1;2;1;1;2;3;4;5;1;2;3;4;5;2;3;1;2;3;1;1;2;1;2;2;3;4;1;2;3;5;6;1;1;1;1;2;3;1;2;3;4;1;1;2;3;2;1;1;2;3;2;3;1;2;1;2;5;6;3;2;3;1;1;2;3;4;1;2;3;4;5;1;2;3;1;2;3;4;1;1;1;2;1;2;3;1;2;3;1;3;1;5;4;6;5;6;2;2;3;1;1;2;1;1;2;1;2;2;3;4;5;2;3;4;5;6;7;8;1;1;1;1;2;1;1;1;1;1;2;3;2;3;4;5;1;1;1;1;2;2;3;1;2;2;3;2;3;4;5;1;2;3;3;1;2;1;2;3;4;5;1;2;1;2;3;2;3;2;3;2;1;2;2;3;1;2;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;3;2;3;2;3;1;2;3;1;2;3;3;4;5;2;1;2;3;1;4;2;3;5;6;1;3;4;5;6;3;4;2;3;4;5;5;6;3;1;2;3;1;2;3;1;2;3;4;5;1;2;3;3;1;3;4;5;3;4;5;3;4;3;4;5;1;2;1;2;1;2;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;3;4;5;2;1;2;1;1;2;5;6;2;3;1;4;5;5;1;1;6;7;8;9;10;5;6;7;8;4;5;6;7;8;9;10;2;1;2;3;4;1;2;5;4;3;4;3;4;5;2;3;4;2;3;1;3;4;5;6;7;3;3;4;3;2;3;2;2;3;4;4;5;2;3;3;2;3;4;2;3;4;5;2;3;4;1;2;1;2;3;4;5;6;7;1;2;2;3;4;5;6;1;2;4;5;2;1;2;3;4;1;2;1;2;1;2;3;4;1;2;3;1;1;2;5;2;3;1;2;4;5;6;7;8;3;4;5;6;7;2;4;5;6;3;4;4;5;6;4;5;6;6;7;8;2;3;3;4;5;3;4;4;5;6;2;3;4;5;6;7;8;2;3;3;4;3;4;5;6;3;4;5;6;5;4;5;6;1;1;2;3;4;5;6;2;3;4;5;6;2;3;4;5;6;7;8;9;10;5;6;7;4;2;3;1;2;3;1;2;1;2;3;1;1;2;3;4;1;2;3;4;1;1;2;1;1;2;1;3;2;2;2;5;2;3;3;4;5;3;1;2;4;5;1;2;3;1;2;1;2;2;2;3;4;2;3;4;5;6;3;4;5;6;7;8;4;5;3;4;5;6;4;3;4;3;2;3;4;5;6;1;2;3;4;5;2;3;4;2;1;2;3;4;5;6;2;3;3;1;2;1;1;3;4;7;1;1;2;3;4;4;4;4;4;1;2;1;2;1;1;2;3;4;1;2;3;4;1;2;3;4;1;2;3;4;1;2;3;4;5;6;1;2;3;4;1;2;3;4;1;1;2;3;2;3;4;5;6;4;2;3;2;3;1;2;1;2;3;4;1;2;3;4;1;2;3;1;2;3;4;5;6;7;1;2;3;4;1;2;1;2;1;2;3;1;2;3;1;2;1;2;3;4;1;2;4;5;2;2;3;1;2;1;1;2;3;4;1;2;3;4;2;1;1;2;1;2;3;4;1;2;1;0;1;2;1;0;1;2;1;|]

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
  let r0 = [R 615] in
  let r1 = [R 126] in
  let r2 = S (T T_DONE) :: r1 in
  let r3 = S (N N_seq_expr) :: r2 in
  let r4 = S (T T_DO) :: r3 in
  let r5 = S (N N_seq_expr) :: r4 in
  let r6 = [R 189] in
  let r7 = S (N N_attributes) :: r6 in
  let r8 = [R 8] in
  let r9 = [R 9] in
  let r10 = S (T T_RBRACKET) :: r9 in
  let r11 = S (N N_payload) :: r10 in
  let r12 = [R 397] in
  let r13 = [R 543] in
  let r14 = [R 675] in
  let r15 = S (T T_LIDENT) :: r14 in
  let r16 = S (T T_DOT) :: r15 in
  let r17 = [R 281] in
  let r18 = S (T T_RPAREN) :: r17 in
  let r19 = [R 280] in
  let r20 = [R 461] in
  let r21 = [R 456] in
  let r22 = [R 689] in
  let r23 = S (T T_RPAREN) :: r22 in
  let r24 = [R 548] in
  let r25 = S (T T_RPAREN) :: r24 in
  let r26 = [R 91] in
  let r27 = [R 550] in
  let r28 = S (T T_RPAREN) :: r27 in
  let r29 = [R 551] in
  let r30 = S (T T_RPAREN) :: r29 in
  let r31 = [R 371] in
  let r32 = [R 372] in
  let r33 = S (N N_core_type) :: r32 in
  let r34 = S (T T_EQUAL) :: r33 in
  let r35 = [R 229] in
  let r36 = S (T T_LIDENT) :: r35 in
  let r37 = [R 283] in
  let r38 = [R 475] in
  let r39 = [R 39] in
  let r40 = S (T T_LIDENT) :: r39 in
  let r41 = [R 468] in
  let r42 = [R 104] in
  let r43 = S (N N_core_type2) :: r42 in
  let r44 = S (T T_MINUSGREATER) :: r43 in
  let r45 = S (N N_core_type2) :: r44 in
  let r46 = S (T T_COLON) :: r45 in
  let r47 = [R 105] in
  let r48 = S (N N_core_type2) :: r47 in
  let r49 = S (T T_MINUSGREATER) :: r48 in
  let r50 = [R 467] in
  let r51 = S (T T_RPAREN) :: r50 in
  let r52 = [R 485] in
  let r53 = S (T T_RPAREN) :: r52 in
  let r54 = [R 316] in
  let r55 = S (N N_ident) :: r54 in
  let r56 = [R 106] in
  let r57 = S (N N_core_type2) :: r56 in
  let r58 = S (T T_MINUSGREATER) :: r57 in
  let r59 = [R 474] in
  let r60 = [R 473] in
  let r61 = [R 193] in
  let r62 = S (N N_attributes) :: r61 in
  let r63 = S (N N_poly_type_no_attr) :: r62 in
  let r64 = [R 686] in
  let r65 = [R 190] in
  let r66 = S (T T_RBRACKET) :: r65 in
  let r67 = S (N N_payload) :: r66 in
  let r68 = [R 396] in
  let r69 = [R 483] in
  let r70 = S (T T_RBRACKET) :: r69 in
  let r71 = S (N N_row_field_list) :: r70 in
  let r72 = [R 482] in
  let r73 = [R 481] in
  let r74 = S (T T_RBRACKET) :: r73 in
  let r75 = [R 479] in
  let r76 = S (T T_RBRACKET) :: r75 in
  let r77 = S (N N_row_field_list) :: r76 in
  let r78 = [R 319] in
  let r79 = [R 476] in
  let r80 = [R 422] in
  let r81 = S (N N_simple_core_type) :: r80 in
  let r82 = [R 424] in
  let r83 = [R 649] in
  let r84 = [R 648] in
  let r85 = S (N N_attributes) :: r84 in
  let r86 = S (N N_amper_type_list) :: r85 in
  let r87 = [R 490] in
  let r88 = [R 111] in
  let r89 = [R 107] in
  let r90 = [R 115] in
  let r91 = S (N N_ident) :: r90 in
  let r92 = [R 6] in
  let r93 = [R 11] in
  let r94 = [R 478] in
  let r95 = [R 480] in
  let r96 = S (T T_RBRACKET) :: r95 in
  let r97 = S (N N_row_field_list) :: r96 in
  let r98 = [R 484] in
  let r99 = S (T T_RBRACKET) :: r98 in
  let r100 = [R 402] in
  let r101 = S (N N_core_type_no_attr) :: r100 in
  let r102 = [R 687] in
  let r103 = [R 272] in
  let r104 = [R 477] in
  let r105 = [R 109] in
  let r106 = [R 374] in
  let r107 = [R 547] in
  let r108 = [R 545] in
  let r109 = S (T T_RBRACKET) :: r108 in
  let r110 = R 355 :: r109 in
  let r111 = [R 90] in
  let r112 = [R 544] in
  let r113 = S (T T_RBRACE) :: r112 in
  let r114 = [R 250] in
  let r115 = [R 246] in
  let r116 = [R 386] in
  let r117 = [R 387] in
  let r118 = [R 540] in
  let r119 = [R 389] in
  let r120 = [R 383] in
  let r121 = [R 382] in
  let r122 = [R 381] in
  let r123 = [R 390] in
  let r124 = [R 549] in
  let r125 = S (T T_RPAREN) :: r124 in
  let r126 = [R 385] in
  let r127 = [R 379] in
  let r128 = [R 546] in
  let r129 = S (T T_BARRBRACKET) :: r128 in
  let r130 = [R 384] in
  let r131 = S (T T_RPAREN) :: r130 in
  let r132 = S (N N_pattern) :: r131 in
  let r133 = S (T T_COMMA) :: r132 in
  let r134 = S (N N_pattern) :: r133 in
  let r135 = S (T T_LPAREN) :: r134 in
  let r136 = [R 398] in
  let r137 = [R 145] in
  let r138 = S (T T_DONE) :: r137 in
  let r139 = S (N N_seq_expr) :: r138 in
  let r140 = S (T T_DO) :: r139 in
  let r141 = S (N N_seq_expr) :: r140 in
  let r142 = [R 122] in
  let r143 = S (N N_seq_expr) :: r142 in
  let r144 = [R 139] in
  let r145 = S (N N_match_cases) :: r144 in
  let r146 = R 351 :: r145 in
  let r147 = S (T T_WITH) :: r146 in
  let r148 = S (N N_seq_expr) :: r147 in
  let r149 = [R 522] in
  let r150 = [R 524] in
  let r151 = S (N N_class_longident) :: r150 in
  let r152 = [R 188] in
  let r153 = [R 504] in
  let r154 = S (T T_RPAREN) :: r153 in
  let r155 = [R 518] in
  let r156 = [R 64] in
  let r157 = S (N N_class_fields) :: r156 in
  let r158 = R 42 :: r157 in
  let r159 = [R 178] in
  let r160 = S (T T_END) :: r159 in
  let r161 = Sub (r158) :: r160 in
  let r162 = [R 40] in
  let r163 = S (T T_RPAREN) :: r162 in
  let r164 = [R 41] in
  let r165 = S (T T_RPAREN) :: r164 in
  let r166 = [R 694] in
  let r167 = S (N N_seq_expr) :: r166 in
  let r168 = S (T T_EQUAL) :: r167 in
  let r169 = S (N N_label) :: r168 in
  let r170 = R 317 :: r169 in
  let r171 = R 368 :: r170 in
  let r172 = [R 25] in
  let r173 = S (N N_post_item_attributes) :: r172 in
  let r174 = [R 693] in
  let r175 = S (N N_core_type) :: r174 in
  let r176 = S (T T_COLON) :: r175 in
  let r177 = S (N N_label) :: r176 in
  let r178 = [R 403] in
  let r179 = S (T T_RBRACKET) :: r178 in
  let r180 = S (N N_payload) :: r179 in
  let r181 = [R 405] in
  let r182 = [R 692] in
  let r183 = S (N N_core_type) :: r182 in
  let r184 = S (T T_COLON) :: r183 in
  let r185 = [R 121] in
  let r186 = S (N N_match_cases) :: r185 in
  let r187 = R 351 :: r186 in
  let r188 = S (T T_WITH) :: r187 in
  let r189 = S (N N_seq_expr) :: r188 in
  let r190 = [R 138] in
  let r191 = S (N N_match_cases) :: r190 in
  let r192 = R 351 :: r191 in
  let r193 = S (T T_WITH) :: r192 in
  let r194 = S (N N_seq_expr) :: r193 in
  let r195 = [R 530] in
  let r196 = S (T T_RPAREN) :: r195 in
  let r197 = [R 294] in
  let r198 = S (T T_END) :: r197 in
  let r199 = [R 299] in
  let r200 = S (T T_RPAREN) :: r199 in
  let r201 = [R 300] in
  let r202 = S (T T_RPAREN) :: r201 in
  let r203 = [R 120] in
  let r204 = S (N N_seq_expr) :: r203 in
  let r205 = S (T T_IN) :: r204 in
  let r206 = S (N N_let_bindings) :: r205 in
  let r207 = S (N N_rec_flag) :: r206 in
  let r208 = [R 253] in
  let r209 = [R 238] in
  let r210 = [R 237] in
  let r211 = S (T T_RPAREN) :: r210 in
  let r212 = [R 227] in
  let r213 = [R 234] in
  let r214 = [R 233] in
  let r215 = S (T T_RPAREN) :: r214 in
  let r216 = R 353 :: r215 in
  let r217 = [R 354] in
  let r218 = [R 134] in
  let r219 = S (N N_seq_expr) :: r218 in
  let r220 = S (T T_IN) :: r219 in
  let r221 = S (N N_expr_open) :: r220 in
  let r222 = [R 184] in
  let r223 = S (N N_mod_longident) :: r222 in
  let r224 = [R 520] in
  let r225 = S (T T_RBRACKET) :: r224 in
  let r226 = R 355 :: r225 in
  let r227 = [R 526] in
  let r228 = [R 194] in
  let r229 = S (N N_expr) :: r228 in
  let r230 = [R 515] in
  let r231 = S (T T_RBRACE) :: r230 in
  let r232 = [R 495] in
  let r233 = S (T T_RPAREN) :: r232 in
  let r234 = S (T T_LPAREN) :: r233 in
  let r235 = [R 506] in
  let r236 = [R 505] in
  let r237 = S (T T_GREATERDOT) :: r236 in
  let r238 = [R 177] in
  let r239 = S (N N_simple_expr) :: r238 in
  let r240 = [R 493] in
  let r241 = [R 508] in
  let r242 = S (T T_END) :: r241 in
  let r243 = [R 144] in
  let r244 = S (N N_expr) :: r243 in
  let r245 = S (T T_THEN) :: r244 in
  let r246 = S (N N_seq_expr) :: r245 in
  let r247 = [R 135] in
  let r248 = S (N N_match_cases) :: r247 in
  let r249 = R 351 :: r248 in
  let r250 = [R 267] in
  let r251 = S (N N_seq_expr) :: r250 in
  let r252 = S (T T_MINUSGREATER) :: r251 in
  let r253 = [R 268] in
  let r254 = S (N N_seq_expr) :: r253 in
  let r255 = S (T T_MINUSGREATER) :: r254 in
  let r256 = [R 199] in
  let r257 = S (N N_seq_expr) :: r256 in
  let r258 = S (T T_MINUSGREATER) :: r257 in
  let r259 = [R 137] in
  let r260 = Sub (r258) :: r259 in
  let r261 = S (N N_newtype) :: r260 in
  let r262 = [R 394] in
  let r263 = S (T T_UNDERSCORE) :: r262 in
  let r264 = [R 236] in
  let r265 = [R 235] in
  let r266 = S (T T_RPAREN) :: r265 in
  let r267 = R 353 :: r266 in
  let r268 = [R 264] in
  let r269 = [R 322] in
  let r270 = S (T T_RPAREN) :: r269 in
  let r271 = [R 239] in
  let r272 = [R 240] in
  let r273 = [R 128] in
  let r274 = S (T T_DONE) :: r273 in
  let r275 = S (N N_seq_expr) :: r274 in
  let r276 = S (T T_DO) :: r275 in
  let r277 = S (N N_seq_expr) :: r276 in
  let r278 = S (T T_IN) :: r277 in
  let r279 = S (N N_pattern) :: r278 in
  let r280 = [R 146] in
  let r281 = S (T T_DONE) :: r280 in
  let r282 = S (N N_seq_expr) :: r281 in
  let r283 = S (T T_DO) :: r282 in
  let r284 = S (N N_seq_expr) :: r283 in
  let r285 = S (N N_direction_flag) :: r284 in
  let r286 = S (N N_seq_expr) :: r285 in
  let r287 = S (T T_EQUAL) :: r286 in
  let r288 = S (N N_pattern) :: r287 in
  let r289 = [R 523] in
  let r290 = [R 691] in
  let r291 = S (N N_val_ident) :: r290 in
  let r292 = S (T T_DOT) :: r291 in
  let r293 = [R 511] in
  let r294 = S (T T_RPAREN) :: r293 in
  let r295 = [R 532] in
  let r296 = S (T T_RPAREN) :: r295 in
  let r297 = S (N N_package_type) :: r296 in
  let r298 = S (T T_COLON) :: r297 in
  let r299 = [R 295] in
  let r300 = S (N N_module_expr) :: r299 in
  let r301 = S (T T_MINUSGREATER) :: r300 in
  let r302 = [R 202] in
  let r303 = [R 203] in
  let r304 = S (T T_RPAREN) :: r303 in
  let r305 = S (N N_module_type) :: r304 in
  let r306 = [R 308] in
  let r307 = S (T T_END) :: r306 in
  let r308 = [R 439] in
  let r309 = S (N N_post_item_attributes) :: r308 in
  let r310 = S (N N_core_type) :: r309 in
  let r311 = S (T T_COLON) :: r310 in
  let r312 = [R 441] in
  let r313 = [R 435] in
  let r314 = [R 364] in
  let r315 = S (T T_RPAREN) :: r314 in
  let r316 = [R 359] in
  let r317 = [R 365] in
  let r318 = [R 361] in
  let r319 = [R 437] in
  let r320 = [R 707] in
  let r321 = [R 660] in
  let r322 = S (N N_post_item_attributes) :: r321 in
  let r323 = R 97 :: r322 in
  let r324 = R 663 :: r323 in
  let r325 = S (T T_LIDENT) :: r324 in
  let r326 = S (N N_optional_type_parameters) :: r325 in
  let r327 = [R 662] in
  let r328 = [R 664] in
  let r329 = [R 665] in
  let r330 = [R 85] in
  let r331 = [R 86] in
  let r332 = S (T T_COLONCOLON) :: r331 in
  let r333 = [R 98] in
  let r334 = S (N N_attributes) :: r333 in
  let r335 = S (N N_generalized_constructor_arguments) :: r334 in
  let r336 = Sub (r332) :: r335 in
  let r337 = [R 100] in
  let r338 = [R 209] in
  let r339 = [R 488] in
  let r340 = S (T T_RPAREN) :: r339 in
  let r341 = S (N N_core_type_comma_list) :: r340 in
  let r342 = [R 113] in
  let r343 = S (N N_simple_core_type_no_attr) :: r342 in
  let r344 = [R 211] in
  let r345 = [R 210] in
  let r346 = S (N N_simple_core_type_no_attr) :: r345 in
  let r347 = [R 668] in
  let r348 = S (N N_constructor_declarations) :: r347 in
  let r349 = [R 669] in
  let r350 = S (T T_RBRACE) :: r349 in
  let r351 = R 355 :: r350 in
  let r352 = [R 218] in
  let r353 = S (N N_attributes) :: r352 in
  let r354 = S (N N_poly_type_no_attr) :: r353 in
  let r355 = S (T T_COLON) :: r354 in
  let r356 = [R 673] in
  let r357 = [R 670] in
  let r358 = S (N N_constructor_declarations) :: r357 in
  let r359 = [R 671] in
  let r360 = S (T T_RBRACE) :: r359 in
  let r361 = R 355 :: r360 in
  let r362 = [R 94] in
  let r363 = S (N N_core_type) :: r362 in
  let r364 = S (T T_EQUAL) :: r363 in
  let r365 = S (N N_core_type) :: r364 in
  let r366 = [R 96] in
  let r367 = [R 436] in
  let r368 = [R 191] in
  let r369 = S (N N_attributes) :: r368 in
  let r370 = S (N N_generalized_constructor_arguments) :: r369 in
  let r371 = Sub (r332) :: r370 in
  let r372 = [R 429] in
  let r373 = Sub (r371) :: r372 in
  let r374 = [R 431] in
  let r375 = S (N N_post_item_attributes) :: r374 in
  let r376 = Sub (r373) :: r375 in
  let r377 = R 351 :: r376 in
  let r378 = R 408 :: r377 in
  let r379 = [R 430] in
  let r380 = [R 433] in
  let r381 = [R 323] in
  let r382 = S (N N_post_item_attributes) :: r381 in
  let r383 = S (N N_mod_longident) :: r382 in
  let r384 = [R 447] in
  let r385 = S (N N_post_item_attributes) :: r384 in
  let r386 = S (N N_ident) :: r385 in
  let r387 = [R 444] in
  let r388 = S (N N_post_item_attributes) :: r387 in
  let r389 = [R 292] in
  let r390 = S (N N_module_declaration) :: r389 in
  let r391 = [R 291] in
  let r392 = S (N N_module_declaration) :: r391 in
  let r393 = S (T T_RPAREN) :: r392 in
  let r394 = S (N N_module_type) :: r393 in
  let r395 = [R 311] in
  let r396 = S (N N_module_expr) :: r395 in
  let r397 = S (T T_OF) :: r396 in
  let r398 = [R 297] in
  let r399 = [R 296] in
  let r400 = [R 312] in
  let r401 = S (T T_RPAREN) :: r400 in
  let r402 = [R 309] in
  let r403 = S (N N_module_type) :: r402 in
  let r404 = S (T T_MINUSGREATER) :: r403 in
  let r405 = [R 310] in
  let r406 = [R 702] in
  let r407 = S (N N_core_type_no_attr) :: r406 in
  let r408 = S (T T_COLONEQUAL) :: r407 in
  let r409 = S (N N_label) :: r408 in
  let r410 = [R 681] in
  let r411 = S (T T_RPAREN) :: r410 in
  let r412 = [R 676] in
  let r413 = [R 682] in
  let r414 = [R 678] in
  let r415 = [R 701] in
  let r416 = R 97 :: r415 in
  let r417 = S (N N_core_type_no_attr) :: r416 in
  let r418 = [R 703] in
  let r419 = S (N N_mod_ext_longident) :: r418 in
  let r420 = S (T T_EQUAL) :: r419 in
  let r421 = S (N N_mod_longident) :: r420 in
  let r422 = [R 704] in
  let r423 = S (N N_mod_ext_longident) :: r422 in
  let r424 = S (T T_MODULE) :: r421 in
  let r425 = [R 706] in
  let r426 = [R 290] in
  let r427 = [R 445] in
  let r428 = S (N N_post_item_attributes) :: r427 in
  let r429 = [R 448] in
  let r430 = S (N N_post_item_attributes) :: r429 in
  let r431 = [R 306] in
  let r432 = S (N N_post_item_attributes) :: r431 in
  let r433 = S (N N_module_type) :: r432 in
  let r434 = S (T T_COLON) :: r433 in
  let r435 = S (T T_UIDENT) :: r434 in
  let r436 = [R 417] in
  let r437 = Sub (r435) :: r436 in
  let r438 = [R 446] in
  let r439 = [R 418] in
  let r440 = [R 216] in
  let r441 = S (T T_RBRACKET) :: r440 in
  let r442 = S (N N_payload) :: r441 in
  let r443 = [R 196] in
  let r444 = S (T T_RBRACKET) :: r443 in
  let r445 = S (N N_payload) :: r444 in
  let r446 = [R 450] in
  let r447 = S (N N_post_item_attributes) :: r446 in
  let r448 = [R 440] in
  let r449 = S (N N_post_item_attributes) :: r448 in
  let r450 = S (N N_primitive_declaration) :: r449 in
  let r451 = S (T T_EQUAL) :: r450 in
  let r452 = S (N N_core_type) :: r451 in
  let r453 = S (T T_COLON) :: r452 in
  let r454 = [R 428] in
  let r455 = S (N N_post_item_attributes) :: r454 in
  let r456 = Sub (r371) :: r455 in
  let r457 = [R 443] in
  let r458 = [R 438] in
  let r459 = [R 451] in
  let r460 = [R 452] in
  let r461 = [R 55] in
  let r462 = S (N N_clty_longident) :: r461 in
  let r463 = [R 70] in
  let r464 = S (N N_post_item_attributes) :: r463 in
  let r465 = Sub (r462) :: r464 in
  let r466 = S (T T_EQUAL) :: r465 in
  let r467 = S (T T_LIDENT) :: r466 in
  let r468 = [R 74] in
  let r469 = S (T T_RBRACKET) :: r468 in
  let r470 = [R 45] in
  let r471 = R 52 :: r470 in
  let r472 = R 44 :: r471 in
  let r473 = [R 56] in
  let r474 = S (T T_END) :: r473 in
  let r475 = [R 43] in
  let r476 = S (T T_RPAREN) :: r475 in
  let r477 = [R 698] in
  let r478 = S (N N_core_type) :: r477 in
  let r479 = S (T T_COLON) :: r478 in
  let r480 = S (N N_label) :: r479 in
  let r481 = [R 47] in
  let r482 = S (N N_post_item_attributes) :: r481 in
  let r483 = [R 696] in
  let r484 = S (N N_core_type) :: r483 in
  let r485 = S (T T_COLON) :: r484 in
  let r486 = S (N N_label) :: r485 in
  let r487 = [R 697] in
  let r488 = S (N N_core_type) :: r487 in
  let r489 = S (T T_COLON) :: r488 in
  let r490 = S (N N_label) :: r489 in
  let r491 = [R 48] in
  let r492 = S (N N_post_item_attributes) :: r491 in
  let r493 = S (N N_poly_type) :: r492 in
  let r494 = S (T T_COLON) :: r493 in
  let r495 = S (N N_label) :: r494 in
  let r496 = [R 400] in
  let r497 = S (N N_core_type) :: r496 in
  let r498 = [R 46] in
  let r499 = S (N N_post_item_attributes) :: r498 in
  let r500 = [R 54] in
  let r501 = S (N N_clty_longident) :: r500 in
  let r502 = S (T T_RBRACKET) :: r501 in
  let r503 = [R 76] in
  let r504 = S (T T_LIDENT) :: r503 in
  let r505 = [R 95] in
  let r506 = S (N N_core_type) :: r505 in
  let r507 = S (T T_EQUAL) :: r506 in
  let r508 = S (N N_core_type) :: r507 in
  let r509 = [R 49] in
  let r510 = S (N N_post_item_attributes) :: r509 in
  let r511 = [R 50] in
  let r512 = [R 71] in
  let r513 = [R 65] in
  let r514 = Sub (r462) :: r513 in
  let r515 = [R 15] in
  let r516 = S (N N_post_item_attributes) :: r515 in
  let r517 = Sub (r514) :: r516 in
  let r518 = S (T T_COLON) :: r517 in
  let r519 = S (T T_LIDENT) :: r518 in
  let r520 = [R 66] in
  let r521 = Sub (r514) :: r520 in
  let r522 = S (T T_MINUSGREATER) :: r521 in
  let r523 = S (N N_simple_core_type_or_tuple_no_attr) :: r522 in
  let r524 = S (T T_COLON) :: r523 in
  let r525 = [R 67] in
  let r526 = Sub (r514) :: r525 in
  let r527 = S (T T_MINUSGREATER) :: r526 in
  let r528 = [R 68] in
  let r529 = Sub (r514) :: r528 in
  let r530 = S (T T_MINUSGREATER) :: r529 in
  let r531 = S (N N_simple_core_type_or_tuple_no_attr) :: r530 in
  let r532 = [R 69] in
  let r533 = Sub (r514) :: r532 in
  let r534 = [R 492] in
  let r535 = [R 16] in
  let r536 = [R 434] in
  let r537 = [R 453] in
  let r538 = [R 176] in
  let r539 = S (N N_simple_expr) :: r538 in
  let r540 = [R 497] in
  let r541 = S (N N_label) :: r540 in
  let r542 = [R 498] in
  let r543 = [R 169] in
  let r544 = [R 222] in
  let r545 = [R 119] in
  let r546 = [R 529] in
  let r547 = [R 510] in
  let r548 = S (N N_label_longident) :: r547 in
  let r549 = [R 512] in
  let r550 = S (T T_RPAREN) :: r549 in
  let r551 = S (N N_seq_expr) :: r550 in
  let r552 = [R 519] in
  let r553 = S (T T_BARRBRACKET) :: r552 in
  let r554 = R 355 :: r553 in
  let r555 = [R 132] in
  let r556 = S (N N_seq_expr) :: r555 in
  let r557 = S (T T_IN) :: r556 in
  let r558 = S (N N_let_bindings) :: r557 in
  let r559 = [R 257] in
  let r560 = S (N N_seq_expr) :: r559 in
  let r561 = S (T T_EQUAL) :: r560 in
  let r562 = [R 175] in
  let r563 = S (N N_expr) :: r562 in
  let r564 = [R 180] in
  let r565 = [R 159] in
  let r566 = [R 153] in
  let r567 = [R 528] in
  let r568 = [R 170] in
  let r569 = [R 156] in
  let r570 = [R 160] in
  let r571 = [R 152] in
  let r572 = [R 155] in
  let r573 = [R 154] in
  let r574 = [R 164] in
  let r575 = [R 158] in
  let r576 = [R 157] in
  let r577 = [R 162] in
  let r578 = [R 151] in
  let r579 = [R 150] in
  let r580 = [R 147] in
  let r581 = [R 149] in
  let r582 = [R 163] in
  let r583 = [R 161] in
  let r584 = [R 165] in
  let r585 = [R 166] in
  let r586 = [R 167] in
  let r587 = [R 181] in
  let r588 = [R 168] in
  let r589 = [R 256] in
  let r590 = S (N N_seq_expr) :: r589 in
  let r591 = [R 259] in
  let r592 = [R 252] in
  let r593 = [R 521] in
  let r594 = S (T T_RBRACKET) :: r593 in
  let r595 = R 355 :: r594 in
  let r596 = [R 527] in
  let r597 = S (T T_GREATERRBRACE) :: r596 in
  let r598 = R 355 :: r597 in
  let r599 = [R 195] in
  let r600 = S (N N_expr) :: r599 in
  let r601 = [R 516] in
  let r602 = S (T T_RBRACE) :: r601 in
  let r603 = [R 419] in
  let r604 = S (N N_lbl_expr_list) :: r603 in
  let r605 = [R 241] in
  let r606 = [R 513] in
  let r607 = S (T T_RBRACKET) :: r606 in
  let r608 = S (N N_seq_expr) :: r607 in
  let r609 = [R 514] in
  let r610 = S (T T_RBRACE) :: r609 in
  let r611 = S (N N_expr) :: r610 in
  let r612 = [R 118] in
  let r613 = [R 223] in
  let r614 = [R 224] in
  let r615 = [R 221] in
  let r616 = [R 172] in
  let r617 = [R 173] in
  let r618 = [R 174] in
  let r619 = [R 171] in
  let r620 = [R 499] in
  let r621 = [R 182] in
  let r622 = [R 127] in
  let r623 = S (T T_DONE) :: r622 in
  let r624 = S (N N_seq_expr) :: r623 in
  let r625 = S (T T_DO) :: r624 in
  let r626 = S (N N_seq_expr) :: r625 in
  let r627 = S (N N_direction_flag) :: r626 in
  let r628 = [R 201] in
  let r629 = Sub (r258) :: r628 in
  let r630 = S (T T_RPAREN) :: r629 in
  let r631 = [R 200] in
  let r632 = [R 136] in
  let r633 = S (N N_pattern) :: r252 in
  let r634 = [R 270] in
  let r635 = [R 143] in
  let r636 = [R 507] in
  let r637 = [R 496] in
  let r638 = [R 525] in
  let r639 = S (T T_GREATERRBRACE) :: r638 in
  let r640 = [R 133] in
  let r641 = S (N N_seq_expr) :: r640 in
  let r642 = S (T T_IN) :: r641 in
  let r643 = S (N N_module_binding_body) :: r642 in
  let r644 = S (T T_UIDENT) :: r643 in
  let r645 = [R 285] in
  let r646 = [R 286] in
  let r647 = S (N N_module_expr) :: r646 in
  let r648 = S (T T_EQUAL) :: r647 in
  let r649 = [R 287] in
  let r650 = [R 131] in
  let r651 = S (N N_seq_expr) :: r650 in
  let r652 = S (T T_IN) :: r651 in
  let r653 = S (N N_let_bindings_no_attrs) :: r652 in
  let r654 = [R 612] in
  let r655 = S (N N_fun_binding) :: r654 in
  let r656 = S (T T_RPAREN) :: r655 in
  let r657 = S (T T_LIDENT) :: r656 in
  let r658 = [R 610] in
  let r659 = S (N N_seq_expr) :: r658 in
  let r660 = [R 659] in
  let r661 = [R 657] in
  let r662 = [R 658] in
  let r663 = [R 198] in
  let r664 = S (N N_seq_expr) :: r663 in
  let r665 = [R 611] in
  let r666 = [R 255] in
  let r667 = S (N N_seq_expr) :: r666 in
  let r668 = S (T T_EQUAL) :: r667 in
  let r669 = S (N N_core_type) :: r668 in
  let r670 = S (T T_DOT) :: r669 in
  let r671 = [R 254] in
  let r672 = S (N N_seq_expr) :: r671 in
  let r673 = S (T T_EQUAL) :: r672 in
  let r674 = S (N N_core_type) :: r673 in
  let r675 = [R 303] in
  let r676 = S (T T_RPAREN) :: r675 in
  let r677 = [R 301] in
  let r678 = S (T T_RPAREN) :: r677 in
  let r679 = [R 302] in
  let r680 = S (T T_RPAREN) :: r679 in
  let r681 = [R 298] in
  let r682 = S (T T_RPAREN) :: r681 in
  let r683 = [R 531] in
  let r684 = S (T T_RPAREN) :: r683 in
  let r685 = [R 148] in
  let r686 = S (T T_RPAREN) :: r685 in
  let r687 = S (N N_expr) :: r686 in
  let r688 = S (T T_COMMA) :: r687 in
  let r689 = S (N N_expr) :: r688 in
  let r690 = S (T T_LPAREN) :: r689 in
  let r691 = [R 509] in
  let r692 = [R 695] in
  let r693 = S (N N_seq_expr) :: r692 in
  let r694 = [R 274] in
  let r695 = S (N N_poly_type) :: r694 in
  let r696 = S (T T_COLON) :: r695 in
  let r697 = S (N N_label) :: r696 in
  let r698 = S (T T_VIRTUAL) :: r697 in
  let r699 = S (T T_PRIVATE) :: r698 in
  let r700 = R 368 :: r699 in
  let r701 = [R 26] in
  let r702 = S (N N_post_item_attributes) :: r701 in
  let r703 = [R 275] in
  let r704 = S (N N_poly_type) :: r703 in
  let r705 = S (T T_COLON) :: r704 in
  let r706 = S (N N_label) :: r705 in
  let r707 = S (T T_EQUAL) :: r659 in
  let r708 = [R 276] in
  let r709 = Sub (r707) :: r708 in
  let r710 = [R 277] in
  let r711 = S (N N_seq_expr) :: r710 in
  let r712 = S (T T_EQUAL) :: r711 in
  let r713 = [R 278] in
  let r714 = S (N N_seq_expr) :: r713 in
  let r715 = S (T T_EQUAL) :: r714 in
  let r716 = S (N N_core_type) :: r715 in
  let r717 = S (T T_DOT) :: r716 in
  let r718 = [R 28] in
  let r719 = S (N N_post_item_attributes) :: r718 in
  let r720 = [R 60] in
  let r721 = S (N N_class_longident) :: r720 in
  let r722 = [R 18] in
  let r723 = Sub (r721) :: r722 in
  let r724 = [R 24] in
  let r725 = S (N N_post_item_attributes) :: r724 in
  let r726 = R 376 :: r725 in
  let r727 = Sub (r723) :: r726 in
  let r728 = [R 61] in
  let r729 = S (T T_END) :: r728 in
  let r730 = [R 63] in
  let r731 = S (T T_RPAREN) :: r730 in
  let r732 = [R 21] in
  let r733 = Sub (r723) :: r732 in
  let r734 = S (T T_IN) :: r733 in
  let r735 = S (N N_let_bindings_no_attrs) :: r734 in
  let r736 = [R 59] in
  let r737 = S (N N_class_longident) :: r736 in
  let r738 = S (T T_RBRACKET) :: r737 in
  let r739 = S (N N_simple_pattern) :: r272 in
  let r740 = [R 36] in
  let r741 = Sub (r723) :: r740 in
  let r742 = S (T T_MINUSGREATER) :: r741 in
  let r743 = Sub (r739) :: r742 in
  let r744 = [R 19] in
  let r745 = [R 62] in
  let r746 = S (T T_RPAREN) :: r745 in
  let r747 = [R 375] in
  let r748 = [R 27] in
  let r749 = S (N N_post_item_attributes) :: r748 in
  let r750 = [R 29] in
  let r751 = [R 517] in
  let r752 = S (T T_BARRBRACKET) :: r751 in
  let r753 = [R 123] in
  let r754 = S (N N_match_cases) :: r753 in
  let r755 = [R 125] in
  let r756 = [R 124] in
  let r757 = [R 630] in
  let r758 = [R 617] in
  let r759 = [R 619] in
  let r760 = [R 618] in
  let r761 = [R 605] in
  let r762 = Sub (r371) :: r761 in
  let r763 = [R 609] in
  let r764 = S (N N_post_item_attributes) :: r763 in
  let r765 = Sub (r762) :: r764 in
  let r766 = R 351 :: r765 in
  let r767 = R 408 :: r766 in
  let r768 = [R 607] in
  let r769 = [R 192] in
  let r770 = S (N N_attributes) :: r769 in
  let r771 = S (N N_constr_longident) :: r770 in
  let r772 = [R 651] in
  let r773 = [R 644] in
  let r774 = [R 622] in
  let r775 = S (N N_seq_expr) :: r774 in
  let r776 = S (T T_EQUAL) :: r775 in
  let r777 = [R 623] in
  let r778 = S (N N_module_expr) :: r777 in
  let r779 = S (T T_EQUAL) :: r778 in
  let r780 = [R 621] in
  let r781 = S (N N_seq_expr) :: r780 in
  let r782 = S (T T_EQUAL) :: r781 in
  let r783 = [R 627] in
  let r784 = S (N N_module_expr) :: r783 in
  let r785 = S (T T_EQUAL) :: r784 in
  let r786 = [R 626] in
  let r787 = S (N N_seq_expr) :: r786 in
  let r788 = S (T T_EQUAL) :: r787 in
  let r789 = S (N N_simple_expr) :: r788 in
  let r790 = S (N N_val_ident) :: r789 in
  let r791 = [R 625] in
  let r792 = S (N N_seq_expr) :: r791 in
  let r793 = S (T T_EQUAL) :: r792 in
  let r794 = [R 624] in
  let r795 = S (N N_seq_expr) :: r794 in
  let r796 = S (T T_EQUAL) :: r795 in
  let r797 = [R 633] in
  let r798 = [R 284] in
  let r799 = S (N N_post_item_attributes) :: r798 in
  let r800 = [R 635] in
  let r801 = S (N N_post_item_attributes) :: r800 in
  let r802 = [R 636] in
  let r803 = S (N N_post_item_attributes) :: r802 in
  let r804 = [R 634] in
  let r805 = [R 289] in
  let r806 = [R 616] in
  let r807 = S (N N_let_bindings) :: r806 in
  let r808 = S (N N_rec_flag) :: r807 in
  let r809 = S (N N_ext_attributes) :: r808 in
  let r810 = [R 628] in
  let r811 = S (N N_let_bindings) :: r810 in
  let r812 = S (N N_rec_flag) :: r811 in
  let r813 = S (N N_ext_attributes) :: r812 in
  let r814 = [R 640] in
  let r815 = S (N N_post_item_attributes) :: r814 in
  let r816 = [R 629] in
  let r817 = S (N N_post_item_attributes) :: r816 in
  let r818 = S (N N_primitive_declaration) :: r817 in
  let r819 = S (T T_EQUAL) :: r818 in
  let r820 = S (N N_core_type) :: r819 in
  let r821 = S (T T_COLON) :: r820 in
  let r822 = [R 603] in
  let r823 = S (N N_post_item_attributes) :: r822 in
  let r824 = Sub (r371) :: r823 in
  let r825 = [R 632] in
  let r826 = [R 620] in
  let r827 = [R 604] in
  let r828 = [R 638] in
  let r829 = [R 639] in
  let r830 = [R 33] in
  let r831 = Sub (r723) :: r830 in
  let r832 = S (T T_EQUAL) :: r831 in
  let r833 = [R 12] in
  let r834 = S (N N_post_item_attributes) :: r833 in
  let r835 = Sub (r832) :: r834 in
  let r836 = S (T T_LIDENT) :: r835 in
  let r837 = [R 34] in
  let r838 = Sub (r723) :: r837 in
  let r839 = S (T T_EQUAL) :: r838 in
  let r840 = [R 35] in
  let r841 = [R 13] in
  let r842 = [R 645] in
  let r843 = [R 641] in
  let r844 = [R 614] in
  let r845 = S (N N_structure_tail) :: r844 in
  let r846 = [R 214] in
  let r847 = [R 215] in
  let r848 = [R 377] in
  function
  | 0 | 1529 | 1533 -> Nothing
  | 1528 -> One ([R 0])
  | 1532 -> One ([R 1])
  | 1536 -> One ([R 2])
  | 378 -> One ([R 3])
  | 377 -> One ([R 4])
  | 189 -> One ([R 5])
  | 54 -> One ([R 7])
  | 1507 -> One ([R 14])
  | 887 -> One ([R 17])
  | 1323 -> One ([R 20])
  | 1326 -> One ([R 22])
  | 1321 -> One ([R 23])
  | 1345 -> One ([R 30])
  | 1346 -> One ([R 32])
  | 1327 -> One ([R 37])
  | 120 -> One ([R 38])
  | 842 -> One ([R 51])
  | 843 -> One ([R 53])
  | 833 -> One ([R 57])
  | 829 -> One ([R 58])
  | 849 -> One ([R 72])
  | 821 -> One ([R 75])
  | 283 -> One ([R 77])
  | 293 -> One ([R 78])
  | 64 -> One ([R 79])
  | 290 -> One ([R 80])
  | 282 -> One ([R 81])
  | 281 -> One ([R 82])
  | 83 -> One ([R 83])
  | 559 | 569 -> One ([R 84])
  | 564 -> One ([R 87])
  | 560 -> One ([R 88])
  | 309 -> One ([R 89])
  | 291 -> One ([R 92])
  | 63 -> One ([R 93])
  | 594 -> One ([R 99])
  | 219 -> One ([R 101])
  | 221 -> One ([R 102])
  | 181 -> One ([R 103])
  | 248 -> One ([R 108])
  | 184 -> One ([R 110])
  | 577 -> One ([R 112])
  | 190 -> One ([R 114])
  | 1104 -> One ([R 116])
  | 1105 -> One ([R 117])
  | 923 -> One ([R 129])
  | 1090 -> One ([R 130])
  | 950 -> One ([R 140])
  | 959 -> One ([R 141])
  | 930 -> One ([R 142])
  | 957 -> One ([R 179])
  | 1100 -> One ([R 183])
  | 1024 -> One ([R 185])
  | 1021 -> One ([R 186])
  | 1208 -> One ([R 197])
  | 523 -> One ([R 204])
  | 522 -> One ([R 205])
  | 715 -> One ([R 206])
  | 716 -> One ([R 207])
  | 126 | 138 -> One ([R 212])
  | 105 -> One ([R 213])
  | 144 | 691 -> One ([R 217])
  | 608 -> One ([R 219])
  | 605 -> One ([R 220])
  | 925 -> One ([R 225])
  | 441 -> One ([R 226])
  | 111 -> One ([R 228])
  | 440 -> One ([R 230])
  | 1091 -> One ([R 231])
  | 1093 -> One ([R 232])
  | 1044 -> One ([R 242])
  | 1041 -> One ([R 243])
  | 1043 -> One ([R 244])
  | 1042 -> One ([R 245])
  | 277 -> One ([R 247])
  | 271 -> One ([R 248])
  | 272 -> One ([R 249])
  | 276 -> One ([R 251])
  | 1018 -> One ([R 258])
  | 1188 -> One ([R 260])
  | 451 -> One ([R 261])
  | 939 -> One ([R 262])
  | 486 -> One ([R 263])
  | 1214 -> One ([R 265])
  | 1215 -> One ([R 266])
  | 1141 -> One ([R 269])
  | 236 -> One ([R 271])
  | 146 -> One ([R 273])
  | 66 | 104 -> One ([R 279])
  | 62 -> One ([R 282])
  | 1460 -> One ([R 288])
  | 670 -> One ([R 293])
  | 669 -> One ([R 304])
  | 671 -> One ([R 305])
  | 676 -> One ([R 307])
  | 714 -> One ([R 313])
  | 713 -> One ([R 314])
  | 140 -> One ([R 315])
  | 392 -> One (R 317 :: r177)
  | 791 -> One (R 317 :: r486)
  | 393 | 409 -> One ([R 318])
  | 218 -> One ([R 320])
  | 217 -> One ([R 321])
  | 316 | 909 -> One ([R 324])
  | 94 | 375 -> One ([R 325])
  | 288 -> One ([R 326])
  | 287 -> One ([R 327])
  | 286 -> One ([R 328])
  | 285 -> One ([R 329])
  | 284 -> One ([R 330])
  | 263 | 908 -> One ([R 331])
  | 92 -> One ([R 332])
  | 319 | 914 -> One ([R 333])
  | 97 | 333 | 425 -> One ([R 334])
  | 96 | 424 -> One ([R 335])
  | 261 | 334 | 907 -> One ([R 336])
  | 260 | 906 -> One ([R 337])
  | 91 -> One ([R 338])
  | 315 -> One ([R 339])
  | 264 -> One ([R 340])
  | 289 -> One ([R 341])
  | 99 -> One ([R 342])
  | 318 -> One ([R 343])
  | 320 -> One ([R 344])
  | 321 -> One ([R 345])
  | 317 -> One ([R 346])
  | 95 -> One ([R 347])
  | 98 -> One ([R 348])
  | 179 -> One ([R 349])
  | 178 -> One (R 350 :: r86)
  | 156 -> One (R 351 :: r71)
  | 615 -> One (R 351 :: r358)
  | 1357 -> One (R 351 :: r754)
  | 157 -> One ([R 352])
  | 234 -> One (R 355 :: r103)
  | 273 -> One (R 355 :: r114)
  | 349 -> One (R 355 :: r129)
  | 1162 -> One (R 355 :: r639)
  | 1348 -> One (R 355 :: r752)
  | 235 | 274 | 343 | 604 | 1020 | 1031 -> One ([R 356])
  | 1415 -> One (R 357 :: r776)
  | 1420 -> One (R 357 :: r779)
  | 1424 -> One (R 357 :: r782)
  | 1416 -> One ([R 358])
  | 547 -> One ([R 360])
  | 631 -> One ([R 363])
  | 539 -> One ([R 366])
  | 398 -> One ([R 367])
  | 652 -> One (R 368 :: r383)
  | 1304 -> One (R 368 :: r727)
  | 399 -> One ([R 369])
  | 108 -> One ([R 370])
  | 257 -> One ([R 373])
  | 296 -> One ([R 378])
  | 301 -> One ([R 380])
  | 306 -> One ([R 388])
  | 347 -> One ([R 391])
  | 344 -> One ([R 392])
  | 492 -> One ([R 393])
  | 223 -> One ([R 395])
  | 819 -> One ([R 399])
  | 233 -> One ([R 401])
  | 762 -> One ([R 406])
  | 763 -> One ([R 407])
  | 1276 -> One (R 408 :: r706)
  | 613 -> One ([R 409])
  | 806 -> One (R 410 :: r495)
  | 809 -> One ([R 411])
  | 807 -> One ([R 412])
  | 810 -> One ([R 413])
  | 808 -> One ([R 414])
  | 435 -> One ([R 416])
  | 1053 -> One ([R 420])
  | 167 -> One ([R 421])
  | 203 -> One ([R 423])
  | 1005 -> One ([R 425])
  | 1006 -> One ([R 426])
  | 1007 -> One ([R 427])
  | 636 -> One ([R 442])
  | 890 -> One ([R 449])
  | 893 -> One ([R 454])
  | 300 -> One ([R 455])
  | 89 -> One ([R 457])
  | 87 -> One ([R 458])
  | 86 -> One ([R 459])
  | 85 -> One ([R 460])
  | 82 -> One ([R 462])
  | 80 -> One ([R 463])
  | 79 -> One ([R 464])
  | 78 -> One ([R 465])
  | 168 -> One ([R 466])
  | 118 -> One ([R 469])
  | 166 -> One ([R 470])
  | 171 -> One ([R 471])
  | 245 -> One ([R 472])
  | 185 | 876 -> One ([R 486])
  | 578 | 591 -> One ([R 487])
  | 182 -> One ([R 489])
  | 870 -> One ([R 491])
  | 368 -> One ([R 494])
  | 512 -> One ([R 500])
  | 913 -> One ([R 501])
  | 912 | 958 -> One ([R 502])
  | 515 | 929 -> One ([R 503])
  | 911 -> One ([R 533])
  | 1094 -> One ([R 534])
  | 1092 -> One ([R 535])
  | 294 -> One ([R 536])
  | 295 | 943 -> One ([R 537])
  | 61 -> One ([R 538])
  | 297 -> One ([R 539])
  | 340 -> One ([R 541])
  | 339 -> One ([R 542])
  | 311 -> One ([R 552])
  | 28 -> One ([R 553])
  | 9 -> One ([R 554])
  | 53 -> One ([R 555])
  | 52 -> One ([R 556])
  | 51 -> One ([R 557])
  | 50 -> One ([R 558])
  | 49 -> One ([R 559])
  | 48 -> One ([R 560])
  | 47 -> One ([R 561])
  | 46 -> One ([R 562])
  | 45 -> One ([R 563])
  | 44 -> One ([R 564])
  | 43 -> One ([R 565])
  | 42 -> One ([R 566])
  | 41 -> One ([R 567])
  | 40 -> One ([R 568])
  | 39 -> One ([R 569])
  | 38 -> One ([R 570])
  | 37 -> One ([R 571])
  | 36 -> One ([R 572])
  | 35 -> One ([R 573])
  | 34 -> One ([R 574])
  | 33 -> One ([R 575])
  | 32 -> One ([R 576])
  | 31 -> One ([R 577])
  | 30 -> One ([R 578])
  | 29 -> One ([R 579])
  | 27 -> One ([R 580])
  | 26 -> One ([R 581])
  | 25 -> One ([R 582])
  | 24 -> One ([R 583])
  | 23 -> One ([R 584])
  | 22 -> One ([R 585])
  | 21 -> One ([R 586])
  | 20 -> One ([R 587])
  | 19 -> One ([R 588])
  | 18 -> One ([R 589])
  | 17 -> One ([R 590])
  | 16 -> One ([R 591])
  | 15 -> One ([R 592])
  | 14 -> One ([R 593])
  | 13 -> One ([R 594])
  | 12 -> One ([R 595])
  | 11 -> One ([R 596])
  | 10 -> One ([R 597])
  | 8 -> One ([R 598])
  | 7 -> One ([R 599])
  | 6 -> One ([R 600])
  | 5 -> One ([R 601])
  | 4 -> One ([R 602])
  | 1402 -> One ([R 606])
  | 1393 -> One ([R 608])
  | 222 -> One ([R 613])
  | 1385 -> One ([R 631])
  | 1519 -> One ([R 637])
  | 1522 -> One ([R 642])
  | 418 -> One ([R 646])
  | 417 -> One ([R 647])
  | 1407 -> One ([R 652])
  | 1408 -> One ([R 653])
  | 1410 -> One ([R 654])
  | 1409 -> One ([R 655])
  | 1406 -> One ([R 656])
  | 632 -> One ([R 661])
  | 622 -> One ([R 666])
  | 567 -> One ([R 667])
  | 595 -> One ([R 672])
  | 67 | 141 -> One ([R 674])
  | 689 -> One ([R 677])
  | 701 -> One ([R 680])
  | 534 -> One ([R 684])
  | 536 -> One ([R 685])
  | 262 | 465 | 1051 -> One ([R 688])
  | 513 -> One ([R 690])
  | 796 -> One (R 699 :: r490)
  | 773 -> One ([R 700])
  | 712 -> One ([R 705])
  | 550 -> One ([R 708])
  | 693 -> One ([R 709])
  | 694 -> One ([R 710])
  | 1038 -> One (S (T T_WITH) :: r604)
  | 73 -> One (S (T T_UIDENT) :: r19)
  | 100 -> One (S (T T_UIDENT) :: r28)
  | 310 -> One (S (T T_UIDENT) :: r37)
  | 656 -> One (S (T T_TYPE) :: r386)
  | 661 -> One (S (T T_TYPE) :: r397)
  | 1193 -> One (S (T T_TYPE) :: r657)
  | 1428 -> One (S (T T_STRING) :: r785)
  | 1432 -> One (S (T T_STRING) :: r790)
  | 1438 -> One (S (T T_STRING) :: r793)
  | 1442 -> One (S (T T_STRING) :: r796)
  | 580 -> One (S (T T_STAR) :: r343)
  | 1397 -> One (S (T T_RPAREN) :: r26)
  | 352 -> One (S (T T_RPAREN) :: r135)
  | 521 -> One (S (T T_RPAREN) :: r302)
  | 562 | 570 -> One (S (T T_RPAREN) :: r330)
  | 658 -> One (S (T T_RPAREN) :: r390)
  | 665 -> One (S (T T_RPAREN) :: r398)
  | 667 -> One (S (T T_RPAREN) :: r399)
  | 920 -> One (S (T T_RPAREN) :: r542)
  | 1096 -> One (S (T T_RPAREN) :: r620)
  | 1157 -> One (S (T T_RPAREN) :: r637)
  | 1251 -> One (S (T T_RPAREN) :: r690)
  | 1260 -> One (S (T T_RPAREN) :: r691)
  | 159 -> One (S (T T_RBRACKET) :: r72)
  | 204 -> One (S (T T_RBRACKET) :: r94)
  | 1398 -> One (S (T T_RBRACKET) :: r111)
  | 193 -> One (S (T T_QUOTE) :: r91)
  | 639 -> One (S (T T_PLUSEQ) :: r378)
  | 1387 -> One (S (T T_PLUSEQ) :: r767)
  | 452 -> One (S (T T_OPEN) :: r221)
  | 585 -> One (S (T T_MINUSGREATER) :: r346)
  | 868 -> One (S (T T_MINUSGREATER) :: r533)
  | 128 -> One (S (T T_LIDENT) :: r46)
  | 495 -> One (S (T T_LIDENT) :: r270)
  | 854 -> One (S (T T_LIDENT) :: r524)
  | 1124 -> One (S (T T_LIDENT) :: r630)
  | 1336 -> One (S (T T_LIDENT) :: r747)
  | 948 -> One (S (T T_LESSMINUS) :: r563)
  | 77 -> One (S (T T_INT) :: r20)
  | 84 -> One (S (T T_INT) :: r21)
  | 460 -> One (S (T T_GREATERRBRACE) :: r227)
  | 143 -> One (S (T T_GREATER) :: r59)
  | 147 -> One (S (T T_GREATER) :: r60)
  | 462 -> One (S (T T_EQUAL) :: r229)
  | 1008 -> One (S (T T_EQUAL) :: r590)
  | 1032 -> One (S (T T_EQUAL) :: r600)
  | 1205 -> One (S (T T_EQUAL) :: r664)
  | 1271 -> One (S (T T_EQUAL) :: r693)
  | 1526 -> One (S (T T_EOF) :: r846)
  | 1530 -> One (S (T T_EOF) :: r847)
  | 1534 -> One (S (T T_EOF) :: r848)
  | 1148 -> One (S (T T_END) :: r636)
  | 612 -> One (S (T T_DOTDOT) :: r356)
  | 112 -> One (S (T T_DOT) :: r36)
  | 121 -> One (S (T T_DOT) :: r40)
  | 136 -> One (S (T T_DOT) :: r55)
  | 226 -> One (S (T T_DOT) :: r101)
  | 814 -> One (S (T T_DOT) :: r497)
  | 825 -> One (S (T T_DOT) :: r504)
  | 1221 -> One (S (T T_DOT) :: r674)
  | 703 -> One (S (T T_COLONEQUAL) :: r423)
  | 149 -> One (S (T T_COLON) :: r63)
  | 525 -> One (S (T T_COLON) :: r305)
  | 659 -> One (S (T T_COLON) :: r394)
  | 862 -> One (S (T T_COLON) :: r531)
  | 265 -> One (S (T T_BARRBRACKET) :: r107)
  | 376 -> One (S (T T_BARRBRACKET) :: r155)
  | 162 | 866 -> One (S (T T_BAR) :: r77)
  | 206 -> One (S (T T_BAR) :: r97)
  | 596 -> One (S (T T_BAR) :: r348)
  | 692 -> One (S (N N_with_type_binder) :: r417)
  | 549 -> One (S (N N_with_extensions) :: r319)
  | 551 -> One (S (N N_with_extensions) :: r320)
  | 634 -> One (S (N N_with_extensions) :: r367)
  | 768 -> One (S (N N_with_extensions) :: r458)
  | 1380 -> One (S (N N_with_extensions) :: r759)
  | 1383 -> One (S (N N_with_extensions) :: r760)
  | 1482 -> One (S (N N_with_extensions) :: r826)
  | 678 -> One (S (N N_with_constraints) :: r405)
  | 331 -> One (S (N N_val_ident) :: r127)
  | 528 -> One (S (N N_val_ident) :: r311)
  | 757 -> One (S (N N_val_ident) :: r453)
  | 1473 -> One (S (N N_val_ident) :: r821)
  | 681 -> One (S (N N_type_variable) :: r412)
  | 679 -> One (S (N N_type_parameters) :: r409)
  | 680 -> One (S (N N_type_parameter_list) :: r411)
  | 776 -> One (S (N N_type_parameter_list) :: r469)
  | 687 -> One (S (N N_type_parameter) :: r414)
  | 65 -> One (S (N N_type_longident) :: r13)
  | 533 -> One (S (N N_type_declarations) :: r312)
  | 535 -> One (S (N N_type_declarations) :: r313)
  | 1377 -> One (S (N N_type_declarations) :: r757)
  | 1378 -> One (S (N N_type_declarations) :: r758)
  | 1 -> One (S (N N_structure_tail) :: r0)
  | 1509 -> One (S (N N_structure_tail) :: r842)
  | 1413 -> One (S (N N_structure_head) :: r773)
  | 427 -> One (S (N N_structure) :: r198)
  | 279 -> One (S (N N_simple_pattern) :: r116)
  | 498 -> One (S (N N_simple_pattern) :: r271)
  | 367 -> One (S (N N_simple_expr) :: r149)
  | 466 -> One (S (N N_simple_expr) :: r234)
  | 467 -> One (S (N N_simple_expr) :: r235)
  | 471 -> One (S (N N_simple_expr) :: r240)
  | 511 -> One (S (N N_simple_expr) :: r289)
  | 931 -> One (S (N N_simple_expr) :: r546)
  | 1067 -> One (S (N N_simple_expr) :: r614)
  | 1069 -> One (S (N N_simple_expr) :: r615)
  | 859 -> One (S (N N_simple_core_type_or_tuple_no_attr) :: r527)
  | 583 -> One (S (N N_simple_core_type_no_attr) :: r344)
  | 187 -> One (S (N N_simple_core_type) :: r88)
  | 298 -> One (S (N N_signed_constant) :: r118)
  | 527 -> One (S (N N_signature) :: r307)
  | 651 -> One (S (N N_signature) :: r380)
  | 888 -> One (S (N N_signature) :: r536)
  | 360 -> One (S (N N_seq_expr) :: r136)
  | 374 -> One (S (N N_seq_expr) :: r154)
  | 450 -> One (S (N N_seq_expr) :: r217)
  | 480 -> One (S (N N_seq_expr) :: r255)
  | 518 -> One (S (N N_seq_expr) :: r294)
  | 1115 -> One (S (N N_seq_expr) :: r627)
  | 1301 -> One (S (N N_seq_expr) :: r719)
  | 1360 -> One (S (N N_seq_expr) :: r755)
  | 1362 -> One (S (N N_seq_expr) :: r756)
  | 161 -> One (S (N N_row_field_list) :: r74)
  | 464 -> One (S (N N_record_expr) :: r231)
  | 1037 -> One (S (N N_record_expr) :: r602)
  | 1183 -> One (S (N N_rec_flag) :: r653)
  | 1310 -> One (S (N N_rec_flag) :: r735)
  | 406 -> One (S (N N_post_item_attributes) :: r181)
  | 840 -> One (S (N N_post_item_attributes) :: r511)
  | 891 -> One (S (N N_post_item_attributes) :: r537)
  | 1015 -> One (S (N N_post_item_attributes) :: r592)
  | 1343 -> One (S (N N_post_item_attributes) :: r750)
  | 1484 -> One (S (N N_post_item_attributes) :: r827)
  | 1520 -> One (S (N N_post_item_attributes) :: r843)
  | 1523 -> One (S (N N_post_item_attributes) :: r845)
  | 1288 -> One (S (N N_poly_type) :: r712)
  | 266 -> One (S (N N_pattern_semi_list) :: r110)
  | 60 -> One (S (N N_pattern) :: r12)
  | 280 | 494 | 1123 -> One (S (N N_pattern) :: r25)
  | 278 -> One (S (N N_pattern) :: r115)
  | 292 -> One (S (N N_pattern) :: r117)
  | 302 -> One (S (N N_pattern) :: r119)
  | 304 -> One (S (N N_pattern) :: r120)
  | 307 -> One (S (N N_pattern) :: r121)
  | 312 -> One (S (N N_pattern) :: r122)
  | 324 -> One (S (N N_pattern) :: r123)
  | 329 -> One (S (N N_pattern) :: r126)
  | 381 -> One (S (N N_pattern) :: r163)
  | 109 -> One (S (N N_package_type_cstrs) :: r31)
  | 258 -> One (S (N N_package_type_cstrs) :: r106)
  | 103 -> One (S (N N_package_type) :: r30)
  | 133 -> One (S (N N_package_type) :: r53)
  | 1232 -> One (S (N N_package_type) :: r676)
  | 1235 -> One (S (N N_package_type) :: r678)
  | 1238 -> One (S (N N_package_type) :: r680)
  | 1248 -> One (S (N N_package_type) :: r684)
  | 538 -> One (S (N N_optional_type_variable) :: r316)
  | 537 -> One (S (N N_optional_type_parameter_list) :: r315)
  | 545 -> One (S (N N_optional_type_parameter) :: r318)
  | 332 -> One (S (N N_operator) :: r23)
  | 214 -> One (S (N N_name_tag_list) :: r99)
  | 672 -> One (S (N N_module_type) :: r401)
  | 721 -> One (S (N N_module_type) :: r426)
  | 733 -> One (S (N N_module_type) :: r430)
  | 754 -> One (S (N N_module_type) :: r447)
  | 1174 -> One (S (N N_module_type) :: r648)
  | 1243 -> One (S (N N_module_type) :: r682)
  | 1452 -> One (S (N N_module_type) :: r803)
  | 426 -> One (S (N N_module_expr) :: r196)
  | 430 -> One (S (N N_module_expr) :: r200)
  | 519 -> One (S (N N_module_expr) :: r298)
  | 1172 -> One (S (N N_module_expr) :: r645)
  | 1470 -> One (S (N N_module_expr) :: r815)
  | 657 -> One (S (N N_module_declaration) :: r388)
  | 1456 -> One (S (N N_module_bindings) :: r804)
  | 1181 -> One (S (N N_module_binding_body) :: r649)
  | 1447 -> One (S (N N_module_binding_body) :: r799)
  | 1446 -> One (S (N N_module_binding) :: r797)
  | 1458 -> One (S (N N_module_binding) :: r805)
  | 726 -> One (S (N N_mod_longident) :: r428)
  | 70 -> One (S (N N_mod_ext_longident) :: r18)
  | 1213 -> One (S (N N_lident_list) :: r670)
  | 1289 -> One (S (N N_lident_list) :: r717)
  | 485 -> One (S (N N_let_pattern) :: r267)
  | 1014 -> One (S (N N_let_binding) :: r591)
  | 268 -> One (S (N N_lbl_pattern_list) :: r113)
  | 438 -> One (S (N N_label_var) :: r209)
  | 447 -> One (S (N N_label_var) :: r213)
  | 110 -> One (S (N N_label_longident) :: r34)
  | 439 -> One (S (N N_label_let_pattern) :: r211)
  | 448 -> One (S (N N_label_let_pattern) :: r216)
  | 924 -> One (S (N N_label_ident) :: r544)
  | 1065 -> One (S (N N_label_ident) :: r613)
  | 597 -> One (S (N N_label_declarations) :: r351)
  | 616 -> One (S (N N_label_declarations) :: r361)
  | 410 -> One (S (N N_label) :: r184)
  | 598 -> One (S (N N_label) :: r355)
  | 960 -> One (S (N N_label) :: r567)
  | 1286 -> One (S (N N_label) :: r709)
  | 125 -> One (S (N N_ident) :: r41)
  | 151 -> One (S (N N_ident) :: r64)
  | 164 -> One (S (N N_ident) :: r78)
  | 227 -> One (S (N N_ident) :: r102)
  | 540 -> One (S (N N_ident) :: r317)
  | 682 -> One (S (N N_ident) :: r413)
  | 1404 -> One (S (N N_ident) :: r772)
  | 1450 -> One (S (N N_ident) :: r801)
  | 520 -> One (S (N N_functor_args) :: r301)
  | 673 -> One (S (N N_functor_args) :: r404)
  | 437 -> One (S (N N_fun_binding) :: r208)
  | 1209 -> One (S (N N_fun_binding) :: r665)
  | 1029 -> One (S (N N_field_expr_list) :: r598)
  | 2 -> One (S (N N_ext_attributes) :: r5)
  | 361 -> One (S (N N_ext_attributes) :: r141)
  | 363 -> One (S (N N_ext_attributes) :: r143)
  | 365 -> One (S (N N_ext_attributes) :: r148)
  | 369 -> One (S (N N_ext_attributes) :: r151)
  | 379 -> One (S (N N_ext_attributes) :: r161)
  | 419 -> One (S (N N_ext_attributes) :: r189)
  | 421 -> One (S (N N_ext_attributes) :: r194)
  | 433 -> One (S (N N_ext_attributes) :: r207)
  | 454 -> One (S (N N_ext_attributes) :: r223)
  | 469 -> One (S (N N_ext_attributes) :: r239)
  | 472 -> One (S (N N_ext_attributes) :: r242)
  | 474 -> One (S (N N_ext_attributes) :: r246)
  | 476 -> One (S (N N_ext_attributes) :: r249)
  | 481 -> One (S (N N_ext_attributes) :: r261)
  | 503 -> One (S (N N_ext_attributes) :: r279)
  | 507 -> One (S (N N_ext_attributes) :: r288)
  | 915 -> One (S (N N_ext_attributes) :: r539)
  | 941 -> One (S (N N_ext_attributes) :: r558)
  | 1169 -> One (S (N N_ext_attributes) :: r644)
  | 940 -> One (S (N N_expr_semi_list) :: r554)
  | 1025 -> One (S (N N_expr_semi_list) :: r595)
  | 431 -> One (S (N N_expr) :: r202)
  | 468 -> One (S (N N_expr) :: r237)
  | 922 -> One (S (N N_expr) :: r543)
  | 928 -> One (S (N N_expr) :: r545)
  | 951 -> One (S (N N_expr) :: r564)
  | 953 -> One (S (N N_expr) :: r565)
  | 955 -> One (S (N N_expr) :: r566)
  | 962 -> One (S (N N_expr) :: r568)
  | 964 -> One (S (N N_expr) :: r569)
  | 966 -> One (S (N N_expr) :: r570)
  | 968 -> One (S (N N_expr) :: r571)
  | 970 -> One (S (N N_expr) :: r572)
  | 972 -> One (S (N N_expr) :: r573)
  | 974 -> One (S (N N_expr) :: r574)
  | 976 -> One (S (N N_expr) :: r575)
  | 978 -> One (S (N N_expr) :: r576)
  | 980 -> One (S (N N_expr) :: r577)
  | 982 -> One (S (N N_expr) :: r578)
  | 984 -> One (S (N N_expr) :: r579)
  | 986 -> One (S (N N_expr) :: r580)
  | 988 -> One (S (N N_expr) :: r581)
  | 990 -> One (S (N N_expr) :: r582)
  | 992 -> One (S (N N_expr) :: r583)
  | 994 -> One (S (N N_expr) :: r584)
  | 996 -> One (S (N N_expr) :: r585)
  | 998 -> One (S (N N_expr) :: r586)
  | 1001 -> One (S (N N_expr) :: r587)
  | 1003 -> One (S (N N_expr) :: r588)
  | 1045 -> One (S (N N_expr) :: r605)
  | 1063 -> One (S (N N_expr) :: r612)
  | 1075 -> One (S (N N_expr) :: r616)
  | 1080 -> One (S (N N_expr) :: r617)
  | 1085 -> One (S (N N_expr) :: r618)
  | 1088 -> One (S (N N_expr) :: r619)
  | 1098 -> One (S (N N_expr) :: r621)
  | 1145 -> One (S (N N_expr) :: r635)
  | 197 -> One (S (N N_core_type_no_attr) :: r92)
  | 573 -> One (S (N N_core_type_list_no_attr) :: r338)
  | 871 -> One (S (N N_core_type_list_no_attr) :: r534)
  | 183 -> One (S (N N_core_type_list) :: r87)
  | 132 -> One (S (N N_core_type_comma_list) :: r51)
  | 822 -> One (S (N N_core_type_comma_list) :: r502)
  | 1314 -> One (S (N N_core_type_comma_list) :: r738)
  | 131 -> One (S (N N_core_type2) :: r49)
  | 142 -> One (S (N N_core_type2) :: r58)
  | 191 -> One (S (N N_core_type2) :: r89)
  | 155 -> One (S (N N_core_type) :: r68)
  | 246 -> One (S (N N_core_type) :: r105)
  | 326 -> One (S (N N_core_type) :: r125)
  | 384 -> One (S (N N_core_type) :: r165)
  | 442 -> One (S (N N_core_type) :: r212)
  | 487 -> One (S (N N_core_type) :: r268)
  | 558 -> One (S (N N_core_type) :: r328)
  | 561 -> One (S (N N_core_type) :: r329)
  | 783 -> One (S (N N_core_type) :: r476)
  | 944 -> One (S (N N_core_type) :: r561)
  | 1199 -> One (S (N N_core_type) :: r660)
  | 1201 | 1212 -> One (S (N N_core_type) :: r661)
  | 1203 -> One (S (N N_core_type) :: r662)
  | 775 -> One (S (N N_class_type_parameters) :: r467)
  | 850 -> One (S (N N_class_type_parameters) :: r519)
  | 1491 -> One (S (N N_class_type_parameters) :: r836)
  | 774 -> One (S (N N_class_type_declarations) :: r460)
  | 1489 -> One (S (N N_class_type_declarations) :: r829)
  | 847 -> One (S (N N_class_type_declaration) :: r512)
  | 119 -> One (S (N N_class_longident) :: r38)
  | 169 -> One (S (N N_class_longident) :: r79)
  | 243 -> One (S (N N_class_longident) :: r104)
  | 772 -> One (S (N N_class_descriptions) :: r459)
  | 885 -> One (S (N N_class_description) :: r535)
  | 1488 -> One (S (N N_class_declarations) :: r828)
  | 1505 -> One (S (N N_class_declaration) :: r841)
  | 177 -> One (S (N N_attributes) :: r83)
  | 200 -> One (S (N N_attributes) :: r93)
  | 372 -> One (S (N N_attributes) :: r152)
  | 3 -> One (S (N N_attr_id) :: r7)
  | 55 -> One (S (N N_attr_id) :: r8)
  | 58 -> One (S (N N_attr_id) :: r11)
  | 153 -> One (S (N N_attr_id) :: r67)
  | 401 -> One (S (N N_attr_id) :: r180)
  | 746 -> One (S (N N_attr_id) :: r442)
  | 750 -> One (S (N N_attr_id) :: r445)
  | 175 -> One (Sub (r81) :: r82)
  | 1306 -> One (Sub (r158) :: r729)
  | 391 -> One (Sub (r171) :: r173)
  | 1127 -> One (Sub (r258) :: r631)
  | 1131 -> One (Sub (r258) :: r632)
  | 483 -> One (Sub (r263) :: r264)
  | 554 -> One (Sub (r326) :: r327)
  | 568 -> One (Sub (r336) :: r337)
  | 625 -> One (Sub (r365) :: r366)
  | 644 -> One (Sub (r371) :: r379)
  | 1392 -> One (Sub (r371) :: r768)
  | 710 -> One (Sub (r424) :: r425)
  | 743 -> One (Sub (r435) :: r439)
  | 737 -> One (Sub (r437) :: r438)
  | 766 -> One (Sub (r456) :: r457)
  | 820 -> One (Sub (r462) :: r499)
  | 782 -> One (Sub (r472) :: r474)
  | 790 -> One (Sub (r480) :: r482)
  | 834 -> One (Sub (r508) :: r510)
  | 1340 -> One (Sub (r508) :: r749)
  | 1332 -> One (Sub (r514) :: r746)
  | 1496 -> One (Sub (r514) :: r839)
  | 1139 -> One (Sub (r633) :: r634)
  | 1274 -> One (Sub (r700) :: r702)
  | 1309 -> One (Sub (r723) :: r731)
  | 1318 -> One (Sub (r743) :: r744)
  | 1480 -> One (Sub (r824) :: r825)
  | 1500 -> One (Sub (r832) :: r840)
  | 1508 -> One (r0)
  | 1376 -> One (r1)
  | 1375 -> One (r2)
  | 1374 -> One (r3)
  | 1373 -> One (r4)
  | 1372 -> One (r5)
  | 1371 -> One (r6)
  | 57 -> One (r7)
  | 56 -> One (r8)
  | 1370 -> One (r9)
  | 1369 -> One (r10)
  | 59 -> One (r11)
  | 359 -> One (r12)
  | 68 -> One (r13)
  | 76 | 875 -> One (r14)
  | 75 | 874 -> One (r15)
  | 69 | 873 -> One (r16)
  | 72 -> One (r17)
  | 71 -> One (r18)
  | 74 -> One (r19)
  | 81 -> One (r20)
  | 88 -> One (r21)
  | 336 -> One (r22)
  | 335 -> One (r23)
  | 323 -> One (r24)
  | 322 -> One (r25)
  | 93 -> One (r26)
  | 102 -> One (r27)
  | 101 -> One (r28)
  | 107 -> One (r29)
  | 106 -> One (r30)
  | 256 -> One (r31)
  | 255 -> One (r32)
  | 117 -> One (r33)
  | 116 -> One (r34)
  | 115 -> One (r35)
  | 113 -> One (r36)
  | 114 -> One (r37)
  | 124 -> One (r38)
  | 123 -> One (r39)
  | 122 -> One (r40)
  | 127 -> One (r41)
  | 254 -> One (r42)
  | 253 -> One (r43)
  | 252 -> One (r44)
  | 130 -> One (r45)
  | 129 -> One (r46)
  | 251 -> One (r47)
  | 250 -> One (r48)
  | 249 -> One (r49)
  | 242 -> One (r50)
  | 241 -> One (r51)
  | 135 -> One (r52)
  | 134 -> One (r53)
  | 139 -> One (r54)
  | 137 -> One (r55)
  | 240 -> One (r56)
  | 239 -> One (r57)
  | 238 -> One (r58)
  | 145 -> One (r59)
  | 148 -> One (r60)
  | 232 -> One (r61)
  | 231 -> One (r62)
  | 150 -> One (r63)
  | 152 -> One (r64)
  | 225 -> One (r65)
  | 224 -> One (r66)
  | 154 -> One (r67)
  | 220 -> One (r68)
  | 213 -> One (r69)
  | 212 -> One (r70)
  | 158 -> One (r71)
  | 160 -> One (r72)
  | 211 -> One (r73)
  | 210 -> One (r74)
  | 174 -> One (r75)
  | 173 -> One (r76)
  | 163 -> One (r77)
  | 165 -> One (r78)
  | 170 -> One (r79)
  | 172 | 867 -> One (r80)
  | 176 -> One (r82)
  | 202 -> One (r83)
  | 199 -> One (r84)
  | 196 -> One (r85)
  | 180 -> One (r86)
  | 186 -> One (r87)
  | 188 -> One (r88)
  | 192 -> One (r89)
  | 195 -> One (r90)
  | 194 -> One (r91)
  | 198 -> One (r92)
  | 201 -> One (r93)
  | 205 -> One (r94)
  | 209 -> One (r95)
  | 208 -> One (r96)
  | 207 -> One (r97)
  | 216 -> One (r98)
  | 215 -> One (r99)
  | 230 -> One (r100)
  | 229 -> One (r101)
  | 228 -> One (r102)
  | 237 -> One (r103)
  | 244 -> One (r104)
  | 247 -> One (r105)
  | 259 -> One (r106)
  | 348 -> One (r107)
  | 346 -> One (r108)
  | 345 -> One (r109)
  | 342 -> One (r110)
  | 267 -> One (r111)
  | 270 -> One (r112)
  | 269 -> One (r113)
  | 275 -> One (r114)
  | 341 -> One (r115)
  | 338 -> One (r116)
  | 314 -> One (r117)
  | 299 -> One (r118)
  | 303 -> One (r119)
  | 305 -> One (r120)
  | 308 -> One (r121)
  | 313 -> One (r122)
  | 325 -> One (r123)
  | 328 -> One (r124)
  | 327 -> One (r125)
  | 330 -> One (r126)
  | 337 -> One (r127)
  | 351 -> One (r128)
  | 350 -> One (r129)
  | 358 -> One (r130)
  | 357 -> One (r131)
  | 356 -> One (r132)
  | 355 -> One (r133)
  | 354 -> One (r134)
  | 353 -> One (r135)
  | 1368 -> One (r136)
  | 1367 -> One (r137)
  | 1366 -> One (r138)
  | 1365 -> One (r139)
  | 1364 -> One (r140)
  | 362 -> One (r141)
  | 1356 -> One (r142)
  | 364 -> One (r143)
  | 1355 -> One (r144)
  | 1354 -> One (r145)
  | 1353 -> One (r146)
  | 1352 -> One (r147)
  | 366 -> One (r148)
  | 1351 -> One (r149)
  | 371 -> One (r150)
  | 370 -> One (r151)
  | 373 -> One (r152)
  | 1259 -> One (r153)
  | 1258 -> One (r154)
  | 1347 -> One (r155)
  | 390 -> One (r156)
  | 389 -> One (r157)
  | 388 -> One (r159)
  | 387 -> One (r160)
  | 380 -> One (r161)
  | 383 -> One (r162)
  | 382 -> One (r163)
  | 386 -> One (r164)
  | 385 -> One (r165)
  | 1270 -> One (r166)
  | 416 -> One (r167)
  | 415 -> One (r168)
  | 414 -> One (r169)
  | 408 -> One (r170)
  | 405 -> One (r172)
  | 400 -> One (r173)
  | 397 -> One (r174)
  | 396 -> One (r175)
  | 395 -> One (r176)
  | 394 -> One (r177)
  | 404 -> One (r178)
  | 403 -> One (r179)
  | 402 -> One (r180)
  | 407 -> One (r181)
  | 413 -> One (r182)
  | 412 -> One (r183)
  | 411 -> One (r184)
  | 1269 -> One (r185)
  | 1268 -> One (r186)
  | 1267 -> One (r187)
  | 1266 -> One (r188)
  | 420 -> One (r189)
  | 1265 -> One (r190)
  | 1264 -> One (r191)
  | 1263 -> One (r192)
  | 1262 -> One (r193)
  | 422 -> One (r194)
  | 1247 -> One (r195)
  | 1246 -> One (r196)
  | 429 -> One (r197)
  | 428 -> One (r198)
  | 1242 -> One (r199)
  | 1241 -> One (r200)
  | 1231 -> One (r201)
  | 1230 -> One (r202)
  | 1229 -> One (r203)
  | 1228 -> One (r204)
  | 1227 -> One (r205)
  | 436 -> One (r206)
  | 434 -> One (r207)
  | 1226 -> One (r208)
  | 446 -> One (r209)
  | 445 -> One (r210)
  | 444 -> One (r211)
  | 443 -> One (r212)
  | 1192 -> One (r213)
  | 1191 -> One (r214)
  | 1190 -> One (r215)
  | 449 -> One (r216)
  | 1189 -> One (r217)
  | 1168 -> One (r218)
  | 458 -> One (r219)
  | 457 -> One (r220)
  | 453 -> One (r221)
  | 456 -> One (r222)
  | 455 -> One (r223)
  | 1167 -> One (r224)
  | 1166 -> One (r225)
  | 1165 -> One (r226)
  | 461 -> One (r227)
  | 1161 -> One (r228)
  | 463 -> One (r229)
  | 1160 -> One (r230)
  | 1159 -> One (r231)
  | 1156 -> One (r232)
  | 1155 -> One (r233)
  | 1154 -> One (r234)
  | 1153 -> One (r235)
  | 1152 -> One (r236)
  | 1151 -> One (r237)
  | 1150 -> One (r238)
  | 470 -> One (r239)
  | 910 -> One (r240)
  | 1147 -> One (r241)
  | 473 -> One (r242)
  | 1144 -> One (r243)
  | 1143 -> One (r244)
  | 1142 -> One (r245)
  | 475 -> One (r246)
  | 1138 -> One (r247)
  | 478 -> One (r248)
  | 477 -> One (r249)
  | 1137 -> One (r250)
  | 1136 -> One (r251)
  | 479 -> One (r252)
  | 1135 -> One (r253)
  | 1134 -> One (r254)
  | 1133 -> One (r255)
  | 1122 -> One (r256)
  | 502 -> One (r257)
  | 1130 -> One (r259)
  | 501 -> One (r260)
  | 482 -> One (r261)
  | 484 -> One (r262)
  | 493 -> One (r264)
  | 491 -> One (r265)
  | 490 -> One (r266)
  | 489 -> One (r267)
  | 488 -> One (r268)
  | 497 -> One (r269)
  | 496 -> One (r270)
  | 499 -> One (r271)
  | 500 -> One (r272)
  | 1114 -> One (r273)
  | 1113 -> One (r274)
  | 1112 -> One (r275)
  | 1111 -> One (r276)
  | 506 -> One (r277)
  | 505 -> One (r278)
  | 504 -> One (r279)
  | 1110 -> One (r280)
  | 1109 -> One (r281)
  | 1108 -> One (r282)
  | 1107 -> One (r283)
  | 1106 -> One (r284)
  | 1103 -> One (r285)
  | 510 -> One (r286)
  | 509 -> One (r287)
  | 508 -> One (r288)
  | 514 -> One (r289)
  | 1052 -> One (r290)
  | 517 | 938 | 1050 | 1412 -> One (r291)
  | 516 | 1049 | 1411 -> One (r292)
  | 1102 -> One (r293)
  | 1101 -> One (r294)
  | 905 -> One (r295)
  | 904 -> One (r296)
  | 903 -> One (r297)
  | 902 -> One (r298)
  | 901 -> One (r299)
  | 900 -> One (r300)
  | 899 -> One (r301)
  | 524 -> One (r302)
  | 898 -> One (r303)
  | 897 -> One (r304)
  | 526 -> One (r305)
  | 896 -> One (r306)
  | 895 -> One (r307)
  | 532 -> One (r308)
  | 531 -> One (r309)
  | 530 -> One (r310)
  | 529 -> One (r311)
  | 633 -> One (r312)
  | 548 -> One (r313)
  | 544 -> One (r314)
  | 543 -> One (r315)
  | 542 -> One (r316)
  | 541 -> One (r317)
  | 546 -> One (r318)
  | 553 -> One (r319)
  | 552 -> One (r320)
  | 630 -> One (r321)
  | 624 -> One (r322)
  | 623 -> One (r323)
  | 557 | 638 -> One (r324)
  | 556 | 637 | 1386 -> One (r325)
  | 555 -> One (r327)
  | 611 -> One (r328)
  | 566 -> One (r329)
  | 563 -> One (r330)
  | 565 -> One (r331)
  | 593 -> One (r333)
  | 592 -> One (r334)
  | 572 -> One (r335)
  | 571 -> One (r337)
  | 579 -> One (r338)
  | 576 | 589 -> One (r339)
  | 575 | 588 -> One (r340)
  | 574 | 587 -> One (r341)
  | 582 -> One (r342)
  | 581 -> One (r343)
  | 584 -> One (r344)
  | 590 -> One (r345)
  | 586 -> One (r346)
  | 610 -> One (r347)
  | 609 -> One (r348)
  | 607 -> One (r349)
  | 606 -> One (r350)
  | 603 -> One (r351)
  | 602 -> One (r352)
  | 601 -> One (r353)
  | 600 -> One (r354)
  | 599 -> One (r355)
  | 614 -> One (r356)
  | 621 -> One (r357)
  | 620 -> One (r358)
  | 619 -> One (r359)
  | 618 -> One (r360)
  | 617 -> One (r361)
  | 628 -> One (r362)
  | 627 -> One (r363)
  | 626 -> One (r364)
  | 629 -> One (r366)
  | 635 -> One (r367)
  | 648 -> One (r368)
  | 647 -> One (r369)
  | 646 -> One (r370)
  | 650 -> One (r372)
  | 649 -> One (r374)
  | 643 -> One (r375)
  | 642 -> One (r376)
  | 641 -> One (r377)
  | 640 -> One (r378)
  | 645 -> One (r379)
  | 894 -> One (r380)
  | 655 -> One (r381)
  | 654 -> One (r382)
  | 653 -> One (r383)
  | 736 -> One (r384)
  | 732 -> One (r385)
  | 731 -> One (r386)
  | 730 -> One (r387)
  | 729 -> One (r388)
  | 725 -> One (r389)
  | 724 -> One (r390)
  | 723 -> One (r391)
  | 720 -> One (r392)
  | 719 -> One (r393)
  | 660 -> One (r394)
  | 664 -> One (r395)
  | 663 -> One (r396)
  | 662 -> One (r397)
  | 666 -> One (r398)
  | 668 -> One (r399)
  | 718 -> One (r400)
  | 717 -> One (r401)
  | 677 -> One (r402)
  | 675 -> One (r403)
  | 674 -> One (r404)
  | 709 -> One (r405)
  | 700 -> One (r406)
  | 699 -> One (r407)
  | 698 -> One (r408)
  | 690 -> One (r409)
  | 686 -> One (r410)
  | 685 -> One (r411)
  | 684 -> One (r412)
  | 683 -> One (r413)
  | 688 -> One (r414)
  | 697 -> One (r415)
  | 696 -> One (r416)
  | 695 -> One (r417)
  | 708 -> One (r418)
  | 707 -> One (r419)
  | 706 -> One (r420)
  | 702 -> One (r421)
  | 705 -> One (r422)
  | 704 -> One (r423)
  | 711 -> One (r425)
  | 722 -> One (r426)
  | 728 -> One (r427)
  | 727 -> One (r428)
  | 735 -> One (r429)
  | 734 -> One (r430)
  | 741 -> One (r431)
  | 740 -> One (r432)
  | 739 -> One (r433)
  | 738 -> One (r434)
  | 745 -> One (r436)
  | 742 -> One (r438)
  | 744 -> One (r439)
  | 749 -> One (r440)
  | 748 -> One (r441)
  | 747 -> One (r442)
  | 753 -> One (r443)
  | 752 -> One (r444)
  | 751 -> One (r445)
  | 756 -> One (r446)
  | 755 -> One (r447)
  | 765 -> One (r448)
  | 764 -> One (r449)
  | 761 -> One (r450)
  | 760 -> One (r451)
  | 759 -> One (r452)
  | 758 -> One (r453)
  | 771 -> One (r454)
  | 770 -> One (r455)
  | 767 -> One (r457)
  | 769 -> One (r458)
  | 884 -> One (r459)
  | 846 -> One (r460)
  | 830 -> One (r461)
  | 845 -> One (r463)
  | 844 -> One (r464)
  | 781 -> One (r465)
  | 780 -> One (r466)
  | 779 -> One (r467)
  | 778 -> One (r468)
  | 777 -> One (r469)
  | 789 -> One (r470)
  | 788 -> One (r471)
  | 787 -> One (r473)
  | 786 -> One (r474)
  | 785 -> One (r475)
  | 784 -> One (r476)
  | 805 -> One (r477)
  | 804 -> One (r478)
  | 803 -> One (r479)
  | 802 -> One (r481)
  | 801 -> One (r482)
  | 795 -> One (r483)
  | 794 -> One (r484)
  | 793 -> One (r485)
  | 792 -> One (r486)
  | 800 -> One (r487)
  | 799 -> One (r488)
  | 798 -> One (r489)
  | 797 -> One (r490)
  | 818 -> One (r491)
  | 817 -> One (r492)
  | 813 -> One (r493)
  | 812 -> One (r494)
  | 811 -> One (r495)
  | 816 -> One (r496)
  | 815 -> One (r497)
  | 832 -> One (r498)
  | 831 -> One (r499)
  | 828 -> One (r500)
  | 824 -> One (r501)
  | 823 -> One (r502)
  | 827 -> One (r503)
  | 826 -> One (r504)
  | 837 -> One (r505)
  | 836 -> One (r506)
  | 835 -> One (r507)
  | 839 -> One (r509)
  | 838 -> One (r510)
  | 841 -> One (r511)
  | 848 -> One (r512)
  | 878 -> One (r513)
  | 883 -> One (r515)
  | 882 -> One (r516)
  | 853 -> One (r517)
  | 852 -> One (r518)
  | 851 -> One (r519)
  | 881 -> One (r520)
  | 858 -> One (r521)
  | 857 -> One (r522)
  | 856 -> One (r523)
  | 855 -> One (r524)
  | 880 -> One (r525)
  | 861 -> One (r526)
  | 860 -> One (r527)
  | 879 -> One (r528)
  | 865 -> One (r529)
  | 864 -> One (r530)
  | 863 -> One (r531)
  | 877 -> One (r532)
  | 869 -> One (r533)
  | 872 -> One (r534)
  | 886 -> One (r535)
  | 889 -> One (r536)
  | 892 -> One (r537)
  | 917 -> One (r538)
  | 916 -> One (r539)
  | 919 | 1062 -> One (r540)
  | 918 | 927 -> One (r541)
  | 921 -> One (r542)
  | 1095 -> One (r543)
  | 926 -> One (r544)
  | 1061 -> One (r545)
  | 932 -> One (r546)
  | 1060 | 1087 -> One (r547)
  | 933 | 1071 -> One (r548)
  | 936 | 1074 -> One (r549)
  | 935 | 1073 -> One (r550)
  | 934 | 1072 -> One (r551)
  | 1023 -> One (r552)
  | 1022 -> One (r553)
  | 1019 -> One (r554)
  | 1013 -> One (r555)
  | 1012 -> One (r556)
  | 1011 -> One (r557)
  | 942 -> One (r558)
  | 947 -> One (r559)
  | 946 -> One (r560)
  | 945 -> One (r561)
  | 1000 -> One (r562)
  | 949 -> One (r563)
  | 952 -> One (r564)
  | 954 -> One (r565)
  | 956 -> One (r566)
  | 961 -> One (r567)
  | 963 -> One (r568)
  | 965 -> One (r569)
  | 967 -> One (r570)
  | 969 -> One (r571)
  | 971 -> One (r572)
  | 973 -> One (r573)
  | 975 -> One (r574)
  | 977 -> One (r575)
  | 979 -> One (r576)
  | 981 -> One (r577)
  | 983 -> One (r578)
  | 985 -> One (r579)
  | 987 -> One (r580)
  | 989 -> One (r581)
  | 991 -> One (r582)
  | 993 -> One (r583)
  | 995 -> One (r584)
  | 997 -> One (r585)
  | 999 -> One (r586)
  | 1002 -> One (r587)
  | 1004 -> One (r588)
  | 1010 -> One (r589)
  | 1009 -> One (r590)
  | 1017 -> One (r591)
  | 1016 -> One (r592)
  | 1028 -> One (r593)
  | 1027 -> One (r594)
  | 1026 -> One (r595)
  | 1036 -> One (r596)
  | 1035 -> One (r597)
  | 1030 -> One (r598)
  | 1034 -> One (r599)
  | 1033 -> One (r600)
  | 1048 -> One (r601)
  | 1047 -> One (r602)
  | 1040 -> One (r603)
  | 1039 -> One (r604)
  | 1046 -> One (r605)
  | 1056 | 1079 -> One (r606)
  | 1055 | 1078 -> One (r607)
  | 1054 | 1077 -> One (r608)
  | 1059 | 1084 -> One (r609)
  | 1058 | 1083 -> One (r610)
  | 1057 | 1082 -> One (r611)
  | 1064 -> One (r612)
  | 1066 -> One (r613)
  | 1068 -> One (r614)
  | 1070 -> One (r615)
  | 1076 -> One (r616)
  | 1081 -> One (r617)
  | 1086 -> One (r618)
  | 1089 -> One (r619)
  | 1097 -> One (r620)
  | 1099 -> One (r621)
  | 1121 -> One (r622)
  | 1120 -> One (r623)
  | 1119 -> One (r624)
  | 1118 -> One (r625)
  | 1117 -> One (r626)
  | 1116 -> One (r627)
  | 1129 -> One (r628)
  | 1126 -> One (r629)
  | 1125 -> One (r630)
  | 1128 -> One (r631)
  | 1132 -> One (r632)
  | 1140 -> One (r634)
  | 1146 -> One (r635)
  | 1149 -> One (r636)
  | 1158 -> One (r637)
  | 1164 -> One (r638)
  | 1163 -> One (r639)
  | 1180 -> One (r640)
  | 1179 -> One (r641)
  | 1178 -> One (r642)
  | 1171 -> One (r643)
  | 1170 -> One (r644)
  | 1173 -> One (r645)
  | 1177 -> One (r646)
  | 1176 -> One (r647)
  | 1175 -> One (r648)
  | 1182 -> One (r649)
  | 1187 -> One (r650)
  | 1186 -> One (r651)
  | 1185 -> One (r652)
  | 1184 -> One (r653)
  | 1211 -> One (r654)
  | 1196 -> One (r655)
  | 1195 -> One (r656)
  | 1194 -> One (r657)
  | 1198 -> One (r658)
  | 1197 -> One (r659)
  | 1200 -> One (r660)
  | 1202 -> One (r661)
  | 1204 -> One (r662)
  | 1207 -> One (r663)
  | 1206 -> One (r664)
  | 1210 -> One (r665)
  | 1220 -> One (r666)
  | 1219 -> One (r667)
  | 1218 -> One (r668)
  | 1217 -> One (r669)
  | 1216 -> One (r670)
  | 1225 -> One (r671)
  | 1224 -> One (r672)
  | 1223 -> One (r673)
  | 1222 -> One (r674)
  | 1234 -> One (r675)
  | 1233 -> One (r676)
  | 1237 -> One (r677)
  | 1236 -> One (r678)
  | 1240 -> One (r679)
  | 1239 -> One (r680)
  | 1245 -> One (r681)
  | 1244 -> One (r682)
  | 1250 -> One (r683)
  | 1249 -> One (r684)
  | 1257 -> One (r685)
  | 1256 -> One (r686)
  | 1255 -> One (r687)
  | 1254 -> One (r688)
  | 1253 -> One (r689)
  | 1252 -> One (r690)
  | 1261 -> One (r691)
  | 1273 -> One (r692)
  | 1272 -> One (r693)
  | 1285 -> One (r694)
  | 1284 -> One (r695)
  | 1283 -> One (r696)
  | 1282 -> One (r697)
  | 1281 -> One (r698)
  | 1275 -> One (r699)
  | 1300 -> One (r701)
  | 1299 -> One (r702)
  | 1280 -> One (r703)
  | 1279 -> One (r704)
  | 1278 -> One (r705)
  | 1277 -> One (r706)
  | 1298 -> One (r708)
  | 1287 -> One (r709)
  | 1297 -> One (r710)
  | 1296 -> One (r711)
  | 1295 -> One (r712)
  | 1294 -> One (r713)
  | 1293 -> One (r714)
  | 1292 -> One (r715)
  | 1291 -> One (r716)
  | 1290 -> One (r717)
  | 1303 -> One (r718)
  | 1302 -> One (r719)
  | 1324 -> One (r720)
  | 1322 -> One (r722)
  | 1339 -> One (r724)
  | 1338 -> One (r725)
  | 1335 -> One (r726)
  | 1305 -> One (r727)
  | 1308 -> One (r728)
  | 1307 -> One (r729)
  | 1331 -> One (r730)
  | 1330 -> One (r731)
  | 1329 -> One (r732)
  | 1313 -> One (r733)
  | 1312 -> One (r734)
  | 1311 -> One (r735)
  | 1317 -> One (r736)
  | 1316 -> One (r737)
  | 1315 -> One (r738)
  | 1325 -> One (r740)
  | 1320 -> One (r741)
  | 1319 -> One (r742)
  | 1328 -> One (r744)
  | 1334 -> One (r745)
  | 1333 -> One (r746)
  | 1337 -> One (r747)
  | 1342 -> One (r748)
  | 1341 -> One (r749)
  | 1344 -> One (r750)
  | 1350 -> One (r751)
  | 1349 -> One (r752)
  | 1359 -> One (r753)
  | 1358 -> One (r754)
  | 1361 -> One (r755)
  | 1363 -> One (r756)
  | 1382 -> One (r757)
  | 1379 -> One (r758)
  | 1381 -> One (r759)
  | 1384 -> One (r760)
  | 1403 -> One (r761)
  | 1401 -> One (r763)
  | 1391 -> One (r764)
  | 1390 -> One (r765)
  | 1389 -> One (r766)
  | 1388 -> One (r767)
  | 1394 -> One (r768)
  | 1400 -> One (r769)
  | 1399 -> One (r770)
  | 1396 -> One (r771)
  | 1405 -> One (r772)
  | 1414 -> One (r773)
  | 1419 -> One (r774)
  | 1418 -> One (r775)
  | 1417 -> One (r776)
  | 1423 -> One (r777)
  | 1422 -> One (r778)
  | 1421 -> One (r779)
  | 1427 -> One (r780)
  | 1426 -> One (r781)
  | 1425 -> One (r782)
  | 1431 -> One (r783)
  | 1430 -> One (r784)
  | 1429 -> One (r785)
  | 1437 -> One (r786)
  | 1436 -> One (r787)
  | 1435 -> One (r788)
  | 1434 -> One (r789)
  | 1433 -> One (r790)
  | 1441 -> One (r791)
  | 1440 -> One (r792)
  | 1439 -> One (r793)
  | 1445 -> One (r794)
  | 1444 -> One (r795)
  | 1443 -> One (r796)
  | 1461 -> One (r797)
  | 1449 -> One (r798)
  | 1448 -> One (r799)
  | 1455 -> One (r800)
  | 1451 -> One (r801)
  | 1454 -> One (r802)
  | 1453 -> One (r803)
  | 1457 -> One (r804)
  | 1459 -> One (r805)
  | 1465 | 1513 -> One (r806)
  | 1464 | 1512 -> One (r807)
  | 1463 | 1511 -> One (r808)
  | 1462 | 1510 -> One (r809)
  | 1469 | 1517 -> One (r810)
  | 1468 | 1516 -> One (r811)
  | 1467 | 1515 -> One (r812)
  | 1466 | 1514 -> One (r813)
  | 1472 -> One (r814)
  | 1471 -> One (r815)
  | 1479 -> One (r816)
  | 1478 -> One (r817)
  | 1477 -> One (r818)
  | 1476 -> One (r819)
  | 1475 -> One (r820)
  | 1474 -> One (r821)
  | 1487 -> One (r822)
  | 1486 -> One (r823)
  | 1481 -> One (r825)
  | 1483 -> One (r826)
  | 1485 -> One (r827)
  | 1504 -> One (r828)
  | 1490 -> One (r829)
  | 1495 -> One (r830)
  | 1494 -> One (r831)
  | 1503 -> One (r833)
  | 1502 -> One (r834)
  | 1493 -> One (r835)
  | 1492 -> One (r836)
  | 1499 -> One (r837)
  | 1498 -> One (r838)
  | 1497 -> One (r839)
  | 1501 -> One (r840)
  | 1506 -> One (r841)
  | 1518 -> One (r842)
  | 1521 -> One (r843)
  | 1525 -> One (r844)
  | 1524 -> One (r845)
  | 1527 -> One (r846)
  | 1531 -> One (r847)
  | 1535 -> One (r848)
  | 937 -> Select (function
    | -1 -> [R 89]
    | _ -> r292)
  | 432 -> Select (function
    | -1 -> [R 688]
    | _ -> [R 217])
  | 423 -> Select (function
    | -1 -> S (T T_RPAREN) :: r26
    | _ -> S (N N_seq_expr) :: r154)
  | 459 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r111
    | _ -> S (N N_expr_semi_list) :: r226)
  | 1395 -> Select (function
    | 1392 -> r370
    | _ -> S (T T_EQUAL) :: r771)
  | 90 -> Select (function
    | 436 | 942 | 1014 | 1184 | 1311 | 1464 | 1468 | 1512 | 1516 -> S (N N_operator) :: r23
    | _ -> S (N N_pattern) :: r25)
  | _ -> raise Not_found
