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
  [|0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;2;1;2;1;1;1;1;1;1;1;1;2;1;2;3;4;2;3;2;3;1;2;2;2;2;2;1;1;2;2;2;2;2;1;1;1;2;1;1;1;1;1;1;2;3;4;4;1;1;5;6;1;2;1;1;1;2;3;3;2;3;1;1;1;1;2;3;2;1;1;2;1;2;3;1;1;2;3;4;1;2;3;3;1;1;2;1;1;2;1;2;3;1;2;1;2;1;2;1;1;1;2;1;2;2;1;2;1;2;1;1;1;2;3;2;1;3;4;2;3;1;2;1;3;1;1;2;1;1;3;2;3;1;1;2;3;2;3;4;4;2;3;5;1;2;2;1;2;3;2;3;4;5;3;4;3;4;4;5;6;2;1;1;2;2;1;2;1;3;4;1;2;3;2;3;3;4;1;1;2;3;2;3;4;5;2;3;4;5;4;2;3;1;2;3;4;4;5;6;4;3;1;2;3;1;1;1;1;1;1;1;2;1;2;3;1;2;3;1;4;3;1;2;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;1;1;2;3;2;3;2;1;2;1;2;1;1;2;2;1;1;1;1;1;1;1;2;3;2;3;3;4;5;2;3;2;1;1;1;2;3;3;2;1;1;3;2;2;3;3;4;1;2;2;3;4;2;3;4;5;6;7;8;2;3;1;2;1;2;1;2;1;1;1;2;3;1;2;1;1;1;1;1;1;2;1;2;3;3;4;5;3;4;1;2;1;1;1;2;3;4;5;1;1;2;1;2;3;4;3;1;2;1;2;3;4;5;6;2;3;4;1;1;1;2;1;2;1;1;1;2;1;2;3;1;2;1;1;2;1;3;1;1;2;1;1;2;3;3;4;2;1;2;3;1;1;1;2;1;2;3;3;4;1;1;2;1;2;1;1;1;1;1;1;2;1;1;2;1;2;1;2;3;1;2;1;2;1;1;2;1;2;3;3;4;5;1;2;1;2;3;4;1;2;1;3;1;1;2;3;4;1;2;3;4;1;1;1;2;1;1;2;3;4;1;1;1;1;2;2;3;1;1;2;3;4;5;1;1;2;1;1;1;1;1;2;2;2;3;2;3;1;3;4;1;2;3;5;2;3;1;2;1;1;1;2;1;2;1;1;3;3;2;1;1;3;1;1;1;2;3;1;1;2;1;2;3;1;2;2;3;1;2;3;4;1;2;3;1;2;2;3;1;2;3;4;5;4;2;3;5;6;1;3;4;2;3;1;4;4;5;6;7;8;5;6;2;3;4;2;1;2;3;3;5;1;1;2;3;4;2;1;2;2;3;4;5;6;2;3;1;2;3;7;1;1;1;2;3;4;1;2;1;2;3;1;2;3;4;2;3;3;4;2;1;1;1;1;2;3;1;4;2;1;1;1;1;2;2;2;3;2;3;1;2;1;3;1;2;4;5;6;3;4;5;1;1;2;3;4;2;3;4;3;2;3;1;2;1;2;1;2;3;4;5;1;2;6;2;3;3;4;5;3;4;2;3;4;5;6;4;2;1;2;3;4;3;2;3;1;1;2;3;4;1;2;3;4;1;2;3;1;2;3;4;5;1;2;6;7;1;2;3;4;1;2;1;1;2;1;1;2;3;2;3;4;1;1;2;3;2;3;1;2;1;1;2;3;4;5;1;2;3;4;5;2;3;1;2;3;1;1;2;1;2;2;3;4;1;2;3;5;6;1;1;1;1;2;3;1;2;3;4;1;1;2;3;2;1;1;2;3;2;3;1;2;1;2;5;6;3;2;3;1;1;2;3;4;1;2;3;4;5;1;2;3;1;2;3;4;1;1;1;2;1;2;3;1;2;3;1;3;1;5;4;6;5;6;2;2;3;1;1;2;1;1;2;1;2;2;3;4;5;2;3;4;5;6;7;8;1;1;1;1;2;1;1;1;1;1;2;3;2;3;4;5;1;1;1;1;2;2;3;1;2;2;3;2;3;4;5;1;2;3;3;1;2;1;2;3;4;5;1;2;1;2;3;2;3;2;3;2;1;2;2;3;1;2;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;3;2;3;2;3;1;2;3;1;2;3;3;4;5;2;1;2;3;1;4;2;3;5;6;1;3;4;5;6;3;4;2;3;4;5;5;6;3;1;2;3;1;2;3;1;2;3;4;5;1;2;3;3;1;3;4;5;3;4;5;3;4;3;4;5;1;2;1;2;1;2;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;3;4;5;2;1;2;1;1;2;5;6;2;3;1;4;5;5;1;1;6;7;8;9;10;5;6;7;8;4;5;6;7;8;9;10;2;1;2;3;4;1;2;5;4;3;4;3;4;5;2;3;4;2;3;1;3;4;5;6;7;3;3;4;3;2;3;2;2;3;4;4;5;2;3;3;2;3;4;2;3;4;5;2;3;4;1;2;1;2;3;4;5;6;7;1;2;2;3;4;5;6;1;2;4;5;2;1;2;3;4;1;2;1;2;1;2;3;4;1;2;3;1;1;2;5;2;3;1;2;4;5;6;7;8;3;4;5;6;7;2;4;5;6;3;4;4;5;6;4;5;6;6;7;8;2;3;3;4;5;3;4;4;5;6;2;3;4;5;6;7;8;2;3;3;4;3;4;5;6;3;4;5;6;5;4;5;6;1;1;2;3;4;5;6;2;3;4;5;6;2;3;4;5;6;7;8;9;10;5;6;7;4;2;3;1;2;3;1;2;1;2;3;1;1;2;3;4;1;2;3;4;1;1;2;1;1;2;1;3;2;2;2;5;2;3;3;4;5;3;1;2;4;5;1;2;3;1;2;1;2;2;2;3;4;2;3;4;5;6;3;4;5;6;7;8;4;5;3;4;5;6;4;3;4;3;2;3;4;5;6;1;2;3;4;5;2;3;4;2;1;2;3;4;5;6;2;3;3;1;2;1;1;3;4;7;1;1;2;3;4;4;4;4;4;1;2;1;2;1;1;2;3;4;1;2;3;4;1;2;3;4;1;2;3;4;1;2;3;4;5;6;1;2;3;4;1;2;3;4;1;1;2;3;2;3;4;5;6;4;2;3;2;3;1;2;1;2;3;4;1;2;3;4;1;2;3;1;2;3;4;5;6;7;1;2;3;4;1;2;1;2;1;2;3;1;2;3;1;2;1;2;3;4;1;2;4;5;2;2;3;1;2;1;1;2;3;4;1;2;3;4;2;1;1;2;1;2;3;4;1;2;1;0;1;2;1;0;1;2;1;|]

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
  | T_EXITPOINT -> true
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
  let r0 = [R 637] in
  let r1 = [R 147] in
  let r2 = S (T T_DONE) :: r1 in
  let r3 = S (N N_seq_expr) :: r2 in
  let r4 = S (T T_DO) :: r3 in
  let r5 = S (N N_seq_expr) :: r4 in
  let r6 = [R 210] in
  let r7 = S (N N_attributes) :: r6 in
  let r8 = [R 9] in
  let r9 = [R 10] in
  let r10 = S (T T_RBRACKET) :: r9 in
  let r11 = S (N N_payload) :: r10 in
  let r12 = [R 418] in
  let r13 = [R 564] in
  let r14 = [R 697] in
  let r15 = S (T T_LIDENT) :: r14 in
  let r16 = S (T T_DOT) :: r15 in
  let r17 = [R 302] in
  let r18 = S (T T_RPAREN) :: r17 in
  let r19 = [R 301] in
  let r20 = [R 482] in
  let r21 = [R 477] in
  let r22 = [R 711] in
  let r23 = S (T T_RPAREN) :: r22 in
  let r24 = [R 569] in
  let r25 = S (T T_RPAREN) :: r24 in
  let r26 = [R 92] in
  let r27 = [R 571] in
  let r28 = S (T T_RPAREN) :: r27 in
  let r29 = [R 572] in
  let r30 = S (T T_RPAREN) :: r29 in
  let r31 = [R 392] in
  let r32 = [R 393] in
  let r33 = S (N N_core_type) :: r32 in
  let r34 = S (T T_EQUAL) :: r33 in
  let r35 = [R 250] in
  let r36 = S (T T_LIDENT) :: r35 in
  let r37 = [R 304] in
  let r38 = [R 496] in
  let r39 = [R 40] in
  let r40 = S (T T_LIDENT) :: r39 in
  let r41 = [R 489] in
  let r42 = [R 105] in
  let r43 = S (N N_core_type2) :: r42 in
  let r44 = S (T T_MINUSGREATER) :: r43 in
  let r45 = S (N N_core_type2) :: r44 in
  let r46 = S (T T_COLON) :: r45 in
  let r47 = [R 106] in
  let r48 = S (N N_core_type2) :: r47 in
  let r49 = S (T T_MINUSGREATER) :: r48 in
  let r50 = [R 488] in
  let r51 = S (T T_RPAREN) :: r50 in
  let r52 = [R 506] in
  let r53 = S (T T_RPAREN) :: r52 in
  let r54 = [R 337] in
  let r55 = S (N N_ident) :: r54 in
  let r56 = [R 107] in
  let r57 = S (N N_core_type2) :: r56 in
  let r58 = S (T T_MINUSGREATER) :: r57 in
  let r59 = [R 495] in
  let r60 = [R 494] in
  let r61 = [R 214] in
  let r62 = S (N N_attributes) :: r61 in
  let r63 = S (N N_poly_type_no_attr) :: r62 in
  let r64 = [R 708] in
  let r65 = [R 211] in
  let r66 = S (T T_RBRACKET) :: r65 in
  let r67 = S (N N_payload) :: r66 in
  let r68 = [R 417] in
  let r69 = [R 504] in
  let r70 = S (T T_RBRACKET) :: r69 in
  let r71 = S (N N_row_field_list) :: r70 in
  let r72 = [R 503] in
  let r73 = [R 502] in
  let r74 = S (T T_RBRACKET) :: r73 in
  let r75 = [R 500] in
  let r76 = S (T T_RBRACKET) :: r75 in
  let r77 = S (N N_row_field_list) :: r76 in
  let r78 = [R 340] in
  let r79 = [R 497] in
  let r80 = [R 443] in
  let r81 = S (N N_simple_core_type) :: r80 in
  let r82 = [R 445] in
  let r83 = [R 671] in
  let r84 = [R 670] in
  let r85 = S (N N_attributes) :: r84 in
  let r86 = S (N N_amper_type_list) :: r85 in
  let r87 = [R 511] in
  let r88 = [R 112] in
  let r89 = [R 108] in
  let r90 = [R 116] in
  let r91 = S (N N_ident) :: r90 in
  let r92 = [R 7] in
  let r93 = [R 12] in
  let r94 = [R 499] in
  let r95 = [R 501] in
  let r96 = S (T T_RBRACKET) :: r95 in
  let r97 = S (N N_row_field_list) :: r96 in
  let r98 = [R 505] in
  let r99 = S (T T_RBRACKET) :: r98 in
  let r100 = [R 423] in
  let r101 = S (N N_core_type_no_attr) :: r100 in
  let r102 = [R 709] in
  let r103 = [R 293] in
  let r104 = [R 498] in
  let r105 = [R 110] in
  let r106 = [R 395] in
  let r107 = [R 568] in
  let r108 = [R 566] in
  let r109 = S (T T_RBRACKET) :: r108 in
  let r110 = R 376 :: r109 in
  let r111 = [R 91] in
  let r112 = [R 565] in
  let r113 = S (T T_RBRACE) :: r112 in
  let r114 = [R 271] in
  let r115 = [R 267] in
  let r116 = [R 407] in
  let r117 = [R 408] in
  let r118 = [R 561] in
  let r119 = [R 410] in
  let r120 = [R 404] in
  let r121 = [R 403] in
  let r122 = [R 402] in
  let r123 = [R 411] in
  let r124 = [R 570] in
  let r125 = S (T T_RPAREN) :: r124 in
  let r126 = [R 406] in
  let r127 = [R 400] in
  let r128 = [R 567] in
  let r129 = S (T T_BARRBRACKET) :: r128 in
  let r130 = [R 405] in
  let r131 = S (T T_RPAREN) :: r130 in
  let r132 = S (N N_pattern) :: r131 in
  let r133 = S (T T_COMMA) :: r132 in
  let r134 = S (N N_pattern) :: r133 in
  let r135 = S (T T_LPAREN) :: r134 in
  let r136 = [R 419] in
  let r137 = [R 166] in
  let r138 = S (T T_DONE) :: r137 in
  let r139 = S (N N_seq_expr) :: r138 in
  let r140 = S (T T_DO) :: r139 in
  let r141 = S (N N_seq_expr) :: r140 in
  let r142 = [R 143] in
  let r143 = S (N N_seq_expr) :: r142 in
  let r144 = [R 160] in
  let r145 = S (N N_match_cases) :: r144 in
  let r146 = R 372 :: r145 in
  let r147 = S (T T_WITH) :: r146 in
  let r148 = S (N N_seq_expr) :: r147 in
  let r149 = [R 543] in
  let r150 = [R 545] in
  let r151 = S (N N_class_longident) :: r150 in
  let r152 = [R 209] in
  let r153 = [R 525] in
  let r154 = S (T T_RPAREN) :: r153 in
  let r155 = [R 539] in
  let r156 = [R 65] in
  let r157 = S (N N_class_fields) :: r156 in
  let r158 = R 43 :: r157 in
  let r159 = [R 199] in
  let r160 = S (T T_END) :: r159 in
  let r161 = Sub (r158) :: r160 in
  let r162 = [R 41] in
  let r163 = S (T T_RPAREN) :: r162 in
  let r164 = [R 42] in
  let r165 = S (T T_RPAREN) :: r164 in
  let r166 = [R 716] in
  let r167 = S (N N_seq_expr) :: r166 in
  let r168 = S (T T_EQUAL) :: r167 in
  let r169 = S (N N_label) :: r168 in
  let r170 = R 338 :: r169 in
  let r171 = R 389 :: r170 in
  let r172 = [R 26] in
  let r173 = S (N N_post_item_attributes) :: r172 in
  let r174 = [R 715] in
  let r175 = S (N N_core_type) :: r174 in
  let r176 = S (T T_COLON) :: r175 in
  let r177 = S (N N_label) :: r176 in
  let r178 = [R 424] in
  let r179 = S (T T_RBRACKET) :: r178 in
  let r180 = S (N N_payload) :: r179 in
  let r181 = [R 426] in
  let r182 = [R 714] in
  let r183 = S (N N_core_type) :: r182 in
  let r184 = S (T T_COLON) :: r183 in
  let r185 = [R 142] in
  let r186 = S (N N_match_cases) :: r185 in
  let r187 = R 372 :: r186 in
  let r188 = S (T T_WITH) :: r187 in
  let r189 = S (N N_seq_expr) :: r188 in
  let r190 = [R 159] in
  let r191 = S (N N_match_cases) :: r190 in
  let r192 = R 372 :: r191 in
  let r193 = S (T T_WITH) :: r192 in
  let r194 = S (N N_seq_expr) :: r193 in
  let r195 = [R 551] in
  let r196 = S (T T_RPAREN) :: r195 in
  let r197 = [R 315] in
  let r198 = S (T T_END) :: r197 in
  let r199 = [R 320] in
  let r200 = S (T T_RPAREN) :: r199 in
  let r201 = [R 321] in
  let r202 = S (T T_RPAREN) :: r201 in
  let r203 = [R 141] in
  let r204 = S (N N_seq_expr) :: r203 in
  let r205 = S (T T_IN) :: r204 in
  let r206 = S (N N_let_bindings) :: r205 in
  let r207 = S (N N_rec_flag) :: r206 in
  let r208 = [R 274] in
  let r209 = [R 259] in
  let r210 = [R 258] in
  let r211 = S (T T_RPAREN) :: r210 in
  let r212 = [R 248] in
  let r213 = [R 255] in
  let r214 = [R 254] in
  let r215 = S (T T_RPAREN) :: r214 in
  let r216 = R 374 :: r215 in
  let r217 = [R 375] in
  let r218 = [R 155] in
  let r219 = S (N N_seq_expr) :: r218 in
  let r220 = S (T T_IN) :: r219 in
  let r221 = S (N N_expr_open) :: r220 in
  let r222 = [R 205] in
  let r223 = S (N N_mod_longident) :: r222 in
  let r224 = [R 541] in
  let r225 = S (T T_RBRACKET) :: r224 in
  let r226 = R 376 :: r225 in
  let r227 = [R 547] in
  let r228 = [R 215] in
  let r229 = S (N N_expr) :: r228 in
  let r230 = [R 536] in
  let r231 = S (T T_RBRACE) :: r230 in
  let r232 = [R 516] in
  let r233 = S (T T_RPAREN) :: r232 in
  let r234 = S (T T_LPAREN) :: r233 in
  let r235 = [R 527] in
  let r236 = [R 526] in
  let r237 = S (T T_GREATERDOT) :: r236 in
  let r238 = [R 198] in
  let r239 = S (N N_simple_expr) :: r238 in
  let r240 = [R 514] in
  let r241 = [R 529] in
  let r242 = S (T T_END) :: r241 in
  let r243 = [R 165] in
  let r244 = S (N N_expr) :: r243 in
  let r245 = S (T T_THEN) :: r244 in
  let r246 = S (N N_seq_expr) :: r245 in
  let r247 = [R 156] in
  let r248 = S (N N_match_cases) :: r247 in
  let r249 = R 372 :: r248 in
  let r250 = [R 288] in
  let r251 = S (N N_seq_expr) :: r250 in
  let r252 = S (T T_MINUSGREATER) :: r251 in
  let r253 = [R 289] in
  let r254 = S (N N_seq_expr) :: r253 in
  let r255 = S (T T_MINUSGREATER) :: r254 in
  let r256 = [R 220] in
  let r257 = S (N N_seq_expr) :: r256 in
  let r258 = S (T T_MINUSGREATER) :: r257 in
  let r259 = [R 158] in
  let r260 = Sub (r258) :: r259 in
  let r261 = S (N N_newtype) :: r260 in
  let r262 = [R 415] in
  let r263 = S (T T_UNDERSCORE) :: r262 in
  let r264 = [R 257] in
  let r265 = [R 256] in
  let r266 = S (T T_RPAREN) :: r265 in
  let r267 = R 374 :: r266 in
  let r268 = [R 285] in
  let r269 = [R 343] in
  let r270 = S (T T_RPAREN) :: r269 in
  let r271 = [R 260] in
  let r272 = [R 261] in
  let r273 = [R 149] in
  let r274 = S (T T_DONE) :: r273 in
  let r275 = S (N N_seq_expr) :: r274 in
  let r276 = S (T T_DO) :: r275 in
  let r277 = S (N N_seq_expr) :: r276 in
  let r278 = S (T T_IN) :: r277 in
  let r279 = S (N N_pattern) :: r278 in
  let r280 = [R 167] in
  let r281 = S (T T_DONE) :: r280 in
  let r282 = S (N N_seq_expr) :: r281 in
  let r283 = S (T T_DO) :: r282 in
  let r284 = S (N N_seq_expr) :: r283 in
  let r285 = S (N N_direction_flag) :: r284 in
  let r286 = S (N N_seq_expr) :: r285 in
  let r287 = S (T T_EQUAL) :: r286 in
  let r288 = S (N N_pattern) :: r287 in
  let r289 = [R 544] in
  let r290 = [R 713] in
  let r291 = S (N N_val_ident) :: r290 in
  let r292 = S (T T_DOT) :: r291 in
  let r293 = [R 532] in
  let r294 = S (T T_RPAREN) :: r293 in
  let r295 = [R 553] in
  let r296 = S (T T_RPAREN) :: r295 in
  let r297 = S (N N_package_type) :: r296 in
  let r298 = S (T T_COLON) :: r297 in
  let r299 = [R 316] in
  let r300 = S (N N_module_expr) :: r299 in
  let r301 = S (T T_MINUSGREATER) :: r300 in
  let r302 = [R 223] in
  let r303 = [R 224] in
  let r304 = S (T T_RPAREN) :: r303 in
  let r305 = S (N N_module_type) :: r304 in
  let r306 = [R 329] in
  let r307 = S (T T_END) :: r306 in
  let r308 = [R 460] in
  let r309 = S (N N_post_item_attributes) :: r308 in
  let r310 = S (N N_core_type) :: r309 in
  let r311 = S (T T_COLON) :: r310 in
  let r312 = [R 462] in
  let r313 = [R 456] in
  let r314 = [R 385] in
  let r315 = S (T T_RPAREN) :: r314 in
  let r316 = [R 380] in
  let r317 = [R 386] in
  let r318 = [R 382] in
  let r319 = [R 458] in
  let r320 = [R 729] in
  let r321 = [R 682] in
  let r322 = S (N N_post_item_attributes) :: r321 in
  let r323 = R 98 :: r322 in
  let r324 = R 685 :: r323 in
  let r325 = S (T T_LIDENT) :: r324 in
  let r326 = S (N N_optional_type_parameters) :: r325 in
  let r327 = [R 684] in
  let r328 = [R 686] in
  let r329 = [R 687] in
  let r330 = [R 86] in
  let r331 = [R 87] in
  let r332 = S (T T_COLONCOLON) :: r331 in
  let r333 = [R 99] in
  let r334 = S (N N_attributes) :: r333 in
  let r335 = S (N N_generalized_constructor_arguments) :: r334 in
  let r336 = Sub (r332) :: r335 in
  let r337 = [R 101] in
  let r338 = [R 230] in
  let r339 = [R 509] in
  let r340 = S (T T_RPAREN) :: r339 in
  let r341 = S (N N_core_type_comma_list) :: r340 in
  let r342 = [R 114] in
  let r343 = S (N N_simple_core_type_no_attr) :: r342 in
  let r344 = [R 232] in
  let r345 = [R 231] in
  let r346 = S (N N_simple_core_type_no_attr) :: r345 in
  let r347 = [R 690] in
  let r348 = S (N N_constructor_declarations) :: r347 in
  let r349 = [R 691] in
  let r350 = S (T T_RBRACE) :: r349 in
  let r351 = R 376 :: r350 in
  let r352 = [R 239] in
  let r353 = S (N N_attributes) :: r352 in
  let r354 = S (N N_poly_type_no_attr) :: r353 in
  let r355 = S (T T_COLON) :: r354 in
  let r356 = [R 695] in
  let r357 = [R 692] in
  let r358 = S (N N_constructor_declarations) :: r357 in
  let r359 = [R 693] in
  let r360 = S (T T_RBRACE) :: r359 in
  let r361 = R 376 :: r360 in
  let r362 = [R 95] in
  let r363 = S (N N_core_type) :: r362 in
  let r364 = S (T T_EQUAL) :: r363 in
  let r365 = S (N N_core_type) :: r364 in
  let r366 = [R 97] in
  let r367 = [R 457] in
  let r368 = [R 212] in
  let r369 = S (N N_attributes) :: r368 in
  let r370 = S (N N_generalized_constructor_arguments) :: r369 in
  let r371 = Sub (r332) :: r370 in
  let r372 = [R 450] in
  let r373 = Sub (r371) :: r372 in
  let r374 = [R 452] in
  let r375 = S (N N_post_item_attributes) :: r374 in
  let r376 = Sub (r373) :: r375 in
  let r377 = R 372 :: r376 in
  let r378 = R 429 :: r377 in
  let r379 = [R 451] in
  let r380 = [R 454] in
  let r381 = [R 344] in
  let r382 = S (N N_post_item_attributes) :: r381 in
  let r383 = S (N N_mod_longident) :: r382 in
  let r384 = [R 468] in
  let r385 = S (N N_post_item_attributes) :: r384 in
  let r386 = S (N N_ident) :: r385 in
  let r387 = [R 465] in
  let r388 = S (N N_post_item_attributes) :: r387 in
  let r389 = [R 313] in
  let r390 = S (N N_module_declaration) :: r389 in
  let r391 = [R 312] in
  let r392 = S (N N_module_declaration) :: r391 in
  let r393 = S (T T_RPAREN) :: r392 in
  let r394 = S (N N_module_type) :: r393 in
  let r395 = [R 332] in
  let r396 = S (N N_module_expr) :: r395 in
  let r397 = S (T T_OF) :: r396 in
  let r398 = [R 318] in
  let r399 = [R 317] in
  let r400 = [R 333] in
  let r401 = S (T T_RPAREN) :: r400 in
  let r402 = [R 330] in
  let r403 = S (N N_module_type) :: r402 in
  let r404 = S (T T_MINUSGREATER) :: r403 in
  let r405 = [R 331] in
  let r406 = [R 724] in
  let r407 = S (N N_core_type_no_attr) :: r406 in
  let r408 = S (T T_COLONEQUAL) :: r407 in
  let r409 = S (N N_label) :: r408 in
  let r410 = [R 703] in
  let r411 = S (T T_RPAREN) :: r410 in
  let r412 = [R 698] in
  let r413 = [R 704] in
  let r414 = [R 700] in
  let r415 = [R 723] in
  let r416 = R 98 :: r415 in
  let r417 = S (N N_core_type_no_attr) :: r416 in
  let r418 = [R 725] in
  let r419 = S (N N_mod_ext_longident) :: r418 in
  let r420 = S (T T_EQUAL) :: r419 in
  let r421 = S (N N_mod_longident) :: r420 in
  let r422 = [R 726] in
  let r423 = S (N N_mod_ext_longident) :: r422 in
  let r424 = S (T T_MODULE) :: r421 in
  let r425 = [R 728] in
  let r426 = [R 311] in
  let r427 = [R 466] in
  let r428 = S (N N_post_item_attributes) :: r427 in
  let r429 = [R 469] in
  let r430 = S (N N_post_item_attributes) :: r429 in
  let r431 = [R 327] in
  let r432 = S (N N_post_item_attributes) :: r431 in
  let r433 = S (N N_module_type) :: r432 in
  let r434 = S (T T_COLON) :: r433 in
  let r435 = S (T T_UIDENT) :: r434 in
  let r436 = [R 438] in
  let r437 = Sub (r435) :: r436 in
  let r438 = [R 467] in
  let r439 = [R 439] in
  let r440 = [R 237] in
  let r441 = S (T T_RBRACKET) :: r440 in
  let r442 = S (N N_payload) :: r441 in
  let r443 = [R 217] in
  let r444 = S (T T_RBRACKET) :: r443 in
  let r445 = S (N N_payload) :: r444 in
  let r446 = [R 471] in
  let r447 = S (N N_post_item_attributes) :: r446 in
  let r448 = [R 461] in
  let r449 = S (N N_post_item_attributes) :: r448 in
  let r450 = S (N N_primitive_declaration) :: r449 in
  let r451 = S (T T_EQUAL) :: r450 in
  let r452 = S (N N_core_type) :: r451 in
  let r453 = S (T T_COLON) :: r452 in
  let r454 = [R 449] in
  let r455 = S (N N_post_item_attributes) :: r454 in
  let r456 = Sub (r371) :: r455 in
  let r457 = [R 464] in
  let r458 = [R 459] in
  let r459 = [R 472] in
  let r460 = [R 473] in
  let r461 = [R 56] in
  let r462 = S (N N_clty_longident) :: r461 in
  let r463 = [R 71] in
  let r464 = S (N N_post_item_attributes) :: r463 in
  let r465 = Sub (r462) :: r464 in
  let r466 = S (T T_EQUAL) :: r465 in
  let r467 = S (T T_LIDENT) :: r466 in
  let r468 = [R 75] in
  let r469 = S (T T_RBRACKET) :: r468 in
  let r470 = [R 46] in
  let r471 = R 53 :: r470 in
  let r472 = R 45 :: r471 in
  let r473 = [R 57] in
  let r474 = S (T T_END) :: r473 in
  let r475 = [R 44] in
  let r476 = S (T T_RPAREN) :: r475 in
  let r477 = [R 720] in
  let r478 = S (N N_core_type) :: r477 in
  let r479 = S (T T_COLON) :: r478 in
  let r480 = S (N N_label) :: r479 in
  let r481 = [R 48] in
  let r482 = S (N N_post_item_attributes) :: r481 in
  let r483 = [R 718] in
  let r484 = S (N N_core_type) :: r483 in
  let r485 = S (T T_COLON) :: r484 in
  let r486 = S (N N_label) :: r485 in
  let r487 = [R 719] in
  let r488 = S (N N_core_type) :: r487 in
  let r489 = S (T T_COLON) :: r488 in
  let r490 = S (N N_label) :: r489 in
  let r491 = [R 49] in
  let r492 = S (N N_post_item_attributes) :: r491 in
  let r493 = S (N N_poly_type) :: r492 in
  let r494 = S (T T_COLON) :: r493 in
  let r495 = S (N N_label) :: r494 in
  let r496 = [R 421] in
  let r497 = S (N N_core_type) :: r496 in
  let r498 = [R 47] in
  let r499 = S (N N_post_item_attributes) :: r498 in
  let r500 = [R 55] in
  let r501 = S (N N_clty_longident) :: r500 in
  let r502 = S (T T_RBRACKET) :: r501 in
  let r503 = [R 77] in
  let r504 = S (T T_LIDENT) :: r503 in
  let r505 = [R 96] in
  let r506 = S (N N_core_type) :: r505 in
  let r507 = S (T T_EQUAL) :: r506 in
  let r508 = S (N N_core_type) :: r507 in
  let r509 = [R 50] in
  let r510 = S (N N_post_item_attributes) :: r509 in
  let r511 = [R 51] in
  let r512 = [R 72] in
  let r513 = [R 66] in
  let r514 = Sub (r462) :: r513 in
  let r515 = [R 16] in
  let r516 = S (N N_post_item_attributes) :: r515 in
  let r517 = Sub (r514) :: r516 in
  let r518 = S (T T_COLON) :: r517 in
  let r519 = S (T T_LIDENT) :: r518 in
  let r520 = [R 67] in
  let r521 = Sub (r514) :: r520 in
  let r522 = S (T T_MINUSGREATER) :: r521 in
  let r523 = S (N N_simple_core_type_or_tuple_no_attr) :: r522 in
  let r524 = S (T T_COLON) :: r523 in
  let r525 = [R 68] in
  let r526 = Sub (r514) :: r525 in
  let r527 = S (T T_MINUSGREATER) :: r526 in
  let r528 = [R 69] in
  let r529 = Sub (r514) :: r528 in
  let r530 = S (T T_MINUSGREATER) :: r529 in
  let r531 = S (N N_simple_core_type_or_tuple_no_attr) :: r530 in
  let r532 = [R 70] in
  let r533 = Sub (r514) :: r532 in
  let r534 = [R 513] in
  let r535 = [R 17] in
  let r536 = [R 455] in
  let r537 = [R 474] in
  let r538 = [R 197] in
  let r539 = S (N N_simple_expr) :: r538 in
  let r540 = [R 518] in
  let r541 = S (N N_label) :: r540 in
  let r542 = [R 519] in
  let r543 = [R 190] in
  let r544 = [R 243] in
  let r545 = [R 140] in
  let r546 = [R 550] in
  let r547 = [R 531] in
  let r548 = S (N N_label_longident) :: r547 in
  let r549 = [R 533] in
  let r550 = S (T T_RPAREN) :: r549 in
  let r551 = S (N N_seq_expr) :: r550 in
  let r552 = [R 540] in
  let r553 = S (T T_BARRBRACKET) :: r552 in
  let r554 = R 376 :: r553 in
  let r555 = [R 153] in
  let r556 = S (N N_seq_expr) :: r555 in
  let r557 = S (T T_IN) :: r556 in
  let r558 = S (N N_let_bindings) :: r557 in
  let r559 = [R 278] in
  let r560 = S (N N_seq_expr) :: r559 in
  let r561 = S (T T_EQUAL) :: r560 in
  let r562 = [R 196] in
  let r563 = S (N N_expr) :: r562 in
  let r564 = [R 201] in
  let r565 = [R 180] in
  let r566 = [R 174] in
  let r567 = [R 549] in
  let r568 = [R 191] in
  let r569 = [R 177] in
  let r570 = [R 181] in
  let r571 = [R 173] in
  let r572 = [R 176] in
  let r573 = [R 175] in
  let r574 = [R 185] in
  let r575 = [R 179] in
  let r576 = [R 178] in
  let r577 = [R 183] in
  let r578 = [R 172] in
  let r579 = [R 171] in
  let r580 = [R 168] in
  let r581 = [R 170] in
  let r582 = [R 184] in
  let r583 = [R 182] in
  let r584 = [R 186] in
  let r585 = [R 187] in
  let r586 = [R 188] in
  let r587 = [R 202] in
  let r588 = [R 189] in
  let r589 = [R 277] in
  let r590 = S (N N_seq_expr) :: r589 in
  let r591 = [R 280] in
  let r592 = [R 273] in
  let r593 = [R 542] in
  let r594 = S (T T_RBRACKET) :: r593 in
  let r595 = R 376 :: r594 in
  let r596 = [R 548] in
  let r597 = S (T T_GREATERRBRACE) :: r596 in
  let r598 = R 376 :: r597 in
  let r599 = [R 216] in
  let r600 = S (N N_expr) :: r599 in
  let r601 = [R 537] in
  let r602 = S (T T_RBRACE) :: r601 in
  let r603 = [R 440] in
  let r604 = S (N N_lbl_expr_list) :: r603 in
  let r605 = [R 262] in
  let r606 = [R 534] in
  let r607 = S (T T_RBRACKET) :: r606 in
  let r608 = S (N N_seq_expr) :: r607 in
  let r609 = [R 535] in
  let r610 = S (T T_RBRACE) :: r609 in
  let r611 = S (N N_expr) :: r610 in
  let r612 = [R 139] in
  let r613 = [R 244] in
  let r614 = [R 245] in
  let r615 = [R 242] in
  let r616 = [R 193] in
  let r617 = [R 194] in
  let r618 = [R 195] in
  let r619 = [R 192] in
  let r620 = [R 520] in
  let r621 = [R 203] in
  let r622 = [R 148] in
  let r623 = S (T T_DONE) :: r622 in
  let r624 = S (N N_seq_expr) :: r623 in
  let r625 = S (T T_DO) :: r624 in
  let r626 = S (N N_seq_expr) :: r625 in
  let r627 = S (N N_direction_flag) :: r626 in
  let r628 = [R 222] in
  let r629 = Sub (r258) :: r628 in
  let r630 = S (T T_RPAREN) :: r629 in
  let r631 = [R 221] in
  let r632 = [R 157] in
  let r633 = S (N N_pattern) :: r252 in
  let r634 = [R 291] in
  let r635 = [R 164] in
  let r636 = [R 528] in
  let r637 = [R 517] in
  let r638 = [R 546] in
  let r639 = S (T T_GREATERRBRACE) :: r638 in
  let r640 = [R 154] in
  let r641 = S (N N_seq_expr) :: r640 in
  let r642 = S (T T_IN) :: r641 in
  let r643 = S (N N_module_binding_body) :: r642 in
  let r644 = S (T T_UIDENT) :: r643 in
  let r645 = [R 306] in
  let r646 = [R 307] in
  let r647 = S (N N_module_expr) :: r646 in
  let r648 = S (T T_EQUAL) :: r647 in
  let r649 = [R 308] in
  let r650 = [R 152] in
  let r651 = S (N N_seq_expr) :: r650 in
  let r652 = S (T T_IN) :: r651 in
  let r653 = S (N N_let_bindings_no_attrs) :: r652 in
  let r654 = [R 633] in
  let r655 = S (N N_fun_binding) :: r654 in
  let r656 = S (T T_RPAREN) :: r655 in
  let r657 = S (T T_LIDENT) :: r656 in
  let r658 = [R 631] in
  let r659 = S (N N_seq_expr) :: r658 in
  let r660 = [R 681] in
  let r661 = [R 679] in
  let r662 = [R 680] in
  let r663 = [R 219] in
  let r664 = S (N N_seq_expr) :: r663 in
  let r665 = [R 632] in
  let r666 = [R 276] in
  let r667 = S (N N_seq_expr) :: r666 in
  let r668 = S (T T_EQUAL) :: r667 in
  let r669 = S (N N_core_type) :: r668 in
  let r670 = S (T T_DOT) :: r669 in
  let r671 = [R 275] in
  let r672 = S (N N_seq_expr) :: r671 in
  let r673 = S (T T_EQUAL) :: r672 in
  let r674 = S (N N_core_type) :: r673 in
  let r675 = [R 324] in
  let r676 = S (T T_RPAREN) :: r675 in
  let r677 = [R 322] in
  let r678 = S (T T_RPAREN) :: r677 in
  let r679 = [R 323] in
  let r680 = S (T T_RPAREN) :: r679 in
  let r681 = [R 319] in
  let r682 = S (T T_RPAREN) :: r681 in
  let r683 = [R 552] in
  let r684 = S (T T_RPAREN) :: r683 in
  let r685 = [R 169] in
  let r686 = S (T T_RPAREN) :: r685 in
  let r687 = S (N N_expr) :: r686 in
  let r688 = S (T T_COMMA) :: r687 in
  let r689 = S (N N_expr) :: r688 in
  let r690 = S (T T_LPAREN) :: r689 in
  let r691 = [R 530] in
  let r692 = [R 717] in
  let r693 = S (N N_seq_expr) :: r692 in
  let r694 = [R 295] in
  let r695 = S (N N_poly_type) :: r694 in
  let r696 = S (T T_COLON) :: r695 in
  let r697 = S (N N_label) :: r696 in
  let r698 = S (T T_VIRTUAL) :: r697 in
  let r699 = S (T T_PRIVATE) :: r698 in
  let r700 = R 389 :: r699 in
  let r701 = [R 27] in
  let r702 = S (N N_post_item_attributes) :: r701 in
  let r703 = [R 296] in
  let r704 = S (N N_poly_type) :: r703 in
  let r705 = S (T T_COLON) :: r704 in
  let r706 = S (N N_label) :: r705 in
  let r707 = S (T T_EQUAL) :: r659 in
  let r708 = [R 297] in
  let r709 = Sub (r707) :: r708 in
  let r710 = [R 298] in
  let r711 = S (N N_seq_expr) :: r710 in
  let r712 = S (T T_EQUAL) :: r711 in
  let r713 = [R 299] in
  let r714 = S (N N_seq_expr) :: r713 in
  let r715 = S (T T_EQUAL) :: r714 in
  let r716 = S (N N_core_type) :: r715 in
  let r717 = S (T T_DOT) :: r716 in
  let r718 = [R 29] in
  let r719 = S (N N_post_item_attributes) :: r718 in
  let r720 = [R 61] in
  let r721 = S (N N_class_longident) :: r720 in
  let r722 = [R 19] in
  let r723 = Sub (r721) :: r722 in
  let r724 = [R 25] in
  let r725 = S (N N_post_item_attributes) :: r724 in
  let r726 = R 397 :: r725 in
  let r727 = Sub (r723) :: r726 in
  let r728 = [R 62] in
  let r729 = S (T T_END) :: r728 in
  let r730 = [R 64] in
  let r731 = S (T T_RPAREN) :: r730 in
  let r732 = [R 22] in
  let r733 = Sub (r723) :: r732 in
  let r734 = S (T T_IN) :: r733 in
  let r735 = S (N N_let_bindings_no_attrs) :: r734 in
  let r736 = [R 60] in
  let r737 = S (N N_class_longident) :: r736 in
  let r738 = S (T T_RBRACKET) :: r737 in
  let r739 = S (N N_simple_pattern) :: r272 in
  let r740 = [R 37] in
  let r741 = Sub (r723) :: r740 in
  let r742 = S (T T_MINUSGREATER) :: r741 in
  let r743 = Sub (r739) :: r742 in
  let r744 = [R 20] in
  let r745 = [R 63] in
  let r746 = S (T T_RPAREN) :: r745 in
  let r747 = [R 396] in
  let r748 = [R 28] in
  let r749 = S (N N_post_item_attributes) :: r748 in
  let r750 = [R 30] in
  let r751 = [R 538] in
  let r752 = S (T T_BARRBRACKET) :: r751 in
  let r753 = [R 144] in
  let r754 = S (N N_match_cases) :: r753 in
  let r755 = [R 146] in
  let r756 = [R 145] in
  let r757 = [R 652] in
  let r758 = [R 639] in
  let r759 = [R 641] in
  let r760 = [R 640] in
  let r761 = [R 626] in
  let r762 = Sub (r371) :: r761 in
  let r763 = [R 630] in
  let r764 = S (N N_post_item_attributes) :: r763 in
  let r765 = Sub (r762) :: r764 in
  let r766 = R 372 :: r765 in
  let r767 = R 429 :: r766 in
  let r768 = [R 628] in
  let r769 = [R 213] in
  let r770 = S (N N_attributes) :: r769 in
  let r771 = S (N N_constr_longident) :: r770 in
  let r772 = [R 673] in
  let r773 = [R 666] in
  let r774 = [R 644] in
  let r775 = S (N N_seq_expr) :: r774 in
  let r776 = S (T T_EQUAL) :: r775 in
  let r777 = [R 645] in
  let r778 = S (N N_module_expr) :: r777 in
  let r779 = S (T T_EQUAL) :: r778 in
  let r780 = [R 643] in
  let r781 = S (N N_seq_expr) :: r780 in
  let r782 = S (T T_EQUAL) :: r781 in
  let r783 = [R 649] in
  let r784 = S (N N_module_expr) :: r783 in
  let r785 = S (T T_EQUAL) :: r784 in
  let r786 = [R 648] in
  let r787 = S (N N_seq_expr) :: r786 in
  let r788 = S (T T_EQUAL) :: r787 in
  let r789 = S (N N_simple_expr) :: r788 in
  let r790 = S (N N_val_ident) :: r789 in
  let r791 = [R 647] in
  let r792 = S (N N_seq_expr) :: r791 in
  let r793 = S (T T_EQUAL) :: r792 in
  let r794 = [R 646] in
  let r795 = S (N N_seq_expr) :: r794 in
  let r796 = S (T T_EQUAL) :: r795 in
  let r797 = [R 655] in
  let r798 = [R 305] in
  let r799 = S (N N_post_item_attributes) :: r798 in
  let r800 = [R 657] in
  let r801 = S (N N_post_item_attributes) :: r800 in
  let r802 = [R 658] in
  let r803 = S (N N_post_item_attributes) :: r802 in
  let r804 = [R 656] in
  let r805 = [R 310] in
  let r806 = [R 638] in
  let r807 = S (N N_let_bindings) :: r806 in
  let r808 = S (N N_rec_flag) :: r807 in
  let r809 = S (N N_ext_attributes) :: r808 in
  let r810 = [R 650] in
  let r811 = S (N N_let_bindings) :: r810 in
  let r812 = S (N N_rec_flag) :: r811 in
  let r813 = S (N N_ext_attributes) :: r812 in
  let r814 = [R 662] in
  let r815 = S (N N_post_item_attributes) :: r814 in
  let r816 = [R 651] in
  let r817 = S (N N_post_item_attributes) :: r816 in
  let r818 = S (N N_primitive_declaration) :: r817 in
  let r819 = S (T T_EQUAL) :: r818 in
  let r820 = S (N N_core_type) :: r819 in
  let r821 = S (T T_COLON) :: r820 in
  let r822 = [R 624] in
  let r823 = S (N N_post_item_attributes) :: r822 in
  let r824 = Sub (r371) :: r823 in
  let r825 = [R 654] in
  let r826 = [R 642] in
  let r827 = [R 625] in
  let r828 = [R 660] in
  let r829 = [R 661] in
  let r830 = [R 34] in
  let r831 = Sub (r723) :: r830 in
  let r832 = S (T T_EQUAL) :: r831 in
  let r833 = [R 13] in
  let r834 = S (N N_post_item_attributes) :: r833 in
  let r835 = Sub (r832) :: r834 in
  let r836 = S (T T_LIDENT) :: r835 in
  let r837 = [R 35] in
  let r838 = Sub (r723) :: r837 in
  let r839 = S (T T_EQUAL) :: r838 in
  let r840 = [R 36] in
  let r841 = [R 14] in
  let r842 = [R 667] in
  let r843 = [R 663] in
  let r844 = [R 636] in
  let r845 = S (N N_structure_tail) :: r844 in
  let r846 = [R 235] in
  let r847 = [R 236] in
  let r848 = [R 398] in
  function
  | 0 | 22 | 1552 | 1556 -> Nothing
  | 21 -> One ([R 0])
  | 1551 -> One ([R 1])
  | 1555 -> One ([R 2])
  | 1559 -> One ([R 3])
  | 401 -> One ([R 4])
  | 400 -> One ([R 5])
  | 211 -> One ([R 6])
  | 76 -> One ([R 8])
  | 1530 -> One ([R 15])
  | 910 -> One ([R 18])
  | 1346 -> One ([R 21])
  | 1349 -> One ([R 23])
  | 1344 -> One ([R 24])
  | 1368 -> One ([R 31])
  | 1369 -> One ([R 33])
  | 1350 -> One ([R 38])
  | 142 -> One ([R 39])
  | 865 -> One ([R 52])
  | 866 -> One ([R 54])
  | 856 -> One ([R 58])
  | 852 -> One ([R 59])
  | 872 -> One ([R 73])
  | 844 -> One ([R 76])
  | 306 -> One ([R 78])
  | 316 -> One ([R 79])
  | 86 -> One ([R 80])
  | 313 -> One ([R 81])
  | 305 -> One ([R 82])
  | 304 -> One ([R 83])
  | 105 -> One ([R 84])
  | 582 | 592 -> One ([R 85])
  | 587 -> One ([R 88])
  | 583 -> One ([R 89])
  | 332 -> One ([R 90])
  | 314 -> One ([R 93])
  | 85 -> One ([R 94])
  | 617 -> One ([R 100])
  | 241 -> One ([R 102])
  | 243 -> One ([R 103])
  | 203 -> One ([R 104])
  | 271 -> One ([R 109])
  | 206 -> One ([R 111])
  | 600 -> One ([R 113])
  | 212 -> One ([R 115])
  | 1127 -> One ([R 117])
  | 1128 -> One ([R 118])
  | 3 -> One ([R 119])
  | 19 -> One ([R 120])
  | 12 -> One ([R 121])
  | 20 -> One ([R 122])
  | 16 -> One ([R 123])
  | 14 -> One ([R 124])
  | 2 -> One ([R 125])
  | 13 -> One ([R 126])
  | 18 -> One ([R 127])
  | 17 -> One ([R 128])
  | 1 -> One ([R 129])
  | 15 -> One ([R 130])
  | 4 -> One ([R 131])
  | 7 -> One ([R 132])
  | 5 -> One ([R 133])
  | 6 -> One ([R 134])
  | 11 -> One ([R 135])
  | 10 -> One ([R 136])
  | 9 -> One ([R 137])
  | 8 -> One ([R 138])
  | 946 -> One ([R 150])
  | 1113 -> One ([R 151])
  | 973 -> One ([R 161])
  | 982 -> One ([R 162])
  | 953 -> One ([R 163])
  | 980 -> One ([R 200])
  | 1123 -> One ([R 204])
  | 1047 -> One ([R 206])
  | 1044 -> One ([R 207])
  | 1231 -> One ([R 218])
  | 546 -> One ([R 225])
  | 545 -> One ([R 226])
  | 738 -> One ([R 227])
  | 739 -> One ([R 228])
  | 148 | 160 -> One ([R 233])
  | 127 -> One ([R 234])
  | 166 | 714 -> One ([R 238])
  | 631 -> One ([R 240])
  | 628 -> One ([R 241])
  | 948 -> One ([R 246])
  | 464 -> One ([R 247])
  | 133 -> One ([R 249])
  | 463 -> One ([R 251])
  | 1114 -> One ([R 252])
  | 1116 -> One ([R 253])
  | 1067 -> One ([R 263])
  | 1064 -> One ([R 264])
  | 1066 -> One ([R 265])
  | 1065 -> One ([R 266])
  | 300 -> One ([R 268])
  | 294 -> One ([R 269])
  | 295 -> One ([R 270])
  | 299 -> One ([R 272])
  | 1041 -> One ([R 279])
  | 1211 -> One ([R 281])
  | 474 -> One ([R 282])
  | 962 -> One ([R 283])
  | 509 -> One ([R 284])
  | 1237 -> One ([R 286])
  | 1238 -> One ([R 287])
  | 1164 -> One ([R 290])
  | 259 -> One ([R 292])
  | 168 -> One ([R 294])
  | 88 | 126 -> One ([R 300])
  | 84 -> One ([R 303])
  | 1483 -> One ([R 309])
  | 693 -> One ([R 314])
  | 692 -> One ([R 325])
  | 694 -> One ([R 326])
  | 699 -> One ([R 328])
  | 737 -> One ([R 334])
  | 736 -> One ([R 335])
  | 162 -> One ([R 336])
  | 415 -> One (R 338 :: r177)
  | 814 -> One (R 338 :: r486)
  | 416 | 432 -> One ([R 339])
  | 240 -> One ([R 341])
  | 239 -> One ([R 342])
  | 339 | 932 -> One ([R 345])
  | 116 | 398 -> One ([R 346])
  | 311 -> One ([R 347])
  | 310 -> One ([R 348])
  | 309 -> One ([R 349])
  | 308 -> One ([R 350])
  | 307 -> One ([R 351])
  | 286 | 931 -> One ([R 352])
  | 114 -> One ([R 353])
  | 342 | 937 -> One ([R 354])
  | 119 | 356 | 448 -> One ([R 355])
  | 118 | 447 -> One ([R 356])
  | 284 | 357 | 930 -> One ([R 357])
  | 283 | 929 -> One ([R 358])
  | 113 -> One ([R 359])
  | 338 -> One ([R 360])
  | 287 -> One ([R 361])
  | 312 -> One ([R 362])
  | 121 -> One ([R 363])
  | 341 -> One ([R 364])
  | 343 -> One ([R 365])
  | 344 -> One ([R 366])
  | 340 -> One ([R 367])
  | 117 -> One ([R 368])
  | 120 -> One ([R 369])
  | 201 -> One ([R 370])
  | 200 -> One (R 371 :: r86)
  | 178 -> One (R 372 :: r71)
  | 638 -> One (R 372 :: r358)
  | 1380 -> One (R 372 :: r754)
  | 179 -> One ([R 373])
  | 257 -> One (R 376 :: r103)
  | 296 -> One (R 376 :: r114)
  | 372 -> One (R 376 :: r129)
  | 1185 -> One (R 376 :: r639)
  | 1371 -> One (R 376 :: r752)
  | 258 | 297 | 366 | 627 | 1043 | 1054 -> One ([R 377])
  | 1438 -> One (R 378 :: r776)
  | 1443 -> One (R 378 :: r779)
  | 1447 -> One (R 378 :: r782)
  | 1439 -> One ([R 379])
  | 570 -> One ([R 381])
  | 654 -> One ([R 384])
  | 562 -> One ([R 387])
  | 421 -> One ([R 388])
  | 675 -> One (R 389 :: r383)
  | 1327 -> One (R 389 :: r727)
  | 422 -> One ([R 390])
  | 130 -> One ([R 391])
  | 280 -> One ([R 394])
  | 319 -> One ([R 399])
  | 324 -> One ([R 401])
  | 329 -> One ([R 409])
  | 370 -> One ([R 412])
  | 367 -> One ([R 413])
  | 515 -> One ([R 414])
  | 246 -> One ([R 416])
  | 842 -> One ([R 420])
  | 256 -> One ([R 422])
  | 785 -> One ([R 427])
  | 786 -> One ([R 428])
  | 1299 -> One (R 429 :: r706)
  | 636 -> One ([R 430])
  | 829 -> One (R 431 :: r495)
  | 832 -> One ([R 432])
  | 830 -> One ([R 433])
  | 833 -> One ([R 434])
  | 831 -> One ([R 435])
  | 458 -> One ([R 437])
  | 1076 -> One ([R 441])
  | 189 -> One ([R 442])
  | 225 -> One ([R 444])
  | 1028 -> One ([R 446])
  | 1029 -> One ([R 447])
  | 1030 -> One ([R 448])
  | 659 -> One ([R 463])
  | 913 -> One ([R 470])
  | 916 -> One ([R 475])
  | 323 -> One ([R 476])
  | 111 -> One ([R 478])
  | 109 -> One ([R 479])
  | 108 -> One ([R 480])
  | 107 -> One ([R 481])
  | 104 -> One ([R 483])
  | 102 -> One ([R 484])
  | 101 -> One ([R 485])
  | 100 -> One ([R 486])
  | 190 -> One ([R 487])
  | 140 -> One ([R 490])
  | 188 -> One ([R 491])
  | 193 -> One ([R 492])
  | 268 -> One ([R 493])
  | 207 | 899 -> One ([R 507])
  | 601 | 614 -> One ([R 508])
  | 204 -> One ([R 510])
  | 893 -> One ([R 512])
  | 391 -> One ([R 515])
  | 535 -> One ([R 521])
  | 936 -> One ([R 522])
  | 935 | 981 -> One ([R 523])
  | 538 | 952 -> One ([R 524])
  | 934 -> One ([R 554])
  | 1117 -> One ([R 555])
  | 1115 -> One ([R 556])
  | 317 -> One ([R 557])
  | 318 | 966 -> One ([R 558])
  | 83 -> One ([R 559])
  | 320 -> One ([R 560])
  | 363 -> One ([R 562])
  | 362 -> One ([R 563])
  | 334 -> One ([R 573])
  | 50 -> One ([R 574])
  | 31 -> One ([R 575])
  | 75 -> One ([R 576])
  | 74 -> One ([R 577])
  | 73 -> One ([R 578])
  | 72 -> One ([R 579])
  | 71 -> One ([R 580])
  | 70 -> One ([R 581])
  | 69 -> One ([R 582])
  | 68 -> One ([R 583])
  | 67 -> One ([R 584])
  | 66 -> One ([R 585])
  | 65 -> One ([R 586])
  | 64 -> One ([R 587])
  | 63 -> One ([R 588])
  | 62 -> One ([R 589])
  | 61 -> One ([R 590])
  | 60 -> One ([R 591])
  | 59 -> One ([R 592])
  | 58 -> One ([R 593])
  | 57 -> One ([R 594])
  | 56 -> One ([R 595])
  | 55 -> One ([R 596])
  | 54 -> One ([R 597])
  | 53 -> One ([R 598])
  | 52 -> One ([R 599])
  | 51 -> One ([R 600])
  | 49 -> One ([R 601])
  | 48 -> One ([R 602])
  | 47 -> One ([R 603])
  | 46 -> One ([R 604])
  | 45 -> One ([R 605])
  | 44 -> One ([R 606])
  | 43 -> One ([R 607])
  | 42 -> One ([R 608])
  | 41 -> One ([R 609])
  | 40 -> One ([R 610])
  | 39 -> One ([R 611])
  | 38 -> One ([R 612])
  | 37 -> One ([R 613])
  | 36 -> One ([R 614])
  | 35 -> One ([R 615])
  | 34 -> One ([R 616])
  | 33 -> One ([R 617])
  | 32 -> One ([R 618])
  | 30 -> One ([R 619])
  | 29 -> One ([R 620])
  | 28 -> One ([R 621])
  | 27 -> One ([R 622])
  | 26 -> One ([R 623])
  | 1425 -> One ([R 627])
  | 1416 -> One ([R 629])
  | 244 -> One ([R 634])
  | 245 -> One ([R 635])
  | 1408 -> One ([R 653])
  | 1542 -> One ([R 659])
  | 1545 -> One ([R 664])
  | 441 -> One ([R 668])
  | 440 -> One ([R 669])
  | 1430 -> One ([R 674])
  | 1431 -> One ([R 675])
  | 1433 -> One ([R 676])
  | 1432 -> One ([R 677])
  | 1429 -> One ([R 678])
  | 655 -> One ([R 683])
  | 645 -> One ([R 688])
  | 590 -> One ([R 689])
  | 618 -> One ([R 694])
  | 89 | 163 -> One ([R 696])
  | 712 -> One ([R 699])
  | 724 -> One ([R 702])
  | 557 -> One ([R 706])
  | 559 -> One ([R 707])
  | 285 | 488 | 1074 -> One ([R 710])
  | 536 -> One ([R 712])
  | 819 -> One (R 721 :: r490)
  | 796 -> One ([R 722])
  | 735 -> One ([R 727])
  | 573 -> One ([R 730])
  | 716 -> One ([R 731])
  | 717 -> One ([R 732])
  | 1061 -> One (S (T T_WITH) :: r604)
  | 95 -> One (S (T T_UIDENT) :: r19)
  | 122 -> One (S (T T_UIDENT) :: r28)
  | 333 -> One (S (T T_UIDENT) :: r37)
  | 679 -> One (S (T T_TYPE) :: r386)
  | 684 -> One (S (T T_TYPE) :: r397)
  | 1216 -> One (S (T T_TYPE) :: r657)
  | 1451 -> One (S (T T_STRING) :: r785)
  | 1455 -> One (S (T T_STRING) :: r790)
  | 1461 -> One (S (T T_STRING) :: r793)
  | 1465 -> One (S (T T_STRING) :: r796)
  | 603 -> One (S (T T_STAR) :: r343)
  | 1420 -> One (S (T T_RPAREN) :: r26)
  | 375 -> One (S (T T_RPAREN) :: r135)
  | 544 -> One (S (T T_RPAREN) :: r302)
  | 585 | 593 -> One (S (T T_RPAREN) :: r330)
  | 681 -> One (S (T T_RPAREN) :: r390)
  | 688 -> One (S (T T_RPAREN) :: r398)
  | 690 -> One (S (T T_RPAREN) :: r399)
  | 943 -> One (S (T T_RPAREN) :: r542)
  | 1119 -> One (S (T T_RPAREN) :: r620)
  | 1180 -> One (S (T T_RPAREN) :: r637)
  | 1274 -> One (S (T T_RPAREN) :: r690)
  | 1283 -> One (S (T T_RPAREN) :: r691)
  | 181 -> One (S (T T_RBRACKET) :: r72)
  | 226 -> One (S (T T_RBRACKET) :: r94)
  | 1421 -> One (S (T T_RBRACKET) :: r111)
  | 215 -> One (S (T T_QUOTE) :: r91)
  | 662 -> One (S (T T_PLUSEQ) :: r378)
  | 1410 -> One (S (T T_PLUSEQ) :: r767)
  | 475 -> One (S (T T_OPEN) :: r221)
  | 608 -> One (S (T T_MINUSGREATER) :: r346)
  | 891 -> One (S (T T_MINUSGREATER) :: r533)
  | 150 -> One (S (T T_LIDENT) :: r46)
  | 518 -> One (S (T T_LIDENT) :: r270)
  | 877 -> One (S (T T_LIDENT) :: r524)
  | 1147 -> One (S (T T_LIDENT) :: r630)
  | 1359 -> One (S (T T_LIDENT) :: r747)
  | 971 -> One (S (T T_LESSMINUS) :: r563)
  | 99 -> One (S (T T_INT) :: r20)
  | 106 -> One (S (T T_INT) :: r21)
  | 483 -> One (S (T T_GREATERRBRACE) :: r227)
  | 165 -> One (S (T T_GREATER) :: r59)
  | 169 -> One (S (T T_GREATER) :: r60)
  | 485 -> One (S (T T_EQUAL) :: r229)
  | 1031 -> One (S (T T_EQUAL) :: r590)
  | 1055 -> One (S (T T_EQUAL) :: r600)
  | 1228 -> One (S (T T_EQUAL) :: r664)
  | 1294 -> One (S (T T_EQUAL) :: r693)
  | 1549 -> One (S (T T_EOF) :: r846)
  | 1553 -> One (S (T T_EOF) :: r847)
  | 1557 -> One (S (T T_EOF) :: r848)
  | 1171 -> One (S (T T_END) :: r636)
  | 635 -> One (S (T T_DOTDOT) :: r356)
  | 134 -> One (S (T T_DOT) :: r36)
  | 143 -> One (S (T T_DOT) :: r40)
  | 158 -> One (S (T T_DOT) :: r55)
  | 249 -> One (S (T T_DOT) :: r101)
  | 837 -> One (S (T T_DOT) :: r497)
  | 848 -> One (S (T T_DOT) :: r504)
  | 1244 -> One (S (T T_DOT) :: r674)
  | 726 -> One (S (T T_COLONEQUAL) :: r423)
  | 171 -> One (S (T T_COLON) :: r63)
  | 548 -> One (S (T T_COLON) :: r305)
  | 682 -> One (S (T T_COLON) :: r394)
  | 885 -> One (S (T T_COLON) :: r531)
  | 288 -> One (S (T T_BARRBRACKET) :: r107)
  | 399 -> One (S (T T_BARRBRACKET) :: r155)
  | 184 | 889 -> One (S (T T_BAR) :: r77)
  | 228 -> One (S (T T_BAR) :: r97)
  | 619 -> One (S (T T_BAR) :: r348)
  | 715 -> One (S (N N_with_type_binder) :: r417)
  | 572 -> One (S (N N_with_extensions) :: r319)
  | 574 -> One (S (N N_with_extensions) :: r320)
  | 657 -> One (S (N N_with_extensions) :: r367)
  | 791 -> One (S (N N_with_extensions) :: r458)
  | 1403 -> One (S (N N_with_extensions) :: r759)
  | 1406 -> One (S (N N_with_extensions) :: r760)
  | 1505 -> One (S (N N_with_extensions) :: r826)
  | 701 -> One (S (N N_with_constraints) :: r405)
  | 354 -> One (S (N N_val_ident) :: r127)
  | 551 -> One (S (N N_val_ident) :: r311)
  | 780 -> One (S (N N_val_ident) :: r453)
  | 1496 -> One (S (N N_val_ident) :: r821)
  | 704 -> One (S (N N_type_variable) :: r412)
  | 702 -> One (S (N N_type_parameters) :: r409)
  | 703 -> One (S (N N_type_parameter_list) :: r411)
  | 799 -> One (S (N N_type_parameter_list) :: r469)
  | 710 -> One (S (N N_type_parameter) :: r414)
  | 87 -> One (S (N N_type_longident) :: r13)
  | 556 -> One (S (N N_type_declarations) :: r312)
  | 558 -> One (S (N N_type_declarations) :: r313)
  | 1400 -> One (S (N N_type_declarations) :: r757)
  | 1401 -> One (S (N N_type_declarations) :: r758)
  | 23 -> One (S (N N_structure_tail) :: r0)
  | 1532 -> One (S (N N_structure_tail) :: r842)
  | 1436 -> One (S (N N_structure_head) :: r773)
  | 450 -> One (S (N N_structure) :: r198)
  | 302 -> One (S (N N_simple_pattern) :: r116)
  | 521 -> One (S (N N_simple_pattern) :: r271)
  | 390 -> One (S (N N_simple_expr) :: r149)
  | 489 -> One (S (N N_simple_expr) :: r234)
  | 490 -> One (S (N N_simple_expr) :: r235)
  | 494 -> One (S (N N_simple_expr) :: r240)
  | 534 -> One (S (N N_simple_expr) :: r289)
  | 954 -> One (S (N N_simple_expr) :: r546)
  | 1090 -> One (S (N N_simple_expr) :: r614)
  | 1092 -> One (S (N N_simple_expr) :: r615)
  | 882 -> One (S (N N_simple_core_type_or_tuple_no_attr) :: r527)
  | 606 -> One (S (N N_simple_core_type_no_attr) :: r344)
  | 209 -> One (S (N N_simple_core_type) :: r88)
  | 321 -> One (S (N N_signed_constant) :: r118)
  | 550 -> One (S (N N_signature) :: r307)
  | 674 -> One (S (N N_signature) :: r380)
  | 911 -> One (S (N N_signature) :: r536)
  | 383 -> One (S (N N_seq_expr) :: r136)
  | 397 -> One (S (N N_seq_expr) :: r154)
  | 473 -> One (S (N N_seq_expr) :: r217)
  | 503 -> One (S (N N_seq_expr) :: r255)
  | 541 -> One (S (N N_seq_expr) :: r294)
  | 1138 -> One (S (N N_seq_expr) :: r627)
  | 1324 -> One (S (N N_seq_expr) :: r719)
  | 1383 -> One (S (N N_seq_expr) :: r755)
  | 1385 -> One (S (N N_seq_expr) :: r756)
  | 183 -> One (S (N N_row_field_list) :: r74)
  | 487 -> One (S (N N_record_expr) :: r231)
  | 1060 -> One (S (N N_record_expr) :: r602)
  | 1206 -> One (S (N N_rec_flag) :: r653)
  | 1333 -> One (S (N N_rec_flag) :: r735)
  | 429 -> One (S (N N_post_item_attributes) :: r181)
  | 863 -> One (S (N N_post_item_attributes) :: r511)
  | 914 -> One (S (N N_post_item_attributes) :: r537)
  | 1038 -> One (S (N N_post_item_attributes) :: r592)
  | 1366 -> One (S (N N_post_item_attributes) :: r750)
  | 1507 -> One (S (N N_post_item_attributes) :: r827)
  | 1543 -> One (S (N N_post_item_attributes) :: r843)
  | 1546 -> One (S (N N_post_item_attributes) :: r845)
  | 1311 -> One (S (N N_poly_type) :: r712)
  | 289 -> One (S (N N_pattern_semi_list) :: r110)
  | 82 -> One (S (N N_pattern) :: r12)
  | 303 | 517 | 1146 -> One (S (N N_pattern) :: r25)
  | 301 -> One (S (N N_pattern) :: r115)
  | 315 -> One (S (N N_pattern) :: r117)
  | 325 -> One (S (N N_pattern) :: r119)
  | 327 -> One (S (N N_pattern) :: r120)
  | 330 -> One (S (N N_pattern) :: r121)
  | 335 -> One (S (N N_pattern) :: r122)
  | 347 -> One (S (N N_pattern) :: r123)
  | 352 -> One (S (N N_pattern) :: r126)
  | 404 -> One (S (N N_pattern) :: r163)
  | 131 -> One (S (N N_package_type_cstrs) :: r31)
  | 281 -> One (S (N N_package_type_cstrs) :: r106)
  | 125 -> One (S (N N_package_type) :: r30)
  | 155 -> One (S (N N_package_type) :: r53)
  | 1255 -> One (S (N N_package_type) :: r676)
  | 1258 -> One (S (N N_package_type) :: r678)
  | 1261 -> One (S (N N_package_type) :: r680)
  | 1271 -> One (S (N N_package_type) :: r684)
  | 561 -> One (S (N N_optional_type_variable) :: r316)
  | 560 -> One (S (N N_optional_type_parameter_list) :: r315)
  | 568 -> One (S (N N_optional_type_parameter) :: r318)
  | 355 -> One (S (N N_operator) :: r23)
  | 236 -> One (S (N N_name_tag_list) :: r99)
  | 695 -> One (S (N N_module_type) :: r401)
  | 744 -> One (S (N N_module_type) :: r426)
  | 756 -> One (S (N N_module_type) :: r430)
  | 777 -> One (S (N N_module_type) :: r447)
  | 1197 -> One (S (N N_module_type) :: r648)
  | 1266 -> One (S (N N_module_type) :: r682)
  | 1475 -> One (S (N N_module_type) :: r803)
  | 449 -> One (S (N N_module_expr) :: r196)
  | 453 -> One (S (N N_module_expr) :: r200)
  | 542 -> One (S (N N_module_expr) :: r298)
  | 1195 -> One (S (N N_module_expr) :: r645)
  | 1493 -> One (S (N N_module_expr) :: r815)
  | 680 -> One (S (N N_module_declaration) :: r388)
  | 1479 -> One (S (N N_module_bindings) :: r804)
  | 1204 -> One (S (N N_module_binding_body) :: r649)
  | 1470 -> One (S (N N_module_binding_body) :: r799)
  | 1469 -> One (S (N N_module_binding) :: r797)
  | 1481 -> One (S (N N_module_binding) :: r805)
  | 749 -> One (S (N N_mod_longident) :: r428)
  | 92 -> One (S (N N_mod_ext_longident) :: r18)
  | 1236 -> One (S (N N_lident_list) :: r670)
  | 1312 -> One (S (N N_lident_list) :: r717)
  | 508 -> One (S (N N_let_pattern) :: r267)
  | 1037 -> One (S (N N_let_binding) :: r591)
  | 291 -> One (S (N N_lbl_pattern_list) :: r113)
  | 461 -> One (S (N N_label_var) :: r209)
  | 470 -> One (S (N N_label_var) :: r213)
  | 132 -> One (S (N N_label_longident) :: r34)
  | 462 -> One (S (N N_label_let_pattern) :: r211)
  | 471 -> One (S (N N_label_let_pattern) :: r216)
  | 947 -> One (S (N N_label_ident) :: r544)
  | 1088 -> One (S (N N_label_ident) :: r613)
  | 620 -> One (S (N N_label_declarations) :: r351)
  | 639 -> One (S (N N_label_declarations) :: r361)
  | 433 -> One (S (N N_label) :: r184)
  | 621 -> One (S (N N_label) :: r355)
  | 983 -> One (S (N N_label) :: r567)
  | 1309 -> One (S (N N_label) :: r709)
  | 147 -> One (S (N N_ident) :: r41)
  | 173 -> One (S (N N_ident) :: r64)
  | 186 -> One (S (N N_ident) :: r78)
  | 250 -> One (S (N N_ident) :: r102)
  | 563 -> One (S (N N_ident) :: r317)
  | 705 -> One (S (N N_ident) :: r413)
  | 1427 -> One (S (N N_ident) :: r772)
  | 1473 -> One (S (N N_ident) :: r801)
  | 543 -> One (S (N N_functor_args) :: r301)
  | 696 -> One (S (N N_functor_args) :: r404)
  | 460 -> One (S (N N_fun_binding) :: r208)
  | 1232 -> One (S (N N_fun_binding) :: r665)
  | 1052 -> One (S (N N_field_expr_list) :: r598)
  | 24 -> One (S (N N_ext_attributes) :: r5)
  | 384 -> One (S (N N_ext_attributes) :: r141)
  | 386 -> One (S (N N_ext_attributes) :: r143)
  | 388 -> One (S (N N_ext_attributes) :: r148)
  | 392 -> One (S (N N_ext_attributes) :: r151)
  | 402 -> One (S (N N_ext_attributes) :: r161)
  | 442 -> One (S (N N_ext_attributes) :: r189)
  | 444 -> One (S (N N_ext_attributes) :: r194)
  | 456 -> One (S (N N_ext_attributes) :: r207)
  | 477 -> One (S (N N_ext_attributes) :: r223)
  | 492 -> One (S (N N_ext_attributes) :: r239)
  | 495 -> One (S (N N_ext_attributes) :: r242)
  | 497 -> One (S (N N_ext_attributes) :: r246)
  | 499 -> One (S (N N_ext_attributes) :: r249)
  | 504 -> One (S (N N_ext_attributes) :: r261)
  | 526 -> One (S (N N_ext_attributes) :: r279)
  | 530 -> One (S (N N_ext_attributes) :: r288)
  | 938 -> One (S (N N_ext_attributes) :: r539)
  | 964 -> One (S (N N_ext_attributes) :: r558)
  | 1192 -> One (S (N N_ext_attributes) :: r644)
  | 963 -> One (S (N N_expr_semi_list) :: r554)
  | 1048 -> One (S (N N_expr_semi_list) :: r595)
  | 454 -> One (S (N N_expr) :: r202)
  | 491 -> One (S (N N_expr) :: r237)
  | 945 -> One (S (N N_expr) :: r543)
  | 951 -> One (S (N N_expr) :: r545)
  | 974 -> One (S (N N_expr) :: r564)
  | 976 -> One (S (N N_expr) :: r565)
  | 978 -> One (S (N N_expr) :: r566)
  | 985 -> One (S (N N_expr) :: r568)
  | 987 -> One (S (N N_expr) :: r569)
  | 989 -> One (S (N N_expr) :: r570)
  | 991 -> One (S (N N_expr) :: r571)
  | 993 -> One (S (N N_expr) :: r572)
  | 995 -> One (S (N N_expr) :: r573)
  | 997 -> One (S (N N_expr) :: r574)
  | 999 -> One (S (N N_expr) :: r575)
  | 1001 -> One (S (N N_expr) :: r576)
  | 1003 -> One (S (N N_expr) :: r577)
  | 1005 -> One (S (N N_expr) :: r578)
  | 1007 -> One (S (N N_expr) :: r579)
  | 1009 -> One (S (N N_expr) :: r580)
  | 1011 -> One (S (N N_expr) :: r581)
  | 1013 -> One (S (N N_expr) :: r582)
  | 1015 -> One (S (N N_expr) :: r583)
  | 1017 -> One (S (N N_expr) :: r584)
  | 1019 -> One (S (N N_expr) :: r585)
  | 1021 -> One (S (N N_expr) :: r586)
  | 1024 -> One (S (N N_expr) :: r587)
  | 1026 -> One (S (N N_expr) :: r588)
  | 1068 -> One (S (N N_expr) :: r605)
  | 1086 -> One (S (N N_expr) :: r612)
  | 1098 -> One (S (N N_expr) :: r616)
  | 1103 -> One (S (N N_expr) :: r617)
  | 1108 -> One (S (N N_expr) :: r618)
  | 1111 -> One (S (N N_expr) :: r619)
  | 1121 -> One (S (N N_expr) :: r621)
  | 1168 -> One (S (N N_expr) :: r635)
  | 219 -> One (S (N N_core_type_no_attr) :: r92)
  | 596 -> One (S (N N_core_type_list_no_attr) :: r338)
  | 894 -> One (S (N N_core_type_list_no_attr) :: r534)
  | 205 -> One (S (N N_core_type_list) :: r87)
  | 154 -> One (S (N N_core_type_comma_list) :: r51)
  | 845 -> One (S (N N_core_type_comma_list) :: r502)
  | 1337 -> One (S (N N_core_type_comma_list) :: r738)
  | 153 -> One (S (N N_core_type2) :: r49)
  | 164 -> One (S (N N_core_type2) :: r58)
  | 213 -> One (S (N N_core_type2) :: r89)
  | 177 -> One (S (N N_core_type) :: r68)
  | 269 -> One (S (N N_core_type) :: r105)
  | 349 -> One (S (N N_core_type) :: r125)
  | 407 -> One (S (N N_core_type) :: r165)
  | 465 -> One (S (N N_core_type) :: r212)
  | 510 -> One (S (N N_core_type) :: r268)
  | 581 -> One (S (N N_core_type) :: r328)
  | 584 -> One (S (N N_core_type) :: r329)
  | 806 -> One (S (N N_core_type) :: r476)
  | 967 -> One (S (N N_core_type) :: r561)
  | 1222 -> One (S (N N_core_type) :: r660)
  | 1224 | 1235 -> One (S (N N_core_type) :: r661)
  | 1226 -> One (S (N N_core_type) :: r662)
  | 798 -> One (S (N N_class_type_parameters) :: r467)
  | 873 -> One (S (N N_class_type_parameters) :: r519)
  | 1514 -> One (S (N N_class_type_parameters) :: r836)
  | 797 -> One (S (N N_class_type_declarations) :: r460)
  | 1512 -> One (S (N N_class_type_declarations) :: r829)
  | 870 -> One (S (N N_class_type_declaration) :: r512)
  | 141 -> One (S (N N_class_longident) :: r38)
  | 191 -> One (S (N N_class_longident) :: r79)
  | 266 -> One (S (N N_class_longident) :: r104)
  | 795 -> One (S (N N_class_descriptions) :: r459)
  | 908 -> One (S (N N_class_description) :: r535)
  | 1511 -> One (S (N N_class_declarations) :: r828)
  | 1528 -> One (S (N N_class_declaration) :: r841)
  | 199 -> One (S (N N_attributes) :: r83)
  | 222 -> One (S (N N_attributes) :: r93)
  | 395 -> One (S (N N_attributes) :: r152)
  | 25 -> One (S (N N_attr_id) :: r7)
  | 77 -> One (S (N N_attr_id) :: r8)
  | 80 -> One (S (N N_attr_id) :: r11)
  | 175 -> One (S (N N_attr_id) :: r67)
  | 424 -> One (S (N N_attr_id) :: r180)
  | 769 -> One (S (N N_attr_id) :: r442)
  | 773 -> One (S (N N_attr_id) :: r445)
  | 197 -> One (Sub (r81) :: r82)
  | 1329 -> One (Sub (r158) :: r729)
  | 414 -> One (Sub (r171) :: r173)
  | 1150 -> One (Sub (r258) :: r631)
  | 1154 -> One (Sub (r258) :: r632)
  | 506 -> One (Sub (r263) :: r264)
  | 577 -> One (Sub (r326) :: r327)
  | 591 -> One (Sub (r336) :: r337)
  | 648 -> One (Sub (r365) :: r366)
  | 667 -> One (Sub (r371) :: r379)
  | 1415 -> One (Sub (r371) :: r768)
  | 733 -> One (Sub (r424) :: r425)
  | 766 -> One (Sub (r435) :: r439)
  | 760 -> One (Sub (r437) :: r438)
  | 789 -> One (Sub (r456) :: r457)
  | 843 -> One (Sub (r462) :: r499)
  | 805 -> One (Sub (r472) :: r474)
  | 813 -> One (Sub (r480) :: r482)
  | 857 -> One (Sub (r508) :: r510)
  | 1363 -> One (Sub (r508) :: r749)
  | 1355 -> One (Sub (r514) :: r746)
  | 1519 -> One (Sub (r514) :: r839)
  | 1162 -> One (Sub (r633) :: r634)
  | 1297 -> One (Sub (r700) :: r702)
  | 1332 -> One (Sub (r723) :: r731)
  | 1341 -> One (Sub (r743) :: r744)
  | 1503 -> One (Sub (r824) :: r825)
  | 1523 -> One (Sub (r832) :: r840)
  | 1531 -> One (r0)
  | 1399 -> One (r1)
  | 1398 -> One (r2)
  | 1397 -> One (r3)
  | 1396 -> One (r4)
  | 1395 -> One (r5)
  | 1394 -> One (r6)
  | 79 -> One (r7)
  | 78 -> One (r8)
  | 1393 -> One (r9)
  | 1392 -> One (r10)
  | 81 -> One (r11)
  | 382 -> One (r12)
  | 90 -> One (r13)
  | 98 | 898 -> One (r14)
  | 97 | 897 -> One (r15)
  | 91 | 896 -> One (r16)
  | 94 -> One (r17)
  | 93 -> One (r18)
  | 96 -> One (r19)
  | 103 -> One (r20)
  | 110 -> One (r21)
  | 359 -> One (r22)
  | 358 -> One (r23)
  | 346 -> One (r24)
  | 345 -> One (r25)
  | 115 -> One (r26)
  | 124 -> One (r27)
  | 123 -> One (r28)
  | 129 -> One (r29)
  | 128 -> One (r30)
  | 279 -> One (r31)
  | 278 -> One (r32)
  | 139 -> One (r33)
  | 138 -> One (r34)
  | 137 -> One (r35)
  | 135 -> One (r36)
  | 136 -> One (r37)
  | 146 -> One (r38)
  | 145 -> One (r39)
  | 144 -> One (r40)
  | 149 -> One (r41)
  | 277 -> One (r42)
  | 276 -> One (r43)
  | 275 -> One (r44)
  | 152 -> One (r45)
  | 151 -> One (r46)
  | 274 -> One (r47)
  | 273 -> One (r48)
  | 272 -> One (r49)
  | 265 -> One (r50)
  | 264 -> One (r51)
  | 157 -> One (r52)
  | 156 -> One (r53)
  | 161 -> One (r54)
  | 159 -> One (r55)
  | 263 -> One (r56)
  | 262 -> One (r57)
  | 261 -> One (r58)
  | 167 -> One (r59)
  | 170 -> One (r60)
  | 255 -> One (r61)
  | 254 -> One (r62)
  | 172 -> One (r63)
  | 174 -> One (r64)
  | 248 -> One (r65)
  | 247 -> One (r66)
  | 176 -> One (r67)
  | 242 -> One (r68)
  | 235 -> One (r69)
  | 234 -> One (r70)
  | 180 -> One (r71)
  | 182 -> One (r72)
  | 233 -> One (r73)
  | 232 -> One (r74)
  | 196 -> One (r75)
  | 195 -> One (r76)
  | 185 -> One (r77)
  | 187 -> One (r78)
  | 192 -> One (r79)
  | 194 | 890 -> One (r80)
  | 198 -> One (r82)
  | 224 -> One (r83)
  | 221 -> One (r84)
  | 218 -> One (r85)
  | 202 -> One (r86)
  | 208 -> One (r87)
  | 210 -> One (r88)
  | 214 -> One (r89)
  | 217 -> One (r90)
  | 216 -> One (r91)
  | 220 -> One (r92)
  | 223 -> One (r93)
  | 227 -> One (r94)
  | 231 -> One (r95)
  | 230 -> One (r96)
  | 229 -> One (r97)
  | 238 -> One (r98)
  | 237 -> One (r99)
  | 253 -> One (r100)
  | 252 -> One (r101)
  | 251 -> One (r102)
  | 260 -> One (r103)
  | 267 -> One (r104)
  | 270 -> One (r105)
  | 282 -> One (r106)
  | 371 -> One (r107)
  | 369 -> One (r108)
  | 368 -> One (r109)
  | 365 -> One (r110)
  | 290 -> One (r111)
  | 293 -> One (r112)
  | 292 -> One (r113)
  | 298 -> One (r114)
  | 364 -> One (r115)
  | 361 -> One (r116)
  | 337 -> One (r117)
  | 322 -> One (r118)
  | 326 -> One (r119)
  | 328 -> One (r120)
  | 331 -> One (r121)
  | 336 -> One (r122)
  | 348 -> One (r123)
  | 351 -> One (r124)
  | 350 -> One (r125)
  | 353 -> One (r126)
  | 360 -> One (r127)
  | 374 -> One (r128)
  | 373 -> One (r129)
  | 381 -> One (r130)
  | 380 -> One (r131)
  | 379 -> One (r132)
  | 378 -> One (r133)
  | 377 -> One (r134)
  | 376 -> One (r135)
  | 1391 -> One (r136)
  | 1390 -> One (r137)
  | 1389 -> One (r138)
  | 1388 -> One (r139)
  | 1387 -> One (r140)
  | 385 -> One (r141)
  | 1379 -> One (r142)
  | 387 -> One (r143)
  | 1378 -> One (r144)
  | 1377 -> One (r145)
  | 1376 -> One (r146)
  | 1375 -> One (r147)
  | 389 -> One (r148)
  | 1374 -> One (r149)
  | 394 -> One (r150)
  | 393 -> One (r151)
  | 396 -> One (r152)
  | 1282 -> One (r153)
  | 1281 -> One (r154)
  | 1370 -> One (r155)
  | 413 -> One (r156)
  | 412 -> One (r157)
  | 411 -> One (r159)
  | 410 -> One (r160)
  | 403 -> One (r161)
  | 406 -> One (r162)
  | 405 -> One (r163)
  | 409 -> One (r164)
  | 408 -> One (r165)
  | 1293 -> One (r166)
  | 439 -> One (r167)
  | 438 -> One (r168)
  | 437 -> One (r169)
  | 431 -> One (r170)
  | 428 -> One (r172)
  | 423 -> One (r173)
  | 420 -> One (r174)
  | 419 -> One (r175)
  | 418 -> One (r176)
  | 417 -> One (r177)
  | 427 -> One (r178)
  | 426 -> One (r179)
  | 425 -> One (r180)
  | 430 -> One (r181)
  | 436 -> One (r182)
  | 435 -> One (r183)
  | 434 -> One (r184)
  | 1292 -> One (r185)
  | 1291 -> One (r186)
  | 1290 -> One (r187)
  | 1289 -> One (r188)
  | 443 -> One (r189)
  | 1288 -> One (r190)
  | 1287 -> One (r191)
  | 1286 -> One (r192)
  | 1285 -> One (r193)
  | 445 -> One (r194)
  | 1270 -> One (r195)
  | 1269 -> One (r196)
  | 452 -> One (r197)
  | 451 -> One (r198)
  | 1265 -> One (r199)
  | 1264 -> One (r200)
  | 1254 -> One (r201)
  | 1253 -> One (r202)
  | 1252 -> One (r203)
  | 1251 -> One (r204)
  | 1250 -> One (r205)
  | 459 -> One (r206)
  | 457 -> One (r207)
  | 1249 -> One (r208)
  | 469 -> One (r209)
  | 468 -> One (r210)
  | 467 -> One (r211)
  | 466 -> One (r212)
  | 1215 -> One (r213)
  | 1214 -> One (r214)
  | 1213 -> One (r215)
  | 472 -> One (r216)
  | 1212 -> One (r217)
  | 1191 -> One (r218)
  | 481 -> One (r219)
  | 480 -> One (r220)
  | 476 -> One (r221)
  | 479 -> One (r222)
  | 478 -> One (r223)
  | 1190 -> One (r224)
  | 1189 -> One (r225)
  | 1188 -> One (r226)
  | 484 -> One (r227)
  | 1184 -> One (r228)
  | 486 -> One (r229)
  | 1183 -> One (r230)
  | 1182 -> One (r231)
  | 1179 -> One (r232)
  | 1178 -> One (r233)
  | 1177 -> One (r234)
  | 1176 -> One (r235)
  | 1175 -> One (r236)
  | 1174 -> One (r237)
  | 1173 -> One (r238)
  | 493 -> One (r239)
  | 933 -> One (r240)
  | 1170 -> One (r241)
  | 496 -> One (r242)
  | 1167 -> One (r243)
  | 1166 -> One (r244)
  | 1165 -> One (r245)
  | 498 -> One (r246)
  | 1161 -> One (r247)
  | 501 -> One (r248)
  | 500 -> One (r249)
  | 1160 -> One (r250)
  | 1159 -> One (r251)
  | 502 -> One (r252)
  | 1158 -> One (r253)
  | 1157 -> One (r254)
  | 1156 -> One (r255)
  | 1145 -> One (r256)
  | 525 -> One (r257)
  | 1153 -> One (r259)
  | 524 -> One (r260)
  | 505 -> One (r261)
  | 507 -> One (r262)
  | 516 -> One (r264)
  | 514 -> One (r265)
  | 513 -> One (r266)
  | 512 -> One (r267)
  | 511 -> One (r268)
  | 520 -> One (r269)
  | 519 -> One (r270)
  | 522 -> One (r271)
  | 523 -> One (r272)
  | 1137 -> One (r273)
  | 1136 -> One (r274)
  | 1135 -> One (r275)
  | 1134 -> One (r276)
  | 529 -> One (r277)
  | 528 -> One (r278)
  | 527 -> One (r279)
  | 1133 -> One (r280)
  | 1132 -> One (r281)
  | 1131 -> One (r282)
  | 1130 -> One (r283)
  | 1129 -> One (r284)
  | 1126 -> One (r285)
  | 533 -> One (r286)
  | 532 -> One (r287)
  | 531 -> One (r288)
  | 537 -> One (r289)
  | 1075 -> One (r290)
  | 540 | 961 | 1073 | 1435 -> One (r291)
  | 539 | 1072 | 1434 -> One (r292)
  | 1125 -> One (r293)
  | 1124 -> One (r294)
  | 928 -> One (r295)
  | 927 -> One (r296)
  | 926 -> One (r297)
  | 925 -> One (r298)
  | 924 -> One (r299)
  | 923 -> One (r300)
  | 922 -> One (r301)
  | 547 -> One (r302)
  | 921 -> One (r303)
  | 920 -> One (r304)
  | 549 -> One (r305)
  | 919 -> One (r306)
  | 918 -> One (r307)
  | 555 -> One (r308)
  | 554 -> One (r309)
  | 553 -> One (r310)
  | 552 -> One (r311)
  | 656 -> One (r312)
  | 571 -> One (r313)
  | 567 -> One (r314)
  | 566 -> One (r315)
  | 565 -> One (r316)
  | 564 -> One (r317)
  | 569 -> One (r318)
  | 576 -> One (r319)
  | 575 -> One (r320)
  | 653 -> One (r321)
  | 647 -> One (r322)
  | 646 -> One (r323)
  | 580 | 661 -> One (r324)
  | 579 | 660 | 1409 -> One (r325)
  | 578 -> One (r327)
  | 634 -> One (r328)
  | 589 -> One (r329)
  | 586 -> One (r330)
  | 588 -> One (r331)
  | 616 -> One (r333)
  | 615 -> One (r334)
  | 595 -> One (r335)
  | 594 -> One (r337)
  | 602 -> One (r338)
  | 599 | 612 -> One (r339)
  | 598 | 611 -> One (r340)
  | 597 | 610 -> One (r341)
  | 605 -> One (r342)
  | 604 -> One (r343)
  | 607 -> One (r344)
  | 613 -> One (r345)
  | 609 -> One (r346)
  | 633 -> One (r347)
  | 632 -> One (r348)
  | 630 -> One (r349)
  | 629 -> One (r350)
  | 626 -> One (r351)
  | 625 -> One (r352)
  | 624 -> One (r353)
  | 623 -> One (r354)
  | 622 -> One (r355)
  | 637 -> One (r356)
  | 644 -> One (r357)
  | 643 -> One (r358)
  | 642 -> One (r359)
  | 641 -> One (r360)
  | 640 -> One (r361)
  | 651 -> One (r362)
  | 650 -> One (r363)
  | 649 -> One (r364)
  | 652 -> One (r366)
  | 658 -> One (r367)
  | 671 -> One (r368)
  | 670 -> One (r369)
  | 669 -> One (r370)
  | 673 -> One (r372)
  | 672 -> One (r374)
  | 666 -> One (r375)
  | 665 -> One (r376)
  | 664 -> One (r377)
  | 663 -> One (r378)
  | 668 -> One (r379)
  | 917 -> One (r380)
  | 678 -> One (r381)
  | 677 -> One (r382)
  | 676 -> One (r383)
  | 759 -> One (r384)
  | 755 -> One (r385)
  | 754 -> One (r386)
  | 753 -> One (r387)
  | 752 -> One (r388)
  | 748 -> One (r389)
  | 747 -> One (r390)
  | 746 -> One (r391)
  | 743 -> One (r392)
  | 742 -> One (r393)
  | 683 -> One (r394)
  | 687 -> One (r395)
  | 686 -> One (r396)
  | 685 -> One (r397)
  | 689 -> One (r398)
  | 691 -> One (r399)
  | 741 -> One (r400)
  | 740 -> One (r401)
  | 700 -> One (r402)
  | 698 -> One (r403)
  | 697 -> One (r404)
  | 732 -> One (r405)
  | 723 -> One (r406)
  | 722 -> One (r407)
  | 721 -> One (r408)
  | 713 -> One (r409)
  | 709 -> One (r410)
  | 708 -> One (r411)
  | 707 -> One (r412)
  | 706 -> One (r413)
  | 711 -> One (r414)
  | 720 -> One (r415)
  | 719 -> One (r416)
  | 718 -> One (r417)
  | 731 -> One (r418)
  | 730 -> One (r419)
  | 729 -> One (r420)
  | 725 -> One (r421)
  | 728 -> One (r422)
  | 727 -> One (r423)
  | 734 -> One (r425)
  | 745 -> One (r426)
  | 751 -> One (r427)
  | 750 -> One (r428)
  | 758 -> One (r429)
  | 757 -> One (r430)
  | 764 -> One (r431)
  | 763 -> One (r432)
  | 762 -> One (r433)
  | 761 -> One (r434)
  | 768 -> One (r436)
  | 765 -> One (r438)
  | 767 -> One (r439)
  | 772 -> One (r440)
  | 771 -> One (r441)
  | 770 -> One (r442)
  | 776 -> One (r443)
  | 775 -> One (r444)
  | 774 -> One (r445)
  | 779 -> One (r446)
  | 778 -> One (r447)
  | 788 -> One (r448)
  | 787 -> One (r449)
  | 784 -> One (r450)
  | 783 -> One (r451)
  | 782 -> One (r452)
  | 781 -> One (r453)
  | 794 -> One (r454)
  | 793 -> One (r455)
  | 790 -> One (r457)
  | 792 -> One (r458)
  | 907 -> One (r459)
  | 869 -> One (r460)
  | 853 -> One (r461)
  | 868 -> One (r463)
  | 867 -> One (r464)
  | 804 -> One (r465)
  | 803 -> One (r466)
  | 802 -> One (r467)
  | 801 -> One (r468)
  | 800 -> One (r469)
  | 812 -> One (r470)
  | 811 -> One (r471)
  | 810 -> One (r473)
  | 809 -> One (r474)
  | 808 -> One (r475)
  | 807 -> One (r476)
  | 828 -> One (r477)
  | 827 -> One (r478)
  | 826 -> One (r479)
  | 825 -> One (r481)
  | 824 -> One (r482)
  | 818 -> One (r483)
  | 817 -> One (r484)
  | 816 -> One (r485)
  | 815 -> One (r486)
  | 823 -> One (r487)
  | 822 -> One (r488)
  | 821 -> One (r489)
  | 820 -> One (r490)
  | 841 -> One (r491)
  | 840 -> One (r492)
  | 836 -> One (r493)
  | 835 -> One (r494)
  | 834 -> One (r495)
  | 839 -> One (r496)
  | 838 -> One (r497)
  | 855 -> One (r498)
  | 854 -> One (r499)
  | 851 -> One (r500)
  | 847 -> One (r501)
  | 846 -> One (r502)
  | 850 -> One (r503)
  | 849 -> One (r504)
  | 860 -> One (r505)
  | 859 -> One (r506)
  | 858 -> One (r507)
  | 862 -> One (r509)
  | 861 -> One (r510)
  | 864 -> One (r511)
  | 871 -> One (r512)
  | 901 -> One (r513)
  | 906 -> One (r515)
  | 905 -> One (r516)
  | 876 -> One (r517)
  | 875 -> One (r518)
  | 874 -> One (r519)
  | 904 -> One (r520)
  | 881 -> One (r521)
  | 880 -> One (r522)
  | 879 -> One (r523)
  | 878 -> One (r524)
  | 903 -> One (r525)
  | 884 -> One (r526)
  | 883 -> One (r527)
  | 902 -> One (r528)
  | 888 -> One (r529)
  | 887 -> One (r530)
  | 886 -> One (r531)
  | 900 -> One (r532)
  | 892 -> One (r533)
  | 895 -> One (r534)
  | 909 -> One (r535)
  | 912 -> One (r536)
  | 915 -> One (r537)
  | 940 -> One (r538)
  | 939 -> One (r539)
  | 942 | 1085 -> One (r540)
  | 941 | 950 -> One (r541)
  | 944 -> One (r542)
  | 1118 -> One (r543)
  | 949 -> One (r544)
  | 1084 -> One (r545)
  | 955 -> One (r546)
  | 1083 | 1110 -> One (r547)
  | 956 | 1094 -> One (r548)
  | 959 | 1097 -> One (r549)
  | 958 | 1096 -> One (r550)
  | 957 | 1095 -> One (r551)
  | 1046 -> One (r552)
  | 1045 -> One (r553)
  | 1042 -> One (r554)
  | 1036 -> One (r555)
  | 1035 -> One (r556)
  | 1034 -> One (r557)
  | 965 -> One (r558)
  | 970 -> One (r559)
  | 969 -> One (r560)
  | 968 -> One (r561)
  | 1023 -> One (r562)
  | 972 -> One (r563)
  | 975 -> One (r564)
  | 977 -> One (r565)
  | 979 -> One (r566)
  | 984 -> One (r567)
  | 986 -> One (r568)
  | 988 -> One (r569)
  | 990 -> One (r570)
  | 992 -> One (r571)
  | 994 -> One (r572)
  | 996 -> One (r573)
  | 998 -> One (r574)
  | 1000 -> One (r575)
  | 1002 -> One (r576)
  | 1004 -> One (r577)
  | 1006 -> One (r578)
  | 1008 -> One (r579)
  | 1010 -> One (r580)
  | 1012 -> One (r581)
  | 1014 -> One (r582)
  | 1016 -> One (r583)
  | 1018 -> One (r584)
  | 1020 -> One (r585)
  | 1022 -> One (r586)
  | 1025 -> One (r587)
  | 1027 -> One (r588)
  | 1033 -> One (r589)
  | 1032 -> One (r590)
  | 1040 -> One (r591)
  | 1039 -> One (r592)
  | 1051 -> One (r593)
  | 1050 -> One (r594)
  | 1049 -> One (r595)
  | 1059 -> One (r596)
  | 1058 -> One (r597)
  | 1053 -> One (r598)
  | 1057 -> One (r599)
  | 1056 -> One (r600)
  | 1071 -> One (r601)
  | 1070 -> One (r602)
  | 1063 -> One (r603)
  | 1062 -> One (r604)
  | 1069 -> One (r605)
  | 1079 | 1102 -> One (r606)
  | 1078 | 1101 -> One (r607)
  | 1077 | 1100 -> One (r608)
  | 1082 | 1107 -> One (r609)
  | 1081 | 1106 -> One (r610)
  | 1080 | 1105 -> One (r611)
  | 1087 -> One (r612)
  | 1089 -> One (r613)
  | 1091 -> One (r614)
  | 1093 -> One (r615)
  | 1099 -> One (r616)
  | 1104 -> One (r617)
  | 1109 -> One (r618)
  | 1112 -> One (r619)
  | 1120 -> One (r620)
  | 1122 -> One (r621)
  | 1144 -> One (r622)
  | 1143 -> One (r623)
  | 1142 -> One (r624)
  | 1141 -> One (r625)
  | 1140 -> One (r626)
  | 1139 -> One (r627)
  | 1152 -> One (r628)
  | 1149 -> One (r629)
  | 1148 -> One (r630)
  | 1151 -> One (r631)
  | 1155 -> One (r632)
  | 1163 -> One (r634)
  | 1169 -> One (r635)
  | 1172 -> One (r636)
  | 1181 -> One (r637)
  | 1187 -> One (r638)
  | 1186 -> One (r639)
  | 1203 -> One (r640)
  | 1202 -> One (r641)
  | 1201 -> One (r642)
  | 1194 -> One (r643)
  | 1193 -> One (r644)
  | 1196 -> One (r645)
  | 1200 -> One (r646)
  | 1199 -> One (r647)
  | 1198 -> One (r648)
  | 1205 -> One (r649)
  | 1210 -> One (r650)
  | 1209 -> One (r651)
  | 1208 -> One (r652)
  | 1207 -> One (r653)
  | 1234 -> One (r654)
  | 1219 -> One (r655)
  | 1218 -> One (r656)
  | 1217 -> One (r657)
  | 1221 -> One (r658)
  | 1220 -> One (r659)
  | 1223 -> One (r660)
  | 1225 -> One (r661)
  | 1227 -> One (r662)
  | 1230 -> One (r663)
  | 1229 -> One (r664)
  | 1233 -> One (r665)
  | 1243 -> One (r666)
  | 1242 -> One (r667)
  | 1241 -> One (r668)
  | 1240 -> One (r669)
  | 1239 -> One (r670)
  | 1248 -> One (r671)
  | 1247 -> One (r672)
  | 1246 -> One (r673)
  | 1245 -> One (r674)
  | 1257 -> One (r675)
  | 1256 -> One (r676)
  | 1260 -> One (r677)
  | 1259 -> One (r678)
  | 1263 -> One (r679)
  | 1262 -> One (r680)
  | 1268 -> One (r681)
  | 1267 -> One (r682)
  | 1273 -> One (r683)
  | 1272 -> One (r684)
  | 1280 -> One (r685)
  | 1279 -> One (r686)
  | 1278 -> One (r687)
  | 1277 -> One (r688)
  | 1276 -> One (r689)
  | 1275 -> One (r690)
  | 1284 -> One (r691)
  | 1296 -> One (r692)
  | 1295 -> One (r693)
  | 1308 -> One (r694)
  | 1307 -> One (r695)
  | 1306 -> One (r696)
  | 1305 -> One (r697)
  | 1304 -> One (r698)
  | 1298 -> One (r699)
  | 1323 -> One (r701)
  | 1322 -> One (r702)
  | 1303 -> One (r703)
  | 1302 -> One (r704)
  | 1301 -> One (r705)
  | 1300 -> One (r706)
  | 1321 -> One (r708)
  | 1310 -> One (r709)
  | 1320 -> One (r710)
  | 1319 -> One (r711)
  | 1318 -> One (r712)
  | 1317 -> One (r713)
  | 1316 -> One (r714)
  | 1315 -> One (r715)
  | 1314 -> One (r716)
  | 1313 -> One (r717)
  | 1326 -> One (r718)
  | 1325 -> One (r719)
  | 1347 -> One (r720)
  | 1345 -> One (r722)
  | 1362 -> One (r724)
  | 1361 -> One (r725)
  | 1358 -> One (r726)
  | 1328 -> One (r727)
  | 1331 -> One (r728)
  | 1330 -> One (r729)
  | 1354 -> One (r730)
  | 1353 -> One (r731)
  | 1352 -> One (r732)
  | 1336 -> One (r733)
  | 1335 -> One (r734)
  | 1334 -> One (r735)
  | 1340 -> One (r736)
  | 1339 -> One (r737)
  | 1338 -> One (r738)
  | 1348 -> One (r740)
  | 1343 -> One (r741)
  | 1342 -> One (r742)
  | 1351 -> One (r744)
  | 1357 -> One (r745)
  | 1356 -> One (r746)
  | 1360 -> One (r747)
  | 1365 -> One (r748)
  | 1364 -> One (r749)
  | 1367 -> One (r750)
  | 1373 -> One (r751)
  | 1372 -> One (r752)
  | 1382 -> One (r753)
  | 1381 -> One (r754)
  | 1384 -> One (r755)
  | 1386 -> One (r756)
  | 1405 -> One (r757)
  | 1402 -> One (r758)
  | 1404 -> One (r759)
  | 1407 -> One (r760)
  | 1426 -> One (r761)
  | 1424 -> One (r763)
  | 1414 -> One (r764)
  | 1413 -> One (r765)
  | 1412 -> One (r766)
  | 1411 -> One (r767)
  | 1417 -> One (r768)
  | 1423 -> One (r769)
  | 1422 -> One (r770)
  | 1419 -> One (r771)
  | 1428 -> One (r772)
  | 1437 -> One (r773)
  | 1442 -> One (r774)
  | 1441 -> One (r775)
  | 1440 -> One (r776)
  | 1446 -> One (r777)
  | 1445 -> One (r778)
  | 1444 -> One (r779)
  | 1450 -> One (r780)
  | 1449 -> One (r781)
  | 1448 -> One (r782)
  | 1454 -> One (r783)
  | 1453 -> One (r784)
  | 1452 -> One (r785)
  | 1460 -> One (r786)
  | 1459 -> One (r787)
  | 1458 -> One (r788)
  | 1457 -> One (r789)
  | 1456 -> One (r790)
  | 1464 -> One (r791)
  | 1463 -> One (r792)
  | 1462 -> One (r793)
  | 1468 -> One (r794)
  | 1467 -> One (r795)
  | 1466 -> One (r796)
  | 1484 -> One (r797)
  | 1472 -> One (r798)
  | 1471 -> One (r799)
  | 1478 -> One (r800)
  | 1474 -> One (r801)
  | 1477 -> One (r802)
  | 1476 -> One (r803)
  | 1480 -> One (r804)
  | 1482 -> One (r805)
  | 1488 | 1536 -> One (r806)
  | 1487 | 1535 -> One (r807)
  | 1486 | 1534 -> One (r808)
  | 1485 | 1533 -> One (r809)
  | 1492 | 1540 -> One (r810)
  | 1491 | 1539 -> One (r811)
  | 1490 | 1538 -> One (r812)
  | 1489 | 1537 -> One (r813)
  | 1495 -> One (r814)
  | 1494 -> One (r815)
  | 1502 -> One (r816)
  | 1501 -> One (r817)
  | 1500 -> One (r818)
  | 1499 -> One (r819)
  | 1498 -> One (r820)
  | 1497 -> One (r821)
  | 1510 -> One (r822)
  | 1509 -> One (r823)
  | 1504 -> One (r825)
  | 1506 -> One (r826)
  | 1508 -> One (r827)
  | 1527 -> One (r828)
  | 1513 -> One (r829)
  | 1518 -> One (r830)
  | 1517 -> One (r831)
  | 1526 -> One (r833)
  | 1525 -> One (r834)
  | 1516 -> One (r835)
  | 1515 -> One (r836)
  | 1522 -> One (r837)
  | 1521 -> One (r838)
  | 1520 -> One (r839)
  | 1524 -> One (r840)
  | 1529 -> One (r841)
  | 1541 -> One (r842)
  | 1544 -> One (r843)
  | 1548 -> One (r844)
  | 1547 -> One (r845)
  | 1550 -> One (r846)
  | 1554 -> One (r847)
  | 1558 -> One (r848)
  | 960 -> Select (function
    | -1 -> [R 90]
    | _ -> r292)
  | 455 -> Select (function
    | -1 -> [R 710]
    | _ -> [R 238])
  | 446 -> Select (function
    | -1 -> S (T T_RPAREN) :: r26
    | _ -> S (N N_seq_expr) :: r154)
  | 482 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r111
    | _ -> S (N N_expr_semi_list) :: r226)
  | 1418 -> Select (function
    | 1415 -> r370
    | _ -> S (T T_EQUAL) :: r771)
  | 112 -> Select (function
    | 459 | 965 | 1037 | 1207 | 1334 | 1487 | 1491 | 1535 | 1539 -> S (N N_operator) :: r23
    | _ -> S (N N_pattern) :: r25)
  | _ -> raise Not_found
