open Parser_raw

module Default = struct

  open Asttypes
  open Parsetree
  open Ast_helper
  let default_loc = ref Location.none
  let default_expr () =
    let id = Location.mkloc "merlin.hole" !default_loc in
    Exp.mk ~loc:!default_loc (Pexp_extension (id, PStr []))
  let default_pattern () = Pat.any ~loc:!default_loc ()
  let default_module_expr () = Mod.structure ~loc:!default_loc[]
  let default_module_type () = Mty.signature ~loc:!default_loc[]

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
    | MenhirInterpreter.T MenhirInterpreter.T_SHARPOP -> ""
    | MenhirInterpreter.T MenhirInterpreter.T_SHARP -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_SEMISEMI -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_SEMI -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_RPAREN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_REC -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_RBRACKET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_RBRACE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_QUOTE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_QUESTIONQUESTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_QUESTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PRIVATE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PREFIXOP -> "!"
    | MenhirInterpreter.T MenhirInterpreter.T_PLUSEQ -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PLUSDOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PLUS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PERCENT -> ()
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
    | MenhirInterpreter.T MenhirInterpreter.T_DOTOP -> raise Not_found
    | MenhirInterpreter.T MenhirInterpreter.T_DOTLESS -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOTDOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DONE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOCSTRING -> raise Not_found
    | MenhirInterpreter.T MenhirInterpreter.T_DO -> ()
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
    | MenhirInterpreter.N MenhirInterpreter.N_simple_delimited_pattern -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_no_exn_comma_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_no_exn -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_gen -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_comma_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern -> default_pattern ()
    | MenhirInterpreter.N MenhirInterpreter.N_parse_expression -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parent_binder -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_paren_module_expr -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_lwt_bindings -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lwt_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lident_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_pattern -> default_pattern ()
    | MenhirInterpreter.N MenhirInterpreter.N_let_exception_declaration -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_inherit_field_semi -> raise Not_found
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
  [|0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;2;1;2;1;2;1;1;2;1;2;1;1;1;1;1;2;1;1;2;3;3;3;1;2;1;2;1;1;1;2;1;1;1;2;1;2;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;4;2;3;4;2;3;4;1;1;1;1;1;2;3;3;4;1;1;1;2;1;1;1;2;1;2;3;1;1;2;3;1;1;2;1;2;1;3;1;1;1;1;1;1;2;2;2;3;2;3;1;4;5;1;1;1;2;1;2;1;2;1;1;1;1;2;1;1;2;2;1;2;1;2;1;1;1;2;3;2;1;2;3;4;2;3;2;3;1;3;4;2;3;1;2;1;3;1;1;2;1;1;3;2;3;1;1;2;3;2;3;4;4;2;3;5;2;1;2;3;2;3;4;5;3;4;3;4;4;5;6;2;1;2;1;1;2;2;3;1;2;1;2;1;2;3;2;3;3;4;5;6;1;1;2;1;2;1;3;4;5;2;3;1;2;3;4;5;4;2;3;2;1;1;2;1;3;1;1;1;1;2;1;1;1;1;1;2;3;4;5;6;7;2;3;1;2;1;1;2;1;2;2;3;4;3;4;3;3;2;1;1;2;3;1;2;2;3;4;5;2;3;1;4;5;6;7;5;5;2;6;7;1;2;1;2;3;4;5;6;7;1;2;3;1;2;3;1;2;1;1;2;3;4;5;4;5;3;4;8;9;1;2;2;2;1;1;1;2;3;4;2;3;1;1;1;1;2;3;3;3;3;3;1;3;2;3;1;1;1;1;1;2;3;4;2;5;1;2;1;1;1;2;1;2;2;1;2;2;1;1;2;3;4;5;5;1;2;3;4;1;2;1;2;3;4;1;1;1;2;1;1;1;2;2;3;1;4;2;1;2;1;1;2;3;3;1;2;4;5;4;5;6;2;1;2;3;3;1;2;3;4;3;4;3;2;3;1;5;2;3;2;1;2;3;3;1;1;3;4;5;2;1;2;3;2;5;6;2;3;1;1;2;3;1;1;1;2;1;2;1;1;2;1;3;1;1;1;2;3;1;2;3;1;4;3;1;1;2;2;3;1;2;1;1;1;1;1;3;1;1;2;3;1;1;1;2;3;4;1;2;1;1;1;2;3;2;3;2;1;2;1;1;2;3;4;5;2;3;2;3;2;3;3;4;2;2;3;3;4;1;3;1;4;2;2;3;4;1;1;2;1;1;2;3;3;4;2;1;2;3;1;1;2;3;4;5;6;1;1;1;2;1;2;1;1;1;1;1;2;1;2;1;2;1;2;3;1;2;1;2;1;1;2;1;2;3;3;4;5;1;2;3;4;1;2;5;6;1;1;2;3;4;1;2;3;4;1;1;2;1;1;2;3;4;1;1;4;5;1;2;3;4;1;5;2;3;2;3;3;4;5;2;2;1;1;6;7;8;9;1;1;1;1;1;1;1;1;2;3;2;3;2;3;1;1;1;1;2;2;3;1;2;1;2;1;2;2;3;1;2;1;2;3;1;1;2;3;1;1;2;1;2;3;2;3;2;3;2;1;2;1;2;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;3;2;3;2;3;1;2;3;4;5;3;1;2;1;2;3;4;5;1;2;3;1;2;3;2;3;2;3;2;3;2;3;2;1;3;4;2;2;4;5;6;7;3;4;5;6;7;3;4;5;6;7;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;8;9;5;6;7;8;9;5;6;7;8;9;3;4;5;2;1;2;1;1;2;4;5;3;4;5;3;4;5;3;4;5;3;4;4;2;3;5;6;1;3;4;4;5;6;3;4;5;1;2;3;2;3;1;2;3;1;2;3;1;1;2;1;2;3;4;1;2;3;4;4;5;1;2;3;1;5;1;1;6;7;8;9;10;5;6;7;8;4;5;6;7;8;9;10;2;1;2;3;4;1;2;1;2;3;4;1;1;2;5;1;2;3;3;4;5;7;3;4;3;4;5;2;3;3;4;2;3;1;3;4;5;6;7;3;3;4;3;2;3;2;2;3;3;2;3;2;3;4;2;2;3;4;7;2;3;4;1;2;3;4;5;6;7;1;2;2;3;4;5;6;1;2;3;2;3;4;5;2;4;5;2;1;2;3;4;1;2;1;2;3;1;1;2;5;2;3;4;5;6;7;8;3;4;5;6;7;2;3;4;2;4;5;3;4;5;6;3;4;5;6;4;5;5;6;7;5;6;7;7;8;9;2;4;5;3;4;5;6;1;2;1;2;3;4;1;2;1;2;3;4;1;2;3;4;5;6;1;2;7;8;1;2;3;4;5;6;7;4;5;6;1;1;1;2;3;1;2;3;4;5;1;2;6;2;3;4;5;6;4;5;3;4;5;6;7;1;2;3;4;1;2;3;1;2;3;1;4;1;1;1;2;2;2;3;2;3;1;5;6;7;1;2;1;2;3;3;4;1;2;1;2;1;2;3;4;5;1;2;3;4;5;3;4;1;2;3;1;2;1;2;1;2;3;4;5;1;2;3;6;7;1;1;2;1;1;2;3;4;5;6;1;2;3;1;2;3;4;1;1;7;2;3;4;1;2;1;2;3;3;4;1;2;1;2;8;9;2;3;4;5;6;1;2;3;4;5;1;2;3;1;2;3;4;1;1;1;2;1;2;3;1;3;1;5;4;6;7;8;1;1;1;2;3;4;5;6;7;2;1;1;2;1;1;1;1;1;2;3;4;5;6;2;1;1;1;1;1;1;1;2;1;1;1;2;3;4;5;6;7;8;2;1;1;1;2;3;4;5;6;7;8;2;1;2;2;2;1;2;3;4;5;1;1;2;3;4;1;2;1;2;3;1;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;5;6;7;1;1;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;8;9;10;11;6;7;8;5;2;3;1;2;3;4;1;2;3;1;1;2;3;4;5;6;1;2;3;4;1;2;1;2;1;2;1;1;2;1;3;2;3;2;3;7;2;3;3;4;5;4;1;2;5;6;1;2;3;4;1;2;1;2;2;1;2;3;4;1;2;6;7;1;1;1;1;1;1;2;1;1;1;1;1;1;2;3;4;5;2;1;1;1;1;1;1;1;1;2;1;1;1;1;2;3;4;5;6;7;2;1;2;1;2;3;1;1;1;3;4;3;4;3;4;5;6;7;2;3;3;4;5;3;4;2;3;4;8;5;6;7;1;2;8;9;2;1;1;1;3;4;4;5;2;3;4;4;5;6;5;6;3;4;2;2;3;4;5;5;6;7;2;3;3;4;3;4;5;6;3;4;5;6;7;8;4;5;3;4;5;6;3;4;3;2;3;4;5;6;1;2;1;0;1;2;1;0;1;2;1;|]

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
  | T_SHARP -> true
  | T_SEMISEMI -> true
  | T_SEMI -> true
  | T_RPAREN -> true
  | T_REC -> true
  | T_RBRACKET -> true
  | T_RBRACE -> true
  | T_QUOTE -> true
  | T_QUESTIONQUESTION -> true
  | T_QUESTION -> true
  | T_PRIVATE -> true
  | T_PLUSEQ -> true
  | T_PLUSDOT -> true
  | T_PLUS -> true
  | T_PERCENT -> true
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
  let r0 = [R 478] in
  let r1 = S (N N_expr) :: r0 in
  let r2 = [R 137] in
  let r3 = S (T T_DONE) :: r2 in
  let r4 = Sub (r1) :: r3 in
  let r5 = S (T T_DO) :: r4 in
  let r6 = Sub (r1) :: r5 in
  let r7 = [R 598] in
  let r8 = S (T T_AND) :: r7 in
  let r9 = [R 14] in
  let r10 = Sub (r8) :: r9 in
  let r11 = [R 208] in
  let r12 = R 17 :: r11 in
  let r13 = [R 15] in
  let r14 = [R 442] in
  let r15 = S (N N_structure) :: r14 in
  let r16 = [R 16] in
  let r17 = S (T T_RBRACKET) :: r16 in
  let r18 = Sub (r15) :: r17 in
  let r19 = [R 156] in
  let r20 = S (T T_DONE) :: r19 in
  let r21 = Sub (r1) :: r20 in
  let r22 = S (T T_DO) :: r21 in
  let r23 = Sub (r1) :: r22 in
  let r24 = [R 315] in
  let r25 = [R 133] in
  let r26 = Sub (r1) :: r25 in
  let r27 = [R 150] in
  let r28 = S (N N_match_cases) :: r27 in
  let r29 = R 387 :: r28 in
  let r30 = S (T T_WITH) :: r29 in
  let r31 = Sub (r1) :: r30 in
  let r32 = [R 575] in
  let r33 = S (T T_QUESTIONQUESTION) :: r32 in
  let r34 = [R 563] in
  let r35 = [R 49] in
  let r36 = S (T T_LIDENT) :: r35 in
  let r37 = [R 565] in
  let r38 = Sub (r36) :: r37 in
  let r39 = [R 50] in
  let r40 = S (T T_LIDENT) :: r39 in
  let r41 = [R 316] in
  let r42 = [R 207] in
  let r43 = [R 18] in
  let r44 = [R 102] in
  let r45 = [R 544] in
  let r46 = S (T T_RPAREN) :: r45 in
  let r47 = [R 721] in
  let r48 = [R 209] in
  let r49 = S (T T_RBRACKET) :: r48 in
  let r50 = Sub (r15) :: r49 in
  let r51 = S (T T_LIDENT) :: r47 in
  let r52 = [R 514] in
  let r53 = S (T T_UNDERSCORE) :: r52 in
  let r54 = [R 511] in
  let r55 = Sub (r53) :: r54 in
  let r56 = [R 532] in
  let r57 = Sub (r55) :: r56 in
  let r58 = [R 118] in
  let r59 = Sub (r57) :: r58 in
  let r60 = [R 127] in
  let r61 = Sub (r59) :: r60 in
  let r62 = [R 116] in
  let r63 = Sub (r61) :: r62 in
  let r64 = [R 729] in
  let r65 = R 452 :: r64 in
  let r66 = Sub (r63) :: r65 in
  let r67 = S (T T_COLON) :: r66 in
  let r68 = Sub (r51) :: r67 in
  let r69 = [R 381] in
  let r70 = S (T T_AMPERAMPER) :: r69 in
  let r71 = [R 722] in
  let r72 = S (T T_RPAREN) :: r71 in
  let r73 = Sub (r70) :: r72 in
  let r74 = [R 362] in
  let r75 = S (T T_RPAREN) :: r74 in
  let r76 = [R 364] in
  let r77 = [R 366] in
  let r78 = [R 312] in
  let r79 = [R 520] in
  let r80 = [R 236] in
  let r81 = S (T T_LIDENT) :: r80 in
  let r82 = [R 513] in
  let r83 = Sub (r81) :: r82 in
  let r84 = [R 119] in
  let r85 = Sub (r59) :: r84 in
  let r86 = S (T T_MINUSGREATER) :: r85 in
  let r87 = Sub (r59) :: r86 in
  let r88 = S (T T_COLON) :: r87 in
  let r89 = [R 120] in
  let r90 = Sub (r59) :: r89 in
  let r91 = S (T T_MINUSGREATER) :: r90 in
  let r92 = [R 407] in
  let r93 = S (N N_module_type) :: r92 in
  let r94 = [R 530] in
  let r95 = S (T T_RPAREN) :: r94 in
  let r96 = Sub (r93) :: r95 in
  let r97 = R 206 :: r96 in
  let r98 = [R 335] in
  let r99 = S (T T_END) :: r98 in
  let r100 = R 488 :: r99 in
  let r101 = [R 696] in
  let r102 = R 452 :: r101 in
  let r103 = R 109 :: r102 in
  let r104 = R 699 :: r103 in
  let r105 = S (T T_LIDENT) :: r104 in
  let r106 = R 400 :: r105 in
  let r107 = R 353 :: r106 in
  let r108 = R 206 :: r107 in
  let r109 = [R 404] in
  let r110 = S (T T_UNDERSCORE) :: r109 in
  let r111 = [R 397] in
  let r112 = Sub (r110) :: r111 in
  let r113 = R 716 :: r112 in
  let r114 = [R 398] in
  let r115 = Sub (r113) :: r114 in
  let r116 = [R 402] in
  let r117 = S (T T_RPAREN) :: r116 in
  let r118 = [R 403] in
  let r119 = [R 399] in
  let r120 = [R 704] in
  let r121 = [R 705] in
  let r122 = [R 95] in
  let r123 = [R 710] in
  let r124 = [R 121] in
  let r125 = Sub (r59) :: r124 in
  let r126 = S (T T_MINUSGREATER) :: r125 in
  let r127 = [R 519] in
  let r128 = [R 475] in
  let r129 = Sub (r55) :: r128 in
  let r130 = [R 476] in
  let r131 = Sub (r129) :: r130 in
  let r132 = [R 528] in
  let r133 = S (T T_RBRACKET) :: r132 in
  let r134 = Sub (r131) :: r133 in
  let r135 = [R 527] in
  let r136 = [R 526] in
  let r137 = S (T T_RBRACKET) :: r136 in
  let r138 = [R 524] in
  let r139 = S (T T_RBRACKET) :: r138 in
  let r140 = Sub (r131) :: r139 in
  let r141 = [R 350] in
  let r142 = Sub (r81) :: r141 in
  let r143 = [R 521] in
  let r144 = [R 711] in
  let r145 = S (T T_LIDENT) :: r144 in
  let r146 = S (T T_DOT) :: r145 in
  let r147 = S (T T_UIDENT) :: r78 in
  let r148 = [R 314] in
  let r149 = S (T T_RPAREN) :: r148 in
  let r150 = [R 313] in
  let r151 = [R 477] in
  let r152 = [R 685] in
  let r153 = [R 5] in
  let r154 = Sub (r61) :: r153 in
  let r155 = [R 684] in
  let r156 = R 17 :: r155 in
  let r157 = Sub (r154) :: r156 in
  let r158 = [R 125] in
  let r159 = Sub (r55) :: r158 in
  let r160 = [R 533] in
  let r161 = [R 126] in
  let r162 = [R 122] in
  let r163 = [R 128] in
  let r164 = Sub (r81) :: r163 in
  let r165 = [R 6] in
  let r166 = [R 523] in
  let r167 = [R 525] in
  let r168 = S (T T_RBRACKET) :: r167 in
  let r169 = Sub (r131) :: r168 in
  let r170 = S (T T_BACKQUOTE) :: r142 in
  let r171 = [R 351] in
  let r172 = Sub (r170) :: r171 in
  let r173 = [R 529] in
  let r174 = S (T T_RBRACKET) :: r173 in
  let r175 = [R 518] in
  let r176 = [R 449] in
  let r177 = Sub (r61) :: r176 in
  let r178 = [R 217] in
  let r179 = R 17 :: r178 in
  let r180 = S (T T_SEMI) :: r179 in
  let r181 = R 17 :: r180 in
  let r182 = Sub (r177) :: r181 in
  let r183 = [R 719] in
  let r184 = [R 450] in
  let r185 = Sub (r61) :: r184 in
  let r186 = [R 720] in
  let r187 = [R 96] in
  let r188 = [R 512] in
  let r189 = [R 522] in
  let r190 = [R 124] in
  let r191 = [R 123] in
  let r192 = [R 94] in
  let r193 = [R 97] in
  let r194 = S (T T_FALSE) :: r193 in
  let r195 = [R 19] in
  let r196 = R 17 :: r195 in
  let r197 = R 231 :: r196 in
  let r198 = [R 110] in
  let r199 = Sub (r159) :: r198 in
  let r200 = [R 232] in
  let r201 = [R 241] in
  let r202 = S (T T_LIDENT) :: r201 in
  let r203 = [R 242] in
  let r204 = R 17 :: r203 in
  let r205 = Sub (r177) :: r204 in
  let r206 = S (T T_COLON) :: r205 in
  let r207 = Sub (r202) :: r206 in
  let r208 = R 348 :: r207 in
  let r209 = [R 244] in
  let r210 = Sub (r208) :: r209 in
  let r211 = [R 111] in
  let r212 = S (T T_RBRACE) :: r211 in
  let r213 = [R 243] in
  let r214 = R 17 :: r213 in
  let r215 = S (T T_SEMI) :: r214 in
  let r216 = R 17 :: r215 in
  let r217 = Sub (r177) :: r216 in
  let r218 = S (T T_COLON) :: r217 in
  let r219 = [R 234] in
  let r220 = [R 233] in
  let r221 = Sub (r55) :: r220 in
  let r222 = [R 112] in
  let r223 = R 17 :: r222 in
  let r224 = [R 706] in
  let r225 = S (T T_RBRACE) :: r224 in
  let r226 = Sub (r210) :: r225 in
  let r227 = [R 708] in
  let r228 = S (T T_DOTDOT) :: r227 in
  let r229 = [R 709] in
  let r230 = S (T T_RBRACE) :: r229 in
  let r231 = [R 451] in
  let r232 = S (T T_RBRACKET) :: r231 in
  let r233 = Sub (r15) :: r232 in
  let r234 = [R 210] in
  let r235 = R 17 :: r234 in
  let r236 = R 231 :: r235 in
  let r237 = Sub (r194) :: r236 in
  let r238 = [R 649] in
  let r239 = Sub (r237) :: r238 in
  let r240 = [R 656] in
  let r241 = R 452 :: r240 in
  let r242 = Sub (r239) :: r241 in
  let r243 = R 457 :: r242 in
  let r244 = [R 20] in
  let r245 = R 17 :: r244 in
  let r246 = R 231 :: r245 in
  let r247 = Sub (r194) :: r246 in
  let r248 = [R 104] in
  let r249 = S (T T_FALSE) :: r248 in
  let r250 = [R 21] in
  let r251 = R 17 :: r250 in
  let r252 = Sub (r249) :: r251 in
  let r253 = S (T T_EQUAL) :: r252 in
  let r254 = [R 103] in
  let r255 = [R 101] in
  let r256 = [R 100] in
  let r257 = S (T T_RPAREN) :: r256 in
  let r258 = S (T T_COLONCOLON) :: r257 in
  let r259 = [R 453] in
  let r260 = [R 211] in
  let r261 = R 17 :: r260 in
  let r262 = Sub (r249) :: r261 in
  let r263 = [R 686] in
  let r264 = [R 680] in
  let r265 = S (T T_UIDENT) :: r24 in
  let r266 = [R 355] in
  let r267 = R 452 :: r266 in
  let r268 = Sub (r265) :: r267 in
  let r269 = R 206 :: r268 in
  let r270 = [R 76] in
  let r271 = R 42 :: r270 in
  let r272 = R 53 :: r271 in
  let r273 = [R 200] in
  let r274 = S (T T_END) :: r273 in
  let r275 = Sub (r272) :: r274 in
  let r276 = [R 51] in
  let r277 = S (T T_RPAREN) :: r276 in
  let r278 = [R 580] in
  let r279 = S (T T_LIDENT) :: r123 in
  let r280 = [R 585] in
  let r281 = [R 509] in
  let r282 = [R 507] in
  let r283 = [R 591] in
  let r284 = S (T T_RPAREN) :: r283 in
  let r285 = [R 593] in
  let r286 = S (T T_RPAREN) :: r285 in
  let r287 = S (T T_UIDENT) :: r286 in
  let r288 = [R 594] in
  let r289 = S (T T_RPAREN) :: r288 in
  let r290 = [R 339] in
  let r291 = S (N N_module_expr) :: r290 in
  let r292 = R 17 :: r291 in
  let r293 = S (T T_OF) :: r292 in
  let r294 = [R 327] in
  let r295 = S (T T_END) :: r294 in
  let r296 = S (N N_structure) :: r295 in
  let r297 = [R 319] in
  let r298 = S (N N_module_expr) :: r297 in
  let r299 = S (T T_EQUAL) :: r298 in
  let r300 = [R 466] in
  let r301 = R 452 :: r300 in
  let r302 = Sub (r299) :: r301 in
  let r303 = S (T T_UIDENT) :: r302 in
  let r304 = S (T T_REC) :: r303 in
  let r305 = [R 343] in
  let r306 = R 452 :: r305 in
  let r307 = R 344 :: r306 in
  let r308 = Sub (r81) :: r307 in
  let r309 = R 206 :: r308 in
  let r310 = [R 345] in
  let r311 = [R 340] in
  let r312 = S (T T_RPAREN) :: r311 in
  let r313 = [R 336] in
  let r314 = S (N N_module_type) :: r313 in
  let r315 = S (T T_MINUSGREATER) :: r314 in
  let r316 = S (N N_functor_args) :: r315 in
  let r317 = [R 225] in
  let r318 = [R 226] in
  let r319 = S (T T_RPAREN) :: r318 in
  let r320 = S (N N_module_type) :: r319 in
  let r321 = [R 737] in
  let r322 = Sub (r147) :: r321 in
  let r323 = S (T T_EQUAL) :: r322 in
  let r324 = Sub (r265) :: r323 in
  let r325 = S (T T_MODULE) :: r324 in
  let r326 = [R 739] in
  let r327 = Sub (r325) :: r326 in
  let r328 = [R 338] in
  let r329 = [R 254] in
  let r330 = S (T T_LIDENT) :: r329 in
  let r331 = [R 736] in
  let r332 = Sub (r61) :: r331 in
  let r333 = S (T T_COLONEQUAL) :: r332 in
  let r334 = Sub (r330) :: r333 in
  let r335 = [R 255] in
  let r336 = S (T T_LIDENT) :: r335 in
  let r337 = [R 735] in
  let r338 = R 109 :: r337 in
  let r339 = [R 106] in
  let r340 = Sub (r63) :: r339 in
  let r341 = S (T T_EQUAL) :: r340 in
  let r342 = Sub (r63) :: r341 in
  let r343 = [R 108] in
  let r344 = [R 738] in
  let r345 = [R 740] in
  let r346 = [R 337] in
  let r347 = [R 347] in
  let r348 = Sub (r81) :: r347 in
  let r349 = [R 318] in
  let r350 = R 452 :: r349 in
  let r351 = Sub (r299) :: r350 in
  let r352 = [R 409] in
  let r353 = S (T T_RPAREN) :: r352 in
  let r354 = [R 410] in
  let r355 = S (T T_RPAREN) :: r354 in
  let r356 = S (N N_expr) :: r355 in
  let r357 = [R 132] in
  let r358 = S (N N_match_cases) :: r357 in
  let r359 = R 387 :: r358 in
  let r360 = S (T T_WITH) :: r359 in
  let r361 = Sub (r1) :: r360 in
  let r362 = [R 149] in
  let r363 = S (N N_match_cases) :: r362 in
  let r364 = R 387 :: r363 in
  let r365 = S (T T_WITH) :: r364 in
  let r366 = Sub (r1) :: r365 in
  let r367 = [R 427] in
  let r368 = S (N N_pattern) :: r367 in
  let r369 = Sub (r249) :: r368 in
  let r370 = [R 435] in
  let r371 = Sub (r369) :: r370 in
  let r372 = [R 283] in
  let r373 = Sub (r1) :: r372 in
  let r374 = S (T T_EQUAL) :: r373 in
  let r375 = Sub (r371) :: r374 in
  let r376 = [R 292] in
  let r377 = R 452 :: r376 in
  let r378 = Sub (r375) :: r377 in
  let r379 = R 464 :: r378 in
  let r380 = [R 537] in
  let r381 = [R 438] in
  let r382 = S (N N_pattern) :: r381 in
  let r383 = [R 535] in
  let r384 = S (T T_RBRACKET) :: r383 in
  let r385 = R 393 :: r384 in
  let r386 = [R 273] in
  let r387 = R 392 :: r386 in
  let r388 = Sub (r330) :: r387 in
  let r389 = [R 274] in
  let r390 = Sub (r388) :: r389 in
  let r391 = [R 534] in
  let r392 = S (T T_RBRACE) :: r391 in
  let r393 = [R 276] in
  let r394 = [R 391] in
  let r395 = [R 272] in
  let r396 = S (T T_UNDERSCORE) :: r278 in
  let r397 = [R 579] in
  let r398 = Sub (r396) :: r397 in
  let r399 = [R 429] in
  let r400 = Sub (r398) :: r399 in
  let r401 = [R 89] in
  let r402 = S (T T_INT) :: r401 in
  let r403 = [R 506] in
  let r404 = Sub (r402) :: r403 in
  let r405 = [R 582] in
  let r406 = [R 588] in
  let r407 = S (T T_RBRACKET) :: r406 in
  let r408 = S (T T_LBRACKET) :: r407 in
  let r409 = [R 589] in
  let r410 = [R 421] in
  let r411 = S (N N_pattern) :: r410 in
  let r412 = [R 424] in
  let r413 = [R 419] in
  let r414 = [R 428] in
  let r415 = [R 590] in
  let r416 = [R 425] in
  let r417 = [R 420] in
  let r418 = [R 417] in
  let r419 = [R 536] in
  let r420 = S (T T_BARRBRACKET) :: r419 in
  let r421 = [R 657] in
  let r422 = Sub (r1) :: r421 in
  let r423 = S (T T_EQUAL) :: r422 in
  let r424 = [R 279] in
  let r425 = [R 256] in
  let r426 = S (T T_LIDENT) :: r425 in
  let r427 = [R 264] in
  let r428 = [R 252] in
  let r429 = Sub (r426) :: r428 in
  let r430 = [R 263] in
  let r431 = S (T T_RPAREN) :: r430 in
  let r432 = [R 253] in
  let r433 = [R 260] in
  let r434 = [R 259] in
  let r435 = S (T T_RPAREN) :: r434 in
  let r436 = R 389 :: r435 in
  let r437 = [R 390] in
  let r438 = [R 287] in
  let r439 = R 17 :: r438 in
  let r440 = R 231 :: r439 in
  let r441 = Sub (r194) :: r440 in
  let r442 = [R 144] in
  let r443 = Sub (r1) :: r442 in
  let r444 = S (T T_IN) :: r443 in
  let r445 = Sub (r441) :: r444 in
  let r446 = R 206 :: r445 in
  let r447 = [R 278] in
  let r448 = R 452 :: r447 in
  let r449 = Sub (r375) :: r448 in
  let r450 = R 464 :: r449 in
  let r451 = R 206 :: r450 in
  let r452 = [R 145] in
  let r453 = Sub (r1) :: r452 in
  let r454 = S (T T_IN) :: r453 in
  let r455 = Sub (r265) :: r454 in
  let r456 = R 206 :: r455 in
  let r457 = [R 557] in
  let r458 = [R 204] in
  let r459 = S (N N_expr) :: r458 in
  let r460 = [R 560] in
  let r461 = S (T T_RBRACKET) :: r460 in
  let r462 = R 393 :: r461 in
  let r463 = [R 567] in
  let r464 = [R 214] in
  let r465 = [R 213] in
  let r466 = [R 268] in
  let r467 = R 396 :: r466 in
  let r468 = Sub (r330) :: r467 in
  let r469 = [R 269] in
  let r470 = Sub (r468) :: r469 in
  let r471 = [R 473] in
  let r472 = Sub (r470) :: r471 in
  let r473 = [R 554] in
  let r474 = S (T T_RBRACE) :: r473 in
  let r475 = [R 539] in
  let r476 = [R 538] in
  let r477 = S (T T_GREATERDOT) :: r476 in
  let r478 = [R 199] in
  let r479 = Sub (r33) :: r478 in
  let r480 = [R 546] in
  let r481 = S (T T_END) :: r480 in
  let r482 = [R 155] in
  let r483 = S (N N_expr) :: r482 in
  let r484 = S (T T_THEN) :: r483 in
  let r485 = Sub (r1) :: r484 in
  let r486 = [R 146] in
  let r487 = S (N N_match_cases) :: r486 in
  let r488 = R 387 :: r487 in
  let r489 = [R 295] in
  let r490 = Sub (r1) :: r489 in
  let r491 = S (T T_MINUSGREATER) :: r490 in
  let r492 = [R 296] in
  let r493 = Sub (r1) :: r492 in
  let r494 = S (T T_MINUSGREATER) :: r493 in
  let r495 = [R 266] in
  let r496 = Sub (r398) :: r495 in
  let r497 = [R 221] in
  let r498 = Sub (r1) :: r497 in
  let r499 = S (T T_MINUSGREATER) :: r498 in
  let r500 = [R 147] in
  let r501 = Sub (r499) :: r500 in
  let r502 = Sub (r496) :: r501 in
  let r503 = [R 441] in
  let r504 = S (T T_UNDERSCORE) :: r503 in
  let r505 = [R 262] in
  let r506 = [R 261] in
  let r507 = S (T T_RPAREN) :: r506 in
  let r508 = R 389 :: r507 in
  let r509 = [R 289] in
  let r510 = [R 290] in
  let r511 = S (T T_LIDENT) :: r510 in
  let r512 = [R 148] in
  let r513 = Sub (r499) :: r512 in
  let r514 = S (T T_RPAREN) :: r513 in
  let r515 = [R 139] in
  let r516 = S (T T_DONE) :: r515 in
  let r517 = Sub (r1) :: r516 in
  let r518 = S (T T_DO) :: r517 in
  let r519 = Sub (r1) :: r518 in
  let r520 = S (T T_IN) :: r519 in
  let r521 = S (N N_pattern) :: r520 in
  let r522 = [R 130] in
  let r523 = S (T T_DOWNTO) :: r522 in
  let r524 = [R 157] in
  let r525 = S (T T_DONE) :: r524 in
  let r526 = Sub (r1) :: r525 in
  let r527 = S (T T_DO) :: r526 in
  let r528 = Sub (r1) :: r527 in
  let r529 = Sub (r523) :: r528 in
  let r530 = Sub (r1) :: r529 in
  let r531 = S (T T_EQUAL) :: r530 in
  let r532 = S (N N_pattern) :: r531 in
  let r533 = [R 564] in
  let r534 = [R 550] in
  let r535 = S (T T_RPAREN) :: r534 in
  let r536 = S (T T_LPAREN) :: r535 in
  let r537 = S (T T_DOT) :: r536 in
  let r538 = [R 573] in
  let r539 = S (T T_RPAREN) :: r538 in
  let r540 = Sub (r93) :: r539 in
  let r541 = S (T T_COLON) :: r540 in
  let r542 = S (N N_module_expr) :: r541 in
  let r543 = [R 328] in
  let r544 = S (N N_module_expr) :: r543 in
  let r545 = S (T T_MINUSGREATER) :: r544 in
  let r546 = S (N N_functor_args) :: r545 in
  let r547 = [R 330] in
  let r548 = [R 408] in
  let r549 = S (T T_RPAREN) :: r548 in
  let r550 = [R 198] in
  let r551 = Sub (r33) :: r550 in
  let r552 = [R 570] in
  let r553 = [R 553] in
  let r554 = S (T T_RBRACE) :: r553 in
  let r555 = S (N N_expr) :: r554 in
  let r556 = S (T T_LBRACE) :: r555 in
  let r557 = [R 551] in
  let r558 = S (T T_RPAREN) :: r557 in
  let r559 = Sub (r1) :: r558 in
  let r560 = [R 179] in
  let r561 = [R 251] in
  let r562 = S (T T_LIDENT) :: r561 in
  let r563 = [R 248] in
  let r564 = [R 569] in
  let r565 = [R 249] in
  let r566 = [R 250] in
  let r567 = [R 247] in
  let r568 = [R 186] in
  let r569 = S (T T_RBRACKET) :: r568 in
  let r570 = S (N N_expr) :: r569 in
  let r571 = [R 188] in
  let r572 = S (T T_RPAREN) :: r571 in
  let r573 = [R 131] in
  let r574 = Sub (r1) :: r573 in
  let r575 = [R 142] in
  let r576 = Sub (r1) :: r575 in
  let r577 = [R 197] in
  let r578 = S (N N_expr) :: r577 in
  let r579 = [R 202] in
  let r580 = [R 169] in
  let r581 = [R 163] in
  let r582 = [R 180] in
  let r583 = [R 166] in
  let r584 = [R 170] in
  let r585 = [R 162] in
  let r586 = [R 165] in
  let r587 = [R 164] in
  let r588 = [R 174] in
  let r589 = [R 168] in
  let r590 = [R 167] in
  let r591 = [R 172] in
  let r592 = [R 161] in
  let r593 = [R 160] in
  let r594 = [R 158] in
  let r595 = [R 159] in
  let r596 = [R 173] in
  let r597 = [R 171] in
  let r598 = [R 175] in
  let r599 = [R 176] in
  let r600 = [R 177] in
  let r601 = [R 203] in
  let r602 = [R 178] in
  let r603 = [R 481] in
  let r604 = Sub (r1) :: r603 in
  let r605 = [R 10] in
  let r606 = R 452 :: r605 in
  let r607 = Sub (r375) :: r606 in
  let r608 = [R 284] in
  let r609 = Sub (r1) :: r608 in
  let r610 = S (T T_EQUAL) :: r609 in
  let r611 = [R 436] in
  let r612 = [R 437] in
  let r613 = [R 432] in
  let r614 = [R 433] in
  let r615 = [R 430] in
  let r616 = [R 187] in
  let r617 = [R 185] in
  let r618 = [R 190] in
  let r619 = S (T T_RBRACE) :: r618 in
  let r620 = [R 189] in
  let r621 = [R 182] in
  let r622 = [R 552] in
  let r623 = S (T T_RBRACKET) :: r622 in
  let r624 = Sub (r1) :: r623 in
  let r625 = [R 183] in
  let r626 = [R 184] in
  let r627 = [R 192] in
  let r628 = S (T T_RBRACKET) :: r627 in
  let r629 = S (N N_expr) :: r628 in
  let r630 = [R 194] in
  let r631 = S (T T_RPAREN) :: r630 in
  let r632 = [R 193] in
  let r633 = [R 191] in
  let r634 = [R 196] in
  let r635 = S (T T_RBRACE) :: r634 in
  let r636 = [R 195] in
  let r637 = [R 181] in
  let r638 = [R 549] in
  let r639 = [R 559] in
  let r640 = [R 558] in
  let r641 = S (T T_BARRBRACKET) :: r640 in
  let r642 = [R 562] in
  let r643 = [R 561] in
  let r644 = S (T T_RBRACKET) :: r643 in
  let r645 = Sub (r202) :: r464 in
  let r646 = [R 215] in
  let r647 = R 393 :: r646 in
  let r648 = Sub (r645) :: r647 in
  let r649 = [R 568] in
  let r650 = S (T T_GREATERRBRACE) :: r649 in
  let r651 = [R 555] in
  let r652 = S (T T_RBRACE) :: r651 in
  let r653 = [R 472] in
  let r654 = Sub (r470) :: r653 in
  let r655 = [R 695] in
  let r656 = [R 693] in
  let r657 = Sub (r63) :: r656 in
  let r658 = [R 694] in
  let r659 = [R 267] in
  let r660 = [R 138] in
  let r661 = S (T T_DONE) :: r660 in
  let r662 = Sub (r1) :: r661 in
  let r663 = S (T T_DO) :: r662 in
  let r664 = Sub (r1) :: r663 in
  let r665 = Sub (r523) :: r664 in
  let r666 = [R 224] in
  let r667 = Sub (r499) :: r666 in
  let r668 = S (T T_RPAREN) :: r667 in
  let r669 = [R 265] in
  let r670 = [R 222] in
  let r671 = Sub (r1) :: r670 in
  let r672 = S (T T_MINUSGREATER) :: r671 in
  let r673 = [R 223] in
  let r674 = [R 592] in
  let r675 = S (T T_RPAREN) :: r674 in
  let r676 = S (N N_pattern) :: r491 in
  let r677 = [R 299] in
  let r678 = [R 154] in
  let r679 = [R 545] in
  let r680 = [R 566] in
  let r681 = [R 556] in
  let r682 = S (T T_BARRBRACKET) :: r681 in
  let r683 = [R 143] in
  let r684 = Sub (r1) :: r683 in
  let r685 = S (T T_IN) :: r684 in
  let r686 = Sub (r299) :: r685 in
  let r687 = S (T T_UIDENT) :: r686 in
  let r688 = [R 320] in
  let r689 = S (N N_module_expr) :: r688 in
  let r690 = S (T T_EQUAL) :: r689 in
  let r691 = [R 321] in
  let r692 = [R 219] in
  let r693 = Sub (r423) :: r692 in
  let r694 = [R 659] in
  let r695 = Sub (r693) :: r694 in
  let r696 = S (T T_RPAREN) :: r695 in
  let r697 = Sub (r511) :: r696 in
  let r698 = [R 220] in
  let r699 = Sub (r1) :: r698 in
  let r700 = [R 658] in
  let r701 = [R 282] in
  let r702 = Sub (r1) :: r701 in
  let r703 = S (T T_EQUAL) :: r702 in
  let r704 = Sub (r63) :: r703 in
  let r705 = S (T T_DOT) :: r704 in
  let r706 = [R 281] in
  let r707 = Sub (r1) :: r706 in
  let r708 = S (T T_EQUAL) :: r707 in
  let r709 = Sub (r63) :: r708 in
  let r710 = [R 280] in
  let r711 = Sub (r1) :: r710 in
  let r712 = [R 413] in
  let r713 = S (T T_RPAREN) :: r712 in
  let r714 = [R 411] in
  let r715 = S (T T_RPAREN) :: r714 in
  let r716 = [R 412] in
  let r717 = S (T T_RPAREN) :: r716 in
  let r718 = [R 240] in
  let r719 = S (T T_RBRACKET) :: r718 in
  let r720 = Sub (r15) :: r719 in
  let r721 = [R 445] in
  let r722 = [R 446] in
  let r723 = [R 218] in
  let r724 = S (T T_RBRACKET) :: r723 in
  let r725 = Sub (r15) :: r724 in
  let r726 = [R 655] in
  let r727 = R 452 :: r726 in
  let r728 = S (N N_module_expr) :: r727 in
  let r729 = [R 455] in
  let r730 = S (T T_STRING) :: r729 in
  let r731 = [R 454] in
  let r732 = R 452 :: r731 in
  let r733 = Sub (r730) :: r732 in
  let r734 = S (T T_EQUAL) :: r733 in
  let r735 = Sub (r63) :: r734 in
  let r736 = S (T T_COLON) :: r735 in
  let r737 = Sub (r51) :: r736 in
  let r738 = [R 648] in
  let r739 = R 452 :: r738 in
  let r740 = R 17 :: r739 in
  let r741 = Sub (r249) :: r740 in
  let r742 = S (T T_EQUAL) :: r741 in
  let r743 = Sub (r194) :: r742 in
  let r744 = [R 482] in
  let r745 = R 452 :: r744 in
  let r746 = R 17 :: r745 in
  let r747 = R 231 :: r746 in
  let r748 = Sub (r194) :: r747 in
  let r749 = R 206 :: r748 in
  let r750 = [R 443] in
  let r751 = [R 489] in
  let r752 = [R 469] in
  let r753 = R 452 :: r752 in
  let r754 = S (N N_module_type) :: r753 in
  let r755 = S (T T_COLON) :: r754 in
  let r756 = S (T T_UIDENT) :: r755 in
  let r757 = S (T T_REC) :: r756 in
  let r758 = [R 323] in
  let r759 = S (N N_module_type) :: r758 in
  let r760 = S (T T_COLON) :: r759 in
  let r761 = [R 322] in
  let r762 = R 452 :: r761 in
  let r763 = [R 325] in
  let r764 = Sub (r760) :: r763 in
  let r765 = [R 324] in
  let r766 = Sub (r760) :: r765 in
  let r767 = S (T T_RPAREN) :: r766 in
  let r768 = S (N N_module_type) :: r767 in
  let r769 = [R 317] in
  let r770 = R 452 :: r769 in
  let r771 = [R 486] in
  let r772 = R 452 :: r771 in
  let r773 = S (N N_module_type) :: r772 in
  let r774 = [R 87] in
  let r775 = S (T T_LIDENT) :: r774 in
  let r776 = [R 66] in
  let r777 = Sub (r775) :: r776 in
  let r778 = [R 82] in
  let r779 = R 452 :: r778 in
  let r780 = Sub (r777) :: r779 in
  let r781 = S (T T_EQUAL) :: r780 in
  let r782 = S (T T_LIDENT) :: r781 in
  let r783 = R 85 :: r782 in
  let r784 = R 733 :: r783 in
  let r785 = R 206 :: r784 in
  let r786 = [R 715] in
  let r787 = Sub (r81) :: r786 in
  let r788 = S (T T_QUOTE) :: r787 in
  let r789 = [R 712] in
  let r790 = Sub (r788) :: r789 in
  let r791 = R 716 :: r790 in
  let r792 = [R 713] in
  let r793 = Sub (r791) :: r792 in
  let r794 = [R 86] in
  let r795 = S (T T_RBRACKET) :: r794 in
  let r796 = [R 714] in
  let r797 = [R 56] in
  let r798 = R 63 :: r797 in
  let r799 = R 55 :: r798 in
  let r800 = [R 67] in
  let r801 = S (T T_END) :: r800 in
  let r802 = Sub (r799) :: r801 in
  let r803 = [R 54] in
  let r804 = S (T T_RPAREN) :: r803 in
  let r805 = [R 732] in
  let r806 = Sub (r63) :: r805 in
  let r807 = S (T T_COLON) :: r806 in
  let r808 = Sub (r202) :: r807 in
  let r809 = [R 58] in
  let r810 = R 452 :: r809 in
  let r811 = Sub (r808) :: r810 in
  let r812 = [R 730] in
  let r813 = Sub (r63) :: r812 in
  let r814 = S (T T_COLON) :: r813 in
  let r815 = Sub (r202) :: r814 in
  let r816 = [R 731] in
  let r817 = Sub (r63) :: r816 in
  let r818 = S (T T_COLON) :: r817 in
  let r819 = Sub (r202) :: r818 in
  let r820 = [R 447] in
  let r821 = Sub (r63) :: r820 in
  let r822 = [R 59] in
  let r823 = R 452 :: r822 in
  let r824 = Sub (r821) :: r823 in
  let r825 = S (T T_COLON) :: r824 in
  let r826 = Sub (r202) :: r825 in
  let r827 = R 459 :: r826 in
  let r828 = [R 448] in
  let r829 = Sub (r63) :: r828 in
  let r830 = [R 57] in
  let r831 = R 452 :: r830 in
  let r832 = Sub (r777) :: r831 in
  let r833 = [R 70] in
  let r834 = Sub (r777) :: r833 in
  let r835 = S (T T_IN) :: r834 in
  let r836 = Sub (r265) :: r835 in
  let r837 = R 17 :: r836 in
  let r838 = R 405 :: r837 in
  let r839 = Sub (r63) :: r191 in
  let r840 = [R 65] in
  let r841 = Sub (r775) :: r840 in
  let r842 = S (T T_RBRACKET) :: r841 in
  let r843 = [R 88] in
  let r844 = S (T T_LIDENT) :: r843 in
  let r845 = [R 107] in
  let r846 = Sub (r63) :: r845 in
  let r847 = S (T T_EQUAL) :: r846 in
  let r848 = Sub (r63) :: r847 in
  let r849 = [R 60] in
  let r850 = R 452 :: r849 in
  let r851 = Sub (r848) :: r850 in
  let r852 = [R 61] in
  let r853 = [R 77] in
  let r854 = Sub (r777) :: r853 in
  let r855 = [R 25] in
  let r856 = R 452 :: r855 in
  let r857 = Sub (r854) :: r856 in
  let r858 = S (T T_COLON) :: r857 in
  let r859 = S (T T_LIDENT) :: r858 in
  let r860 = R 85 :: r859 in
  let r861 = [R 78] in
  let r862 = Sub (r854) :: r861 in
  let r863 = S (T T_MINUSGREATER) :: r862 in
  let r864 = Sub (r57) :: r863 in
  let r865 = S (T T_COLON) :: r864 in
  let r866 = [R 79] in
  let r867 = Sub (r854) :: r866 in
  let r868 = S (T T_MINUSGREATER) :: r867 in
  let r869 = [R 80] in
  let r870 = Sub (r854) :: r869 in
  let r871 = S (T T_MINUSGREATER) :: r870 in
  let r872 = [R 81] in
  let r873 = Sub (r854) :: r872 in
  let r874 = [R 13] in
  let r875 = R 452 :: r874 in
  let r876 = R 109 :: r875 in
  let r877 = R 699 :: r876 in
  let r878 = S (T T_LIDENT) :: r877 in
  let r879 = R 400 :: r878 in
  let r880 = [R 490] in
  let r881 = [R 12] in
  let r882 = R 452 :: r881 in
  let r883 = S (N N_module_type) :: r882 in
  let r884 = S (T T_COLON) :: r883 in
  let r885 = S (T T_UIDENT) :: r884 in
  let r886 = [R 504] in
  let r887 = [R 9] in
  let r888 = R 452 :: r887 in
  let r889 = Sub (r777) :: r888 in
  let r890 = S (T T_EQUAL) :: r889 in
  let r891 = S (T T_LIDENT) :: r890 in
  let r892 = R 85 :: r891 in
  let r893 = R 733 :: r892 in
  let r894 = [R 8] in
  let r895 = R 452 :: r894 in
  let r896 = Sub (r854) :: r895 in
  let r897 = S (T T_COLON) :: r896 in
  let r898 = S (T T_LIDENT) :: r897 in
  let r899 = R 85 :: r898 in
  let r900 = R 733 :: r899 in
  let r901 = [R 72] in
  let r902 = Sub (r36) :: r901 in
  let r903 = [R 28] in
  let r904 = Sub (r902) :: r903 in
  let r905 = [R 44] in
  let r906 = Sub (r904) :: r905 in
  let r907 = S (T T_EQUAL) :: r906 in
  let r908 = [R 22] in
  let r909 = R 452 :: r908 in
  let r910 = Sub (r907) :: r909 in
  let r911 = S (T T_LIDENT) :: r910 in
  let r912 = R 85 :: r911 in
  let r913 = [R 73] in
  let r914 = S (T T_END) :: r913 in
  let r915 = Sub (r272) :: r914 in
  let r916 = [R 727] in
  let r917 = Sub (r1) :: r916 in
  let r918 = S (T T_EQUAL) :: r917 in
  let r919 = Sub (r202) :: r918 in
  let r920 = R 348 :: r919 in
  let r921 = R 17 :: r920 in
  let r922 = R 405 :: r921 in
  let r923 = [R 36] in
  let r924 = R 452 :: r923 in
  let r925 = [R 726] in
  let r926 = Sub (r63) :: r925 in
  let r927 = S (T T_COLON) :: r926 in
  let r928 = Sub (r202) :: r927 in
  let r929 = [R 725] in
  let r930 = Sub (r63) :: r929 in
  let r931 = S (T T_COLON) :: r930 in
  let r932 = [R 728] in
  let r933 = Sub (r1) :: r932 in
  let r934 = [R 309] in
  let r935 = Sub (r423) :: r934 in
  let r936 = Sub (r202) :: r935 in
  let r937 = R 457 :: r936 in
  let r938 = R 17 :: r937 in
  let r939 = R 405 :: r938 in
  let r940 = [R 37] in
  let r941 = R 452 :: r940 in
  let r942 = [R 308] in
  let r943 = Sub (r821) :: r942 in
  let r944 = S (T T_COLON) :: r943 in
  let r945 = Sub (r202) :: r944 in
  let r946 = [R 307] in
  let r947 = Sub (r821) :: r946 in
  let r948 = S (T T_COLON) :: r947 in
  let r949 = [R 310] in
  let r950 = Sub (r1) :: r949 in
  let r951 = S (T T_EQUAL) :: r950 in
  let r952 = [R 311] in
  let r953 = Sub (r1) :: r952 in
  let r954 = S (T T_EQUAL) :: r953 in
  let r955 = Sub (r63) :: r954 in
  let r956 = S (T T_DOT) :: r955 in
  let r957 = [R 39] in
  let r958 = R 452 :: r957 in
  let r959 = Sub (r1) :: r958 in
  let r960 = [R 35] in
  let r961 = R 452 :: r960 in
  let r962 = R 415 :: r961 in
  let r963 = Sub (r904) :: r962 in
  let r964 = R 17 :: r963 in
  let r965 = [R 75] in
  let r966 = S (T T_RPAREN) :: r965 in
  let r967 = [R 32] in
  let r968 = Sub (r904) :: r967 in
  let r969 = S (T T_IN) :: r968 in
  let r970 = Sub (r265) :: r969 in
  let r971 = R 17 :: r970 in
  let r972 = R 405 :: r971 in
  let r973 = [R 71] in
  let r974 = Sub (r36) :: r973 in
  let r975 = S (T T_RBRACKET) :: r974 in
  let r976 = [R 47] in
  let r977 = Sub (r904) :: r976 in
  let r978 = S (T T_MINUSGREATER) :: r977 in
  let r979 = Sub (r496) :: r978 in
  let r980 = [R 29] in
  let r981 = Sub (r979) :: r980 in
  let r982 = [R 31] in
  let r983 = Sub (r904) :: r982 in
  let r984 = [R 74] in
  let r985 = S (T T_RPAREN) :: r984 in
  let r986 = [R 414] in
  let r987 = [R 38] in
  let r988 = R 452 :: r987 in
  let r989 = Sub (r848) :: r988 in
  let r990 = [R 40] in
  let r991 = [R 45] in
  let r992 = Sub (r904) :: r991 in
  let r993 = S (T T_EQUAL) :: r992 in
  let r994 = [R 46] in
  let r995 = [R 661] in
  let r996 = [R 681] in
  let r997 = [R 11] in
  let r998 = R 452 :: r997 in
  let r999 = Sub (r299) :: r998 in
  let r1000 = S (T T_UIDENT) :: r999 in
  let r1001 = [R 677] in
  let r1002 = [R 7] in
  let r1003 = R 452 :: r1002 in
  let r1004 = Sub (r907) :: r1003 in
  let r1005 = S (T T_LIDENT) :: r1004 in
  let r1006 = R 85 :: r1005 in
  let r1007 = R 733 :: r1006 in
  let r1008 = [R 660] in
  let r1009 = R 679 :: r1008 in
  let r1010 = [R 52] in
  let r1011 = S (T T_RPAREN) :: r1010 in
  let r1012 = [R 483] in
  let r1013 = Sub (r237) :: r1012 in
  let r1014 = [R 487] in
  let r1015 = R 452 :: r1014 in
  let r1016 = Sub (r1013) :: r1015 in
  let r1017 = R 457 :: r1016 in
  let r1018 = [R 571] in
  let r1019 = S (T T_RPAREN) :: r1018 in
  let r1020 = S (N N_module_expr) :: r1019 in
  let r1021 = [R 572] in
  let r1022 = S (T T_RPAREN) :: r1021 in
  let r1023 = [R 547] in
  let r1024 = [R 134] in
  let r1025 = S (N N_match_cases) :: r1024 in
  let r1026 = [R 136] in
  let r1027 = [R 135] in
  let r1028 = [R 237] in
  let r1029 = [R 239] in
  let r1030 = [R 416] in
  function
  | 0 | 1595 | 1599 -> Nothing
  | 1594 -> One ([R 0])
  | 1598 -> One ([R 1])
  | 1602 -> One ([R 2])
  | 402 -> One ([R 3])
  | 401 -> One ([R 4])
  | 79 -> One (R 17 :: r42)
  | 81 -> One (R 17 :: r43)
  | 145 -> One (R 17 :: r100)
  | 206 -> One (R 17 :: r152)
  | 433 -> One (R 17 :: r296)
  | 441 -> One (R 17 :: r316)
  | 505 -> One (R 17 :: r356)
  | 672 -> One (R 17 :: r546)
  | 793 -> One (R 17 :: r607)
  | 1176 -> One (R 17 :: r802)
  | 1185 -> One (R 17 :: r811)
  | 1202 -> One (R 17 :: r827)
  | 1217 -> One (R 17 :: r832)
  | 1239 -> One (R 17 :: r851)
  | 1286 -> One (R 17 :: r879)
  | 1301 -> One (R 17 :: r885)
  | 1318 -> One (R 17 :: r893)
  | 1329 -> One (R 17 :: r900)
  | 1348 -> One (R 17 :: r915)
  | 1404 -> One (R 17 :: r959)
  | 1422 -> One (R 17 :: r981)
  | 1448 -> One (R 17 :: r989)
  | 1477 -> One (R 17 :: r1000)
  | 1495 -> One (R 17 :: r1007)
  | 1503 -> One ([R 23])
  | 1502 -> One ([R 24])
  | 1338 -> One ([R 26])
  | 1337 -> One ([R 27])
  | 1430 -> One ([R 30])
  | 1433 -> One ([R 33])
  | 1428 -> One ([R 34])
  | 1454 -> One ([R 41])
  | 1455 -> One ([R 43])
  | 1435 -> One ([R 48])
  | 1248 -> One ([R 62])
  | 1249 -> One ([R 64])
  | 1236 -> One ([R 68])
  | 1233 -> One ([R 69])
  | 1327 -> One ([R 83])
  | 1326 -> One ([R 84])
  | 538 -> One ([R 90])
  | 68 -> One ([R 91])
  | 537 -> One ([R 92])
  | 167 | 291 -> One ([R 93])
  | 168 -> One ([R 98])
  | 363 -> One ([R 99])
  | 67 -> One ([R 105])
  | 323 -> One ([R 113])
  | 327 -> One ([R 114])
  | 322 -> One ([R 115])
  | 284 -> One ([R 117])
  | 932 -> One ([R 129])
  | 708 -> One ([R 140])
  | 870 -> One ([R 141])
  | 734 -> One ([R 151])
  | 743 -> One ([R 152])
  | 723 -> One ([R 153])
  | 741 -> One ([R 201])
  | 891 -> One ([R 205])
  | 1 -> One (R 206 :: r6)
  | 60 -> One (R 206 :: r23)
  | 63 -> One (R 206 :: r26)
  | 65 -> One (R 206 :: r31)
  | 71 -> One (R 206 :: r38)
  | 91 -> One (R 206 :: r68)
  | 410 -> One (R 206 :: r275)
  | 424 -> One (R 206 :: r287)
  | 509 -> One (R 206 :: r361)
  | 511 -> One (R 206 :: r366)
  | 514 -> One (R 206 :: r379)
  | 534 -> One (R 206 :: r400)
  | 552 -> One (R 206 :: r411)
  | 621 -> One (R 206 :: r479)
  | 623 -> One (R 206 :: r481)
  | 625 -> One (R 206 :: r485)
  | 627 -> One (R 206 :: r488)
  | 632 -> One (R 206 :: r502)
  | 652 -> One (R 206 :: r521)
  | 656 -> One (R 206 :: r532)
  | 670 -> One (R 206 :: r542)
  | 700 -> One (R 206 :: r551)
  | 1009 -> One (R 206 :: r687)
  | 1102 -> One (R 206 :: r728)
  | 1106 -> One (R 206 :: r737)
  | 1128 -> One (R 206 :: r757)
  | 1151 -> One (R 206 :: r773)
  | 1557 -> One (R 206 :: r1020)
  | 905 -> One ([R 216])
  | 445 -> One ([R 227])
  | 444 -> One ([R 228])
  | 494 -> One ([R 229])
  | 495 -> One ([R 230])
  | 324 -> One (R 231 :: r223)
  | 134 | 487 -> One ([R 235])
  | 249 -> One ([R 238])
  | 308 -> One ([R 245])
  | 309 -> One ([R 246])
  | 871 -> One ([R 257])
  | 873 -> One ([R 258])
  | 913 -> One ([R 270])
  | 912 -> One ([R 271])
  | 524 -> One ([R 275])
  | 528 -> One ([R 277])
  | 731 -> One ([R 285])
  | 818 -> One ([R 286])
  | 637 -> One ([R 288])
  | 648 -> One ([R 291])
  | 727 -> One ([R 293])
  | 819 -> One ([R 294])
  | 978 -> One ([R 297])
  | 983 -> One ([R 298])
  | 269 -> One ([R 300])
  | 267 -> One ([R 301])
  | 268 -> One ([R 302])
  | 270 -> One ([R 303])
  | 266 -> One ([R 304])
  | 248 -> One ([R 305])
  | 247 -> One ([R 306])
  | 687 -> One ([R 326])
  | 685 -> One ([R 329])
  | 676 -> One ([R 331])
  | 686 -> One ([R 332])
  | 688 -> One ([R 333])
  | 449 -> One ([R 334])
  | 490 -> One ([R 341])
  | 484 -> One ([R 342])
  | 489 -> One ([R 346])
  | 1187 -> One (R 348 :: r815)
  | 1359 -> One (R 348 :: r928)
  | 297 | 1364 -> One ([R 349])
  | 244 -> One ([R 352])
  | 149 -> One ([R 354])
  | 87 | 94 -> One ([R 356])
  | 107 -> One ([R 357])
  | 106 -> One ([R 358])
  | 105 -> One ([R 359])
  | 104 -> One ([R 360])
  | 103 -> One ([R 361])
  | 113 -> One ([R 363])
  | 116 -> One ([R 365])
  | 119 -> One ([R 367])
  | 85 -> One ([R 368])
  | 122 | 696 -> One ([R 369])
  | 97 | 423 | 669 -> One ([R 370])
  | 96 | 668 -> One ([R 371])
  | 101 | 695 | 965 -> One ([R 372])
  | 100 | 694 -> One ([R 373])
  | 84 -> One ([R 374])
  | 109 -> One ([R 375])
  | 102 -> One ([R 376])
  | 108 -> One ([R 377])
  | 99 -> One ([R 378])
  | 121 -> One ([R 379])
  | 123 -> One ([R 380])
  | 120 -> One ([R 382])
  | 95 -> One ([R 383])
  | 98 -> One ([R 384])
  | 208 -> One ([R 385])
  | 207 -> One (R 386 :: r157)
  | 176 -> One (R 387 :: r134)
  | 1573 -> One (R 387 :: r1025)
  | 177 -> One ([R 388])
  | 525 -> One (R 393 :: r393)
  | 588 -> One (R 393 :: r420)
  | 889 -> One (R 393 :: r641)
  | 897 -> One (R 393 :: r644)
  | 1005 -> One (R 393 :: r682)
  | 526 | 579 | 890 | 904 -> One ([R 394])
  | 921 -> One ([R 395])
  | 452 -> One (R 400 :: r334)
  | 388 -> One ([R 401])
  | 403 -> One (R 405 :: r269)
  | 606 -> One (R 405 :: r456)
  | 1408 -> One (R 405 :: r964)
  | 404 -> One ([R 406])
  | 556 -> One ([R 418])
  | 561 -> One ([R 422])
  | 555 -> One ([R 423])
  | 554 -> One ([R 426])
  | 800 -> One ([R 431])
  | 814 -> One ([R 434])
  | 580 -> One ([R 439])
  | 643 -> One ([R 440])
  | 1341 -> One ([R 444])
  | 374 -> One (R 452 :: r259)
  | 1246 -> One (R 452 :: r852)
  | 1314 -> One (R 452 :: r886)
  | 1452 -> One (R 452 :: r990)
  | 1490 -> One (R 452 :: r1001)
  | 1505 -> One (R 452 :: r1009)
  | 1113 -> One ([R 456])
  | 334 -> One (R 457 :: r228)
  | 1379 -> One (R 457 :: r945)
  | 335 | 1384 -> One ([R 458])
  | 1206 -> One ([R 460])
  | 1204 -> One ([R 461])
  | 1207 -> One ([R 462])
  | 1205 -> One ([R 463])
  | 516 -> One ([R 465])
  | 1483 -> One ([R 467])
  | 1482 -> One ([R 468])
  | 1308 -> One ([R 470])
  | 1307 -> One ([R 471])
  | 188 -> One ([R 474])
  | 788 -> One ([R 479])
  | 792 -> One ([R 480])
  | 1541 -> One ([R 484])
  | 1538 -> One ([R 485])
  | 1126 -> One (R 488 :: r750)
  | 1127 -> One (R 488 :: r751)
  | 1295 -> One (R 488 :: r880)
  | 1284 -> One ([R 491])
  | 1309 -> One ([R 492])
  | 1285 -> One ([R 493])
  | 1297 -> One ([R 494])
  | 1299 -> One ([R 495])
  | 1312 -> One ([R 496])
  | 1313 -> One ([R 497])
  | 1300 -> One ([R 498])
  | 1311 -> One ([R 499])
  | 1310 -> One ([R 500])
  | 1298 -> One ([R 501])
  | 1328 -> One ([R 502])
  | 1317 -> One ([R 503])
  | 1316 -> One ([R 505])
  | 421 -> One ([R 508])
  | 418 -> One ([R 510])
  | 187 -> One ([R 515])
  | 192 -> One ([R 516])
  | 281 -> One ([R 517])
  | 214 | 1276 -> One ([R 531])
  | 661 -> One ([R 540])
  | 699 -> One ([R 541])
  | 698 | 742 -> One ([R 542])
  | 663 | 722 -> One ([R 543])
  | 867 | 884 -> One ([R 548])
  | 697 -> One ([R 574])
  | 874 -> One ([R 576])
  | 872 -> One ([R 577])
  | 539 -> One ([R 578])
  | 543 -> One ([R 581])
  | 585 -> One ([R 583])
  | 547 -> One ([R 584])
  | 542 -> One ([R 586])
  | 584 -> One ([R 587])
  | 564 -> One ([R 595])
  | 28 -> One ([R 596])
  | 8 -> One ([R 597])
  | 52 -> One ([R 599])
  | 51 -> One ([R 600])
  | 50 -> One ([R 601])
  | 49 -> One ([R 602])
  | 48 -> One ([R 603])
  | 47 -> One ([R 604])
  | 46 -> One ([R 605])
  | 45 -> One ([R 606])
  | 44 -> One ([R 607])
  | 43 -> One ([R 608])
  | 42 -> One ([R 609])
  | 41 -> One ([R 610])
  | 40 -> One ([R 611])
  | 39 -> One ([R 612])
  | 38 -> One ([R 613])
  | 37 -> One ([R 614])
  | 36 -> One ([R 615])
  | 35 -> One ([R 616])
  | 34 -> One ([R 617])
  | 33 -> One ([R 618])
  | 32 -> One ([R 619])
  | 31 -> One ([R 620])
  | 30 -> One ([R 621])
  | 29 -> One ([R 622])
  | 27 -> One ([R 623])
  | 26 -> One ([R 624])
  | 25 -> One ([R 625])
  | 24 -> One ([R 626])
  | 23 -> One ([R 627])
  | 22 -> One ([R 628])
  | 21 -> One ([R 629])
  | 20 -> One ([R 630])
  | 19 -> One ([R 631])
  | 18 -> One ([R 632])
  | 17 -> One ([R 633])
  | 16 -> One ([R 634])
  | 15 -> One ([R 635])
  | 14 -> One ([R 636])
  | 13 -> One ([R 637])
  | 12 -> One ([R 638])
  | 11 -> One ([R 639])
  | 10 -> One ([R 640])
  | 9 -> One ([R 641])
  | 7 -> One ([R 642])
  | 6 -> One ([R 643])
  | 5 -> One ([R 644])
  | 4 -> One ([R 645])
  | 3 -> One ([R 646])
  | 1475 -> One ([R 647])
  | 387 -> One ([R 650])
  | 378 -> One ([R 651])
  | 386 -> One ([R 652])
  | 377 -> One ([R 653])
  | 376 -> One ([R 654])
  | 1468 -> One ([R 662])
  | 1488 | 1508 -> One ([R 663])
  | 1489 | 1509 -> One ([R 664])
  | 1484 -> One ([R 665])
  | 1465 -> One ([R 666])
  | 1466 -> One ([R 667])
  | 1472 -> One ([R 668])
  | 1474 -> One ([R 669])
  | 1487 -> One ([R 670])
  | 1476 -> One ([R 671])
  | 1486 -> One ([R 672])
  | 1485 -> One ([R 673])
  | 1494 -> One ([R 674])
  | 1493 -> One ([R 675])
  | 1473 -> One ([R 676])
  | 1492 -> One ([R 678])
  | 1469 -> One (R 679 :: r996)
  | 508 -> One ([R 682])
  | 507 -> One ([R 683])
  | 392 -> One ([R 687])
  | 393 -> One ([R 688])
  | 395 -> One ([R 689])
  | 397 -> One ([R 690])
  | 394 -> One ([R 691])
  | 391 -> One ([R 692])
  | 1294 -> One ([R 697])
  | 1293 -> One ([R 698])
  | 333 -> One ([R 700])
  | 320 -> One ([R 701])
  | 342 -> One ([R 702])
  | 321 -> One ([R 703])
  | 341 -> One ([R 707])
  | 151 -> One ([R 717])
  | 152 -> One ([R 718])
  | 396 -> One ([R 723])
  | 399 -> One ([R 724])
  | 1192 -> One (R 733 :: r819)
  | 1252 -> One (R 733 :: r860)
  | 1343 -> One (R 733 :: r912)
  | 1161 -> One ([R 734])
  | 459 -> One ([R 741])
  | 460 -> One ([R 742])
  | 908 -> One (S (T T_WITH) :: r654)
  | 398 | 408 -> One (S (T T_UIDENT) :: r41)
  | 197 -> One (S (T T_UIDENT) :: r150)
  | 429 -> One (S (T T_TYPE) :: r293)
  | 1037 -> One (S (T T_TYPE) :: r697)
  | 1158 | 1342 -> One (S (T T_TYPE) :: r785)
  | 357 -> One (S (T T_RPAREN) :: r44)
  | 170 | 292 -> One (S (T T_RPAREN) :: r122)
  | 274 -> One (S (T T_RPAREN) :: r187)
  | 277 -> One (S (T T_RPAREN) :: r188)
  | 358 -> One (S (T T_RPAREN) :: r254)
  | 443 -> One (S (T T_RPAREN) :: r317)
  | 550 -> One (S (T T_RPAREN) :: r409)
  | 568 -> One (S (T T_RPAREN) :: r415)
  | 678 -> One (S (T T_RPAREN) :: r547)
  | 885 -> One (S (T T_RPAREN) :: r638)
  | 1131 -> One (S (T T_RPAREN) :: r764)
  | 1566 -> One (S (T T_RPAREN) :: r1023)
  | 114 -> One (S (T T_RBRACKET) :: r76)
  | 180 -> One (S (T T_RBRACKET) :: r135)
  | 231 -> One (S (T T_RBRACKET) :: r166)
  | 286 | 293 -> One (S (T T_RBRACKET) :: r192)
  | 360 -> One (S (T T_RBRACKET) :: r255)
  | 895 -> One (S (T T_RBRACKET) :: r642)
  | 117 -> One (S (T T_RBRACE) :: r77)
  | 222 -> One (S (T T_QUOTE) :: r164)
  | 351 -> One (S (T T_PLUSEQ) :: r243)
  | 1531 -> One (S (T T_PLUSEQ) :: r1017)
  | 1220 -> One (S (T T_OPEN) :: r838)
  | 1412 -> One (S (T T_OPEN) :: r972)
  | 141 -> One (S (T T_MODULE) :: r97)
  | 315 -> One (S (T T_MINUSGREATER) :: r221)
  | 1271 -> One (S (T T_MINUSGREATER) :: r873)
  | 110 -> One (S (T T_LPAREN) :: r75)
  | 364 -> One (S (T T_LPAREN) :: r258)
  | 137 -> One (S (T T_LIDENT) :: r88)
  | 1257 -> One (S (T T_LIDENT) :: r865)
  | 1444 -> One (S (T T_LIDENT) :: r986)
  | 732 -> One (S (T T_LESSMINUS) :: r578)
  | 720 -> One (S (T T_LBRACKET) :: r570)
  | 851 -> One (S (T T_LBRACKET) :: r629)
  | 329 -> One (S (T T_LBRACE) :: r226)
  | 416 -> One (S (T T_INT) :: r281)
  | 419 -> One (S (T T_INT) :: r282)
  | 724 -> One (S (T T_IN) :: r574)
  | 728 -> One (S (T T_IN) :: r576)
  | 1426 -> One (S (T T_IN) :: r983)
  | 613 -> One (S (T T_GREATERRBRACE) :: r463)
  | 999 -> One (S (T T_GREATERRBRACE) :: r680)
  | 174 -> One (S (T T_GREATER) :: r127)
  | 250 -> One (S (T T_GREATER) :: r175)
  | 380 -> One (S (T T_EQUAL) :: r262)
  | 1043 -> One (S (T T_EQUAL) :: r699)
  | 1062 -> One (S (T T_EQUAL) :: r711)
  | 1373 -> One (S (T T_EQUAL) :: r933)
  | 1592 -> One (S (T T_EOF) :: r1028)
  | 1596 -> One (S (T T_EOF) :: r1029)
  | 1600 -> One (S (T T_EOF) :: r1030)
  | 990 -> One (S (T T_END) :: r679)
  | 166 -> One (S (T T_DOTDOT) :: r120)
  | 169 -> One (S (T T_DOTDOT) :: r121)
  | 74 -> One (S (T T_DOT) :: r40)
  | 256 -> One (S (T T_DOT) :: r185)
  | 455 | 850 -> One (S (T T_DOT) :: r336)
  | 485 -> One (S (T T_DOT) :: r348)
  | 548 -> One (S (T T_DOT) :: r408)
  | 1057 -> One (S (T T_DOT) :: r709)
  | 1211 -> One (S (T T_DOT) :: r829)
  | 1229 -> One (S (T T_DOT) :: r844)
  | 252 -> One (S (T T_COLON) :: r182)
  | 447 -> One (S (T T_COLON) :: r320)
  | 1132 -> One (S (T T_COLON) :: r768)
  | 518 -> One (S (T T_BARRBRACKET) :: r380)
  | 611 -> One (S (T T_BARRBRACKET) :: r457)
  | 887 -> One (S (T T_BARRBRACKET) :: r639)
  | 183 | 1269 -> One (S (T T_BAR) :: r140)
  | 233 -> One (S (T T_BAR) :: r169)
  | 400 -> One (S (N N_structure) :: r264)
  | 1467 -> One (S (N N_structure) :: r995)
  | 412 -> One (S (N N_pattern) :: r277)
  | 645 | 951 -> One (S (N N_pattern) :: r284)
  | 533 -> One (S (N N_pattern) :: r395)
  | 557 -> One (S (N N_pattern) :: r412)
  | 559 -> One (S (N N_pattern) :: r413)
  | 562 -> One (S (N N_pattern) :: r414)
  | 570 -> One (S (N N_pattern) :: r416)
  | 572 -> One (S (N N_pattern) :: r417)
  | 801 -> One (S (N N_pattern) :: r611)
  | 806 -> One (S (N N_pattern) :: r612)
  | 808 -> One (S (N N_pattern) :: r613)
  | 810 -> One (S (N N_pattern) :: r614)
  | 1096 -> One (S (N N_pattern) :: r721)
  | 439 -> One (S (N N_module_type) :: r310)
  | 440 -> One (S (N N_module_type) :: r312)
  | 482 -> One (S (N N_module_type) :: r346)
  | 682 -> One (S (N N_module_type) :: r549)
  | 1012 -> One (S (N N_module_type) :: r690)
  | 504 -> One (S (N N_module_expr) :: r353)
  | 636 -> One (S (N N_let_pattern) :: r508)
  | 616 -> One (S (N N_expr) :: r465)
  | 620 -> One (S (N N_expr) :: r477)
  | 707 -> One (S (N N_expr) :: r560)
  | 721 -> One (S (N N_expr) :: r572)
  | 735 -> One (S (N N_expr) :: r579)
  | 737 -> One (S (N N_expr) :: r580)
  | 739 -> One (S (N N_expr) :: r581)
  | 744 -> One (S (N N_expr) :: r582)
  | 746 -> One (S (N N_expr) :: r583)
  | 748 -> One (S (N N_expr) :: r584)
  | 750 -> One (S (N N_expr) :: r585)
  | 752 -> One (S (N N_expr) :: r586)
  | 754 -> One (S (N N_expr) :: r587)
  | 756 -> One (S (N N_expr) :: r588)
  | 758 -> One (S (N N_expr) :: r589)
  | 760 -> One (S (N N_expr) :: r590)
  | 762 -> One (S (N N_expr) :: r591)
  | 764 -> One (S (N N_expr) :: r592)
  | 766 -> One (S (N N_expr) :: r593)
  | 768 -> One (S (N N_expr) :: r594)
  | 770 -> One (S (N N_expr) :: r595)
  | 772 -> One (S (N N_expr) :: r596)
  | 774 -> One (S (N N_expr) :: r597)
  | 776 -> One (S (N N_expr) :: r598)
  | 778 -> One (S (N N_expr) :: r599)
  | 780 -> One (S (N N_expr) :: r600)
  | 783 -> One (S (N N_expr) :: r601)
  | 785 -> One (S (N N_expr) :: r602)
  | 822 -> One (S (N N_expr) :: r616)
  | 827 -> One (S (N N_expr) :: r617)
  | 829 -> One (S (N N_expr) :: r619)
  | 832 -> One (S (N N_expr) :: r620)
  | 838 -> One (S (N N_expr) :: r621)
  | 843 -> One (S (N N_expr) :: r625)
  | 848 -> One (S (N N_expr) :: r626)
  | 852 -> One (S (N N_expr) :: r631)
  | 855 -> One (S (N N_expr) :: r632)
  | 860 -> One (S (N N_expr) :: r633)
  | 862 -> One (S (N N_expr) :: r635)
  | 865 -> One (S (N N_expr) :: r636)
  | 868 -> One (S (N N_expr) :: r637)
  | 923 -> One (S (N N_expr) :: r659)
  | 987 -> One (S (N N_expr) :: r678)
  | 604 -> One (Sub (r1) :: r437)
  | 631 -> One (Sub (r1) :: r494)
  | 943 -> One (Sub (r1) :: r665)
  | 1098 -> One (Sub (r1) :: r722)
  | 1576 -> One (Sub (r1) :: r1026)
  | 1578 -> One (Sub (r1) :: r1027)
  | 2 -> One (Sub (r10) :: r12)
  | 55 -> One (Sub (r10) :: r13)
  | 58 -> One (Sub (r10) :: r18)
  | 89 -> One (Sub (r10) :: r50)
  | 345 -> One (Sub (r10) :: r233)
  | 789 -> One (Sub (r10) :: r604)
  | 1094 -> One (Sub (r10) :: r720)
  | 1100 -> One (Sub (r10) :: r725)
  | 70 -> One (Sub (r33) :: r34)
  | 619 -> One (Sub (r33) :: r475)
  | 660 -> One (Sub (r33) :: r533)
  | 703 -> One (Sub (r33) :: r552)
  | 716 -> One (Sub (r33) :: r566)
  | 718 -> One (Sub (r33) :: r567)
  | 131 -> One (Sub (r36) :: r79)
  | 190 -> One (Sub (r36) :: r143)
  | 279 -> One (Sub (r36) :: r189)
  | 574 -> One (Sub (r51) :: r418)
  | 812 -> One (Sub (r51) :: r615)
  | 216 -> One (Sub (r55) :: r161)
  | 313 -> One (Sub (r55) :: r219)
  | 957 -> One (Sub (r55) :: r672)
  | 1262 -> One (Sub (r57) :: r868)
  | 1266 -> One (Sub (r57) :: r871)
  | 140 -> One (Sub (r59) :: r91)
  | 173 -> One (Sub (r59) :: r126)
  | 220 -> One (Sub (r59) :: r162)
  | 226 -> One (Sub (r61) :: r165)
  | 463 -> One (Sub (r61) :: r338)
  | 282 -> One (Sub (r63) :: r190)
  | 530 -> One (Sub (r63) :: r394)
  | 596 -> One (Sub (r63) :: r432)
  | 638 -> One (Sub (r63) :: r509)
  | 796 -> One (Sub (r63) :: r610)
  | 915 -> One (Sub (r63) :: r655)
  | 919 -> One (Sub (r63) :: r658)
  | 968 -> One (Sub (r63) :: r675)
  | 1178 -> One (Sub (r63) :: r804)
  | 1522 -> One (Sub (r63) :: r1011)
  | 156 -> One (Sub (r81) :: r118)
  | 257 -> One (Sub (r81) :: r186)
  | 389 -> One (Sub (r81) :: r263)
  | 428 -> One (Sub (r93) :: r289)
  | 1078 -> One (Sub (r93) :: r713)
  | 1081 -> One (Sub (r93) :: r715)
  | 1084 -> One (Sub (r93) :: r717)
  | 1561 -> One (Sub (r93) :: r1022)
  | 161 -> One (Sub (r113) :: r119)
  | 153 -> One (Sub (r115) :: r117)
  | 204 -> One (Sub (r129) :: r151)
  | 182 -> One (Sub (r131) :: r137)
  | 194 -> One (Sub (r147) :: r149)
  | 475 -> One (Sub (r147) :: r344)
  | 212 -> One (Sub (r159) :: r160)
  | 241 -> One (Sub (r172) :: r174)
  | 290 -> One (Sub (r194) :: r197)
  | 295 -> One (Sub (r199) :: r200)
  | 298 -> One (Sub (r202) :: r218)
  | 712 -> One (Sub (r202) :: r564)
  | 1365 -> One (Sub (r202) :: r931)
  | 1385 -> One (Sub (r202) :: r948)
  | 296 -> One (Sub (r210) :: r212)
  | 337 -> One (Sub (r210) :: r230)
  | 1141 -> One (Sub (r265) :: r770)
  | 414 -> One (Sub (r279) :: r280)
  | 1019 -> One (Sub (r299) :: r691)
  | 478 -> One (Sub (r325) :: r345)
  | 451 -> One (Sub (r327) :: r328)
  | 466 -> One (Sub (r342) :: r343)
  | 519 -> One (Sub (r382) :: r385)
  | 520 -> One (Sub (r390) :: r392)
  | 955 -> One (Sub (r398) :: r669)
  | 544 -> One (Sub (r404) :: r405)
  | 591 -> One (Sub (r423) :: r424)
  | 592 -> One (Sub (r426) :: r427)
  | 601 -> One (Sub (r426) :: r433)
  | 593 -> One (Sub (r429) :: r431)
  | 602 -> One (Sub (r429) :: r436)
  | 617 -> One (Sub (r472) :: r474)
  | 907 -> One (Sub (r472) :: r652)
  | 962 -> One (Sub (r499) :: r673)
  | 634 -> One (Sub (r504) :: r505)
  | 646 -> One (Sub (r511) :: r514)
  | 952 -> One (Sub (r511) :: r668)
  | 1051 -> One (Sub (r511) :: r705)
  | 1392 -> One (Sub (r511) :: r956)
  | 709 -> One (Sub (r562) :: r563)
  | 714 -> One (Sub (r562) :: r565)
  | 900 -> One (Sub (r648) :: r650)
  | 981 -> One (Sub (r676) :: r677)
  | 1047 -> One (Sub (r693) :: r700)
  | 1130 -> One (Sub (r760) :: r762)
  | 1170 -> One (Sub (r791) :: r796)
  | 1163 -> One (Sub (r793) :: r795)
  | 1391 -> One (Sub (r821) :: r951)
  | 1226 -> One (Sub (r839) :: r842)
  | 1418 -> One (Sub (r839) :: r975)
  | 1440 -> One (Sub (r854) :: r985)
  | 1457 -> One (Sub (r854) :: r993)
  | 1411 -> One (Sub (r904) :: r966)
  | 1461 -> One (Sub (r907) :: r994)
  | 1354 -> One (Sub (r922) :: r924)
  | 1376 -> One (Sub (r939) :: r941)
  | 787 -> One (r0)
  | 1591 -> One (r2)
  | 1590 -> One (r3)
  | 1589 -> One (r4)
  | 1588 -> One (r5)
  | 1587 -> One (r6)
  | 53 -> One (r7)
  | 54 -> One (r9)
  | 1586 -> One (r11)
  | 57 -> One (r12)
  | 56 -> One (r13)
  | 1510 -> One (r14)
  | 1585 -> One (r16)
  | 1584 -> One (r17)
  | 59 -> One (r18)
  | 1583 -> One (r19)
  | 1582 -> One (r20)
  | 1581 -> One (r21)
  | 1580 -> One (r22)
  | 61 -> One (r23)
  | 62 -> One (r24)
  | 1572 -> One (r25)
  | 64 -> One (r26)
  | 1571 -> One (r27)
  | 1570 -> One (r28)
  | 1569 -> One (r29)
  | 1568 -> One (r30)
  | 66 -> One (r31)
  | 69 -> One (r32)
  | 1556 -> One (r34)
  | 73 -> One (r35)
  | 78 -> One (r37)
  | 72 -> One (r38)
  | 77 -> One (r39)
  | 75 -> One (r40)
  | 76 -> One (r41)
  | 80 -> One (r42)
  | 82 -> One (r43)
  | 86 -> One (r44)
  | 1565 -> One (r45)
  | 1564 -> One (r46)
  | 88 | 618 | 929 -> One (r47)
  | 1555 -> One (r48)
  | 1554 -> One (r49)
  | 90 -> One (r50)
  | 129 -> One (r52)
  | 189 -> One (r54)
  | 211 -> One (r56)
  | 210 -> One (r58)
  | 219 -> One (r60)
  | 276 -> One (r62)
  | 1553 -> One (r64)
  | 1552 -> One (r65)
  | 128 -> One (r66)
  | 127 -> One (r67)
  | 92 -> One (r68)
  | 124 -> One (r69)
  | 126 -> One (r71)
  | 125 -> One (r72)
  | 93 -> One (r73)
  | 112 -> One (r74)
  | 111 -> One (r75)
  | 115 -> One (r76)
  | 118 -> One (r77)
  | 130 | 144 -> One (r78)
  | 132 -> One (r79)
  | 135 -> One (r80)
  | 136 -> One (r82)
  | 133 -> One (r83)
  | 1551 -> One (r84)
  | 1550 -> One (r85)
  | 1549 -> One (r86)
  | 139 -> One (r87)
  | 138 -> One (r88)
  | 1548 -> One (r89)
  | 1547 -> One (r90)
  | 1546 -> One (r91)
  | 693 -> One (r92)
  | 1545 -> One (r94)
  | 1544 -> One (r95)
  | 143 -> One (r96)
  | 142 -> One (r97)
  | 1543 -> One (r98)
  | 1542 -> One (r99)
  | 146 -> One (r100)
  | 1530 -> One (r101)
  | 344 -> One (r102)
  | 343 -> One (r103)
  | 165 -> One (r104)
  | 164 | 350 -> One (r105)
  | 150 | 349 -> One (r106)
  | 148 | 348 -> One (r107)
  | 147 | 347 -> One (r108)
  | 155 -> One (r109)
  | 158 -> One (r111)
  | 154 -> One (r112)
  | 163 -> One (r114)
  | 160 -> One (r116)
  | 159 -> One (r117)
  | 157 -> One (r118)
  | 162 -> One (r119)
  | 328 -> One (r120)
  | 289 -> One (r121)
  | 171 -> One (r122)
  | 172 | 175 | 179 | 1265 -> One (r123)
  | 273 -> One (r124)
  | 272 -> One (r125)
  | 271 -> One (r126)
  | 246 -> One (r127)
  | 201 | 1270 -> One (r128)
  | 230 -> One (r130)
  | 240 -> One (r132)
  | 239 -> One (r133)
  | 178 -> One (r134)
  | 181 -> One (r135)
  | 238 -> One (r136)
  | 237 -> One (r137)
  | 203 -> One (r138)
  | 202 -> One (r139)
  | 184 -> One (r140)
  | 186 -> One (r141)
  | 185 -> One (r142)
  | 191 -> One (r143)
  | 200 | 1275 -> One (r144)
  | 199 | 1274 -> One (r145)
  | 193 | 1273 -> One (r146)
  | 196 -> One (r148)
  | 195 -> One (r149)
  | 198 -> One (r150)
  | 205 -> One (r151)
  | 229 -> One (r152)
  | 218 -> One (r153)
  | 228 -> One (r155)
  | 225 -> One (r156)
  | 209 -> One (r157)
  | 213 -> One (r158)
  | 215 -> One (r160)
  | 217 -> One (r161)
  | 221 -> One (r162)
  | 224 -> One (r163)
  | 223 -> One (r164)
  | 227 -> One (r165)
  | 232 -> One (r166)
  | 236 -> One (r167)
  | 235 -> One (r168)
  | 234 -> One (r169)
  | 245 -> One (r171)
  | 243 -> One (r173)
  | 242 -> One (r174)
  | 251 -> One (r175)
  | 265 -> One (r176)
  | 264 -> One (r178)
  | 263 -> One (r179)
  | 262 -> One (r180)
  | 261 -> One (r181)
  | 253 -> One (r182)
  | 260 -> One (r184)
  | 259 -> One (r185)
  | 258 -> One (r186)
  | 275 -> One (r187)
  | 278 -> One (r188)
  | 280 -> One (r189)
  | 283 -> One (r190)
  | 285 -> One (r191)
  | 287 -> One (r192)
  | 288 -> One (r193)
  | 319 -> One (r195)
  | 318 -> One (r196)
  | 294 -> One (r197)
  | 311 -> One (r198)
  | 312 -> One (r200)
  | 299 -> One (r201)
  | 310 -> One (r209)
  | 307 -> One (r211)
  | 306 -> One (r212)
  | 305 -> One (r213)
  | 304 -> One (r214)
  | 303 -> One (r215)
  | 302 -> One (r216)
  | 301 -> One (r217)
  | 300 -> One (r218)
  | 314 -> One (r219)
  | 317 -> One (r220)
  | 316 -> One (r221)
  | 326 -> One (r222)
  | 325 -> One (r223)
  | 332 -> One (r224)
  | 331 -> One (r225)
  | 330 -> One (r226)
  | 340 -> One (r227)
  | 336 -> One (r228)
  | 339 -> One (r229)
  | 338 -> One (r230)
  | 1529 -> One (r231)
  | 1528 -> One (r232)
  | 346 -> One (r233)
  | 385 -> One (r234)
  | 384 -> One (r235)
  | 1540 -> One (r236)
  | 379 -> One (r238)
  | 373 -> One (r240)
  | 372 -> One (r241)
  | 353 -> One (r242)
  | 352 -> One (r243)
  | 371 -> One (r244)
  | 370 -> One (r245)
  | 1535 -> One (r246)
  | 1534 -> One (r247)
  | 362 -> One (r248)
  | 369 -> One (r250)
  | 368 -> One (r251)
  | 356 -> One (r252)
  | 359 -> One (r254)
  | 361 -> One (r255)
  | 367 -> One (r256)
  | 366 -> One (r257)
  | 365 -> One (r258)
  | 375 -> One (r259)
  | 383 -> One (r260)
  | 382 -> One (r261)
  | 381 -> One (r262)
  | 390 -> One (r263)
  | 1527 -> One (r264)
  | 409 -> One (r266)
  | 407 -> One (r267)
  | 406 -> One (r268)
  | 405 -> One (r269)
  | 1353 -> One (r270)
  | 1352 -> One (r271)
  | 1526 -> One (r273)
  | 1525 -> One (r274)
  | 411 -> One (r275)
  | 1521 -> One (r276)
  | 1520 -> One (r277)
  | 413 -> One (r278)
  | 415 -> One (r280)
  | 417 -> One (r281)
  | 420 -> One (r282)
  | 967 -> One (r283)
  | 966 -> One (r284)
  | 427 -> One (r285)
  | 426 -> One (r286)
  | 425 -> One (r287)
  | 1519 -> One (r288)
  | 1518 -> One (r289)
  | 1517 -> One (r290)
  | 432 -> One (r291)
  | 431 -> One (r292)
  | 430 -> One (r293)
  | 1516 -> One (r294)
  | 1515 -> One (r295)
  | 434 -> One (r296)
  | 1087 -> One (r297)
  | 503 -> One (r298)
  | 1093 -> One (r300)
  | 1092 -> One (r301)
  | 1091 -> One (r302)
  | 1090 -> One (r303)
  | 500 -> One (r305)
  | 499 -> One (r306)
  | 438 -> One (r307)
  | 437 -> One (r308)
  | 436 -> One (r309)
  | 498 -> One (r310)
  | 497 -> One (r311)
  | 496 -> One (r312)
  | 493 -> One (r313)
  | 492 -> One (r314)
  | 491 -> One (r315)
  | 442 -> One (r316)
  | 446 -> One (r317)
  | 481 -> One (r318)
  | 450 -> One (r319)
  | 448 -> One (r320)
  | 474 -> One (r321)
  | 473 -> One (r322)
  | 472 -> One (r323)
  | 471 -> One (r324)
  | 480 -> One (r326)
  | 477 -> One (r328)
  | 454 -> One (r329)
  | 462 -> One (r331)
  | 461 -> One (r332)
  | 458 -> One (r333)
  | 453 -> One (r334)
  | 457 -> One (r335)
  | 456 -> One (r336)
  | 465 -> One (r337)
  | 464 -> One (r338)
  | 469 -> One (r339)
  | 468 -> One (r340)
  | 467 -> One (r341)
  | 470 -> One (r343)
  | 476 -> One (r344)
  | 479 -> One (r345)
  | 483 -> One (r346)
  | 488 -> One (r347)
  | 486 -> One (r348)
  | 1089 -> One (r349)
  | 1088 -> One (r350)
  | 502 -> One (r351)
  | 681 -> One (r352)
  | 680 -> One (r353)
  | 1077 -> One (r354)
  | 1076 -> One (r355)
  | 506 -> One (r356)
  | 1075 -> One (r357)
  | 1074 -> One (r358)
  | 1073 -> One (r359)
  | 1072 -> One (r360)
  | 510 -> One (r361)
  | 1071 -> One (r362)
  | 1070 -> One (r363)
  | 1069 -> One (r364)
  | 1068 -> One (r365)
  | 512 -> One (r366)
  | 566 -> One (r367)
  | 565 -> One (r368)
  | 815 -> One (r370)
  | 805 -> One (r372)
  | 804 -> One (r373)
  | 803 -> One (r374)
  | 1067 -> One (r376)
  | 1066 -> One (r377)
  | 517 -> One (r378)
  | 515 -> One (r379)
  | 587 -> One (r380)
  | 583 -> One (r381)
  | 582 -> One (r383)
  | 581 -> One (r384)
  | 578 -> One (r385)
  | 532 -> One (r386)
  | 529 -> One (r387)
  | 523 -> One (r389)
  | 522 -> One (r391)
  | 521 -> One (r392)
  | 527 -> One (r393)
  | 531 -> One (r394)
  | 586 -> One (r395)
  | 540 | 795 -> One (r397)
  | 541 -> One (r399)
  | 535 -> One (r400)
  | 536 -> One (r401)
  | 546 -> One (r403)
  | 545 -> One (r405)
  | 577 -> One (r406)
  | 576 -> One (r407)
  | 549 -> One (r408)
  | 551 -> One (r409)
  | 567 -> One (r410)
  | 553 -> One (r411)
  | 558 -> One (r412)
  | 560 -> One (r413)
  | 563 -> One (r414)
  | 569 -> One (r415)
  | 571 -> One (r416)
  | 573 -> One (r417)
  | 575 -> One (r418)
  | 590 -> One (r419)
  | 589 -> One (r420)
  | 1042 -> One (r421)
  | 1041 -> One (r422)
  | 1065 -> One (r424)
  | 594 -> One (r425)
  | 600 -> One (r427)
  | 595 -> One (r428)
  | 599 -> One (r430)
  | 598 -> One (r431)
  | 597 -> One (r432)
  | 1036 -> One (r433)
  | 1035 -> One (r434)
  | 1034 -> One (r435)
  | 603 -> One (r436)
  | 1033 -> One (r437)
  | 1028 -> One (r438)
  | 1027 -> One (r439)
  | 1026 -> One (r440)
  | 1025 -> One (r442)
  | 1024 -> One (r443)
  | 1023 -> One (r444)
  | 1022 -> One (r445)
  | 1021 -> One (r446)
  | 1032 -> One (r447)
  | 1031 -> One (r448)
  | 1030 -> One (r449)
  | 1029 -> One (r450)
  | 1470 -> One (r451)
  | 1008 -> One (r452)
  | 610 -> One (r453)
  | 609 -> One (r454)
  | 608 -> One (r455)
  | 607 -> One (r456)
  | 1004 -> One (r457)
  | 894 -> One (r458)
  | 1003 -> One (r460)
  | 1002 -> One (r461)
  | 1001 -> One (r462)
  | 614 -> One (r463)
  | 615 -> One (r464)
  | 998 -> One (r465)
  | 922 -> One (r466)
  | 914 -> One (r467)
  | 911 -> One (r469)
  | 930 -> One (r471)
  | 997 -> One (r473)
  | 996 -> One (r474)
  | 995 -> One (r475)
  | 994 -> One (r476)
  | 993 -> One (r477)
  | 992 -> One (r478)
  | 622 -> One (r479)
  | 989 -> One (r480)
  | 624 -> One (r481)
  | 986 -> One (r482)
  | 985 -> One (r483)
  | 984 -> One (r484)
  | 626 -> One (r485)
  | 980 -> One (r486)
  | 629 -> One (r487)
  | 628 -> One (r488)
  | 979 -> One (r489)
  | 977 -> One (r490)
  | 630 -> One (r491)
  | 976 -> One (r492)
  | 975 -> One (r493)
  | 974 -> One (r494)
  | 961 -> One (r495)
  | 950 -> One (r497)
  | 651 -> One (r498)
  | 973 -> One (r500)
  | 972 -> One (r501)
  | 633 -> One (r502)
  | 635 -> One (r503)
  | 644 -> One (r505)
  | 642 -> One (r506)
  | 641 -> One (r507)
  | 640 -> One (r508)
  | 639 -> One (r509)
  | 647 -> One (r510)
  | 971 -> One (r512)
  | 650 -> One (r513)
  | 649 -> One (r514)
  | 942 -> One (r515)
  | 941 -> One (r516)
  | 940 -> One (r517)
  | 939 -> One (r518)
  | 655 -> One (r519)
  | 654 -> One (r520)
  | 653 -> One (r521)
  | 933 -> One (r522)
  | 938 -> One (r524)
  | 937 -> One (r525)
  | 936 -> One (r526)
  | 935 -> One (r527)
  | 934 -> One (r528)
  | 931 -> One (r529)
  | 659 -> One (r530)
  | 658 -> One (r531)
  | 657 -> One (r532)
  | 662 -> One (r533)
  | 667 -> One (r534)
  | 666 -> One (r535)
  | 665 | 928 -> One (r536)
  | 927 -> One (r537)
  | 692 -> One (r538)
  | 691 -> One (r539)
  | 690 -> One (r540)
  | 689 -> One (r541)
  | 671 -> One (r542)
  | 677 -> One (r543)
  | 675 -> One (r544)
  | 674 -> One (r545)
  | 673 -> One (r546)
  | 679 -> One (r547)
  | 684 -> One (r548)
  | 683 -> One (r549)
  | 702 -> One (r550)
  | 701 -> One (r551)
  | 704 -> One (r552)
  | 847 | 883 -> One (r553)
  | 846 | 882 -> One (r554)
  | 845 | 881 -> One (r555)
  | 705 | 834 -> One (r556)
  | 837 | 877 -> One (r557)
  | 836 | 876 -> One (r558)
  | 706 | 835 -> One (r559)
  | 875 -> One (r560)
  | 710 -> One (r561)
  | 711 -> One (r563)
  | 713 -> One (r564)
  | 715 -> One (r565)
  | 717 -> One (r566)
  | 719 -> One (r567)
  | 826 -> One (r568)
  | 825 -> One (r569)
  | 824 -> One (r570)
  | 821 -> One (r571)
  | 820 -> One (r572)
  | 726 -> One (r573)
  | 725 -> One (r574)
  | 730 -> One (r575)
  | 729 -> One (r576)
  | 782 -> One (r577)
  | 733 -> One (r578)
  | 736 -> One (r579)
  | 738 -> One (r580)
  | 740 -> One (r581)
  | 745 -> One (r582)
  | 747 -> One (r583)
  | 749 -> One (r584)
  | 751 -> One (r585)
  | 753 -> One (r586)
  | 755 -> One (r587)
  | 757 -> One (r588)
  | 759 -> One (r589)
  | 761 -> One (r590)
  | 763 -> One (r591)
  | 765 -> One (r592)
  | 767 -> One (r593)
  | 769 -> One (r594)
  | 771 -> One (r595)
  | 773 -> One (r596)
  | 775 -> One (r597)
  | 777 -> One (r598)
  | 779 -> One (r599)
  | 781 -> One (r600)
  | 784 -> One (r601)
  | 786 -> One (r602)
  | 791 -> One (r603)
  | 790 -> One (r604)
  | 817 -> One (r605)
  | 816 -> One (r606)
  | 794 -> One (r607)
  | 799 -> One (r608)
  | 798 -> One (r609)
  | 797 -> One (r610)
  | 802 -> One (r611)
  | 807 -> One (r612)
  | 809 -> One (r613)
  | 811 -> One (r614)
  | 813 -> One (r615)
  | 823 -> One (r616)
  | 828 -> One (r617)
  | 831 -> One (r618)
  | 830 -> One (r619)
  | 833 -> One (r620)
  | 839 -> One (r621)
  | 842 | 880 -> One (r622)
  | 841 | 879 -> One (r623)
  | 840 | 878 -> One (r624)
  | 844 -> One (r625)
  | 849 -> One (r626)
  | 859 -> One (r627)
  | 858 -> One (r628)
  | 857 -> One (r629)
  | 854 -> One (r630)
  | 853 -> One (r631)
  | 856 -> One (r632)
  | 861 -> One (r633)
  | 864 -> One (r634)
  | 863 -> One (r635)
  | 866 -> One (r636)
  | 869 -> One (r637)
  | 886 -> One (r638)
  | 888 -> One (r639)
  | 893 -> One (r640)
  | 892 -> One (r641)
  | 896 -> One (r642)
  | 899 -> One (r643)
  | 898 -> One (r644)
  | 906 -> One (r646)
  | 903 -> One (r647)
  | 902 -> One (r649)
  | 901 -> One (r650)
  | 926 -> One (r651)
  | 925 -> One (r652)
  | 910 -> One (r653)
  | 909 -> One (r654)
  | 916 -> One (r655)
  | 918 -> One (r656)
  | 917 | 1050 -> One (r657)
  | 920 -> One (r658)
  | 924 -> One (r659)
  | 949 -> One (r660)
  | 948 -> One (r661)
  | 947 -> One (r662)
  | 946 -> One (r663)
  | 945 -> One (r664)
  | 944 -> One (r665)
  | 964 -> One (r666)
  | 954 -> One (r667)
  | 953 -> One (r668)
  | 956 -> One (r669)
  | 960 -> One (r670)
  | 959 -> One (r671)
  | 958 -> One (r672)
  | 963 -> One (r673)
  | 970 -> One (r674)
  | 969 -> One (r675)
  | 982 -> One (r677)
  | 988 -> One (r678)
  | 991 -> One (r679)
  | 1000 -> One (r680)
  | 1007 -> One (r681)
  | 1006 -> One (r682)
  | 1018 -> One (r683)
  | 1017 -> One (r684)
  | 1016 -> One (r685)
  | 1011 -> One (r686)
  | 1010 -> One (r687)
  | 1015 -> One (r688)
  | 1014 -> One (r689)
  | 1013 -> One (r690)
  | 1020 -> One (r691)
  | 1046 -> One (r692)
  | 1049 -> One (r694)
  | 1040 -> One (r695)
  | 1039 -> One (r696)
  | 1038 -> One (r697)
  | 1045 -> One (r698)
  | 1044 -> One (r699)
  | 1048 -> One (r700)
  | 1056 -> One (r701)
  | 1055 -> One (r702)
  | 1054 -> One (r703)
  | 1053 -> One (r704)
  | 1052 -> One (r705)
  | 1061 -> One (r706)
  | 1060 -> One (r707)
  | 1059 -> One (r708)
  | 1058 -> One (r709)
  | 1064 -> One (r710)
  | 1063 -> One (r711)
  | 1080 -> One (r712)
  | 1079 -> One (r713)
  | 1083 -> One (r714)
  | 1082 -> One (r715)
  | 1086 -> One (r716)
  | 1085 -> One (r717)
  | 1514 -> One (r718)
  | 1513 -> One (r719)
  | 1095 -> One (r720)
  | 1097 -> One (r721)
  | 1099 -> One (r722)
  | 1512 -> One (r723)
  | 1511 -> One (r724)
  | 1101 -> One (r725)
  | 1105 -> One (r726)
  | 1104 -> One (r727)
  | 1103 -> One (r728)
  | 1112 -> One (r729)
  | 1115 -> One (r731)
  | 1114 -> One (r732)
  | 1111 -> One (r733)
  | 1110 -> One (r734)
  | 1109 -> One (r735)
  | 1108 -> One (r736)
  | 1107 -> One (r737)
  | 1122 -> One (r738)
  | 1121 -> One (r739)
  | 1120 -> One (r740)
  | 1119 -> One (r741)
  | 1125 -> One (r744)
  | 1124 -> One (r745)
  | 1123 -> One (r746)
  | 1157 -> One (r747)
  | 1156 -> One (r748)
  | 1155 -> One (r749)
  | 1340 -> One (r750)
  | 1339 -> One (r751)
  | 1150 -> One (r752)
  | 1149 -> One (r753)
  | 1148 -> One (r754)
  | 1147 -> One (r755)
  | 1146 -> One (r756)
  | 1129 -> One (r757)
  | 1137 -> One (r758)
  | 1136 -> One (r759)
  | 1145 -> One (r761)
  | 1144 -> One (r762)
  | 1140 -> One (r763)
  | 1139 -> One (r764)
  | 1138 -> One (r765)
  | 1135 -> One (r766)
  | 1134 -> One (r767)
  | 1133 -> One (r768)
  | 1143 -> One (r769)
  | 1142 -> One (r770)
  | 1154 -> One (r771)
  | 1153 -> One (r772)
  | 1152 -> One (r773)
  | 1219 -> One (r774)
  | 1234 -> One (r776)
  | 1251 -> One (r778)
  | 1250 -> One (r779)
  | 1175 -> One (r780)
  | 1174 -> One (r781)
  | 1173 -> One (r782)
  | 1162 -> One (r783)
  | 1160 -> One (r784)
  | 1159 -> One (r785)
  | 1166 -> One (r786)
  | 1165 -> One (r787)
  | 1167 -> One (r789)
  | 1164 -> One (r790)
  | 1172 -> One (r792)
  | 1169 -> One (r794)
  | 1168 -> One (r795)
  | 1171 -> One (r796)
  | 1184 -> One (r797)
  | 1183 -> One (r798)
  | 1182 -> One (r800)
  | 1181 -> One (r801)
  | 1177 -> One (r802)
  | 1180 -> One (r803)
  | 1179 -> One (r804)
  | 1201 -> One (r805)
  | 1200 -> One (r806)
  | 1199 -> One (r807)
  | 1198 -> One (r809)
  | 1197 -> One (r810)
  | 1186 -> One (r811)
  | 1191 -> One (r812)
  | 1190 -> One (r813)
  | 1189 -> One (r814)
  | 1188 -> One (r815)
  | 1196 -> One (r816)
  | 1195 -> One (r817)
  | 1194 -> One (r818)
  | 1193 -> One (r819)
  | 1216 -> One (r820)
  | 1215 -> One (r822)
  | 1214 -> One (r823)
  | 1210 -> One (r824)
  | 1209 -> One (r825)
  | 1208 -> One (r826)
  | 1203 -> One (r827)
  | 1213 -> One (r828)
  | 1212 -> One (r829)
  | 1238 -> One (r830)
  | 1237 -> One (r831)
  | 1218 -> One (r832)
  | 1235 -> One (r833)
  | 1225 -> One (r834)
  | 1224 -> One (r835)
  | 1223 -> One (r836)
  | 1222 -> One (r837)
  | 1221 -> One (r838)
  | 1232 -> One (r840)
  | 1228 -> One (r841)
  | 1227 -> One (r842)
  | 1231 -> One (r843)
  | 1230 -> One (r844)
  | 1243 -> One (r845)
  | 1242 -> One (r846)
  | 1241 -> One (r847)
  | 1245 -> One (r849)
  | 1244 -> One (r850)
  | 1240 -> One (r851)
  | 1247 -> One (r852)
  | 1278 -> One (r853)
  | 1283 -> One (r855)
  | 1282 -> One (r856)
  | 1256 -> One (r857)
  | 1255 -> One (r858)
  | 1254 -> One (r859)
  | 1253 -> One (r860)
  | 1281 -> One (r861)
  | 1261 -> One (r862)
  | 1260 -> One (r863)
  | 1259 -> One (r864)
  | 1258 -> One (r865)
  | 1280 -> One (r866)
  | 1264 -> One (r867)
  | 1263 -> One (r868)
  | 1279 -> One (r869)
  | 1268 -> One (r870)
  | 1267 -> One (r871)
  | 1277 -> One (r872)
  | 1272 -> One (r873)
  | 1292 -> One (r874)
  | 1291 -> One (r875)
  | 1290 -> One (r876)
  | 1289 -> One (r877)
  | 1288 -> One (r878)
  | 1287 -> One (r879)
  | 1296 -> One (r880)
  | 1306 -> One (r881)
  | 1305 -> One (r882)
  | 1304 -> One (r883)
  | 1303 -> One (r884)
  | 1302 -> One (r885)
  | 1315 -> One (r886)
  | 1325 -> One (r887)
  | 1324 -> One (r888)
  | 1323 -> One (r889)
  | 1322 -> One (r890)
  | 1321 -> One (r891)
  | 1320 -> One (r892)
  | 1319 -> One (r893)
  | 1336 -> One (r894)
  | 1335 -> One (r895)
  | 1334 -> One (r896)
  | 1333 -> One (r897)
  | 1332 -> One (r898)
  | 1331 -> One (r899)
  | 1330 -> One (r900)
  | 1431 -> One (r901)
  | 1429 -> One (r903)
  | 1456 -> One (r905)
  | 1347 -> One (r906)
  | 1464 -> One (r908)
  | 1463 -> One (r909)
  | 1346 -> One (r910)
  | 1345 -> One (r911)
  | 1344 -> One (r912)
  | 1351 -> One (r913)
  | 1350 -> One (r914)
  | 1349 -> One (r915)
  | 1372 -> One (r916)
  | 1371 -> One (r917)
  | 1370 -> One (r918)
  | 1369 -> One (r919)
  | 1358 -> One (r920)
  | 1357 -> One (r921)
  | 1356 -> One (r923)
  | 1355 -> One (r924)
  | 1363 -> One (r925)
  | 1362 -> One (r926)
  | 1361 -> One (r927)
  | 1360 -> One (r928)
  | 1368 -> One (r929)
  | 1367 -> One (r930)
  | 1366 -> One (r931)
  | 1375 -> One (r932)
  | 1374 -> One (r933)
  | 1401 -> One (r934)
  | 1390 -> One (r935)
  | 1389 -> One (r936)
  | 1378 -> One (r937)
  | 1377 -> One (r938)
  | 1403 -> One (r940)
  | 1402 -> One (r941)
  | 1383 -> One (r942)
  | 1382 -> One (r943)
  | 1381 -> One (r944)
  | 1380 -> One (r945)
  | 1388 -> One (r946)
  | 1387 -> One (r947)
  | 1386 -> One (r948)
  | 1400 -> One (r949)
  | 1399 -> One (r950)
  | 1398 -> One (r951)
  | 1397 -> One (r952)
  | 1396 -> One (r953)
  | 1395 -> One (r954)
  | 1394 -> One (r955)
  | 1393 -> One (r956)
  | 1407 -> One (r957)
  | 1406 -> One (r958)
  | 1405 -> One (r959)
  | 1447 -> One (r960)
  | 1446 -> One (r961)
  | 1443 -> One (r962)
  | 1410 -> One (r963)
  | 1409 -> One (r964)
  | 1439 -> One (r965)
  | 1438 -> One (r966)
  | 1437 -> One (r967)
  | 1417 -> One (r968)
  | 1416 -> One (r969)
  | 1415 -> One (r970)
  | 1414 -> One (r971)
  | 1413 -> One (r972)
  | 1421 -> One (r973)
  | 1420 -> One (r974)
  | 1419 -> One (r975)
  | 1434 -> One (r976)
  | 1425 -> One (r977)
  | 1424 -> One (r978)
  | 1436 -> One (r980)
  | 1423 -> One (r981)
  | 1432 -> One (r982)
  | 1427 -> One (r983)
  | 1442 -> One (r984)
  | 1441 -> One (r985)
  | 1445 -> One (r986)
  | 1451 -> One (r987)
  | 1450 -> One (r988)
  | 1449 -> One (r989)
  | 1453 -> One (r990)
  | 1460 -> One (r991)
  | 1459 -> One (r992)
  | 1458 -> One (r993)
  | 1462 -> One (r994)
  | 1504 -> One (r995)
  | 1471 -> One (r996)
  | 1481 -> One (r997)
  | 1480 -> One (r998)
  | 1479 -> One (r999)
  | 1478 -> One (r1000)
  | 1491 -> One (r1001)
  | 1501 -> One (r1002)
  | 1500 -> One (r1003)
  | 1499 -> One (r1004)
  | 1498 -> One (r1005)
  | 1497 -> One (r1006)
  | 1496 -> One (r1007)
  | 1507 -> One (r1008)
  | 1506 -> One (r1009)
  | 1524 -> One (r1010)
  | 1523 -> One (r1011)
  | 1539 -> One (r1012)
  | 1537 -> One (r1014)
  | 1536 -> One (r1015)
  | 1533 -> One (r1016)
  | 1532 -> One (r1017)
  | 1560 -> One (r1018)
  | 1559 -> One (r1019)
  | 1558 -> One (r1020)
  | 1563 -> One (r1021)
  | 1562 -> One (r1022)
  | 1567 -> One (r1023)
  | 1575 -> One (r1024)
  | 1574 -> One (r1025)
  | 1577 -> One (r1026)
  | 1579 -> One (r1027)
  | 1593 -> One (r1028)
  | 1597 -> One (r1029)
  | 1601 -> One (r1030)
  | 664 -> Select (function
    | -1 -> [R 99]
    | _ -> r537)
  | 435 -> Select (function
    | -1 -> S (T T_TYPE) :: r309
    | _ -> R 206 :: r304)
  | 1116 -> Select (function
    | -1 -> r749
    | _ -> R 206 :: r743)
  | 501 -> Select (function
    | -1 -> S (T T_UIDENT) :: r351
    | _ -> r304)
  | 83 -> Select (function
    | -1 -> S (T T_RPAREN) :: r44
    | _ -> Sub (r1) :: r46)
  | 612 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r255
    | _ -> Sub (r459) :: r462)
  | 605 -> Select (function
    | 59 | 90 | 346 | 400 | 434 | 1095 | 1101 | 1467 -> r451
    | _ -> S (T T_EXCEPTION) :: r446)
  | 422 -> Select (function
    | 517 | 794 | 1030 -> r73
    | _ -> S (N N_pattern) :: r284)
  | 254 -> Select (function
    | 1050 -> r83
    | _ -> Sub (r81) :: r183)
  | 354 -> Select (function
    | 372 -> r247
    | _ -> Sub (r194) :: r253)
  | 513 -> Select (function
    | -1 -> r47
    | _ -> r201)
  | 255 -> Select (function
    | 1050 -> r82
    | _ -> r183)
  | 355 -> Select (function
    | 353 -> r253
    | _ -> r246)
  | 1118 -> Select (function
    | -1 -> r747
    | _ -> r742)
  | 1117 -> Select (function
    | -1 -> r748
    | _ -> r743)
  | _ -> raise Not_found
