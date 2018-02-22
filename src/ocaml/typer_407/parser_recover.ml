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
  [|0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;2;1;2;1;2;1;1;2;1;2;1;1;1;1;1;2;1;1;2;3;3;3;1;2;1;2;1;1;1;2;1;1;1;2;1;2;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;4;2;3;4;2;3;4;1;1;1;1;1;2;3;3;4;1;1;1;2;1;1;1;2;1;2;3;1;1;2;3;1;1;2;1;2;1;3;1;1;1;1;1;1;2;2;2;3;2;3;1;4;5;1;1;1;2;1;2;1;2;1;1;1;1;2;1;1;2;2;1;2;1;2;1;1;1;2;3;2;1;2;3;4;2;3;2;3;1;3;4;2;3;1;2;1;3;1;1;2;1;1;3;2;3;1;1;2;3;2;3;4;4;2;3;5;2;1;2;3;2;3;4;5;3;4;3;4;4;5;6;2;1;2;1;1;2;2;3;1;2;1;2;1;2;3;2;3;3;4;5;6;1;1;2;1;2;1;3;4;5;2;3;1;2;3;4;5;4;2;3;2;1;1;2;1;3;1;1;1;1;2;1;1;1;1;1;2;3;4;5;6;7;2;3;1;2;1;1;2;1;2;2;3;4;3;4;3;3;2;1;1;2;3;1;2;2;3;4;5;2;3;1;4;5;6;7;5;5;2;6;7;1;2;1;2;3;4;5;6;7;1;2;3;1;2;3;1;2;1;1;2;3;4;5;4;5;3;4;8;9;1;2;2;2;1;1;1;2;3;4;2;3;1;1;1;1;2;3;3;3;3;3;1;3;2;3;1;1;1;1;1;2;3;4;2;5;1;2;1;1;1;2;1;2;2;1;2;2;1;1;2;3;4;5;5;1;2;3;4;1;2;1;2;3;4;1;1;1;2;1;1;1;2;2;3;1;4;2;1;2;1;1;2;3;3;1;2;4;5;4;5;6;2;1;2;3;3;1;2;3;4;3;4;3;2;3;1;5;2;3;2;1;2;3;3;1;1;3;4;5;2;1;2;3;2;5;6;2;3;1;1;2;3;1;1;1;2;1;2;1;1;2;1;3;1;1;1;2;3;1;2;3;1;4;3;1;1;2;2;3;1;2;1;1;1;1;1;3;1;1;2;3;1;1;1;2;3;4;1;2;1;1;1;2;3;2;3;2;1;2;1;1;2;3;4;5;2;3;2;3;2;3;3;4;2;2;3;3;4;1;3;1;4;2;2;3;4;1;1;2;1;1;2;3;3;4;2;1;2;3;1;1;2;3;4;5;6;1;1;1;2;1;2;1;1;1;1;1;2;1;2;1;2;1;2;3;1;2;1;2;1;1;2;1;2;3;3;4;5;1;2;3;4;1;2;5;6;1;1;2;3;4;1;2;3;4;1;1;2;1;1;2;3;4;1;1;4;5;1;2;3;4;1;5;2;3;2;3;3;4;5;2;2;1;1;6;7;8;9;1;1;1;1;1;1;1;1;2;3;2;3;2;3;1;1;1;1;2;2;3;1;2;1;2;2;3;4;5;1;2;1;2;3;1;1;2;3;1;1;2;1;2;3;2;3;2;3;2;1;2;1;2;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;3;2;3;2;3;1;2;3;4;5;3;1;2;1;2;3;4;5;1;2;3;1;2;3;2;3;2;3;2;3;2;3;2;1;3;4;2;2;3;4;5;3;4;5;3;4;5;6;7;5;6;7;5;6;7;3;1;2;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;8;9;5;6;7;8;9;5;6;7;8;9;3;4;5;2;1;2;1;1;2;4;5;3;4;5;3;4;5;4;5;3;4;4;2;3;5;6;1;3;4;4;5;6;3;4;5;1;2;3;2;3;1;2;3;1;2;3;1;1;2;1;2;3;4;1;2;3;4;4;5;1;2;3;1;5;1;1;6;7;8;9;10;5;6;7;8;4;5;6;7;8;9;10;2;1;2;3;4;1;2;1;2;3;4;1;1;2;5;1;2;3;3;4;5;7;3;4;3;4;5;2;3;3;4;2;3;1;3;4;5;6;7;3;3;4;3;2;3;2;2;3;3;2;3;2;3;4;2;2;3;4;7;2;3;4;1;2;3;4;5;6;7;1;2;2;3;4;5;6;1;2;3;2;3;4;5;2;4;5;2;1;2;3;4;1;2;1;2;3;1;1;2;5;2;3;4;5;6;7;8;3;4;5;6;7;2;3;4;2;4;5;3;4;5;6;3;4;5;6;4;5;5;6;7;5;6;7;7;8;9;2;4;5;3;4;5;6;1;2;1;2;3;4;1;2;1;2;3;4;1;2;3;4;5;6;1;2;7;8;1;2;3;4;5;6;7;4;5;6;1;1;1;2;3;1;2;3;4;5;1;2;6;2;3;4;5;6;4;5;3;4;5;6;7;1;2;3;4;1;2;3;1;2;3;1;4;1;1;1;2;2;2;3;2;3;1;5;6;7;1;2;1;2;3;3;4;1;2;1;2;1;2;3;4;5;1;2;3;4;5;3;4;1;2;3;1;2;1;2;1;2;3;4;5;1;2;3;6;7;1;1;2;1;1;2;3;4;5;6;1;2;3;1;2;3;4;1;1;7;2;3;4;1;2;1;2;3;3;4;1;2;1;2;8;9;2;3;4;5;6;1;2;3;4;5;1;2;3;1;2;3;4;1;1;1;2;1;2;3;1;3;1;5;4;6;7;8;1;1;1;2;3;4;5;6;7;2;1;1;2;1;1;1;1;1;2;3;4;5;6;2;1;1;1;1;1;1;1;2;1;1;1;2;3;4;5;6;7;8;2;1;1;1;2;3;4;5;6;7;8;2;1;2;2;2;1;2;3;4;5;1;1;2;3;4;1;2;1;2;3;1;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;5;6;7;1;1;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;8;9;10;11;6;7;8;5;2;3;1;2;3;4;1;2;3;1;1;2;3;4;5;6;1;2;3;4;1;2;1;2;1;2;1;1;2;1;3;2;3;2;3;7;2;3;3;4;5;4;1;2;5;6;1;2;3;4;1;2;1;2;2;1;2;3;4;1;2;6;7;1;1;1;1;1;1;2;1;1;1;1;1;1;2;3;4;5;2;1;1;1;1;1;1;1;1;2;1;1;1;1;2;3;4;5;6;7;2;1;2;1;2;3;1;1;1;3;4;3;4;3;4;5;6;7;2;3;3;4;5;3;4;2;3;4;8;5;6;7;1;2;8;9;2;1;1;1;3;4;4;5;2;3;4;4;5;6;5;6;3;4;2;2;3;4;5;5;6;7;2;3;3;4;3;4;5;6;3;4;5;6;7;8;4;5;3;4;5;6;3;4;3;2;3;4;5;6;1;2;1;0;1;2;1;0;1;2;1;|]

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
  let r0 = [R 472] in
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
  let r11 = [R 202] in
  let r12 = R 17 :: r11 in
  let r13 = [R 15] in
  let r14 = [R 436] in
  let r15 = S (N N_structure) :: r14 in
  let r16 = [R 16] in
  let r17 = S (T T_RBRACKET) :: r16 in
  let r18 = Sub (r15) :: r17 in
  let r19 = [R 156] in
  let r20 = S (T T_DONE) :: r19 in
  let r21 = Sub (r1) :: r20 in
  let r22 = S (T T_DO) :: r21 in
  let r23 = Sub (r1) :: r22 in
  let r24 = [R 309] in
  let r25 = [R 133] in
  let r26 = Sub (r1) :: r25 in
  let r27 = [R 150] in
  let r28 = S (N N_match_cases) :: r27 in
  let r29 = R 381 :: r28 in
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
  let r41 = [R 310] in
  let r42 = [R 201] in
  let r43 = [R 18] in
  let r44 = [R 102] in
  let r45 = [R 538] in
  let r46 = S (T T_RPAREN) :: r45 in
  let r47 = [R 721] in
  let r48 = [R 203] in
  let r49 = S (T T_RBRACKET) :: r48 in
  let r50 = Sub (r15) :: r49 in
  let r51 = S (T T_LIDENT) :: r47 in
  let r52 = [R 508] in
  let r53 = S (T T_UNDERSCORE) :: r52 in
  let r54 = [R 505] in
  let r55 = Sub (r53) :: r54 in
  let r56 = [R 526] in
  let r57 = Sub (r55) :: r56 in
  let r58 = [R 118] in
  let r59 = Sub (r57) :: r58 in
  let r60 = [R 127] in
  let r61 = Sub (r59) :: r60 in
  let r62 = [R 116] in
  let r63 = Sub (r61) :: r62 in
  let r64 = [R 729] in
  let r65 = R 446 :: r64 in
  let r66 = Sub (r63) :: r65 in
  let r67 = S (T T_COLON) :: r66 in
  let r68 = Sub (r51) :: r67 in
  let r69 = [R 375] in
  let r70 = S (T T_AMPERAMPER) :: r69 in
  let r71 = [R 722] in
  let r72 = S (T T_RPAREN) :: r71 in
  let r73 = Sub (r70) :: r72 in
  let r74 = [R 356] in
  let r75 = S (T T_RPAREN) :: r74 in
  let r76 = [R 358] in
  let r77 = [R 360] in
  let r78 = [R 306] in
  let r79 = [R 514] in
  let r80 = [R 230] in
  let r81 = S (T T_LIDENT) :: r80 in
  let r82 = [R 507] in
  let r83 = Sub (r81) :: r82 in
  let r84 = [R 119] in
  let r85 = Sub (r59) :: r84 in
  let r86 = S (T T_MINUSGREATER) :: r85 in
  let r87 = Sub (r59) :: r86 in
  let r88 = S (T T_COLON) :: r87 in
  let r89 = [R 120] in
  let r90 = Sub (r59) :: r89 in
  let r91 = S (T T_MINUSGREATER) :: r90 in
  let r92 = [R 401] in
  let r93 = S (N N_module_type) :: r92 in
  let r94 = [R 524] in
  let r95 = S (T T_RPAREN) :: r94 in
  let r96 = Sub (r93) :: r95 in
  let r97 = R 200 :: r96 in
  let r98 = [R 329] in
  let r99 = S (T T_END) :: r98 in
  let r100 = R 482 :: r99 in
  let r101 = [R 696] in
  let r102 = R 446 :: r101 in
  let r103 = R 109 :: r102 in
  let r104 = R 699 :: r103 in
  let r105 = S (T T_LIDENT) :: r104 in
  let r106 = R 394 :: r105 in
  let r107 = R 347 :: r106 in
  let r108 = R 200 :: r107 in
  let r109 = [R 398] in
  let r110 = S (T T_UNDERSCORE) :: r109 in
  let r111 = [R 391] in
  let r112 = Sub (r110) :: r111 in
  let r113 = R 716 :: r112 in
  let r114 = [R 392] in
  let r115 = Sub (r113) :: r114 in
  let r116 = [R 396] in
  let r117 = S (T T_RPAREN) :: r116 in
  let r118 = [R 397] in
  let r119 = [R 393] in
  let r120 = [R 704] in
  let r121 = [R 705] in
  let r122 = [R 95] in
  let r123 = [R 710] in
  let r124 = [R 121] in
  let r125 = Sub (r59) :: r124 in
  let r126 = S (T T_MINUSGREATER) :: r125 in
  let r127 = [R 513] in
  let r128 = [R 469] in
  let r129 = Sub (r55) :: r128 in
  let r130 = [R 470] in
  let r131 = Sub (r129) :: r130 in
  let r132 = [R 522] in
  let r133 = S (T T_RBRACKET) :: r132 in
  let r134 = Sub (r131) :: r133 in
  let r135 = [R 521] in
  let r136 = [R 520] in
  let r137 = S (T T_RBRACKET) :: r136 in
  let r138 = [R 518] in
  let r139 = S (T T_RBRACKET) :: r138 in
  let r140 = Sub (r131) :: r139 in
  let r141 = [R 344] in
  let r142 = Sub (r81) :: r141 in
  let r143 = [R 515] in
  let r144 = [R 711] in
  let r145 = S (T T_LIDENT) :: r144 in
  let r146 = S (T T_DOT) :: r145 in
  let r147 = S (T T_UIDENT) :: r78 in
  let r148 = [R 308] in
  let r149 = S (T T_RPAREN) :: r148 in
  let r150 = [R 307] in
  let r151 = [R 471] in
  let r152 = [R 685] in
  let r153 = [R 5] in
  let r154 = Sub (r61) :: r153 in
  let r155 = [R 684] in
  let r156 = R 17 :: r155 in
  let r157 = Sub (r154) :: r156 in
  let r158 = [R 125] in
  let r159 = Sub (r55) :: r158 in
  let r160 = [R 527] in
  let r161 = [R 126] in
  let r162 = [R 122] in
  let r163 = [R 128] in
  let r164 = Sub (r81) :: r163 in
  let r165 = [R 6] in
  let r166 = [R 517] in
  let r167 = [R 519] in
  let r168 = S (T T_RBRACKET) :: r167 in
  let r169 = Sub (r131) :: r168 in
  let r170 = S (T T_BACKQUOTE) :: r142 in
  let r171 = [R 345] in
  let r172 = Sub (r170) :: r171 in
  let r173 = [R 523] in
  let r174 = S (T T_RBRACKET) :: r173 in
  let r175 = [R 512] in
  let r176 = [R 443] in
  let r177 = Sub (r61) :: r176 in
  let r178 = [R 211] in
  let r179 = R 17 :: r178 in
  let r180 = S (T T_SEMI) :: r179 in
  let r181 = R 17 :: r180 in
  let r182 = Sub (r177) :: r181 in
  let r183 = [R 719] in
  let r184 = [R 444] in
  let r185 = Sub (r61) :: r184 in
  let r186 = [R 720] in
  let r187 = [R 96] in
  let r188 = [R 506] in
  let r189 = [R 516] in
  let r190 = [R 124] in
  let r191 = [R 123] in
  let r192 = [R 94] in
  let r193 = [R 97] in
  let r194 = S (T T_FALSE) :: r193 in
  let r195 = [R 19] in
  let r196 = R 17 :: r195 in
  let r197 = R 225 :: r196 in
  let r198 = [R 110] in
  let r199 = Sub (r159) :: r198 in
  let r200 = [R 226] in
  let r201 = [R 235] in
  let r202 = S (T T_LIDENT) :: r201 in
  let r203 = [R 236] in
  let r204 = R 17 :: r203 in
  let r205 = Sub (r177) :: r204 in
  let r206 = S (T T_COLON) :: r205 in
  let r207 = Sub (r202) :: r206 in
  let r208 = R 342 :: r207 in
  let r209 = [R 238] in
  let r210 = Sub (r208) :: r209 in
  let r211 = [R 111] in
  let r212 = S (T T_RBRACE) :: r211 in
  let r213 = [R 237] in
  let r214 = R 17 :: r213 in
  let r215 = S (T T_SEMI) :: r214 in
  let r216 = R 17 :: r215 in
  let r217 = Sub (r177) :: r216 in
  let r218 = S (T T_COLON) :: r217 in
  let r219 = [R 228] in
  let r220 = [R 227] in
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
  let r231 = [R 445] in
  let r232 = S (T T_RBRACKET) :: r231 in
  let r233 = Sub (r15) :: r232 in
  let r234 = [R 204] in
  let r235 = R 17 :: r234 in
  let r236 = R 225 :: r235 in
  let r237 = Sub (r194) :: r236 in
  let r238 = [R 649] in
  let r239 = Sub (r237) :: r238 in
  let r240 = [R 656] in
  let r241 = R 446 :: r240 in
  let r242 = Sub (r239) :: r241 in
  let r243 = R 451 :: r242 in
  let r244 = [R 20] in
  let r245 = R 17 :: r244 in
  let r246 = R 225 :: r245 in
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
  let r259 = [R 447] in
  let r260 = [R 205] in
  let r261 = R 17 :: r260 in
  let r262 = Sub (r249) :: r261 in
  let r263 = [R 686] in
  let r264 = [R 680] in
  let r265 = S (T T_UIDENT) :: r24 in
  let r266 = [R 349] in
  let r267 = R 446 :: r266 in
  let r268 = Sub (r265) :: r267 in
  let r269 = R 200 :: r268 in
  let r270 = [R 76] in
  let r271 = R 42 :: r270 in
  let r272 = R 53 :: r271 in
  let r273 = [R 194] in
  let r274 = S (T T_END) :: r273 in
  let r275 = Sub (r272) :: r274 in
  let r276 = [R 51] in
  let r277 = S (T T_RPAREN) :: r276 in
  let r278 = [R 580] in
  let r279 = S (T T_LIDENT) :: r123 in
  let r280 = [R 585] in
  let r281 = [R 503] in
  let r282 = [R 501] in
  let r283 = [R 591] in
  let r284 = S (T T_RPAREN) :: r283 in
  let r285 = [R 593] in
  let r286 = S (T T_RPAREN) :: r285 in
  let r287 = S (T T_UIDENT) :: r286 in
  let r288 = [R 594] in
  let r289 = S (T T_RPAREN) :: r288 in
  let r290 = [R 333] in
  let r291 = S (N N_module_expr) :: r290 in
  let r292 = R 17 :: r291 in
  let r293 = S (T T_OF) :: r292 in
  let r294 = [R 321] in
  let r295 = S (T T_END) :: r294 in
  let r296 = S (N N_structure) :: r295 in
  let r297 = [R 313] in
  let r298 = S (N N_module_expr) :: r297 in
  let r299 = S (T T_EQUAL) :: r298 in
  let r300 = [R 460] in
  let r301 = R 446 :: r300 in
  let r302 = Sub (r299) :: r301 in
  let r303 = S (T T_UIDENT) :: r302 in
  let r304 = S (T T_REC) :: r303 in
  let r305 = [R 337] in
  let r306 = R 446 :: r305 in
  let r307 = R 338 :: r306 in
  let r308 = Sub (r81) :: r307 in
  let r309 = R 200 :: r308 in
  let r310 = [R 339] in
  let r311 = [R 334] in
  let r312 = S (T T_RPAREN) :: r311 in
  let r313 = [R 330] in
  let r314 = S (N N_module_type) :: r313 in
  let r315 = S (T T_MINUSGREATER) :: r314 in
  let r316 = S (N N_functor_args) :: r315 in
  let r317 = [R 219] in
  let r318 = [R 220] in
  let r319 = S (T T_RPAREN) :: r318 in
  let r320 = S (N N_module_type) :: r319 in
  let r321 = [R 737] in
  let r322 = Sub (r147) :: r321 in
  let r323 = S (T T_EQUAL) :: r322 in
  let r324 = Sub (r265) :: r323 in
  let r325 = S (T T_MODULE) :: r324 in
  let r326 = [R 739] in
  let r327 = Sub (r325) :: r326 in
  let r328 = [R 332] in
  let r329 = [R 248] in
  let r330 = S (T T_LIDENT) :: r329 in
  let r331 = [R 736] in
  let r332 = Sub (r61) :: r331 in
  let r333 = S (T T_COLONEQUAL) :: r332 in
  let r334 = Sub (r330) :: r333 in
  let r335 = [R 249] in
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
  let r346 = [R 331] in
  let r347 = [R 341] in
  let r348 = Sub (r81) :: r347 in
  let r349 = [R 312] in
  let r350 = R 446 :: r349 in
  let r351 = Sub (r299) :: r350 in
  let r352 = [R 403] in
  let r353 = S (T T_RPAREN) :: r352 in
  let r354 = [R 404] in
  let r355 = S (T T_RPAREN) :: r354 in
  let r356 = S (N N_expr) :: r355 in
  let r357 = [R 132] in
  let r358 = S (N N_match_cases) :: r357 in
  let r359 = R 381 :: r358 in
  let r360 = S (T T_WITH) :: r359 in
  let r361 = Sub (r1) :: r360 in
  let r362 = [R 149] in
  let r363 = S (N N_match_cases) :: r362 in
  let r364 = R 381 :: r363 in
  let r365 = S (T T_WITH) :: r364 in
  let r366 = Sub (r1) :: r365 in
  let r367 = [R 421] in
  let r368 = S (N N_pattern) :: r367 in
  let r369 = Sub (r249) :: r368 in
  let r370 = [R 429] in
  let r371 = Sub (r369) :: r370 in
  let r372 = [R 277] in
  let r373 = Sub (r1) :: r372 in
  let r374 = S (T T_EQUAL) :: r373 in
  let r375 = Sub (r371) :: r374 in
  let r376 = [R 286] in
  let r377 = R 446 :: r376 in
  let r378 = Sub (r375) :: r377 in
  let r379 = R 458 :: r378 in
  let r380 = [R 531] in
  let r381 = [R 432] in
  let r382 = S (N N_pattern) :: r381 in
  let r383 = [R 529] in
  let r384 = S (T T_RBRACKET) :: r383 in
  let r385 = R 387 :: r384 in
  let r386 = [R 267] in
  let r387 = R 386 :: r386 in
  let r388 = Sub (r330) :: r387 in
  let r389 = [R 268] in
  let r390 = Sub (r388) :: r389 in
  let r391 = [R 528] in
  let r392 = S (T T_RBRACE) :: r391 in
  let r393 = [R 270] in
  let r394 = [R 385] in
  let r395 = [R 266] in
  let r396 = S (T T_UNDERSCORE) :: r278 in
  let r397 = [R 579] in
  let r398 = Sub (r396) :: r397 in
  let r399 = [R 423] in
  let r400 = Sub (r398) :: r399 in
  let r401 = [R 89] in
  let r402 = S (T T_INT) :: r401 in
  let r403 = [R 500] in
  let r404 = Sub (r402) :: r403 in
  let r405 = [R 582] in
  let r406 = [R 588] in
  let r407 = S (T T_RBRACKET) :: r406 in
  let r408 = S (T T_LBRACKET) :: r407 in
  let r409 = [R 589] in
  let r410 = [R 415] in
  let r411 = S (N N_pattern) :: r410 in
  let r412 = [R 418] in
  let r413 = [R 413] in
  let r414 = [R 422] in
  let r415 = [R 590] in
  let r416 = [R 419] in
  let r417 = [R 414] in
  let r418 = [R 411] in
  let r419 = [R 530] in
  let r420 = S (T T_BARRBRACKET) :: r419 in
  let r421 = [R 657] in
  let r422 = Sub (r1) :: r421 in
  let r423 = S (T T_EQUAL) :: r422 in
  let r424 = [R 273] in
  let r425 = [R 250] in
  let r426 = S (T T_LIDENT) :: r425 in
  let r427 = [R 258] in
  let r428 = [R 246] in
  let r429 = Sub (r426) :: r428 in
  let r430 = [R 257] in
  let r431 = S (T T_RPAREN) :: r430 in
  let r432 = [R 247] in
  let r433 = [R 254] in
  let r434 = [R 253] in
  let r435 = S (T T_RPAREN) :: r434 in
  let r436 = R 383 :: r435 in
  let r437 = [R 384] in
  let r438 = [R 281] in
  let r439 = R 17 :: r438 in
  let r440 = R 225 :: r439 in
  let r441 = Sub (r194) :: r440 in
  let r442 = [R 144] in
  let r443 = Sub (r1) :: r442 in
  let r444 = S (T T_IN) :: r443 in
  let r445 = Sub (r441) :: r444 in
  let r446 = R 200 :: r445 in
  let r447 = [R 272] in
  let r448 = R 446 :: r447 in
  let r449 = Sub (r375) :: r448 in
  let r450 = R 458 :: r449 in
  let r451 = R 200 :: r450 in
  let r452 = [R 145] in
  let r453 = Sub (r1) :: r452 in
  let r454 = S (T T_IN) :: r453 in
  let r455 = Sub (r265) :: r454 in
  let r456 = R 200 :: r455 in
  let r457 = [R 557] in
  let r458 = [R 198] in
  let r459 = S (N N_expr) :: r458 in
  let r460 = [R 560] in
  let r461 = S (T T_RBRACKET) :: r460 in
  let r462 = R 387 :: r461 in
  let r463 = [R 567] in
  let r464 = [R 208] in
  let r465 = [R 207] in
  let r466 = [R 262] in
  let r467 = R 390 :: r466 in
  let r468 = Sub (r330) :: r467 in
  let r469 = [R 263] in
  let r470 = Sub (r468) :: r469 in
  let r471 = [R 467] in
  let r472 = Sub (r470) :: r471 in
  let r473 = [R 554] in
  let r474 = S (T T_RBRACE) :: r473 in
  let r475 = [R 533] in
  let r476 = [R 532] in
  let r477 = S (T T_GREATERDOT) :: r476 in
  let r478 = [R 193] in
  let r479 = Sub (r33) :: r478 in
  let r480 = [R 540] in
  let r481 = S (T T_END) :: r480 in
  let r482 = [R 155] in
  let r483 = S (N N_expr) :: r482 in
  let r484 = S (T T_THEN) :: r483 in
  let r485 = Sub (r1) :: r484 in
  let r486 = [R 146] in
  let r487 = S (N N_match_cases) :: r486 in
  let r488 = R 381 :: r487 in
  let r489 = [R 289] in
  let r490 = Sub (r1) :: r489 in
  let r491 = S (T T_MINUSGREATER) :: r490 in
  let r492 = [R 290] in
  let r493 = Sub (r1) :: r492 in
  let r494 = S (T T_MINUSGREATER) :: r493 in
  let r495 = [R 260] in
  let r496 = Sub (r398) :: r495 in
  let r497 = [R 215] in
  let r498 = Sub (r1) :: r497 in
  let r499 = S (T T_MINUSGREATER) :: r498 in
  let r500 = [R 147] in
  let r501 = Sub (r499) :: r500 in
  let r502 = Sub (r496) :: r501 in
  let r503 = [R 435] in
  let r504 = S (T T_UNDERSCORE) :: r503 in
  let r505 = [R 256] in
  let r506 = [R 255] in
  let r507 = S (T T_RPAREN) :: r506 in
  let r508 = R 383 :: r507 in
  let r509 = [R 283] in
  let r510 = [R 284] in
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
  let r534 = [R 544] in
  let r535 = S (T T_RPAREN) :: r534 in
  let r536 = S (T T_LPAREN) :: r535 in
  let r537 = S (T T_DOT) :: r536 in
  let r538 = [R 573] in
  let r539 = S (T T_RPAREN) :: r538 in
  let r540 = Sub (r93) :: r539 in
  let r541 = S (T T_COLON) :: r540 in
  let r542 = S (N N_module_expr) :: r541 in
  let r543 = [R 322] in
  let r544 = S (N N_module_expr) :: r543 in
  let r545 = S (T T_MINUSGREATER) :: r544 in
  let r546 = S (N N_functor_args) :: r545 in
  let r547 = [R 324] in
  let r548 = [R 402] in
  let r549 = S (T T_RPAREN) :: r548 in
  let r550 = [R 192] in
  let r551 = Sub (r33) :: r550 in
  let r552 = [R 570] in
  let r553 = [R 547] in
  let r554 = S (T T_RBRACKET) :: r553 in
  let r555 = Sub (r1) :: r554 in
  let r556 = S (T T_LBRACKET) :: r555 in
  let r557 = [R 548] in
  let r558 = S (T T_RPAREN) :: r557 in
  let r559 = Sub (r1) :: r558 in
  let r560 = [R 179] in
  let r561 = [R 245] in
  let r562 = S (T T_LIDENT) :: r561 in
  let r563 = [R 242] in
  let r564 = [R 569] in
  let r565 = [R 243] in
  let r566 = [R 244] in
  let r567 = [R 542] in
  let r568 = Sub (r330) :: r567 in
  let r569 = [R 545] in
  let r570 = S (T T_RPAREN) :: r569 in
  let r571 = Sub (r1) :: r570 in
  let r572 = [R 131] in
  let r573 = Sub (r1) :: r572 in
  let r574 = [R 142] in
  let r575 = Sub (r1) :: r574 in
  let r576 = [R 191] in
  let r577 = S (N N_expr) :: r576 in
  let r578 = [R 196] in
  let r579 = [R 169] in
  let r580 = [R 163] in
  let r581 = [R 180] in
  let r582 = [R 166] in
  let r583 = [R 170] in
  let r584 = [R 162] in
  let r585 = [R 165] in
  let r586 = [R 164] in
  let r587 = [R 174] in
  let r588 = [R 168] in
  let r589 = [R 167] in
  let r590 = [R 172] in
  let r591 = [R 161] in
  let r592 = [R 160] in
  let r593 = [R 158] in
  let r594 = [R 159] in
  let r595 = [R 173] in
  let r596 = [R 171] in
  let r597 = [R 175] in
  let r598 = [R 176] in
  let r599 = [R 177] in
  let r600 = [R 197] in
  let r601 = [R 178] in
  let r602 = [R 475] in
  let r603 = Sub (r1) :: r602 in
  let r604 = [R 10] in
  let r605 = R 446 :: r604 in
  let r606 = Sub (r375) :: r605 in
  let r607 = [R 278] in
  let r608 = Sub (r1) :: r607 in
  let r609 = S (T T_EQUAL) :: r608 in
  let r610 = [R 430] in
  let r611 = [R 431] in
  let r612 = [R 426] in
  let r613 = [R 427] in
  let r614 = [R 424] in
  let r615 = [R 546] in
  let r616 = S (T T_RBRACKET) :: r615 in
  let r617 = Sub (r1) :: r616 in
  let r618 = [R 553] in
  let r619 = S (T T_RBRACE) :: r618 in
  let r620 = Sub (r1) :: r619 in
  let r621 = [R 550] in
  let r622 = S (T T_RBRACKET) :: r621 in
  let r623 = Sub (r1) :: r622 in
  let r624 = S (T T_LBRACKET) :: r623 in
  let r625 = [R 551] in
  let r626 = S (T T_RPAREN) :: r625 in
  let r627 = Sub (r1) :: r626 in
  let r628 = [R 552] in
  let r629 = S (T T_RBRACE) :: r628 in
  let r630 = Sub (r1) :: r629 in
  let r631 = [R 241] in
  let r632 = [R 186] in
  let r633 = [R 185] in
  let r634 = [R 549] in
  let r635 = S (T T_RBRACE) :: r634 in
  let r636 = Sub (r1) :: r635 in
  let r637 = [R 187] in
  let r638 = [R 182] in
  let r639 = [R 183] in
  let r640 = [R 184] in
  let r641 = [R 189] in
  let r642 = [R 188] in
  let r643 = [R 190] in
  let r644 = [R 181] in
  let r645 = [R 543] in
  let r646 = [R 559] in
  let r647 = [R 558] in
  let r648 = S (T T_BARRBRACKET) :: r647 in
  let r649 = [R 562] in
  let r650 = [R 561] in
  let r651 = S (T T_RBRACKET) :: r650 in
  let r652 = Sub (r202) :: r464 in
  let r653 = [R 209] in
  let r654 = R 387 :: r653 in
  let r655 = Sub (r652) :: r654 in
  let r656 = [R 568] in
  let r657 = S (T T_GREATERRBRACE) :: r656 in
  let r658 = [R 555] in
  let r659 = S (T T_RBRACE) :: r658 in
  let r660 = [R 466] in
  let r661 = Sub (r470) :: r660 in
  let r662 = [R 695] in
  let r663 = [R 693] in
  let r664 = Sub (r63) :: r663 in
  let r665 = [R 694] in
  let r666 = [R 261] in
  let r667 = [R 138] in
  let r668 = S (T T_DONE) :: r667 in
  let r669 = Sub (r1) :: r668 in
  let r670 = S (T T_DO) :: r669 in
  let r671 = Sub (r1) :: r670 in
  let r672 = Sub (r523) :: r671 in
  let r673 = [R 218] in
  let r674 = Sub (r499) :: r673 in
  let r675 = S (T T_RPAREN) :: r674 in
  let r676 = [R 259] in
  let r677 = [R 216] in
  let r678 = Sub (r1) :: r677 in
  let r679 = S (T T_MINUSGREATER) :: r678 in
  let r680 = [R 217] in
  let r681 = [R 592] in
  let r682 = S (T T_RPAREN) :: r681 in
  let r683 = S (N N_pattern) :: r491 in
  let r684 = [R 293] in
  let r685 = [R 154] in
  let r686 = [R 539] in
  let r687 = [R 566] in
  let r688 = [R 556] in
  let r689 = S (T T_BARRBRACKET) :: r688 in
  let r690 = [R 143] in
  let r691 = Sub (r1) :: r690 in
  let r692 = S (T T_IN) :: r691 in
  let r693 = Sub (r299) :: r692 in
  let r694 = S (T T_UIDENT) :: r693 in
  let r695 = [R 314] in
  let r696 = S (N N_module_expr) :: r695 in
  let r697 = S (T T_EQUAL) :: r696 in
  let r698 = [R 315] in
  let r699 = [R 213] in
  let r700 = Sub (r423) :: r699 in
  let r701 = [R 659] in
  let r702 = Sub (r700) :: r701 in
  let r703 = S (T T_RPAREN) :: r702 in
  let r704 = Sub (r511) :: r703 in
  let r705 = [R 214] in
  let r706 = Sub (r1) :: r705 in
  let r707 = [R 658] in
  let r708 = [R 276] in
  let r709 = Sub (r1) :: r708 in
  let r710 = S (T T_EQUAL) :: r709 in
  let r711 = Sub (r63) :: r710 in
  let r712 = S (T T_DOT) :: r711 in
  let r713 = [R 275] in
  let r714 = Sub (r1) :: r713 in
  let r715 = S (T T_EQUAL) :: r714 in
  let r716 = Sub (r63) :: r715 in
  let r717 = [R 274] in
  let r718 = Sub (r1) :: r717 in
  let r719 = [R 407] in
  let r720 = S (T T_RPAREN) :: r719 in
  let r721 = [R 405] in
  let r722 = S (T T_RPAREN) :: r721 in
  let r723 = [R 406] in
  let r724 = S (T T_RPAREN) :: r723 in
  let r725 = [R 234] in
  let r726 = S (T T_RBRACKET) :: r725 in
  let r727 = Sub (r15) :: r726 in
  let r728 = [R 439] in
  let r729 = [R 440] in
  let r730 = [R 212] in
  let r731 = S (T T_RBRACKET) :: r730 in
  let r732 = Sub (r15) :: r731 in
  let r733 = [R 655] in
  let r734 = R 446 :: r733 in
  let r735 = S (N N_module_expr) :: r734 in
  let r736 = [R 449] in
  let r737 = S (T T_STRING) :: r736 in
  let r738 = [R 448] in
  let r739 = R 446 :: r738 in
  let r740 = Sub (r737) :: r739 in
  let r741 = S (T T_EQUAL) :: r740 in
  let r742 = Sub (r63) :: r741 in
  let r743 = S (T T_COLON) :: r742 in
  let r744 = Sub (r51) :: r743 in
  let r745 = [R 648] in
  let r746 = R 446 :: r745 in
  let r747 = R 17 :: r746 in
  let r748 = Sub (r249) :: r747 in
  let r749 = S (T T_EQUAL) :: r748 in
  let r750 = Sub (r194) :: r749 in
  let r751 = [R 476] in
  let r752 = R 446 :: r751 in
  let r753 = R 17 :: r752 in
  let r754 = R 225 :: r753 in
  let r755 = Sub (r194) :: r754 in
  let r756 = R 200 :: r755 in
  let r757 = [R 437] in
  let r758 = [R 483] in
  let r759 = [R 463] in
  let r760 = R 446 :: r759 in
  let r761 = S (N N_module_type) :: r760 in
  let r762 = S (T T_COLON) :: r761 in
  let r763 = S (T T_UIDENT) :: r762 in
  let r764 = S (T T_REC) :: r763 in
  let r765 = [R 317] in
  let r766 = S (N N_module_type) :: r765 in
  let r767 = S (T T_COLON) :: r766 in
  let r768 = [R 316] in
  let r769 = R 446 :: r768 in
  let r770 = [R 319] in
  let r771 = Sub (r767) :: r770 in
  let r772 = [R 318] in
  let r773 = Sub (r767) :: r772 in
  let r774 = S (T T_RPAREN) :: r773 in
  let r775 = S (N N_module_type) :: r774 in
  let r776 = [R 311] in
  let r777 = R 446 :: r776 in
  let r778 = [R 480] in
  let r779 = R 446 :: r778 in
  let r780 = S (N N_module_type) :: r779 in
  let r781 = [R 87] in
  let r782 = S (T T_LIDENT) :: r781 in
  let r783 = [R 66] in
  let r784 = Sub (r782) :: r783 in
  let r785 = [R 82] in
  let r786 = R 446 :: r785 in
  let r787 = Sub (r784) :: r786 in
  let r788 = S (T T_EQUAL) :: r787 in
  let r789 = S (T T_LIDENT) :: r788 in
  let r790 = R 85 :: r789 in
  let r791 = R 733 :: r790 in
  let r792 = R 200 :: r791 in
  let r793 = [R 715] in
  let r794 = Sub (r81) :: r793 in
  let r795 = S (T T_QUOTE) :: r794 in
  let r796 = [R 712] in
  let r797 = Sub (r795) :: r796 in
  let r798 = R 716 :: r797 in
  let r799 = [R 713] in
  let r800 = Sub (r798) :: r799 in
  let r801 = [R 86] in
  let r802 = S (T T_RBRACKET) :: r801 in
  let r803 = [R 714] in
  let r804 = [R 56] in
  let r805 = R 63 :: r804 in
  let r806 = R 55 :: r805 in
  let r807 = [R 67] in
  let r808 = S (T T_END) :: r807 in
  let r809 = Sub (r806) :: r808 in
  let r810 = [R 54] in
  let r811 = S (T T_RPAREN) :: r810 in
  let r812 = [R 732] in
  let r813 = Sub (r63) :: r812 in
  let r814 = S (T T_COLON) :: r813 in
  let r815 = Sub (r202) :: r814 in
  let r816 = [R 58] in
  let r817 = R 446 :: r816 in
  let r818 = Sub (r815) :: r817 in
  let r819 = [R 730] in
  let r820 = Sub (r63) :: r819 in
  let r821 = S (T T_COLON) :: r820 in
  let r822 = Sub (r202) :: r821 in
  let r823 = [R 731] in
  let r824 = Sub (r63) :: r823 in
  let r825 = S (T T_COLON) :: r824 in
  let r826 = Sub (r202) :: r825 in
  let r827 = [R 441] in
  let r828 = Sub (r63) :: r827 in
  let r829 = [R 59] in
  let r830 = R 446 :: r829 in
  let r831 = Sub (r828) :: r830 in
  let r832 = S (T T_COLON) :: r831 in
  let r833 = Sub (r202) :: r832 in
  let r834 = R 453 :: r833 in
  let r835 = [R 442] in
  let r836 = Sub (r63) :: r835 in
  let r837 = [R 57] in
  let r838 = R 446 :: r837 in
  let r839 = Sub (r784) :: r838 in
  let r840 = [R 70] in
  let r841 = Sub (r784) :: r840 in
  let r842 = S (T T_IN) :: r841 in
  let r843 = Sub (r265) :: r842 in
  let r844 = R 17 :: r843 in
  let r845 = R 399 :: r844 in
  let r846 = Sub (r63) :: r191 in
  let r847 = [R 65] in
  let r848 = Sub (r782) :: r847 in
  let r849 = S (T T_RBRACKET) :: r848 in
  let r850 = [R 88] in
  let r851 = S (T T_LIDENT) :: r850 in
  let r852 = [R 107] in
  let r853 = Sub (r63) :: r852 in
  let r854 = S (T T_EQUAL) :: r853 in
  let r855 = Sub (r63) :: r854 in
  let r856 = [R 60] in
  let r857 = R 446 :: r856 in
  let r858 = Sub (r855) :: r857 in
  let r859 = [R 61] in
  let r860 = [R 77] in
  let r861 = Sub (r784) :: r860 in
  let r862 = [R 25] in
  let r863 = R 446 :: r862 in
  let r864 = Sub (r861) :: r863 in
  let r865 = S (T T_COLON) :: r864 in
  let r866 = S (T T_LIDENT) :: r865 in
  let r867 = R 85 :: r866 in
  let r868 = [R 78] in
  let r869 = Sub (r861) :: r868 in
  let r870 = S (T T_MINUSGREATER) :: r869 in
  let r871 = Sub (r57) :: r870 in
  let r872 = S (T T_COLON) :: r871 in
  let r873 = [R 79] in
  let r874 = Sub (r861) :: r873 in
  let r875 = S (T T_MINUSGREATER) :: r874 in
  let r876 = [R 80] in
  let r877 = Sub (r861) :: r876 in
  let r878 = S (T T_MINUSGREATER) :: r877 in
  let r879 = [R 81] in
  let r880 = Sub (r861) :: r879 in
  let r881 = [R 13] in
  let r882 = R 446 :: r881 in
  let r883 = R 109 :: r882 in
  let r884 = R 699 :: r883 in
  let r885 = S (T T_LIDENT) :: r884 in
  let r886 = R 394 :: r885 in
  let r887 = [R 484] in
  let r888 = [R 12] in
  let r889 = R 446 :: r888 in
  let r890 = S (N N_module_type) :: r889 in
  let r891 = S (T T_COLON) :: r890 in
  let r892 = S (T T_UIDENT) :: r891 in
  let r893 = [R 498] in
  let r894 = [R 9] in
  let r895 = R 446 :: r894 in
  let r896 = Sub (r784) :: r895 in
  let r897 = S (T T_EQUAL) :: r896 in
  let r898 = S (T T_LIDENT) :: r897 in
  let r899 = R 85 :: r898 in
  let r900 = R 733 :: r899 in
  let r901 = [R 8] in
  let r902 = R 446 :: r901 in
  let r903 = Sub (r861) :: r902 in
  let r904 = S (T T_COLON) :: r903 in
  let r905 = S (T T_LIDENT) :: r904 in
  let r906 = R 85 :: r905 in
  let r907 = R 733 :: r906 in
  let r908 = [R 72] in
  let r909 = Sub (r36) :: r908 in
  let r910 = [R 28] in
  let r911 = Sub (r909) :: r910 in
  let r912 = [R 44] in
  let r913 = Sub (r911) :: r912 in
  let r914 = S (T T_EQUAL) :: r913 in
  let r915 = [R 22] in
  let r916 = R 446 :: r915 in
  let r917 = Sub (r914) :: r916 in
  let r918 = S (T T_LIDENT) :: r917 in
  let r919 = R 85 :: r918 in
  let r920 = [R 73] in
  let r921 = S (T T_END) :: r920 in
  let r922 = Sub (r272) :: r921 in
  let r923 = [R 727] in
  let r924 = Sub (r1) :: r923 in
  let r925 = S (T T_EQUAL) :: r924 in
  let r926 = Sub (r202) :: r925 in
  let r927 = R 342 :: r926 in
  let r928 = R 17 :: r927 in
  let r929 = R 399 :: r928 in
  let r930 = [R 36] in
  let r931 = R 446 :: r930 in
  let r932 = [R 726] in
  let r933 = Sub (r63) :: r932 in
  let r934 = S (T T_COLON) :: r933 in
  let r935 = Sub (r202) :: r934 in
  let r936 = [R 725] in
  let r937 = Sub (r63) :: r936 in
  let r938 = S (T T_COLON) :: r937 in
  let r939 = [R 728] in
  let r940 = Sub (r1) :: r939 in
  let r941 = [R 303] in
  let r942 = Sub (r423) :: r941 in
  let r943 = Sub (r202) :: r942 in
  let r944 = R 451 :: r943 in
  let r945 = R 17 :: r944 in
  let r946 = R 399 :: r945 in
  let r947 = [R 37] in
  let r948 = R 446 :: r947 in
  let r949 = [R 302] in
  let r950 = Sub (r828) :: r949 in
  let r951 = S (T T_COLON) :: r950 in
  let r952 = Sub (r202) :: r951 in
  let r953 = [R 301] in
  let r954 = Sub (r828) :: r953 in
  let r955 = S (T T_COLON) :: r954 in
  let r956 = [R 304] in
  let r957 = Sub (r1) :: r956 in
  let r958 = S (T T_EQUAL) :: r957 in
  let r959 = [R 305] in
  let r960 = Sub (r1) :: r959 in
  let r961 = S (T T_EQUAL) :: r960 in
  let r962 = Sub (r63) :: r961 in
  let r963 = S (T T_DOT) :: r962 in
  let r964 = [R 39] in
  let r965 = R 446 :: r964 in
  let r966 = Sub (r1) :: r965 in
  let r967 = [R 35] in
  let r968 = R 446 :: r967 in
  let r969 = R 409 :: r968 in
  let r970 = Sub (r911) :: r969 in
  let r971 = R 17 :: r970 in
  let r972 = [R 75] in
  let r973 = S (T T_RPAREN) :: r972 in
  let r974 = [R 32] in
  let r975 = Sub (r911) :: r974 in
  let r976 = S (T T_IN) :: r975 in
  let r977 = Sub (r265) :: r976 in
  let r978 = R 17 :: r977 in
  let r979 = R 399 :: r978 in
  let r980 = [R 71] in
  let r981 = Sub (r36) :: r980 in
  let r982 = S (T T_RBRACKET) :: r981 in
  let r983 = [R 47] in
  let r984 = Sub (r911) :: r983 in
  let r985 = S (T T_MINUSGREATER) :: r984 in
  let r986 = Sub (r496) :: r985 in
  let r987 = [R 29] in
  let r988 = Sub (r986) :: r987 in
  let r989 = [R 31] in
  let r990 = Sub (r911) :: r989 in
  let r991 = [R 74] in
  let r992 = S (T T_RPAREN) :: r991 in
  let r993 = [R 408] in
  let r994 = [R 38] in
  let r995 = R 446 :: r994 in
  let r996 = Sub (r855) :: r995 in
  let r997 = [R 40] in
  let r998 = [R 45] in
  let r999 = Sub (r911) :: r998 in
  let r1000 = S (T T_EQUAL) :: r999 in
  let r1001 = [R 46] in
  let r1002 = [R 661] in
  let r1003 = [R 681] in
  let r1004 = [R 11] in
  let r1005 = R 446 :: r1004 in
  let r1006 = Sub (r299) :: r1005 in
  let r1007 = S (T T_UIDENT) :: r1006 in
  let r1008 = [R 677] in
  let r1009 = [R 7] in
  let r1010 = R 446 :: r1009 in
  let r1011 = Sub (r914) :: r1010 in
  let r1012 = S (T T_LIDENT) :: r1011 in
  let r1013 = R 85 :: r1012 in
  let r1014 = R 733 :: r1013 in
  let r1015 = [R 660] in
  let r1016 = R 679 :: r1015 in
  let r1017 = [R 52] in
  let r1018 = S (T T_RPAREN) :: r1017 in
  let r1019 = [R 477] in
  let r1020 = Sub (r237) :: r1019 in
  let r1021 = [R 481] in
  let r1022 = R 446 :: r1021 in
  let r1023 = Sub (r1020) :: r1022 in
  let r1024 = R 451 :: r1023 in
  let r1025 = [R 571] in
  let r1026 = S (T T_RPAREN) :: r1025 in
  let r1027 = S (N N_module_expr) :: r1026 in
  let r1028 = [R 572] in
  let r1029 = S (T T_RPAREN) :: r1028 in
  let r1030 = [R 541] in
  let r1031 = [R 134] in
  let r1032 = S (N N_match_cases) :: r1031 in
  let r1033 = [R 136] in
  let r1034 = [R 135] in
  let r1035 = [R 231] in
  let r1036 = [R 233] in
  let r1037 = [R 410] in
  function
  | 0 | 1616 | 1620 -> Nothing
  | 1615 -> One ([R 0])
  | 1619 -> One ([R 1])
  | 1623 -> One ([R 2])
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
  | 793 -> One (R 17 :: r606)
  | 1197 -> One (R 17 :: r809)
  | 1206 -> One (R 17 :: r818)
  | 1223 -> One (R 17 :: r834)
  | 1238 -> One (R 17 :: r839)
  | 1260 -> One (R 17 :: r858)
  | 1307 -> One (R 17 :: r886)
  | 1322 -> One (R 17 :: r892)
  | 1339 -> One (R 17 :: r900)
  | 1350 -> One (R 17 :: r907)
  | 1369 -> One (R 17 :: r922)
  | 1425 -> One (R 17 :: r966)
  | 1443 -> One (R 17 :: r988)
  | 1469 -> One (R 17 :: r996)
  | 1498 -> One (R 17 :: r1007)
  | 1516 -> One (R 17 :: r1014)
  | 1524 -> One ([R 23])
  | 1523 -> One ([R 24])
  | 1359 -> One ([R 26])
  | 1358 -> One ([R 27])
  | 1451 -> One ([R 30])
  | 1454 -> One ([R 33])
  | 1449 -> One ([R 34])
  | 1475 -> One ([R 41])
  | 1476 -> One ([R 43])
  | 1456 -> One ([R 48])
  | 1269 -> One ([R 62])
  | 1270 -> One ([R 64])
  | 1257 -> One ([R 68])
  | 1254 -> One ([R 69])
  | 1348 -> One ([R 83])
  | 1347 -> One ([R 84])
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
  | 953 -> One ([R 129])
  | 708 -> One ([R 140])
  | 892 -> One ([R 141])
  | 734 -> One ([R 151])
  | 743 -> One ([R 152])
  | 723 -> One ([R 153])
  | 741 -> One ([R 195])
  | 912 -> One ([R 199])
  | 1 -> One (R 200 :: r6)
  | 60 -> One (R 200 :: r23)
  | 63 -> One (R 200 :: r26)
  | 65 -> One (R 200 :: r31)
  | 71 -> One (R 200 :: r38)
  | 91 -> One (R 200 :: r68)
  | 410 -> One (R 200 :: r275)
  | 424 -> One (R 200 :: r287)
  | 509 -> One (R 200 :: r361)
  | 511 -> One (R 200 :: r366)
  | 514 -> One (R 200 :: r379)
  | 534 -> One (R 200 :: r400)
  | 552 -> One (R 200 :: r411)
  | 621 -> One (R 200 :: r479)
  | 623 -> One (R 200 :: r481)
  | 625 -> One (R 200 :: r485)
  | 627 -> One (R 200 :: r488)
  | 632 -> One (R 200 :: r502)
  | 652 -> One (R 200 :: r521)
  | 656 -> One (R 200 :: r532)
  | 670 -> One (R 200 :: r542)
  | 700 -> One (R 200 :: r551)
  | 1030 -> One (R 200 :: r694)
  | 1123 -> One (R 200 :: r735)
  | 1127 -> One (R 200 :: r744)
  | 1149 -> One (R 200 :: r764)
  | 1172 -> One (R 200 :: r780)
  | 1578 -> One (R 200 :: r1027)
  | 926 -> One ([R 210])
  | 445 -> One ([R 221])
  | 444 -> One ([R 222])
  | 494 -> One ([R 223])
  | 495 -> One ([R 224])
  | 324 -> One (R 225 :: r223)
  | 134 | 487 -> One ([R 229])
  | 249 -> One ([R 232])
  | 308 -> One ([R 239])
  | 309 -> One ([R 240])
  | 893 -> One ([R 251])
  | 895 -> One ([R 252])
  | 934 -> One ([R 264])
  | 933 -> One ([R 265])
  | 524 -> One ([R 269])
  | 528 -> One ([R 271])
  | 731 -> One ([R 279])
  | 818 -> One ([R 280])
  | 637 -> One ([R 282])
  | 648 -> One ([R 285])
  | 727 -> One ([R 287])
  | 819 -> One ([R 288])
  | 999 -> One ([R 291])
  | 1004 -> One ([R 292])
  | 269 -> One ([R 294])
  | 267 -> One ([R 295])
  | 268 -> One ([R 296])
  | 270 -> One ([R 297])
  | 266 -> One ([R 298])
  | 248 -> One ([R 299])
  | 247 -> One ([R 300])
  | 687 -> One ([R 320])
  | 685 -> One ([R 323])
  | 676 -> One ([R 325])
  | 686 -> One ([R 326])
  | 688 -> One ([R 327])
  | 449 -> One ([R 328])
  | 490 -> One ([R 335])
  | 484 -> One ([R 336])
  | 489 -> One ([R 340])
  | 1208 -> One (R 342 :: r822)
  | 1380 -> One (R 342 :: r935)
  | 297 | 1385 -> One ([R 343])
  | 244 -> One ([R 346])
  | 149 -> One ([R 348])
  | 87 | 94 -> One ([R 350])
  | 107 -> One ([R 351])
  | 106 -> One ([R 352])
  | 105 -> One ([R 353])
  | 104 -> One ([R 354])
  | 103 -> One ([R 355])
  | 113 -> One ([R 357])
  | 116 -> One ([R 359])
  | 119 -> One ([R 361])
  | 85 -> One ([R 362])
  | 122 | 696 -> One ([R 363])
  | 97 | 423 | 669 -> One ([R 364])
  | 96 | 668 -> One ([R 365])
  | 101 | 695 | 986 -> One ([R 366])
  | 100 | 694 -> One ([R 367])
  | 84 -> One ([R 368])
  | 109 -> One ([R 369])
  | 102 -> One ([R 370])
  | 108 -> One ([R 371])
  | 99 -> One ([R 372])
  | 121 -> One ([R 373])
  | 123 -> One ([R 374])
  | 120 -> One ([R 376])
  | 95 -> One ([R 377])
  | 98 -> One ([R 378])
  | 208 -> One ([R 379])
  | 207 -> One (R 380 :: r157)
  | 176 -> One (R 381 :: r134)
  | 1594 -> One (R 381 :: r1032)
  | 177 -> One ([R 382])
  | 525 -> One (R 387 :: r393)
  | 588 -> One (R 387 :: r420)
  | 910 -> One (R 387 :: r648)
  | 918 -> One (R 387 :: r651)
  | 1026 -> One (R 387 :: r689)
  | 526 | 579 | 911 | 925 -> One ([R 388])
  | 942 -> One ([R 389])
  | 452 -> One (R 394 :: r334)
  | 388 -> One ([R 395])
  | 403 -> One (R 399 :: r269)
  | 606 -> One (R 399 :: r456)
  | 1429 -> One (R 399 :: r971)
  | 404 -> One ([R 400])
  | 556 -> One ([R 412])
  | 561 -> One ([R 416])
  | 555 -> One ([R 417])
  | 554 -> One ([R 420])
  | 800 -> One ([R 425])
  | 814 -> One ([R 428])
  | 580 -> One ([R 433])
  | 643 -> One ([R 434])
  | 1362 -> One ([R 438])
  | 374 -> One (R 446 :: r259)
  | 1267 -> One (R 446 :: r859)
  | 1335 -> One (R 446 :: r893)
  | 1473 -> One (R 446 :: r997)
  | 1511 -> One (R 446 :: r1008)
  | 1526 -> One (R 446 :: r1016)
  | 1134 -> One ([R 450])
  | 334 -> One (R 451 :: r228)
  | 1400 -> One (R 451 :: r952)
  | 335 | 1405 -> One ([R 452])
  | 1227 -> One ([R 454])
  | 1225 -> One ([R 455])
  | 1228 -> One ([R 456])
  | 1226 -> One ([R 457])
  | 516 -> One ([R 459])
  | 1504 -> One ([R 461])
  | 1503 -> One ([R 462])
  | 1329 -> One ([R 464])
  | 1328 -> One ([R 465])
  | 188 -> One ([R 468])
  | 788 -> One ([R 473])
  | 792 -> One ([R 474])
  | 1562 -> One ([R 478])
  | 1559 -> One ([R 479])
  | 1147 -> One (R 482 :: r757)
  | 1148 -> One (R 482 :: r758)
  | 1316 -> One (R 482 :: r887)
  | 1305 -> One ([R 485])
  | 1330 -> One ([R 486])
  | 1306 -> One ([R 487])
  | 1318 -> One ([R 488])
  | 1320 -> One ([R 489])
  | 1333 -> One ([R 490])
  | 1334 -> One ([R 491])
  | 1321 -> One ([R 492])
  | 1332 -> One ([R 493])
  | 1331 -> One ([R 494])
  | 1319 -> One ([R 495])
  | 1349 -> One ([R 496])
  | 1338 -> One ([R 497])
  | 1337 -> One ([R 499])
  | 421 -> One ([R 502])
  | 418 -> One ([R 504])
  | 187 -> One ([R 509])
  | 192 -> One ([R 510])
  | 281 -> One ([R 511])
  | 214 | 1297 -> One ([R 525])
  | 661 -> One ([R 534])
  | 699 -> One ([R 535])
  | 698 | 742 -> One ([R 536])
  | 663 | 722 -> One ([R 537])
  | 697 -> One ([R 574])
  | 896 -> One ([R 576])
  | 894 -> One ([R 577])
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
  | 1496 -> One ([R 647])
  | 387 -> One ([R 650])
  | 378 -> One ([R 651])
  | 386 -> One ([R 652])
  | 377 -> One ([R 653])
  | 376 -> One ([R 654])
  | 1489 -> One ([R 662])
  | 1509 | 1529 -> One ([R 663])
  | 1510 | 1530 -> One ([R 664])
  | 1505 -> One ([R 665])
  | 1486 -> One ([R 666])
  | 1487 -> One ([R 667])
  | 1493 -> One ([R 668])
  | 1495 -> One ([R 669])
  | 1508 -> One ([R 670])
  | 1497 -> One ([R 671])
  | 1507 -> One ([R 672])
  | 1506 -> One ([R 673])
  | 1515 -> One ([R 674])
  | 1514 -> One ([R 675])
  | 1494 -> One ([R 676])
  | 1513 -> One ([R 678])
  | 1490 -> One (R 679 :: r1003)
  | 508 -> One ([R 682])
  | 507 -> One ([R 683])
  | 392 -> One ([R 687])
  | 393 -> One ([R 688])
  | 395 -> One ([R 689])
  | 397 -> One ([R 690])
  | 394 -> One ([R 691])
  | 391 -> One ([R 692])
  | 1315 -> One ([R 697])
  | 1314 -> One ([R 698])
  | 333 -> One ([R 700])
  | 320 -> One ([R 701])
  | 342 -> One ([R 702])
  | 321 -> One ([R 703])
  | 341 -> One ([R 707])
  | 151 -> One ([R 717])
  | 152 -> One ([R 718])
  | 396 -> One ([R 723])
  | 399 -> One ([R 724])
  | 1213 -> One (R 733 :: r826)
  | 1273 -> One (R 733 :: r867)
  | 1364 -> One (R 733 :: r919)
  | 1182 -> One ([R 734])
  | 459 -> One ([R 741])
  | 460 -> One ([R 742])
  | 929 -> One (S (T T_WITH) :: r661)
  | 398 | 408 -> One (S (T T_UIDENT) :: r41)
  | 197 -> One (S (T T_UIDENT) :: r150)
  | 429 -> One (S (T T_TYPE) :: r293)
  | 1058 -> One (S (T T_TYPE) :: r704)
  | 1179 | 1363 -> One (S (T T_TYPE) :: r792)
  | 357 -> One (S (T T_RPAREN) :: r44)
  | 170 | 292 -> One (S (T T_RPAREN) :: r122)
  | 274 -> One (S (T T_RPAREN) :: r187)
  | 277 -> One (S (T T_RPAREN) :: r188)
  | 358 -> One (S (T T_RPAREN) :: r254)
  | 443 -> One (S (T T_RPAREN) :: r317)
  | 550 -> One (S (T T_RPAREN) :: r409)
  | 568 -> One (S (T T_RPAREN) :: r415)
  | 678 -> One (S (T T_RPAREN) :: r547)
  | 906 -> One (S (T T_RPAREN) :: r645)
  | 1152 -> One (S (T T_RPAREN) :: r771)
  | 1587 -> One (S (T T_RPAREN) :: r1030)
  | 114 -> One (S (T T_RBRACKET) :: r76)
  | 180 -> One (S (T T_RBRACKET) :: r135)
  | 231 -> One (S (T T_RBRACKET) :: r166)
  | 286 | 293 -> One (S (T T_RBRACKET) :: r192)
  | 360 -> One (S (T T_RBRACKET) :: r255)
  | 916 -> One (S (T T_RBRACKET) :: r649)
  | 117 -> One (S (T T_RBRACE) :: r77)
  | 222 -> One (S (T T_QUOTE) :: r164)
  | 351 -> One (S (T T_PLUSEQ) :: r243)
  | 1552 -> One (S (T T_PLUSEQ) :: r1024)
  | 1241 -> One (S (T T_OPEN) :: r845)
  | 1433 -> One (S (T T_OPEN) :: r979)
  | 141 -> One (S (T T_MODULE) :: r97)
  | 315 -> One (S (T T_MINUSGREATER) :: r221)
  | 1292 -> One (S (T T_MINUSGREATER) :: r880)
  | 110 -> One (S (T T_LPAREN) :: r75)
  | 364 -> One (S (T T_LPAREN) :: r258)
  | 137 -> One (S (T T_LIDENT) :: r88)
  | 1278 -> One (S (T T_LIDENT) :: r872)
  | 1465 -> One (S (T T_LIDENT) :: r993)
  | 732 -> One (S (T T_LESSMINUS) :: r577)
  | 329 -> One (S (T T_LBRACE) :: r226)
  | 416 -> One (S (T T_INT) :: r281)
  | 419 -> One (S (T T_INT) :: r282)
  | 724 -> One (S (T T_IN) :: r573)
  | 728 -> One (S (T T_IN) :: r575)
  | 1447 -> One (S (T T_IN) :: r990)
  | 613 -> One (S (T T_GREATERRBRACE) :: r463)
  | 1020 -> One (S (T T_GREATERRBRACE) :: r687)
  | 174 -> One (S (T T_GREATER) :: r127)
  | 250 -> One (S (T T_GREATER) :: r175)
  | 380 -> One (S (T T_EQUAL) :: r262)
  | 1064 -> One (S (T T_EQUAL) :: r706)
  | 1083 -> One (S (T T_EQUAL) :: r718)
  | 1394 -> One (S (T T_EQUAL) :: r940)
  | 1613 -> One (S (T T_EOF) :: r1035)
  | 1617 -> One (S (T T_EOF) :: r1036)
  | 1621 -> One (S (T T_EOF) :: r1037)
  | 1011 -> One (S (T T_END) :: r686)
  | 166 -> One (S (T T_DOTDOT) :: r120)
  | 169 -> One (S (T T_DOTDOT) :: r121)
  | 74 -> One (S (T T_DOT) :: r40)
  | 256 -> One (S (T T_DOT) :: r185)
  | 455 | 826 | 872 -> One (S (T T_DOT) :: r336)
  | 485 -> One (S (T T_DOT) :: r348)
  | 548 -> One (S (T T_DOT) :: r408)
  | 1078 -> One (S (T T_DOT) :: r716)
  | 1232 -> One (S (T T_DOT) :: r836)
  | 1250 -> One (S (T T_DOT) :: r851)
  | 252 -> One (S (T T_COLON) :: r182)
  | 447 -> One (S (T T_COLON) :: r320)
  | 1153 -> One (S (T T_COLON) :: r775)
  | 518 -> One (S (T T_BARRBRACKET) :: r380)
  | 611 -> One (S (T T_BARRBRACKET) :: r457)
  | 908 -> One (S (T T_BARRBRACKET) :: r646)
  | 183 | 1290 -> One (S (T T_BAR) :: r140)
  | 233 -> One (S (T T_BAR) :: r169)
  | 400 -> One (S (N N_structure) :: r264)
  | 1488 -> One (S (N N_structure) :: r1002)
  | 412 -> One (S (N N_pattern) :: r277)
  | 645 | 972 -> One (S (N N_pattern) :: r284)
  | 533 -> One (S (N N_pattern) :: r395)
  | 557 -> One (S (N N_pattern) :: r412)
  | 559 -> One (S (N N_pattern) :: r413)
  | 562 -> One (S (N N_pattern) :: r414)
  | 570 -> One (S (N N_pattern) :: r416)
  | 572 -> One (S (N N_pattern) :: r417)
  | 801 -> One (S (N N_pattern) :: r610)
  | 806 -> One (S (N N_pattern) :: r611)
  | 808 -> One (S (N N_pattern) :: r612)
  | 810 -> One (S (N N_pattern) :: r613)
  | 1117 -> One (S (N N_pattern) :: r728)
  | 439 -> One (S (N N_module_type) :: r310)
  | 440 -> One (S (N N_module_type) :: r312)
  | 482 -> One (S (N N_module_type) :: r346)
  | 682 -> One (S (N N_module_type) :: r549)
  | 1033 -> One (S (N N_module_type) :: r697)
  | 504 -> One (S (N N_module_expr) :: r353)
  | 636 -> One (S (N N_let_pattern) :: r508)
  | 616 -> One (S (N N_expr) :: r465)
  | 620 -> One (S (N N_expr) :: r477)
  | 707 -> One (S (N N_expr) :: r560)
  | 735 -> One (S (N N_expr) :: r578)
  | 737 -> One (S (N N_expr) :: r579)
  | 739 -> One (S (N N_expr) :: r580)
  | 744 -> One (S (N N_expr) :: r581)
  | 746 -> One (S (N N_expr) :: r582)
  | 748 -> One (S (N N_expr) :: r583)
  | 750 -> One (S (N N_expr) :: r584)
  | 752 -> One (S (N N_expr) :: r585)
  | 754 -> One (S (N N_expr) :: r586)
  | 756 -> One (S (N N_expr) :: r587)
  | 758 -> One (S (N N_expr) :: r588)
  | 760 -> One (S (N N_expr) :: r589)
  | 762 -> One (S (N N_expr) :: r590)
  | 764 -> One (S (N N_expr) :: r591)
  | 766 -> One (S (N N_expr) :: r592)
  | 768 -> One (S (N N_expr) :: r593)
  | 770 -> One (S (N N_expr) :: r594)
  | 772 -> One (S (N N_expr) :: r595)
  | 774 -> One (S (N N_expr) :: r596)
  | 776 -> One (S (N N_expr) :: r597)
  | 778 -> One (S (N N_expr) :: r598)
  | 780 -> One (S (N N_expr) :: r599)
  | 783 -> One (S (N N_expr) :: r600)
  | 785 -> One (S (N N_expr) :: r601)
  | 844 -> One (S (N N_expr) :: r632)
  | 849 -> One (S (N N_expr) :: r633)
  | 854 -> One (S (N N_expr) :: r637)
  | 860 -> One (S (N N_expr) :: r638)
  | 865 -> One (S (N N_expr) :: r639)
  | 870 -> One (S (N N_expr) :: r640)
  | 877 -> One (S (N N_expr) :: r641)
  | 882 -> One (S (N N_expr) :: r642)
  | 887 -> One (S (N N_expr) :: r643)
  | 890 -> One (S (N N_expr) :: r644)
  | 944 -> One (S (N N_expr) :: r666)
  | 1008 -> One (S (N N_expr) :: r685)
  | 604 -> One (Sub (r1) :: r437)
  | 631 -> One (Sub (r1) :: r494)
  | 964 -> One (Sub (r1) :: r672)
  | 1119 -> One (Sub (r1) :: r729)
  | 1597 -> One (Sub (r1) :: r1033)
  | 1599 -> One (Sub (r1) :: r1034)
  | 2 -> One (Sub (r10) :: r12)
  | 55 -> One (Sub (r10) :: r13)
  | 58 -> One (Sub (r10) :: r18)
  | 89 -> One (Sub (r10) :: r50)
  | 345 -> One (Sub (r10) :: r233)
  | 789 -> One (Sub (r10) :: r603)
  | 1115 -> One (Sub (r10) :: r727)
  | 1121 -> One (Sub (r10) :: r732)
  | 70 -> One (Sub (r33) :: r34)
  | 619 -> One (Sub (r33) :: r475)
  | 660 -> One (Sub (r33) :: r533)
  | 703 -> One (Sub (r33) :: r552)
  | 716 -> One (Sub (r33) :: r566)
  | 838 -> One (Sub (r33) :: r631)
  | 131 -> One (Sub (r36) :: r79)
  | 190 -> One (Sub (r36) :: r143)
  | 279 -> One (Sub (r36) :: r189)
  | 574 -> One (Sub (r51) :: r418)
  | 812 -> One (Sub (r51) :: r614)
  | 216 -> One (Sub (r55) :: r161)
  | 313 -> One (Sub (r55) :: r219)
  | 978 -> One (Sub (r55) :: r679)
  | 1283 -> One (Sub (r57) :: r875)
  | 1287 -> One (Sub (r57) :: r878)
  | 140 -> One (Sub (r59) :: r91)
  | 173 -> One (Sub (r59) :: r126)
  | 220 -> One (Sub (r59) :: r162)
  | 226 -> One (Sub (r61) :: r165)
  | 463 -> One (Sub (r61) :: r338)
  | 282 -> One (Sub (r63) :: r190)
  | 530 -> One (Sub (r63) :: r394)
  | 596 -> One (Sub (r63) :: r432)
  | 638 -> One (Sub (r63) :: r509)
  | 796 -> One (Sub (r63) :: r609)
  | 936 -> One (Sub (r63) :: r662)
  | 940 -> One (Sub (r63) :: r665)
  | 989 -> One (Sub (r63) :: r682)
  | 1199 -> One (Sub (r63) :: r811)
  | 1543 -> One (Sub (r63) :: r1018)
  | 156 -> One (Sub (r81) :: r118)
  | 257 -> One (Sub (r81) :: r186)
  | 389 -> One (Sub (r81) :: r263)
  | 428 -> One (Sub (r93) :: r289)
  | 1099 -> One (Sub (r93) :: r720)
  | 1102 -> One (Sub (r93) :: r722)
  | 1105 -> One (Sub (r93) :: r724)
  | 1582 -> One (Sub (r93) :: r1029)
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
  | 1386 -> One (Sub (r202) :: r938)
  | 1406 -> One (Sub (r202) :: r955)
  | 296 -> One (Sub (r210) :: r212)
  | 337 -> One (Sub (r210) :: r230)
  | 1162 -> One (Sub (r265) :: r777)
  | 414 -> One (Sub (r279) :: r280)
  | 1040 -> One (Sub (r299) :: r698)
  | 478 -> One (Sub (r325) :: r345)
  | 451 -> One (Sub (r327) :: r328)
  | 466 -> One (Sub (r342) :: r343)
  | 519 -> One (Sub (r382) :: r385)
  | 520 -> One (Sub (r390) :: r392)
  | 976 -> One (Sub (r398) :: r676)
  | 544 -> One (Sub (r404) :: r405)
  | 591 -> One (Sub (r423) :: r424)
  | 592 -> One (Sub (r426) :: r427)
  | 601 -> One (Sub (r426) :: r433)
  | 593 -> One (Sub (r429) :: r431)
  | 602 -> One (Sub (r429) :: r436)
  | 617 -> One (Sub (r472) :: r474)
  | 928 -> One (Sub (r472) :: r659)
  | 983 -> One (Sub (r499) :: r680)
  | 634 -> One (Sub (r504) :: r505)
  | 646 -> One (Sub (r511) :: r514)
  | 973 -> One (Sub (r511) :: r675)
  | 1072 -> One (Sub (r511) :: r712)
  | 1413 -> One (Sub (r511) :: r963)
  | 709 -> One (Sub (r562) :: r563)
  | 714 -> One (Sub (r562) :: r565)
  | 921 -> One (Sub (r655) :: r657)
  | 1002 -> One (Sub (r683) :: r684)
  | 1068 -> One (Sub (r700) :: r707)
  | 1151 -> One (Sub (r767) :: r769)
  | 1191 -> One (Sub (r798) :: r803)
  | 1184 -> One (Sub (r800) :: r802)
  | 1412 -> One (Sub (r828) :: r958)
  | 1247 -> One (Sub (r846) :: r849)
  | 1439 -> One (Sub (r846) :: r982)
  | 1461 -> One (Sub (r861) :: r992)
  | 1478 -> One (Sub (r861) :: r1000)
  | 1432 -> One (Sub (r911) :: r973)
  | 1482 -> One (Sub (r914) :: r1001)
  | 1375 -> One (Sub (r929) :: r931)
  | 1397 -> One (Sub (r946) :: r948)
  | 787 -> One (r0)
  | 1612 -> One (r2)
  | 1611 -> One (r3)
  | 1610 -> One (r4)
  | 1609 -> One (r5)
  | 1608 -> One (r6)
  | 53 -> One (r7)
  | 54 -> One (r9)
  | 1607 -> One (r11)
  | 57 -> One (r12)
  | 56 -> One (r13)
  | 1531 -> One (r14)
  | 1606 -> One (r16)
  | 1605 -> One (r17)
  | 59 -> One (r18)
  | 1604 -> One (r19)
  | 1603 -> One (r20)
  | 1602 -> One (r21)
  | 1601 -> One (r22)
  | 61 -> One (r23)
  | 62 -> One (r24)
  | 1593 -> One (r25)
  | 64 -> One (r26)
  | 1592 -> One (r27)
  | 1591 -> One (r28)
  | 1590 -> One (r29)
  | 1589 -> One (r30)
  | 66 -> One (r31)
  | 69 -> One (r32)
  | 1577 -> One (r34)
  | 73 -> One (r35)
  | 78 -> One (r37)
  | 72 -> One (r38)
  | 77 -> One (r39)
  | 75 -> One (r40)
  | 76 -> One (r41)
  | 80 -> One (r42)
  | 82 -> One (r43)
  | 86 -> One (r44)
  | 1586 -> One (r45)
  | 1585 -> One (r46)
  | 88 | 618 | 950 -> One (r47)
  | 1576 -> One (r48)
  | 1575 -> One (r49)
  | 90 -> One (r50)
  | 129 -> One (r52)
  | 189 -> One (r54)
  | 211 -> One (r56)
  | 210 -> One (r58)
  | 219 -> One (r60)
  | 276 -> One (r62)
  | 1574 -> One (r64)
  | 1573 -> One (r65)
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
  | 1572 -> One (r84)
  | 1571 -> One (r85)
  | 1570 -> One (r86)
  | 139 -> One (r87)
  | 138 -> One (r88)
  | 1569 -> One (r89)
  | 1568 -> One (r90)
  | 1567 -> One (r91)
  | 693 -> One (r92)
  | 1566 -> One (r94)
  | 1565 -> One (r95)
  | 143 -> One (r96)
  | 142 -> One (r97)
  | 1564 -> One (r98)
  | 1563 -> One (r99)
  | 146 -> One (r100)
  | 1551 -> One (r101)
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
  | 172 | 175 | 179 | 1286 -> One (r123)
  | 273 -> One (r124)
  | 272 -> One (r125)
  | 271 -> One (r126)
  | 246 -> One (r127)
  | 201 | 1291 -> One (r128)
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
  | 200 | 1296 -> One (r144)
  | 199 | 1295 -> One (r145)
  | 193 | 1294 -> One (r146)
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
  | 1550 -> One (r231)
  | 1549 -> One (r232)
  | 346 -> One (r233)
  | 385 -> One (r234)
  | 384 -> One (r235)
  | 1561 -> One (r236)
  | 379 -> One (r238)
  | 373 -> One (r240)
  | 372 -> One (r241)
  | 353 -> One (r242)
  | 352 -> One (r243)
  | 371 -> One (r244)
  | 370 -> One (r245)
  | 1556 -> One (r246)
  | 1555 -> One (r247)
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
  | 1548 -> One (r264)
  | 409 -> One (r266)
  | 407 -> One (r267)
  | 406 -> One (r268)
  | 405 -> One (r269)
  | 1374 -> One (r270)
  | 1373 -> One (r271)
  | 1547 -> One (r273)
  | 1546 -> One (r274)
  | 411 -> One (r275)
  | 1542 -> One (r276)
  | 1541 -> One (r277)
  | 413 -> One (r278)
  | 415 -> One (r280)
  | 417 -> One (r281)
  | 420 -> One (r282)
  | 988 -> One (r283)
  | 987 -> One (r284)
  | 427 -> One (r285)
  | 426 -> One (r286)
  | 425 -> One (r287)
  | 1540 -> One (r288)
  | 1539 -> One (r289)
  | 1538 -> One (r290)
  | 432 -> One (r291)
  | 431 -> One (r292)
  | 430 -> One (r293)
  | 1537 -> One (r294)
  | 1536 -> One (r295)
  | 434 -> One (r296)
  | 1108 -> One (r297)
  | 503 -> One (r298)
  | 1114 -> One (r300)
  | 1113 -> One (r301)
  | 1112 -> One (r302)
  | 1111 -> One (r303)
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
  | 1110 -> One (r349)
  | 1109 -> One (r350)
  | 502 -> One (r351)
  | 681 -> One (r352)
  | 680 -> One (r353)
  | 1098 -> One (r354)
  | 1097 -> One (r355)
  | 506 -> One (r356)
  | 1096 -> One (r357)
  | 1095 -> One (r358)
  | 1094 -> One (r359)
  | 1093 -> One (r360)
  | 510 -> One (r361)
  | 1092 -> One (r362)
  | 1091 -> One (r363)
  | 1090 -> One (r364)
  | 1089 -> One (r365)
  | 512 -> One (r366)
  | 566 -> One (r367)
  | 565 -> One (r368)
  | 815 -> One (r370)
  | 805 -> One (r372)
  | 804 -> One (r373)
  | 803 -> One (r374)
  | 1088 -> One (r376)
  | 1087 -> One (r377)
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
  | 1063 -> One (r421)
  | 1062 -> One (r422)
  | 1086 -> One (r424)
  | 594 -> One (r425)
  | 600 -> One (r427)
  | 595 -> One (r428)
  | 599 -> One (r430)
  | 598 -> One (r431)
  | 597 -> One (r432)
  | 1057 -> One (r433)
  | 1056 -> One (r434)
  | 1055 -> One (r435)
  | 603 -> One (r436)
  | 1054 -> One (r437)
  | 1049 -> One (r438)
  | 1048 -> One (r439)
  | 1047 -> One (r440)
  | 1046 -> One (r442)
  | 1045 -> One (r443)
  | 1044 -> One (r444)
  | 1043 -> One (r445)
  | 1042 -> One (r446)
  | 1053 -> One (r447)
  | 1052 -> One (r448)
  | 1051 -> One (r449)
  | 1050 -> One (r450)
  | 1491 -> One (r451)
  | 1029 -> One (r452)
  | 610 -> One (r453)
  | 609 -> One (r454)
  | 608 -> One (r455)
  | 607 -> One (r456)
  | 1025 -> One (r457)
  | 915 -> One (r458)
  | 1024 -> One (r460)
  | 1023 -> One (r461)
  | 1022 -> One (r462)
  | 614 -> One (r463)
  | 615 -> One (r464)
  | 1019 -> One (r465)
  | 943 -> One (r466)
  | 935 -> One (r467)
  | 932 -> One (r469)
  | 951 -> One (r471)
  | 1018 -> One (r473)
  | 1017 -> One (r474)
  | 1016 -> One (r475)
  | 1015 -> One (r476)
  | 1014 -> One (r477)
  | 1013 -> One (r478)
  | 622 -> One (r479)
  | 1010 -> One (r480)
  | 624 -> One (r481)
  | 1007 -> One (r482)
  | 1006 -> One (r483)
  | 1005 -> One (r484)
  | 626 -> One (r485)
  | 1001 -> One (r486)
  | 629 -> One (r487)
  | 628 -> One (r488)
  | 1000 -> One (r489)
  | 998 -> One (r490)
  | 630 -> One (r491)
  | 997 -> One (r492)
  | 996 -> One (r493)
  | 995 -> One (r494)
  | 982 -> One (r495)
  | 971 -> One (r497)
  | 651 -> One (r498)
  | 994 -> One (r500)
  | 993 -> One (r501)
  | 633 -> One (r502)
  | 635 -> One (r503)
  | 644 -> One (r505)
  | 642 -> One (r506)
  | 641 -> One (r507)
  | 640 -> One (r508)
  | 639 -> One (r509)
  | 647 -> One (r510)
  | 992 -> One (r512)
  | 650 -> One (r513)
  | 649 -> One (r514)
  | 963 -> One (r515)
  | 962 -> One (r516)
  | 961 -> One (r517)
  | 960 -> One (r518)
  | 655 -> One (r519)
  | 654 -> One (r520)
  | 653 -> One (r521)
  | 954 -> One (r522)
  | 959 -> One (r524)
  | 958 -> One (r525)
  | 957 -> One (r526)
  | 956 -> One (r527)
  | 955 -> One (r528)
  | 952 -> One (r529)
  | 659 -> One (r530)
  | 658 -> One (r531)
  | 657 -> One (r532)
  | 662 -> One (r533)
  | 667 -> One (r534)
  | 666 -> One (r535)
  | 665 | 949 -> One (r536)
  | 948 -> One (r537)
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
  | 848 | 902 -> One (r553)
  | 847 | 901 -> One (r554)
  | 846 | 900 -> One (r555)
  | 705 | 840 -> One (r556)
  | 843 | 899 -> One (r557)
  | 842 | 898 -> One (r558)
  | 706 | 841 -> One (r559)
  | 897 -> One (r560)
  | 710 -> One (r561)
  | 711 -> One (r563)
  | 713 -> One (r564)
  | 715 -> One (r565)
  | 717 -> One (r566)
  | 837 | 889 -> One (r567)
  | 718 | 856 -> One (r568)
  | 721 | 859 -> One (r569)
  | 720 | 858 -> One (r570)
  | 719 | 857 -> One (r571)
  | 726 -> One (r572)
  | 725 -> One (r573)
  | 730 -> One (r574)
  | 729 -> One (r575)
  | 782 -> One (r576)
  | 733 -> One (r577)
  | 736 -> One (r578)
  | 738 -> One (r579)
  | 740 -> One (r580)
  | 745 -> One (r581)
  | 747 -> One (r582)
  | 749 -> One (r583)
  | 751 -> One (r584)
  | 753 -> One (r585)
  | 755 -> One (r586)
  | 757 -> One (r587)
  | 759 -> One (r588)
  | 761 -> One (r589)
  | 763 -> One (r590)
  | 765 -> One (r591)
  | 767 -> One (r592)
  | 769 -> One (r593)
  | 771 -> One (r594)
  | 773 -> One (r595)
  | 775 -> One (r596)
  | 777 -> One (r597)
  | 779 -> One (r598)
  | 781 -> One (r599)
  | 784 -> One (r600)
  | 786 -> One (r601)
  | 791 -> One (r602)
  | 790 -> One (r603)
  | 817 -> One (r604)
  | 816 -> One (r605)
  | 794 -> One (r606)
  | 799 -> One (r607)
  | 798 -> One (r608)
  | 797 -> One (r609)
  | 802 -> One (r610)
  | 807 -> One (r611)
  | 809 -> One (r612)
  | 811 -> One (r613)
  | 813 -> One (r614)
  | 822 | 864 -> One (r615)
  | 821 | 863 -> One (r616)
  | 820 | 862 -> One (r617)
  | 825 | 869 -> One (r618)
  | 824 | 868 -> One (r619)
  | 823 | 867 -> One (r620)
  | 833 | 881 -> One (r621)
  | 832 | 880 -> One (r622)
  | 831 | 879 -> One (r623)
  | 827 | 873 -> One (r624)
  | 830 | 876 -> One (r625)
  | 829 | 875 -> One (r626)
  | 828 | 874 -> One (r627)
  | 836 | 886 -> One (r628)
  | 835 | 885 -> One (r629)
  | 834 | 884 -> One (r630)
  | 839 -> One (r631)
  | 845 -> One (r632)
  | 850 -> One (r633)
  | 853 | 905 -> One (r634)
  | 852 | 904 -> One (r635)
  | 851 | 903 -> One (r636)
  | 855 -> One (r637)
  | 861 -> One (r638)
  | 866 -> One (r639)
  | 871 -> One (r640)
  | 878 -> One (r641)
  | 883 -> One (r642)
  | 888 -> One (r643)
  | 891 -> One (r644)
  | 907 -> One (r645)
  | 909 -> One (r646)
  | 914 -> One (r647)
  | 913 -> One (r648)
  | 917 -> One (r649)
  | 920 -> One (r650)
  | 919 -> One (r651)
  | 927 -> One (r653)
  | 924 -> One (r654)
  | 923 -> One (r656)
  | 922 -> One (r657)
  | 947 -> One (r658)
  | 946 -> One (r659)
  | 931 -> One (r660)
  | 930 -> One (r661)
  | 937 -> One (r662)
  | 939 -> One (r663)
  | 938 | 1071 -> One (r664)
  | 941 -> One (r665)
  | 945 -> One (r666)
  | 970 -> One (r667)
  | 969 -> One (r668)
  | 968 -> One (r669)
  | 967 -> One (r670)
  | 966 -> One (r671)
  | 965 -> One (r672)
  | 985 -> One (r673)
  | 975 -> One (r674)
  | 974 -> One (r675)
  | 977 -> One (r676)
  | 981 -> One (r677)
  | 980 -> One (r678)
  | 979 -> One (r679)
  | 984 -> One (r680)
  | 991 -> One (r681)
  | 990 -> One (r682)
  | 1003 -> One (r684)
  | 1009 -> One (r685)
  | 1012 -> One (r686)
  | 1021 -> One (r687)
  | 1028 -> One (r688)
  | 1027 -> One (r689)
  | 1039 -> One (r690)
  | 1038 -> One (r691)
  | 1037 -> One (r692)
  | 1032 -> One (r693)
  | 1031 -> One (r694)
  | 1036 -> One (r695)
  | 1035 -> One (r696)
  | 1034 -> One (r697)
  | 1041 -> One (r698)
  | 1067 -> One (r699)
  | 1070 -> One (r701)
  | 1061 -> One (r702)
  | 1060 -> One (r703)
  | 1059 -> One (r704)
  | 1066 -> One (r705)
  | 1065 -> One (r706)
  | 1069 -> One (r707)
  | 1077 -> One (r708)
  | 1076 -> One (r709)
  | 1075 -> One (r710)
  | 1074 -> One (r711)
  | 1073 -> One (r712)
  | 1082 -> One (r713)
  | 1081 -> One (r714)
  | 1080 -> One (r715)
  | 1079 -> One (r716)
  | 1085 -> One (r717)
  | 1084 -> One (r718)
  | 1101 -> One (r719)
  | 1100 -> One (r720)
  | 1104 -> One (r721)
  | 1103 -> One (r722)
  | 1107 -> One (r723)
  | 1106 -> One (r724)
  | 1535 -> One (r725)
  | 1534 -> One (r726)
  | 1116 -> One (r727)
  | 1118 -> One (r728)
  | 1120 -> One (r729)
  | 1533 -> One (r730)
  | 1532 -> One (r731)
  | 1122 -> One (r732)
  | 1126 -> One (r733)
  | 1125 -> One (r734)
  | 1124 -> One (r735)
  | 1133 -> One (r736)
  | 1136 -> One (r738)
  | 1135 -> One (r739)
  | 1132 -> One (r740)
  | 1131 -> One (r741)
  | 1130 -> One (r742)
  | 1129 -> One (r743)
  | 1128 -> One (r744)
  | 1143 -> One (r745)
  | 1142 -> One (r746)
  | 1141 -> One (r747)
  | 1140 -> One (r748)
  | 1146 -> One (r751)
  | 1145 -> One (r752)
  | 1144 -> One (r753)
  | 1178 -> One (r754)
  | 1177 -> One (r755)
  | 1176 -> One (r756)
  | 1361 -> One (r757)
  | 1360 -> One (r758)
  | 1171 -> One (r759)
  | 1170 -> One (r760)
  | 1169 -> One (r761)
  | 1168 -> One (r762)
  | 1167 -> One (r763)
  | 1150 -> One (r764)
  | 1158 -> One (r765)
  | 1157 -> One (r766)
  | 1166 -> One (r768)
  | 1165 -> One (r769)
  | 1161 -> One (r770)
  | 1160 -> One (r771)
  | 1159 -> One (r772)
  | 1156 -> One (r773)
  | 1155 -> One (r774)
  | 1154 -> One (r775)
  | 1164 -> One (r776)
  | 1163 -> One (r777)
  | 1175 -> One (r778)
  | 1174 -> One (r779)
  | 1173 -> One (r780)
  | 1240 -> One (r781)
  | 1255 -> One (r783)
  | 1272 -> One (r785)
  | 1271 -> One (r786)
  | 1196 -> One (r787)
  | 1195 -> One (r788)
  | 1194 -> One (r789)
  | 1183 -> One (r790)
  | 1181 -> One (r791)
  | 1180 -> One (r792)
  | 1187 -> One (r793)
  | 1186 -> One (r794)
  | 1188 -> One (r796)
  | 1185 -> One (r797)
  | 1193 -> One (r799)
  | 1190 -> One (r801)
  | 1189 -> One (r802)
  | 1192 -> One (r803)
  | 1205 -> One (r804)
  | 1204 -> One (r805)
  | 1203 -> One (r807)
  | 1202 -> One (r808)
  | 1198 -> One (r809)
  | 1201 -> One (r810)
  | 1200 -> One (r811)
  | 1222 -> One (r812)
  | 1221 -> One (r813)
  | 1220 -> One (r814)
  | 1219 -> One (r816)
  | 1218 -> One (r817)
  | 1207 -> One (r818)
  | 1212 -> One (r819)
  | 1211 -> One (r820)
  | 1210 -> One (r821)
  | 1209 -> One (r822)
  | 1217 -> One (r823)
  | 1216 -> One (r824)
  | 1215 -> One (r825)
  | 1214 -> One (r826)
  | 1237 -> One (r827)
  | 1236 -> One (r829)
  | 1235 -> One (r830)
  | 1231 -> One (r831)
  | 1230 -> One (r832)
  | 1229 -> One (r833)
  | 1224 -> One (r834)
  | 1234 -> One (r835)
  | 1233 -> One (r836)
  | 1259 -> One (r837)
  | 1258 -> One (r838)
  | 1239 -> One (r839)
  | 1256 -> One (r840)
  | 1246 -> One (r841)
  | 1245 -> One (r842)
  | 1244 -> One (r843)
  | 1243 -> One (r844)
  | 1242 -> One (r845)
  | 1253 -> One (r847)
  | 1249 -> One (r848)
  | 1248 -> One (r849)
  | 1252 -> One (r850)
  | 1251 -> One (r851)
  | 1264 -> One (r852)
  | 1263 -> One (r853)
  | 1262 -> One (r854)
  | 1266 -> One (r856)
  | 1265 -> One (r857)
  | 1261 -> One (r858)
  | 1268 -> One (r859)
  | 1299 -> One (r860)
  | 1304 -> One (r862)
  | 1303 -> One (r863)
  | 1277 -> One (r864)
  | 1276 -> One (r865)
  | 1275 -> One (r866)
  | 1274 -> One (r867)
  | 1302 -> One (r868)
  | 1282 -> One (r869)
  | 1281 -> One (r870)
  | 1280 -> One (r871)
  | 1279 -> One (r872)
  | 1301 -> One (r873)
  | 1285 -> One (r874)
  | 1284 -> One (r875)
  | 1300 -> One (r876)
  | 1289 -> One (r877)
  | 1288 -> One (r878)
  | 1298 -> One (r879)
  | 1293 -> One (r880)
  | 1313 -> One (r881)
  | 1312 -> One (r882)
  | 1311 -> One (r883)
  | 1310 -> One (r884)
  | 1309 -> One (r885)
  | 1308 -> One (r886)
  | 1317 -> One (r887)
  | 1327 -> One (r888)
  | 1326 -> One (r889)
  | 1325 -> One (r890)
  | 1324 -> One (r891)
  | 1323 -> One (r892)
  | 1336 -> One (r893)
  | 1346 -> One (r894)
  | 1345 -> One (r895)
  | 1344 -> One (r896)
  | 1343 -> One (r897)
  | 1342 -> One (r898)
  | 1341 -> One (r899)
  | 1340 -> One (r900)
  | 1357 -> One (r901)
  | 1356 -> One (r902)
  | 1355 -> One (r903)
  | 1354 -> One (r904)
  | 1353 -> One (r905)
  | 1352 -> One (r906)
  | 1351 -> One (r907)
  | 1452 -> One (r908)
  | 1450 -> One (r910)
  | 1477 -> One (r912)
  | 1368 -> One (r913)
  | 1485 -> One (r915)
  | 1484 -> One (r916)
  | 1367 -> One (r917)
  | 1366 -> One (r918)
  | 1365 -> One (r919)
  | 1372 -> One (r920)
  | 1371 -> One (r921)
  | 1370 -> One (r922)
  | 1393 -> One (r923)
  | 1392 -> One (r924)
  | 1391 -> One (r925)
  | 1390 -> One (r926)
  | 1379 -> One (r927)
  | 1378 -> One (r928)
  | 1377 -> One (r930)
  | 1376 -> One (r931)
  | 1384 -> One (r932)
  | 1383 -> One (r933)
  | 1382 -> One (r934)
  | 1381 -> One (r935)
  | 1389 -> One (r936)
  | 1388 -> One (r937)
  | 1387 -> One (r938)
  | 1396 -> One (r939)
  | 1395 -> One (r940)
  | 1422 -> One (r941)
  | 1411 -> One (r942)
  | 1410 -> One (r943)
  | 1399 -> One (r944)
  | 1398 -> One (r945)
  | 1424 -> One (r947)
  | 1423 -> One (r948)
  | 1404 -> One (r949)
  | 1403 -> One (r950)
  | 1402 -> One (r951)
  | 1401 -> One (r952)
  | 1409 -> One (r953)
  | 1408 -> One (r954)
  | 1407 -> One (r955)
  | 1421 -> One (r956)
  | 1420 -> One (r957)
  | 1419 -> One (r958)
  | 1418 -> One (r959)
  | 1417 -> One (r960)
  | 1416 -> One (r961)
  | 1415 -> One (r962)
  | 1414 -> One (r963)
  | 1428 -> One (r964)
  | 1427 -> One (r965)
  | 1426 -> One (r966)
  | 1468 -> One (r967)
  | 1467 -> One (r968)
  | 1464 -> One (r969)
  | 1431 -> One (r970)
  | 1430 -> One (r971)
  | 1460 -> One (r972)
  | 1459 -> One (r973)
  | 1458 -> One (r974)
  | 1438 -> One (r975)
  | 1437 -> One (r976)
  | 1436 -> One (r977)
  | 1435 -> One (r978)
  | 1434 -> One (r979)
  | 1442 -> One (r980)
  | 1441 -> One (r981)
  | 1440 -> One (r982)
  | 1455 -> One (r983)
  | 1446 -> One (r984)
  | 1445 -> One (r985)
  | 1457 -> One (r987)
  | 1444 -> One (r988)
  | 1453 -> One (r989)
  | 1448 -> One (r990)
  | 1463 -> One (r991)
  | 1462 -> One (r992)
  | 1466 -> One (r993)
  | 1472 -> One (r994)
  | 1471 -> One (r995)
  | 1470 -> One (r996)
  | 1474 -> One (r997)
  | 1481 -> One (r998)
  | 1480 -> One (r999)
  | 1479 -> One (r1000)
  | 1483 -> One (r1001)
  | 1525 -> One (r1002)
  | 1492 -> One (r1003)
  | 1502 -> One (r1004)
  | 1501 -> One (r1005)
  | 1500 -> One (r1006)
  | 1499 -> One (r1007)
  | 1512 -> One (r1008)
  | 1522 -> One (r1009)
  | 1521 -> One (r1010)
  | 1520 -> One (r1011)
  | 1519 -> One (r1012)
  | 1518 -> One (r1013)
  | 1517 -> One (r1014)
  | 1528 -> One (r1015)
  | 1527 -> One (r1016)
  | 1545 -> One (r1017)
  | 1544 -> One (r1018)
  | 1560 -> One (r1019)
  | 1558 -> One (r1021)
  | 1557 -> One (r1022)
  | 1554 -> One (r1023)
  | 1553 -> One (r1024)
  | 1581 -> One (r1025)
  | 1580 -> One (r1026)
  | 1579 -> One (r1027)
  | 1584 -> One (r1028)
  | 1583 -> One (r1029)
  | 1588 -> One (r1030)
  | 1596 -> One (r1031)
  | 1595 -> One (r1032)
  | 1598 -> One (r1033)
  | 1600 -> One (r1034)
  | 1614 -> One (r1035)
  | 1618 -> One (r1036)
  | 1622 -> One (r1037)
  | 664 -> Select (function
    | -1 -> [R 99]
    | _ -> r537)
  | 435 -> Select (function
    | -1 -> S (T T_TYPE) :: r309
    | _ -> R 200 :: r304)
  | 1137 -> Select (function
    | -1 -> r756
    | _ -> R 200 :: r750)
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
    | 59 | 90 | 346 | 400 | 434 | 1116 | 1122 | 1488 -> r451
    | _ -> S (T T_EXCEPTION) :: r446)
  | 422 -> Select (function
    | 517 | 794 | 1051 -> r73
    | _ -> S (N N_pattern) :: r284)
  | 254 -> Select (function
    | 1071 -> r83
    | _ -> Sub (r81) :: r183)
  | 354 -> Select (function
    | 372 -> r247
    | _ -> Sub (r194) :: r253)
  | 513 -> Select (function
    | -1 -> r47
    | _ -> r201)
  | 255 -> Select (function
    | 1071 -> r82
    | _ -> r183)
  | 355 -> Select (function
    | 353 -> r253
    | _ -> r246)
  | 1139 -> Select (function
    | -1 -> r754
    | _ -> r749)
  | 1138 -> Select (function
    | -1 -> r755
    | _ -> r750)
  | _ -> raise Not_found
