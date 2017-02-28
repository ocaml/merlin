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
  [|0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;2;1;2;1;2;1;1;2;1;2;1;1;1;1;1;2;1;1;2;3;3;3;1;2;1;2;1;1;1;2;1;1;1;2;1;2;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;3;4;1;1;1;2;1;1;1;2;1;2;3;1;1;2;3;1;1;2;1;2;1;3;1;1;1;1;1;1;2;2;2;3;2;3;1;4;5;1;1;1;2;1;2;1;2;1;1;2;1;2;3;1;2;1;2;1;1;2;1;1;2;2;1;2;1;2;1;1;1;2;3;2;1;2;3;4;2;3;2;3;1;3;4;2;3;1;2;1;3;1;1;2;1;1;3;2;3;1;1;2;3;2;3;4;4;2;3;5;2;1;2;3;2;3;4;5;3;4;3;4;4;5;6;2;1;1;2;3;2;3;3;4;5;6;1;1;2;1;3;4;5;2;3;1;2;3;4;5;4;2;3;2;1;1;2;1;1;1;1;1;2;1;1;1;1;2;3;4;5;6;7;2;3;1;2;1;1;2;1;2;2;3;4;3;4;3;3;2;1;1;2;3;1;2;2;3;4;5;2;3;1;4;4;5;6;7;5;2;6;7;1;2;1;2;3;4;5;6;7;1;2;3;1;1;2;1;1;2;4;5;3;4;8;9;1;2;2;2;1;1;1;2;3;4;2;3;1;1;1;1;2;3;3;3;3;3;1;3;2;3;1;1;1;1;1;2;3;4;5;1;2;1;1;1;2;1;2;2;1;2;2;1;1;2;3;4;5;5;1;2;3;4;1;2;1;2;3;4;1;1;1;2;1;1;1;2;2;3;1;4;2;1;1;1;1;2;2;2;3;2;3;1;2;1;1;2;3;3;1;2;4;5;6;2;1;2;3;3;3;4;5;1;1;2;3;4;2;3;4;3;2;3;1;5;2;3;2;1;2;3;3;1;1;3;4;5;2;1;2;3;2;5;6;2;3;1;1;2;3;1;1;1;2;1;2;1;1;1;2;3;1;2;3;4;1;5;2;3;2;3;3;4;5;2;1;1;1;2;4;5;5;6;7;1;1;1;1;1;2;1;3;1;1;1;1;2;3;1;2;3;1;4;3;1;1;2;2;3;1;2;1;1;1;1;1;2;1;1;1;1;1;1;2;3;1;1;1;2;3;2;3;2;1;2;1;2;3;4;4;5;2;3;1;1;2;2;3;2;3;3;4;2;2;3;3;4;1;3;3;2;3;3;4;5;3;1;1;4;2;2;3;4;1;1;2;1;1;2;3;3;4;2;1;2;3;1;1;2;3;4;5;6;1;1;1;2;1;2;1;1;1;1;1;2;1;2;1;2;1;2;3;1;2;1;2;1;1;2;1;2;3;3;4;5;1;2;3;4;1;2;5;6;1;1;2;3;4;1;2;3;4;1;1;2;1;1;2;3;4;4;5;6;7;8;9;1;1;1;1;1;2;3;2;3;2;3;1;1;1;1;2;2;3;1;2;1;2;1;2;2;3;4;5;6;1;2;1;2;3;1;1;2;3;1;1;2;1;2;3;2;3;2;3;2;1;2;1;2;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;3;2;3;2;3;1;2;3;4;5;3;1;2;1;2;3;4;5;1;2;3;1;2;3;2;3;2;3;2;3;2;3;2;1;3;4;2;2;7;3;4;5;6;7;3;4;5;6;7;3;4;5;2;1;2;1;1;2;4;5;3;4;5;3;4;5;3;4;5;3;4;4;2;3;5;6;1;3;4;4;5;6;3;4;5;1;2;3;2;3;1;2;3;1;2;3;1;1;2;1;2;3;4;1;2;3;4;4;5;1;2;3;1;5;1;1;6;7;8;9;10;5;6;7;8;4;5;6;7;8;9;10;2;1;2;3;4;1;2;1;2;3;4;1;1;2;5;7;3;4;3;4;5;2;3;3;4;2;3;1;3;4;5;6;7;3;3;4;3;2;3;2;2;3;3;2;3;2;3;4;2;2;3;4;7;2;3;4;1;2;3;4;5;6;7;1;2;2;3;4;5;6;1;2;3;2;3;4;5;2;4;5;2;1;2;3;4;1;2;1;2;3;1;1;2;5;2;3;4;5;6;7;8;3;4;5;6;7;2;4;5;2;3;4;5;6;7;8;2;3;3;4;3;4;5;6;3;4;5;6;4;5;5;6;7;5;6;7;7;8;9;2;4;5;3;4;5;6;1;2;1;2;3;4;1;2;1;2;3;4;1;2;3;4;5;6;1;2;7;8;1;2;3;4;5;6;7;4;5;6;1;1;1;2;3;1;2;3;4;5;1;2;6;2;3;4;5;6;4;5;3;4;5;6;7;1;2;3;4;1;2;3;1;2;3;1;4;1;2;3;5;6;7;1;2;1;2;3;3;4;1;2;1;2;1;2;3;4;5;1;2;3;4;5;3;4;1;2;3;1;2;1;2;1;2;3;4;5;1;2;3;6;7;1;1;2;1;1;2;3;1;2;3;4;1;1;3;4;2;1;2;1;2;3;3;4;1;2;1;2;8;9;2;3;4;5;6;1;2;3;4;5;1;2;3;1;2;3;4;1;1;1;2;1;2;3;1;3;1;5;4;6;7;8;1;1;1;2;3;4;5;6;7;2;1;1;2;1;1;1;1;1;2;3;4;5;6;2;1;1;1;1;1;1;1;2;1;1;1;2;3;4;5;6;7;8;2;1;1;1;2;3;4;5;6;7;8;2;1;2;2;2;1;2;3;4;5;1;1;2;3;4;1;2;1;2;3;1;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;5;6;7;1;1;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;8;9;10;11;6;7;8;5;2;3;1;2;3;4;1;2;3;1;1;1;2;3;4;1;2;1;2;1;2;1;1;2;1;3;2;3;2;3;2;3;3;4;5;4;1;2;5;6;1;2;3;4;1;2;1;2;2;1;2;3;4;1;2;6;7;1;1;1;1;1;2;1;1;1;1;1;1;2;3;4;5;2;1;1;1;1;1;1;1;1;2;1;1;1;1;2;3;4;5;6;7;2;1;2;1;2;3;1;1;1;3;4;3;4;3;4;5;6;7;2;3;4;5;6;7;8;2;3;3;4;5;3;4;2;3;4;8;5;6;7;1;2;8;9;2;1;1;1;3;4;4;5;2;3;4;4;5;6;5;6;3;4;2;3;4;5;6;3;4;5;6;7;8;4;5;3;4;5;6;3;4;3;2;3;4;5;6;1;2;1;0;1;2;1;0;1;2;1;|]

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
  let r0 = [R 455] in
  let r1 = S (N N_expr) :: r0 in
  let r2 = [R 133] in
  let r3 = S (T T_DONE) :: r2 in
  let r4 = Sub (r1) :: r3 in
  let r5 = S (T T_DO) :: r4 in
  let r6 = Sub (r1) :: r5 in
  let r7 = [R 575] in
  let r8 = S (T T_AND) :: r7 in
  let r9 = [R 14] in
  let r10 = Sub (r8) :: r9 in
  let r11 = [R 193] in
  let r12 = R 17 :: r11 in
  let r13 = [R 15] in
  let r14 = [R 419] in
  let r15 = S (N N_structure) :: r14 in
  let r16 = [R 16] in
  let r17 = S (T T_RBRACKET) :: r16 in
  let r18 = Sub (r15) :: r17 in
  let r19 = Sub (r10) :: r18 in
  let r20 = [R 152] in
  let r21 = S (T T_DONE) :: r20 in
  let r22 = Sub (r1) :: r21 in
  let r23 = S (T T_DO) :: r22 in
  let r24 = Sub (r1) :: r23 in
  let r25 = [R 295] in
  let r26 = [R 129] in
  let r27 = Sub (r1) :: r26 in
  let r28 = [R 146] in
  let r29 = S (N N_match_cases) :: r28 in
  let r30 = R 361 :: r29 in
  let r31 = S (T T_WITH) :: r30 in
  let r32 = Sub (r1) :: r31 in
  let r33 = [R 552] in
  let r34 = S (T T_QUESTIONQUESTION) :: r33 in
  let r35 = [R 540] in
  let r36 = [R 48] in
  let r37 = S (T T_LIDENT) :: r36 in
  let r38 = [R 542] in
  let r39 = Sub (r37) :: r38 in
  let r40 = [R 49] in
  let r41 = S (T T_LIDENT) :: r40 in
  let r42 = [R 296] in
  let r43 = [R 192] in
  let r44 = [R 18] in
  let r45 = [R 521] in
  let r46 = S (T T_RPAREN) :: r45 in
  let r47 = Sub (r1) :: r46 in
  let r48 = [R 99] in
  let r49 = [R 700] in
  let r50 = [R 194] in
  let r51 = S (T T_RBRACKET) :: r50 in
  let r52 = Sub (r15) :: r51 in
  let r53 = S (T T_LIDENT) :: r49 in
  let r54 = [R 491] in
  let r55 = S (T T_UNDERSCORE) :: r54 in
  let r56 = [R 488] in
  let r57 = Sub (r55) :: r56 in
  let r58 = [R 509] in
  let r59 = Sub (r57) :: r58 in
  let r60 = [R 114] in
  let r61 = Sub (r59) :: r60 in
  let r62 = [R 123] in
  let r63 = Sub (r61) :: r62 in
  let r64 = [R 112] in
  let r65 = Sub (r63) :: r64 in
  let r66 = [R 708] in
  let r67 = R 429 :: r66 in
  let r68 = Sub (r65) :: r67 in
  let r69 = S (T T_COLON) :: r68 in
  let r70 = Sub (r53) :: r69 in
  let r71 = [R 355] in
  let r72 = S (T T_AMPERAMPER) :: r71 in
  let r73 = [R 701] in
  let r74 = S (T T_RPAREN) :: r73 in
  let r75 = [R 292] in
  let r76 = [R 497] in
  let r77 = [R 221] in
  let r78 = S (T T_LIDENT) :: r77 in
  let r79 = [R 490] in
  let r80 = Sub (r78) :: r79 in
  let r81 = [R 115] in
  let r82 = Sub (r61) :: r81 in
  let r83 = S (T T_MINUSGREATER) :: r82 in
  let r84 = Sub (r61) :: r83 in
  let r85 = S (T T_COLON) :: r84 in
  let r86 = [R 116] in
  let r87 = Sub (r61) :: r86 in
  let r88 = S (T T_MINUSGREATER) :: r87 in
  let r89 = [R 381] in
  let r90 = S (N N_module_type) :: r89 in
  let r91 = [R 507] in
  let r92 = S (T T_RPAREN) :: r91 in
  let r93 = Sub (r90) :: r92 in
  let r94 = R 191 :: r93 in
  let r95 = [R 315] in
  let r96 = S (T T_END) :: r95 in
  let r97 = R 465 :: r96 in
  let r98 = [R 673] in
  let r99 = R 429 :: r98 in
  let r100 = R 105 :: r99 in
  let r101 = R 676 :: r100 in
  let r102 = S (T T_LIDENT) :: r101 in
  let r103 = R 374 :: r102 in
  let r104 = R 333 :: r103 in
  let r105 = R 191 :: r104 in
  let r106 = [R 378] in
  let r107 = S (T T_UNDERSCORE) :: r106 in
  let r108 = [R 371] in
  let r109 = Sub (r107) :: r108 in
  let r110 = R 695 :: r109 in
  let r111 = [R 372] in
  let r112 = Sub (r110) :: r111 in
  let r113 = [R 376] in
  let r114 = S (T T_RPAREN) :: r113 in
  let r115 = [R 377] in
  let r116 = [R 373] in
  let r117 = [R 681] in
  let r118 = [R 95] in
  let r119 = S (T T_FALSE) :: r118 in
  let r120 = [R 108] in
  let r121 = R 17 :: r120 in
  let r122 = R 216 :: r121 in
  let r123 = Sub (r119) :: r122 in
  let r124 = [R 109] in
  let r125 = Sub (r123) :: r124 in
  let r126 = [R 680] in
  let r127 = [R 93] in
  let r128 = [R 686] in
  let r129 = [R 117] in
  let r130 = Sub (r61) :: r129 in
  let r131 = S (T T_MINUSGREATER) :: r130 in
  let r132 = [R 496] in
  let r133 = [R 225] in
  let r134 = [R 495] in
  let r135 = [R 426] in
  let r136 = Sub (r63) :: r135 in
  let r137 = [R 202] in
  let r138 = R 17 :: r137 in
  let r139 = S (T T_SEMI) :: r138 in
  let r140 = R 17 :: r139 in
  let r141 = Sub (r136) :: r140 in
  let r142 = [R 698] in
  let r143 = [R 452] in
  let r144 = Sub (r57) :: r143 in
  let r145 = [R 453] in
  let r146 = Sub (r144) :: r145 in
  let r147 = [R 505] in
  let r148 = S (T T_RBRACKET) :: r147 in
  let r149 = Sub (r146) :: r148 in
  let r150 = [R 504] in
  let r151 = [R 503] in
  let r152 = S (T T_RBRACKET) :: r151 in
  let r153 = [R 501] in
  let r154 = S (T T_RBRACKET) :: r153 in
  let r155 = Sub (r146) :: r154 in
  let r156 = [R 330] in
  let r157 = Sub (r78) :: r156 in
  let r158 = [R 498] in
  let r159 = [R 687] in
  let r160 = S (T T_LIDENT) :: r159 in
  let r161 = S (T T_DOT) :: r160 in
  let r162 = S (T T_UIDENT) :: r75 in
  let r163 = [R 294] in
  let r164 = S (T T_RPAREN) :: r163 in
  let r165 = [R 293] in
  let r166 = [R 454] in
  let r167 = [R 662] in
  let r168 = [R 5] in
  let r169 = Sub (r63) :: r168 in
  let r170 = [R 661] in
  let r171 = R 17 :: r170 in
  let r172 = Sub (r169) :: r171 in
  let r173 = [R 121] in
  let r174 = Sub (r57) :: r173 in
  let r175 = [R 510] in
  let r176 = [R 122] in
  let r177 = [R 118] in
  let r178 = [R 124] in
  let r179 = Sub (r78) :: r178 in
  let r180 = [R 6] in
  let r181 = [R 500] in
  let r182 = [R 502] in
  let r183 = S (T T_RBRACKET) :: r182 in
  let r184 = Sub (r146) :: r183 in
  let r185 = S (T T_BACKQUOTE) :: r157 in
  let r186 = [R 331] in
  let r187 = Sub (r185) :: r186 in
  let r188 = [R 506] in
  let r189 = S (T T_RBRACKET) :: r188 in
  let r190 = [R 427] in
  let r191 = Sub (r63) :: r190 in
  let r192 = [R 699] in
  let r193 = [R 94] in
  let r194 = [R 489] in
  let r195 = [R 499] in
  let r196 = [R 120] in
  let r197 = [R 119] in
  let r198 = [R 92] in
  let r199 = [R 19] in
  let r200 = R 17 :: r199 in
  let r201 = R 216 :: r200 in
  let r202 = [R 106] in
  let r203 = Sub (r174) :: r202 in
  let r204 = [R 217] in
  let r205 = S (T T_LIDENT) :: r133 in
  let r206 = [R 226] in
  let r207 = R 17 :: r206 in
  let r208 = Sub (r136) :: r207 in
  let r209 = S (T T_COLON) :: r208 in
  let r210 = Sub (r205) :: r209 in
  let r211 = R 328 :: r210 in
  let r212 = [R 228] in
  let r213 = Sub (r211) :: r212 in
  let r214 = [R 107] in
  let r215 = S (T T_RBRACE) :: r214 in
  let r216 = [R 227] in
  let r217 = R 17 :: r216 in
  let r218 = S (T T_SEMI) :: r217 in
  let r219 = R 17 :: r218 in
  let r220 = Sub (r136) :: r219 in
  let r221 = S (T T_COLON) :: r220 in
  let r222 = [R 219] in
  let r223 = [R 218] in
  let r224 = Sub (r57) :: r223 in
  let r225 = [R 682] in
  let r226 = S (T T_RBRACE) :: r225 in
  let r227 = Sub (r213) :: r226 in
  let r228 = [R 684] in
  let r229 = [R 683] in
  let r230 = [R 685] in
  let r231 = S (T T_RBRACE) :: r230 in
  let r232 = [R 428] in
  let r233 = S (T T_RBRACKET) :: r232 in
  let r234 = Sub (r15) :: r233 in
  let r235 = [R 195] in
  let r236 = R 17 :: r235 in
  let r237 = R 216 :: r236 in
  let r238 = Sub (r119) :: r237 in
  let r239 = [R 626] in
  let r240 = Sub (r238) :: r239 in
  let r241 = [R 633] in
  let r242 = R 429 :: r241 in
  let r243 = Sub (r240) :: r242 in
  let r244 = R 434 :: r243 in
  let r245 = [R 20] in
  let r246 = R 17 :: r245 in
  let r247 = R 216 :: r246 in
  let r248 = Sub (r119) :: r247 in
  let r249 = [R 100] in
  let r250 = S (T T_FALSE) :: r249 in
  let r251 = [R 21] in
  let r252 = R 17 :: r251 in
  let r253 = Sub (r250) :: r252 in
  let r254 = S (T T_EQUAL) :: r253 in
  let r255 = [R 98] in
  let r256 = [R 430] in
  let r257 = [R 196] in
  let r258 = R 17 :: r257 in
  let r259 = Sub (r250) :: r258 in
  let r260 = [R 663] in
  let r261 = [R 657] in
  let r262 = S (T T_UIDENT) :: r25 in
  let r263 = [R 335] in
  let r264 = R 429 :: r263 in
  let r265 = Sub (r262) :: r264 in
  let r266 = R 191 :: r265 in
  let r267 = [R 74] in
  let r268 = R 41 :: r267 in
  let r269 = R 52 :: r268 in
  let r270 = [R 185] in
  let r271 = S (T T_END) :: r270 in
  let r272 = Sub (r269) :: r271 in
  let r273 = [R 50] in
  let r274 = S (T T_RPAREN) :: r273 in
  let r275 = [R 557] in
  let r276 = S (T T_LIDENT) :: r128 in
  let r277 = [R 562] in
  let r278 = [R 486] in
  let r279 = [R 484] in
  let r280 = [R 568] in
  let r281 = S (T T_RPAREN) :: r280 in
  let r282 = [R 570] in
  let r283 = S (T T_RPAREN) :: r282 in
  let r284 = S (T T_UIDENT) :: r283 in
  let r285 = [R 571] in
  let r286 = S (T T_RPAREN) :: r285 in
  let r287 = [R 319] in
  let r288 = S (N N_module_expr) :: r287 in
  let r289 = R 17 :: r288 in
  let r290 = S (T T_OF) :: r289 in
  let r291 = [R 307] in
  let r292 = S (T T_END) :: r291 in
  let r293 = S (N N_structure) :: r292 in
  let r294 = [R 299] in
  let r295 = S (N N_module_expr) :: r294 in
  let r296 = S (T T_EQUAL) :: r295 in
  let r297 = [R 443] in
  let r298 = R 429 :: r297 in
  let r299 = Sub (r296) :: r298 in
  let r300 = S (T T_UIDENT) :: r299 in
  let r301 = S (T T_REC) :: r300 in
  let r302 = [R 323] in
  let r303 = R 429 :: r302 in
  let r304 = R 324 :: r303 in
  let r305 = Sub (r78) :: r304 in
  let r306 = R 191 :: r305 in
  let r307 = [R 325] in
  let r308 = [R 320] in
  let r309 = S (T T_RPAREN) :: r308 in
  let r310 = [R 316] in
  let r311 = S (N N_module_type) :: r310 in
  let r312 = S (T T_MINUSGREATER) :: r311 in
  let r313 = S (N N_functor_args) :: r312 in
  let r314 = [R 210] in
  let r315 = [R 211] in
  let r316 = S (T T_RPAREN) :: r315 in
  let r317 = S (N N_module_type) :: r316 in
  let r318 = [R 717] in
  let r319 = Sub (r162) :: r318 in
  let r320 = S (T T_COLONEQUAL) :: r319 in
  let r321 = S (T T_UIDENT) :: r320 in
  let r322 = S (T T_MODULE) :: r321 in
  let r323 = [R 718] in
  let r324 = Sub (r322) :: r323 in
  let r325 = [R 318] in
  let r326 = [R 715] in
  let r327 = Sub (r63) :: r326 in
  let r328 = S (T T_COLONEQUAL) :: r327 in
  let r329 = Sub (r205) :: r328 in
  let r330 = [R 694] in
  let r331 = Sub (r78) :: r330 in
  let r332 = S (T T_QUOTE) :: r331 in
  let r333 = [R 688] in
  let r334 = Sub (r332) :: r333 in
  let r335 = R 695 :: r334 in
  let r336 = [R 689] in
  let r337 = Sub (r335) :: r336 in
  let r338 = [R 693] in
  let r339 = S (T T_RPAREN) :: r338 in
  let r340 = [R 690] in
  let r341 = [R 239] in
  let r342 = S (T T_LIDENT) :: r341 in
  let r343 = [R 720] in
  let r344 = S (T T_EQUAL) :: r343 in
  let r345 = [R 714] in
  let r346 = R 105 :: r345 in
  let r347 = Sub (r63) :: r346 in
  let r348 = [R 102] in
  let r349 = Sub (r65) :: r348 in
  let r350 = S (T T_EQUAL) :: r349 in
  let r351 = Sub (r65) :: r350 in
  let r352 = [R 104] in
  let r353 = [R 716] in
  let r354 = Sub (r162) :: r353 in
  let r355 = [R 719] in
  let r356 = [R 317] in
  let r357 = [R 327] in
  let r358 = Sub (r78) :: r357 in
  let r359 = [R 298] in
  let r360 = R 429 :: r359 in
  let r361 = Sub (r296) :: r360 in
  let r362 = [R 383] in
  let r363 = S (T T_RPAREN) :: r362 in
  let r364 = [R 384] in
  let r365 = S (T T_RPAREN) :: r364 in
  let r366 = S (N N_expr) :: r365 in
  let r367 = [R 128] in
  let r368 = S (N N_match_cases) :: r367 in
  let r369 = R 361 :: r368 in
  let r370 = S (T T_WITH) :: r369 in
  let r371 = Sub (r1) :: r370 in
  let r372 = [R 145] in
  let r373 = S (N N_match_cases) :: r372 in
  let r374 = R 361 :: r373 in
  let r375 = S (T T_WITH) :: r374 in
  let r376 = Sub (r1) :: r375 in
  let r377 = [R 548] in
  let r378 = S (T T_RPAREN) :: r377 in
  let r379 = S (N N_module_expr) :: r378 in
  let r380 = [R 308] in
  let r381 = S (N N_module_expr) :: r380 in
  let r382 = S (T T_MINUSGREATER) :: r381 in
  let r383 = S (N N_functor_args) :: r382 in
  let r384 = [R 310] in
  let r385 = [R 382] in
  let r386 = S (T T_RPAREN) :: r385 in
  let r387 = S (T T_LBRACKETAT) :: r19 in
  let r388 = [R 388] in
  let r389 = [R 549] in
  let r390 = S (T T_RPAREN) :: r389 in
  let r391 = [R 403] in
  let r392 = S (N N_pattern) :: r391 in
  let r393 = Sub (r250) :: r392 in
  let r394 = [R 412] in
  let r395 = Sub (r393) :: r394 in
  let r396 = [R 266] in
  let r397 = Sub (r1) :: r396 in
  let r398 = S (T T_EQUAL) :: r397 in
  let r399 = Sub (r395) :: r398 in
  let r400 = [R 275] in
  let r401 = R 429 :: r400 in
  let r402 = Sub (r399) :: r401 in
  let r403 = R 441 :: r402 in
  let r404 = [R 514] in
  let r405 = [R 415] in
  let r406 = S (N N_pattern) :: r405 in
  let r407 = [R 512] in
  let r408 = S (T T_RBRACKET) :: r407 in
  let r409 = R 367 :: r408 in
  let r410 = [R 238] in
  let r411 = S (T T_LIDENT) :: r410 in
  let r412 = [R 257] in
  let r413 = R 366 :: r412 in
  let r414 = Sub (r411) :: r413 in
  let r415 = [R 258] in
  let r416 = Sub (r414) :: r415 in
  let r417 = [R 511] in
  let r418 = S (T T_RBRACE) :: r417 in
  let r419 = [R 260] in
  let r420 = [R 365] in
  let r421 = [R 256] in
  let r422 = S (T T_UNDERSCORE) :: r275 in
  let r423 = [R 556] in
  let r424 = Sub (r422) :: r423 in
  let r425 = [R 406] in
  let r426 = Sub (r424) :: r425 in
  let r427 = [R 87] in
  let r428 = [R 397] in
  let r429 = S (N N_pattern) :: r428 in
  let r430 = S (T T_INT) :: r427 in
  let r431 = [R 483] in
  let r432 = Sub (r430) :: r431 in
  let r433 = [R 559] in
  let r434 = [R 400] in
  let r435 = [R 395] in
  let r436 = [R 404] in
  let r437 = [R 565] in
  let r438 = S (T T_RBRACKET) :: r437 in
  let r439 = S (T T_LBRACKET) :: r438 in
  let r440 = [R 566] in
  let r441 = [R 567] in
  let r442 = [R 401] in
  let r443 = [R 396] in
  let r444 = [R 393] in
  let r445 = [R 569] in
  let r446 = S (T T_RPAREN) :: r445 in
  let r447 = [R 513] in
  let r448 = S (T T_BARRBRACKET) :: r447 in
  let r449 = [R 634] in
  let r450 = Sub (r1) :: r449 in
  let r451 = S (T T_EQUAL) :: r450 in
  let r452 = [R 204] in
  let r453 = Sub (r451) :: r452 in
  let r454 = [R 263] in
  let r455 = [R 240] in
  let r456 = S (T T_LIDENT) :: r455 in
  let r457 = [R 248] in
  let r458 = [R 236] in
  let r459 = Sub (r456) :: r458 in
  let r460 = [R 247] in
  let r461 = S (T T_RPAREN) :: r460 in
  let r462 = [R 237] in
  let r463 = [R 244] in
  let r464 = [R 243] in
  let r465 = S (T T_RPAREN) :: r464 in
  let r466 = R 363 :: r465 in
  let r467 = [R 364] in
  let r468 = [R 270] in
  let r469 = R 17 :: r468 in
  let r470 = R 216 :: r469 in
  let r471 = Sub (r119) :: r470 in
  let r472 = [R 140] in
  let r473 = Sub (r1) :: r472 in
  let r474 = S (T T_IN) :: r473 in
  let r475 = Sub (r471) :: r474 in
  let r476 = R 191 :: r475 in
  let r477 = [R 262] in
  let r478 = R 429 :: r477 in
  let r479 = Sub (r399) :: r478 in
  let r480 = R 441 :: r479 in
  let r481 = R 191 :: r480 in
  let r482 = [R 141] in
  let r483 = Sub (r1) :: r482 in
  let r484 = S (T T_IN) :: r483 in
  let r485 = Sub (r262) :: r484 in
  let r486 = R 191 :: r485 in
  let r487 = [R 534] in
  let r488 = [R 189] in
  let r489 = S (N N_expr) :: r488 in
  let r490 = [R 537] in
  let r491 = S (T T_RBRACKET) :: r490 in
  let r492 = R 367 :: r491 in
  let r493 = [R 544] in
  let r494 = [R 199] in
  let r495 = [R 198] in
  let r496 = [R 252] in
  let r497 = R 370 :: r496 in
  let r498 = Sub (r411) :: r497 in
  let r499 = [R 253] in
  let r500 = Sub (r498) :: r499 in
  let r501 = [R 450] in
  let r502 = Sub (r500) :: r501 in
  let r503 = [R 531] in
  let r504 = S (T T_RBRACE) :: r503 in
  let r505 = [R 516] in
  let r506 = [R 515] in
  let r507 = S (T T_GREATERDOT) :: r506 in
  let r508 = [R 184] in
  let r509 = Sub (r34) :: r508 in
  let r510 = [R 523] in
  let r511 = S (T T_END) :: r510 in
  let r512 = [R 151] in
  let r513 = S (N N_expr) :: r512 in
  let r514 = S (T T_THEN) :: r513 in
  let r515 = Sub (r1) :: r514 in
  let r516 = [R 142] in
  let r517 = S (N N_match_cases) :: r516 in
  let r518 = R 361 :: r517 in
  let r519 = [R 278] in
  let r520 = Sub (r1) :: r519 in
  let r521 = S (T T_MINUSGREATER) :: r520 in
  let r522 = [R 279] in
  let r523 = Sub (r1) :: r522 in
  let r524 = S (T T_MINUSGREATER) :: r523 in
  let r525 = [R 250] in
  let r526 = Sub (r424) :: r525 in
  let r527 = [R 206] in
  let r528 = Sub (r1) :: r527 in
  let r529 = S (T T_MINUSGREATER) :: r528 in
  let r530 = [R 143] in
  let r531 = Sub (r529) :: r530 in
  let r532 = Sub (r526) :: r531 in
  let r533 = [R 418] in
  let r534 = S (T T_UNDERSCORE) :: r533 in
  let r535 = [R 246] in
  let r536 = [R 245] in
  let r537 = S (T T_RPAREN) :: r536 in
  let r538 = R 363 :: r537 in
  let r539 = [R 272] in
  let r540 = [R 273] in
  let r541 = S (T T_LIDENT) :: r540 in
  let r542 = [R 144] in
  let r543 = Sub (r529) :: r542 in
  let r544 = S (T T_RPAREN) :: r543 in
  let r545 = [R 135] in
  let r546 = S (T T_DONE) :: r545 in
  let r547 = Sub (r1) :: r546 in
  let r548 = S (T T_DO) :: r547 in
  let r549 = Sub (r1) :: r548 in
  let r550 = S (T T_IN) :: r549 in
  let r551 = S (N N_pattern) :: r550 in
  let r552 = [R 126] in
  let r553 = S (T T_DOWNTO) :: r552 in
  let r554 = [R 153] in
  let r555 = S (T T_DONE) :: r554 in
  let r556 = Sub (r1) :: r555 in
  let r557 = S (T T_DO) :: r556 in
  let r558 = Sub (r1) :: r557 in
  let r559 = Sub (r553) :: r558 in
  let r560 = Sub (r1) :: r559 in
  let r561 = S (T T_EQUAL) :: r560 in
  let r562 = S (N N_pattern) :: r561 in
  let r563 = [R 541] in
  let r564 = [R 527] in
  let r565 = S (T T_RPAREN) :: r564 in
  let r566 = S (T T_LPAREN) :: r565 in
  let r567 = S (T T_DOT) :: r566 in
  let r568 = [R 550] in
  let r569 = S (T T_RPAREN) :: r568 in
  let r570 = Sub (r90) :: r569 in
  let r571 = S (T T_COLON) :: r570 in
  let r572 = S (N N_module_expr) :: r571 in
  let r573 = [R 183] in
  let r574 = Sub (r34) :: r573 in
  let r575 = [R 547] in
  let r576 = [R 530] in
  let r577 = S (T T_RBRACE) :: r576 in
  let r578 = S (N N_expr) :: r577 in
  let r579 = S (T T_LBRACE) :: r578 in
  let r580 = [R 528] in
  let r581 = S (T T_RPAREN) :: r580 in
  let r582 = Sub (r1) :: r581 in
  let r583 = [R 176] in
  let r584 = [R 235] in
  let r585 = S (T T_LIDENT) :: r584 in
  let r586 = [R 232] in
  let r587 = [R 546] in
  let r588 = [R 233] in
  let r589 = [R 234] in
  let r590 = [R 231] in
  let r591 = [R 179] in
  let r592 = [R 127] in
  let r593 = Sub (r1) :: r592 in
  let r594 = [R 138] in
  let r595 = Sub (r1) :: r594 in
  let r596 = [R 182] in
  let r597 = S (N N_expr) :: r596 in
  let r598 = [R 187] in
  let r599 = [R 166] in
  let r600 = [R 160] in
  let r601 = [R 177] in
  let r602 = [R 163] in
  let r603 = [R 167] in
  let r604 = [R 159] in
  let r605 = [R 162] in
  let r606 = [R 161] in
  let r607 = [R 171] in
  let r608 = [R 165] in
  let r609 = [R 164] in
  let r610 = [R 169] in
  let r611 = [R 158] in
  let r612 = [R 157] in
  let r613 = [R 154] in
  let r614 = [R 156] in
  let r615 = [R 170] in
  let r616 = [R 168] in
  let r617 = [R 172] in
  let r618 = [R 173] in
  let r619 = [R 174] in
  let r620 = [R 188] in
  let r621 = [R 175] in
  let r622 = [R 458] in
  let r623 = Sub (r1) :: r622 in
  let r624 = [R 10] in
  let r625 = R 429 :: r624 in
  let r626 = Sub (r399) :: r625 in
  let r627 = [R 267] in
  let r628 = Sub (r1) :: r627 in
  let r629 = S (T T_EQUAL) :: r628 in
  let r630 = [R 413] in
  let r631 = [R 414] in
  let r632 = [R 409] in
  let r633 = [R 410] in
  let r634 = [R 407] in
  let r635 = [R 529] in
  let r636 = S (T T_RBRACKET) :: r635 in
  let r637 = Sub (r1) :: r636 in
  let r638 = [R 180] in
  let r639 = [R 181] in
  let r640 = [R 178] in
  let r641 = [R 526] in
  let r642 = [R 536] in
  let r643 = [R 535] in
  let r644 = S (T T_BARRBRACKET) :: r643 in
  let r645 = [R 539] in
  let r646 = [R 538] in
  let r647 = S (T T_RBRACKET) :: r646 in
  let r648 = Sub (r205) :: r494 in
  let r649 = [R 200] in
  let r650 = R 367 :: r649 in
  let r651 = Sub (r648) :: r650 in
  let r652 = [R 545] in
  let r653 = S (T T_GREATERRBRACE) :: r652 in
  let r654 = [R 532] in
  let r655 = S (T T_RBRACE) :: r654 in
  let r656 = [R 449] in
  let r657 = Sub (r500) :: r656 in
  let r658 = [R 672] in
  let r659 = [R 670] in
  let r660 = Sub (r65) :: r659 in
  let r661 = [R 671] in
  let r662 = [R 251] in
  let r663 = [R 134] in
  let r664 = S (T T_DONE) :: r663 in
  let r665 = Sub (r1) :: r664 in
  let r666 = S (T T_DO) :: r665 in
  let r667 = Sub (r1) :: r666 in
  let r668 = Sub (r553) :: r667 in
  let r669 = [R 209] in
  let r670 = Sub (r529) :: r669 in
  let r671 = S (T T_RPAREN) :: r670 in
  let r672 = [R 249] in
  let r673 = [R 207] in
  let r674 = Sub (r1) :: r673 in
  let r675 = S (T T_MINUSGREATER) :: r674 in
  let r676 = [R 208] in
  let r677 = S (N N_pattern) :: r521 in
  let r678 = [R 282] in
  let r679 = [R 150] in
  let r680 = [R 522] in
  let r681 = [R 543] in
  let r682 = [R 533] in
  let r683 = S (T T_BARRBRACKET) :: r682 in
  let r684 = [R 139] in
  let r685 = Sub (r1) :: r684 in
  let r686 = S (T T_IN) :: r685 in
  let r687 = Sub (r296) :: r686 in
  let r688 = S (T T_UIDENT) :: r687 in
  let r689 = [R 300] in
  let r690 = S (N N_module_expr) :: r689 in
  let r691 = S (T T_EQUAL) :: r690 in
  let r692 = [R 301] in
  let r693 = [R 636] in
  let r694 = Sub (r453) :: r693 in
  let r695 = S (T T_RPAREN) :: r694 in
  let r696 = Sub (r541) :: r695 in
  let r697 = [R 205] in
  let r698 = Sub (r1) :: r697 in
  let r699 = [R 635] in
  let r700 = [R 265] in
  let r701 = Sub (r1) :: r700 in
  let r702 = S (T T_EQUAL) :: r701 in
  let r703 = Sub (r65) :: r702 in
  let r704 = S (T T_DOT) :: r703 in
  let r705 = [R 264] in
  let r706 = Sub (r1) :: r705 in
  let r707 = S (T T_EQUAL) :: r706 in
  let r708 = Sub (r65) :: r707 in
  let r709 = [R 155] in
  let r710 = S (T T_RPAREN) :: r709 in
  let r711 = S (N N_expr) :: r710 in
  let r712 = S (T T_COMMA) :: r711 in
  let r713 = S (N N_expr) :: r712 in
  let r714 = S (T T_LPAREN) :: r713 in
  let r715 = [R 524] in
  let r716 = [R 387] in
  let r717 = S (T T_RPAREN) :: r716 in
  let r718 = [R 385] in
  let r719 = S (T T_RPAREN) :: r718 in
  let r720 = [R 386] in
  let r721 = S (T T_RPAREN) :: r720 in
  let r722 = [R 224] in
  let r723 = S (T T_RBRACKET) :: r722 in
  let r724 = Sub (r15) :: r723 in
  let r725 = [R 422] in
  let r726 = [R 423] in
  let r727 = [R 203] in
  let r728 = S (T T_RBRACKET) :: r727 in
  let r729 = Sub (r15) :: r728 in
  let r730 = [R 632] in
  let r731 = R 429 :: r730 in
  let r732 = S (N N_module_expr) :: r731 in
  let r733 = [R 432] in
  let r734 = S (T T_STRING) :: r733 in
  let r735 = [R 431] in
  let r736 = R 429 :: r735 in
  let r737 = Sub (r734) :: r736 in
  let r738 = S (T T_EQUAL) :: r737 in
  let r739 = Sub (r65) :: r738 in
  let r740 = S (T T_COLON) :: r739 in
  let r741 = Sub (r53) :: r740 in
  let r742 = [R 625] in
  let r743 = R 429 :: r742 in
  let r744 = R 17 :: r743 in
  let r745 = Sub (r250) :: r744 in
  let r746 = S (T T_EQUAL) :: r745 in
  let r747 = Sub (r119) :: r746 in
  let r748 = [R 459] in
  let r749 = R 429 :: r748 in
  let r750 = R 17 :: r749 in
  let r751 = R 216 :: r750 in
  let r752 = Sub (r119) :: r751 in
  let r753 = R 191 :: r752 in
  let r754 = [R 420] in
  let r755 = [R 466] in
  let r756 = [R 446] in
  let r757 = R 429 :: r756 in
  let r758 = S (N N_module_type) :: r757 in
  let r759 = S (T T_COLON) :: r758 in
  let r760 = S (T T_UIDENT) :: r759 in
  let r761 = S (T T_REC) :: r760 in
  let r762 = [R 303] in
  let r763 = S (N N_module_type) :: r762 in
  let r764 = S (T T_COLON) :: r763 in
  let r765 = [R 302] in
  let r766 = R 429 :: r765 in
  let r767 = [R 305] in
  let r768 = Sub (r764) :: r767 in
  let r769 = [R 304] in
  let r770 = Sub (r764) :: r769 in
  let r771 = S (T T_RPAREN) :: r770 in
  let r772 = S (N N_module_type) :: r771 in
  let r773 = [R 297] in
  let r774 = R 429 :: r773 in
  let r775 = [R 463] in
  let r776 = R 429 :: r775 in
  let r777 = S (N N_module_type) :: r776 in
  let r778 = [R 85] in
  let r779 = S (T T_LIDENT) :: r778 in
  let r780 = [R 65] in
  let r781 = Sub (r779) :: r780 in
  let r782 = [R 80] in
  let r783 = R 429 :: r782 in
  let r784 = Sub (r781) :: r783 in
  let r785 = S (T T_EQUAL) :: r784 in
  let r786 = S (T T_LIDENT) :: r785 in
  let r787 = R 83 :: r786 in
  let r788 = R 712 :: r787 in
  let r789 = R 191 :: r788 in
  let r790 = [R 84] in
  let r791 = S (T T_RBRACKET) :: r790 in
  let r792 = [R 55] in
  let r793 = R 62 :: r792 in
  let r794 = R 54 :: r793 in
  let r795 = [R 66] in
  let r796 = S (T T_END) :: r795 in
  let r797 = Sub (r794) :: r796 in
  let r798 = [R 53] in
  let r799 = S (T T_RPAREN) :: r798 in
  let r800 = [R 711] in
  let r801 = Sub (r65) :: r800 in
  let r802 = S (T T_COLON) :: r801 in
  let r803 = Sub (r205) :: r802 in
  let r804 = [R 57] in
  let r805 = R 429 :: r804 in
  let r806 = Sub (r803) :: r805 in
  let r807 = [R 709] in
  let r808 = Sub (r65) :: r807 in
  let r809 = S (T T_COLON) :: r808 in
  let r810 = Sub (r205) :: r809 in
  let r811 = [R 710] in
  let r812 = Sub (r65) :: r811 in
  let r813 = S (T T_COLON) :: r812 in
  let r814 = Sub (r205) :: r813 in
  let r815 = [R 424] in
  let r816 = Sub (r65) :: r815 in
  let r817 = [R 58] in
  let r818 = R 429 :: r817 in
  let r819 = Sub (r816) :: r818 in
  let r820 = S (T T_COLON) :: r819 in
  let r821 = Sub (r205) :: r820 in
  let r822 = R 436 :: r821 in
  let r823 = [R 425] in
  let r824 = Sub (r65) :: r823 in
  let r825 = [R 56] in
  let r826 = R 429 :: r825 in
  let r827 = Sub (r781) :: r826 in
  let r828 = Sub (r65) :: r197 in
  let r829 = [R 64] in
  let r830 = Sub (r779) :: r829 in
  let r831 = S (T T_RBRACKET) :: r830 in
  let r832 = [R 86] in
  let r833 = S (T T_LIDENT) :: r832 in
  let r834 = [R 103] in
  let r835 = Sub (r65) :: r834 in
  let r836 = S (T T_EQUAL) :: r835 in
  let r837 = Sub (r65) :: r836 in
  let r838 = [R 59] in
  let r839 = R 429 :: r838 in
  let r840 = Sub (r837) :: r839 in
  let r841 = [R 60] in
  let r842 = [R 75] in
  let r843 = Sub (r781) :: r842 in
  let r844 = [R 25] in
  let r845 = R 429 :: r844 in
  let r846 = Sub (r843) :: r845 in
  let r847 = S (T T_COLON) :: r846 in
  let r848 = S (T T_LIDENT) :: r847 in
  let r849 = R 83 :: r848 in
  let r850 = [R 76] in
  let r851 = Sub (r843) :: r850 in
  let r852 = S (T T_MINUSGREATER) :: r851 in
  let r853 = Sub (r59) :: r852 in
  let r854 = S (T T_COLON) :: r853 in
  let r855 = [R 77] in
  let r856 = Sub (r843) :: r855 in
  let r857 = S (T T_MINUSGREATER) :: r856 in
  let r858 = [R 78] in
  let r859 = Sub (r843) :: r858 in
  let r860 = S (T T_MINUSGREATER) :: r859 in
  let r861 = [R 79] in
  let r862 = Sub (r843) :: r861 in
  let r863 = [R 13] in
  let r864 = R 429 :: r863 in
  let r865 = R 105 :: r864 in
  let r866 = R 676 :: r865 in
  let r867 = S (T T_LIDENT) :: r866 in
  let r868 = R 374 :: r867 in
  let r869 = [R 467] in
  let r870 = [R 12] in
  let r871 = R 429 :: r870 in
  let r872 = S (N N_module_type) :: r871 in
  let r873 = S (T T_COLON) :: r872 in
  let r874 = S (T T_UIDENT) :: r873 in
  let r875 = [R 481] in
  let r876 = [R 9] in
  let r877 = R 429 :: r876 in
  let r878 = Sub (r781) :: r877 in
  let r879 = S (T T_EQUAL) :: r878 in
  let r880 = S (T T_LIDENT) :: r879 in
  let r881 = R 83 :: r880 in
  let r882 = R 712 :: r881 in
  let r883 = [R 8] in
  let r884 = R 429 :: r883 in
  let r885 = Sub (r843) :: r884 in
  let r886 = S (T T_COLON) :: r885 in
  let r887 = S (T T_LIDENT) :: r886 in
  let r888 = R 83 :: r887 in
  let r889 = R 712 :: r888 in
  let r890 = [R 70] in
  let r891 = Sub (r37) :: r890 in
  let r892 = [R 28] in
  let r893 = Sub (r891) :: r892 in
  let r894 = [R 43] in
  let r895 = Sub (r893) :: r894 in
  let r896 = S (T T_EQUAL) :: r895 in
  let r897 = [R 22] in
  let r898 = R 429 :: r897 in
  let r899 = Sub (r896) :: r898 in
  let r900 = S (T T_LIDENT) :: r899 in
  let r901 = R 83 :: r900 in
  let r902 = [R 71] in
  let r903 = S (T T_END) :: r902 in
  let r904 = Sub (r269) :: r903 in
  let r905 = [R 706] in
  let r906 = Sub (r1) :: r905 in
  let r907 = S (T T_EQUAL) :: r906 in
  let r908 = Sub (r205) :: r907 in
  let r909 = R 328 :: r908 in
  let r910 = R 17 :: r909 in
  let r911 = R 379 :: r910 in
  let r912 = [R 35] in
  let r913 = R 429 :: r912 in
  let r914 = [R 705] in
  let r915 = Sub (r65) :: r914 in
  let r916 = S (T T_COLON) :: r915 in
  let r917 = Sub (r205) :: r916 in
  let r918 = [R 704] in
  let r919 = Sub (r65) :: r918 in
  let r920 = S (T T_COLON) :: r919 in
  let r921 = [R 707] in
  let r922 = Sub (r1) :: r921 in
  let r923 = [R 289] in
  let r924 = Sub (r451) :: r923 in
  let r925 = Sub (r205) :: r924 in
  let r926 = R 434 :: r925 in
  let r927 = R 17 :: r926 in
  let r928 = R 379 :: r927 in
  let r929 = [R 36] in
  let r930 = R 429 :: r929 in
  let r931 = [R 288] in
  let r932 = Sub (r816) :: r931 in
  let r933 = S (T T_COLON) :: r932 in
  let r934 = Sub (r205) :: r933 in
  let r935 = [R 287] in
  let r936 = Sub (r816) :: r935 in
  let r937 = S (T T_COLON) :: r936 in
  let r938 = [R 290] in
  let r939 = Sub (r1) :: r938 in
  let r940 = S (T T_EQUAL) :: r939 in
  let r941 = [R 291] in
  let r942 = Sub (r1) :: r941 in
  let r943 = S (T T_EQUAL) :: r942 in
  let r944 = Sub (r65) :: r943 in
  let r945 = S (T T_DOT) :: r944 in
  let r946 = [R 38] in
  let r947 = R 429 :: r946 in
  let r948 = Sub (r1) :: r947 in
  let r949 = [R 34] in
  let r950 = R 429 :: r949 in
  let r951 = R 391 :: r950 in
  let r952 = Sub (r893) :: r951 in
  let r953 = R 17 :: r952 in
  let r954 = [R 73] in
  let r955 = S (T T_RPAREN) :: r954 in
  let r956 = [R 69] in
  let r957 = Sub (r37) :: r956 in
  let r958 = S (T T_RBRACKET) :: r957 in
  let r959 = [R 46] in
  let r960 = Sub (r893) :: r959 in
  let r961 = S (T T_MINUSGREATER) :: r960 in
  let r962 = Sub (r526) :: r961 in
  let r963 = [R 29] in
  let r964 = Sub (r962) :: r963 in
  let r965 = [R 31] in
  let r966 = Sub (r893) :: r965 in
  let r967 = [R 72] in
  let r968 = S (T T_RPAREN) :: r967 in
  let r969 = [R 390] in
  let r970 = [R 37] in
  let r971 = R 429 :: r970 in
  let r972 = Sub (r837) :: r971 in
  let r973 = [R 39] in
  let r974 = [R 44] in
  let r975 = Sub (r893) :: r974 in
  let r976 = S (T T_EQUAL) :: r975 in
  let r977 = [R 45] in
  let r978 = [R 638] in
  let r979 = [R 658] in
  let r980 = [R 11] in
  let r981 = R 429 :: r980 in
  let r982 = Sub (r296) :: r981 in
  let r983 = S (T T_UIDENT) :: r982 in
  let r984 = [R 654] in
  let r985 = [R 7] in
  let r986 = R 429 :: r985 in
  let r987 = Sub (r896) :: r986 in
  let r988 = S (T T_LIDENT) :: r987 in
  let r989 = R 83 :: r988 in
  let r990 = R 712 :: r989 in
  let r991 = [R 637] in
  let r992 = R 656 :: r991 in
  let r993 = [R 405] in
  let r994 = S (T T_RPAREN) :: r993 in
  let r995 = S (N N_pattern) :: r994 in
  let r996 = S (T T_COMMA) :: r995 in
  let r997 = S (N N_pattern) :: r996 in
  let r998 = S (T T_LPAREN) :: r997 in
  let r999 = [R 51] in
  let r1000 = S (T T_RPAREN) :: r999 in
  let r1001 = [R 460] in
  let r1002 = Sub (r238) :: r1001 in
  let r1003 = [R 464] in
  let r1004 = R 429 :: r1003 in
  let r1005 = Sub (r1002) :: r1004 in
  let r1006 = R 434 :: r1005 in
  let r1007 = [R 130] in
  let r1008 = S (N N_match_cases) :: r1007 in
  let r1009 = [R 132] in
  let r1010 = [R 131] in
  let r1011 = [R 222] in
  let r1012 = [R 223] in
  let r1013 = [R 392] in
  function
  | 0 | 1547 | 1551 -> Nothing
  | 1546 -> One ([R 0])
  | 1550 -> One ([R 1])
  | 1554 -> One ([R 2])
  | 381 -> One ([R 3])
  | 380 -> One ([R 4])
  | 79 -> One (R 17 :: r43)
  | 81 -> One (R 17 :: r44)
  | 135 -> One (R 17 :: r97)
  | 204 -> One (R 17 :: r167)
  | 411 -> One (R 17 :: r293)
  | 419 -> One (R 17 :: r313)
  | 496 -> One (R 17 :: r366)
  | 509 -> One (R 17 :: r383)
  | 804 -> One (R 17 :: r626)
  | 1146 -> One (R 17 :: r797)
  | 1155 -> One (R 17 :: r806)
  | 1172 -> One (R 17 :: r822)
  | 1187 -> One (R 17 :: r827)
  | 1202 -> One (R 17 :: r840)
  | 1249 -> One (R 17 :: r868)
  | 1264 -> One (R 17 :: r874)
  | 1281 -> One (R 17 :: r882)
  | 1292 -> One (R 17 :: r889)
  | 1311 -> One (R 17 :: r904)
  | 1367 -> One (R 17 :: r948)
  | 1380 -> One (R 17 :: r964)
  | 1405 -> One (R 17 :: r972)
  | 1433 -> One (R 17 :: r983)
  | 1451 -> One (R 17 :: r990)
  | 1459 -> One ([R 23])
  | 1458 -> One ([R 24])
  | 1301 -> One ([R 26])
  | 1300 -> One ([R 27])
  | 1388 -> One ([R 30])
  | 1391 -> One ([R 32])
  | 1386 -> One ([R 33])
  | 1411 -> One ([R 40])
  | 1412 -> One ([R 42])
  | 1393 -> One ([R 47])
  | 1211 -> One ([R 61])
  | 1212 -> One ([R 63])
  | 1201 -> One ([R 67])
  | 1197 -> One ([R 68])
  | 1290 -> One ([R 81])
  | 1289 -> One ([R 82])
  | 565 -> One ([R 88])
  | 68 -> One ([R 89])
  | 562 -> One ([R 90])
  | 157 | 276 -> One ([R 91])
  | 158 -> One ([R 96])
  | 345 -> One ([R 97])
  | 67 -> One ([R 101])
  | 311 -> One ([R 110])
  | 306 -> One ([R 111])
  | 270 -> One ([R 113])
  | 907 -> One ([R 125])
  | 716 -> One ([R 136])
  | 845 -> One ([R 137])
  | 745 -> One ([R 147])
  | 754 -> One ([R 148])
  | 734 -> One ([R 149])
  | 752 -> One ([R 186])
  | 866 -> One ([R 190])
  | 1 -> One (R 191 :: r6)
  | 60 -> One (R 191 :: r24)
  | 63 -> One (R 191 :: r27)
  | 65 -> One (R 191 :: r32)
  | 71 -> One (R 191 :: r39)
  | 91 -> One (R 191 :: r70)
  | 388 -> One (R 191 :: r272)
  | 402 -> One (R 191 :: r284)
  | 500 -> One (R 191 :: r371)
  | 502 -> One (R 191 :: r376)
  | 507 -> One (R 191 :: r379)
  | 536 -> One (R 191 :: r403)
  | 557 -> One (R 191 :: r426)
  | 563 -> One (R 191 :: r429)
  | 651 -> One (R 191 :: r509)
  | 653 -> One (R 191 :: r511)
  | 655 -> One (R 191 :: r515)
  | 657 -> One (R 191 :: r518)
  | 662 -> One (R 191 :: r532)
  | 682 -> One (R 191 :: r551)
  | 686 -> One (R 191 :: r562)
  | 698 -> One (R 191 :: r572)
  | 708 -> One (R 191 :: r574)
  | 978 -> One (R 191 :: r688)
  | 1079 -> One (R 191 :: r732)
  | 1083 -> One (R 191 :: r741)
  | 1105 -> One (R 191 :: r761)
  | 1128 -> One (R 191 :: r777)
  | 880 -> One ([R 201])
  | 423 -> One ([R 212])
  | 422 -> One ([R 213])
  | 485 -> One ([R 214])
  | 486 -> One ([R 215])
  | 124 | 478 -> One ([R 220])
  | 292 -> One ([R 229])
  | 293 -> One ([R 230])
  | 846 -> One ([R 241])
  | 848 -> One ([R 242])
  | 888 -> One ([R 254])
  | 887 -> One ([R 255])
  | 547 -> One ([R 259])
  | 551 -> One ([R 261])
  | 742 -> One ([R 268])
  | 829 -> One ([R 269])
  | 667 -> One ([R 271])
  | 678 -> One ([R 274])
  | 738 -> One ([R 276])
  | 830 -> One ([R 277])
  | 947 -> One ([R 280])
  | 952 -> One ([R 281])
  | 255 -> One ([R 283])
  | 254 -> One ([R 284])
  | 256 -> One ([R 285])
  | 167 -> One ([R 286])
  | 524 -> One ([R 306])
  | 522 -> One ([R 309])
  | 513 -> One ([R 311])
  | 427 -> One ([R 314])
  | 481 -> One ([R 321])
  | 475 -> One ([R 322])
  | 480 -> One ([R 326])
  | 1157 -> One (R 328 :: r810)
  | 1322 -> One (R 328 :: r917)
  | 282 | 1327 -> One ([R 329])
  | 242 -> One ([R 332])
  | 139 -> One ([R 334])
  | 87 | 94 -> One ([R 336])
  | 107 -> One ([R 337])
  | 106 -> One ([R 338])
  | 105 -> One ([R 339])
  | 104 -> One ([R 340])
  | 103 -> One ([R 341])
  | 85 -> One ([R 342])
  | 112 | 704 -> One ([R 343])
  | 97 | 401 | 506 -> One ([R 344])
  | 96 | 505 -> One ([R 345])
  | 101 | 534 | 560 -> One ([R 346])
  | 100 | 533 -> One ([R 347])
  | 84 -> One ([R 348])
  | 109 -> One ([R 349])
  | 102 -> One ([R 350])
  | 108 -> One ([R 351])
  | 99 -> One ([R 352])
  | 111 -> One ([R 353])
  | 113 -> One ([R 354])
  | 110 -> One ([R 356])
  | 95 -> One ([R 357])
  | 98 -> One ([R 358])
  | 206 -> One ([R 359])
  | 205 -> One (R 360 :: r172)
  | 174 -> One (R 361 :: r149)
  | 1525 -> One (R 361 :: r1008)
  | 175 -> One ([R 362])
  | 548 -> One (R 367 :: r419)
  | 618 -> One (R 367 :: r448)
  | 864 -> One (R 367 :: r644)
  | 872 -> One (R 367 :: r647)
  | 974 -> One (R 367 :: r683)
  | 549 | 601 | 865 | 879 -> One ([R 368])
  | 896 -> One ([R 369])
  | 367 -> One ([R 375])
  | 382 -> One (R 379 :: r266)
  | 636 -> One (R 379 :: r486)
  | 1371 -> One (R 379 :: r953)
  | 383 -> One ([R 380])
  | 575 -> One ([R 394])
  | 580 -> One ([R 398])
  | 574 -> One ([R 399])
  | 568 -> One ([R 402])
  | 811 -> One ([R 408])
  | 825 -> One ([R 411])
  | 602 -> One ([R 416])
  | 673 -> One ([R 417])
  | 1304 -> One ([R 421])
  | 353 -> One (R 429 :: r256)
  | 1209 -> One (R 429 :: r841)
  | 1277 -> One (R 429 :: r875)
  | 1409 -> One (R 429 :: r973)
  | 1446 -> One (R 429 :: r984)
  | 1461 -> One (R 429 :: r992)
  | 1090 -> One ([R 433])
  | 1342 -> One (R 434 :: r934)
  | 319 | 1347 -> One ([R 435])
  | 1176 -> One ([R 437])
  | 1174 -> One ([R 438])
  | 1177 -> One ([R 439])
  | 1175 -> One ([R 440])
  | 538 -> One ([R 442])
  | 1439 -> One ([R 444])
  | 1438 -> One ([R 445])
  | 1271 -> One ([R 447])
  | 1270 -> One ([R 448])
  | 186 -> One ([R 451])
  | 799 -> One ([R 456])
  | 803 -> One ([R 457])
  | 1504 -> One ([R 461])
  | 1501 -> One ([R 462])
  | 1103 -> One (R 465 :: r754)
  | 1104 -> One (R 465 :: r755)
  | 1258 -> One (R 465 :: r869)
  | 1247 -> One ([R 468])
  | 1272 -> One ([R 469])
  | 1248 -> One ([R 470])
  | 1260 -> One ([R 471])
  | 1262 -> One ([R 472])
  | 1275 -> One ([R 473])
  | 1276 -> One ([R 474])
  | 1263 -> One ([R 475])
  | 1274 -> One ([R 476])
  | 1273 -> One ([R 477])
  | 1261 -> One ([R 478])
  | 1291 -> One ([R 479])
  | 1280 -> One ([R 480])
  | 1279 -> One ([R 482])
  | 399 -> One ([R 485])
  | 396 -> One ([R 487])
  | 185 -> One ([R 492])
  | 190 -> One ([R 493])
  | 267 -> One ([R 494])
  | 212 | 1239 -> One ([R 508])
  | 691 -> One ([R 517])
  | 707 -> One ([R 518])
  | 706 | 753 -> One ([R 519])
  | 693 | 733 -> One ([R 520])
  | 842 | 859 -> One ([R 525])
  | 705 -> One ([R 551])
  | 849 -> One ([R 553])
  | 847 -> One ([R 554])
  | 566 -> One ([R 555])
  | 570 -> One ([R 558])
  | 615 -> One ([R 560])
  | 614 -> One ([R 561])
  | 569 -> One ([R 563])
  | 606 -> One ([R 564])
  | 591 -> One ([R 572])
  | 28 -> One ([R 573])
  | 8 -> One ([R 574])
  | 52 -> One ([R 576])
  | 51 -> One ([R 577])
  | 50 -> One ([R 578])
  | 49 -> One ([R 579])
  | 48 -> One ([R 580])
  | 47 -> One ([R 581])
  | 46 -> One ([R 582])
  | 45 -> One ([R 583])
  | 44 -> One ([R 584])
  | 43 -> One ([R 585])
  | 42 -> One ([R 586])
  | 41 -> One ([R 587])
  | 40 -> One ([R 588])
  | 39 -> One ([R 589])
  | 38 -> One ([R 590])
  | 37 -> One ([R 591])
  | 36 -> One ([R 592])
  | 35 -> One ([R 593])
  | 34 -> One ([R 594])
  | 33 -> One ([R 595])
  | 32 -> One ([R 596])
  | 31 -> One ([R 597])
  | 30 -> One ([R 598])
  | 29 -> One ([R 599])
  | 27 -> One ([R 600])
  | 26 -> One ([R 601])
  | 25 -> One ([R 602])
  | 24 -> One ([R 603])
  | 23 -> One ([R 604])
  | 22 -> One ([R 605])
  | 21 -> One ([R 606])
  | 20 -> One ([R 607])
  | 19 -> One ([R 608])
  | 18 -> One ([R 609])
  | 17 -> One ([R 610])
  | 16 -> One ([R 611])
  | 15 -> One ([R 612])
  | 14 -> One ([R 613])
  | 13 -> One ([R 614])
  | 12 -> One ([R 615])
  | 11 -> One ([R 616])
  | 10 -> One ([R 617])
  | 9 -> One ([R 618])
  | 7 -> One ([R 619])
  | 6 -> One ([R 620])
  | 5 -> One ([R 621])
  | 4 -> One ([R 622])
  | 3 -> One ([R 623])
  | 1431 -> One ([R 624])
  | 366 -> One ([R 627])
  | 357 -> One ([R 628])
  | 365 -> One ([R 629])
  | 356 -> One ([R 630])
  | 355 -> One ([R 631])
  | 1425 -> One ([R 639])
  | 1444 | 1464 -> One ([R 640])
  | 1445 | 1465 -> One ([R 641])
  | 1440 -> One ([R 642])
  | 1422 -> One ([R 643])
  | 1423 -> One ([R 644])
  | 1428 -> One ([R 645])
  | 1430 -> One ([R 646])
  | 1443 -> One ([R 647])
  | 1432 -> One ([R 648])
  | 1442 -> One ([R 649])
  | 1441 -> One ([R 650])
  | 1450 -> One ([R 651])
  | 1449 -> One ([R 652])
  | 1429 -> One ([R 653])
  | 1448 -> One ([R 655])
  | 1426 -> One (R 656 :: r979)
  | 499 -> One ([R 659])
  | 498 -> One ([R 660])
  | 371 -> One ([R 664])
  | 372 -> One ([R 665])
  | 374 -> One ([R 666])
  | 376 -> One ([R 667])
  | 373 -> One ([R 668])
  | 370 -> One ([R 669])
  | 1257 -> One ([R 674])
  | 1256 -> One ([R 675])
  | 317 -> One ([R 677])
  | 304 -> One ([R 678])
  | 326 -> One ([R 679])
  | 430 -> One (R 691 :: r329)
  | 460 -> One ([R 692])
  | 141 -> One ([R 696])
  | 142 -> One ([R 697])
  | 375 -> One ([R 702])
  | 378 -> One ([R 703])
  | 1162 -> One (R 712 :: r814)
  | 1215 -> One (R 712 :: r849)
  | 1306 -> One (R 712 :: r901)
  | 1138 -> One ([R 713])
  | 448 -> One ([R 721])
  | 883 -> One (S (T T_WITH) :: r657)
  | 346 | 377 -> One (S (T T_UIDENT) :: r42)
  | 195 -> One (S (T T_UIDENT) :: r165)
  | 407 -> One (S (T T_TYPE) :: r290)
  | 1006 -> One (S (T T_TYPE) :: r696)
  | 1135 | 1305 -> One (S (T T_TYPE) :: r789)
  | 341 -> One (S (T T_RPAREN) :: r48)
  | 160 | 277 -> One (S (T T_RPAREN) :: r127)
  | 260 -> One (S (T T_RPAREN) :: r193)
  | 263 -> One (S (T T_RPAREN) :: r194)
  | 421 -> One (S (T T_RPAREN) :: r314)
  | 515 -> One (S (T T_RPAREN) :: r384)
  | 585 -> One (S (T T_RPAREN) :: r440)
  | 587 -> One (S (T T_RPAREN) :: r441)
  | 860 -> One (S (T T_RPAREN) :: r641)
  | 1034 -> One (S (T T_RPAREN) :: r714)
  | 1043 -> One (S (T T_RPAREN) :: r715)
  | 1108 -> One (S (T T_RPAREN) :: r768)
  | 1476 -> One (S (T T_RPAREN) :: r998)
  | 178 -> One (S (T T_RBRACKET) :: r150)
  | 229 -> One (S (T T_RBRACKET) :: r181)
  | 272 | 278 -> One (S (T T_RBRACKET) :: r198)
  | 342 -> One (S (T T_RBRACKET) :: r255)
  | 870 -> One (S (T T_RBRACKET) :: r645)
  | 220 -> One (S (T T_QUOTE) :: r179)
  | 335 -> One (S (T T_PLUSEQ) :: r244)
  | 1494 -> One (S (T T_PLUSEQ) :: r1006)
  | 131 -> One (S (T T_MODULE) :: r94)
  | 299 -> One (S (T T_MINUSGREATER) :: r224)
  | 1234 -> One (S (T T_MINUSGREATER) :: r862)
  | 127 -> One (S (T T_LIDENT) :: r85)
  | 1220 -> One (S (T T_LIDENT) :: r854)
  | 1401 -> One (S (T T_LIDENT) :: r969)
  | 743 -> One (S (T T_LESSMINUS) :: r597)
  | 313 -> One (S (T T_LBRACE) :: r227)
  | 394 -> One (S (T T_INT) :: r278)
  | 397 -> One (S (T T_INT) :: r279)
  | 735 -> One (S (T T_IN) :: r593)
  | 739 -> One (S (T T_IN) :: r595)
  | 1384 -> One (S (T T_IN) :: r966)
  | 643 -> One (S (T T_GREATERRBRACE) :: r493)
  | 968 -> One (S (T T_GREATERRBRACE) :: r681)
  | 164 -> One (S (T T_GREATER) :: r132)
  | 168 -> One (S (T T_GREATER) :: r134)
  | 359 -> One (S (T T_EQUAL) :: r259)
  | 465 -> One (S (T T_EQUAL) :: r354)
  | 1012 -> One (S (T T_EQUAL) :: r698)
  | 1336 -> One (S (T T_EQUAL) :: r922)
  | 1544 -> One (S (T T_EOF) :: r1011)
  | 1548 -> One (S (T T_EOF) :: r1012)
  | 1552 -> One (S (T T_EOF) :: r1013)
  | 959 -> One (S (T T_END) :: r680)
  | 156 -> One (S (T T_DOTDOT) :: r117)
  | 318 -> One (S (T T_DOTDOT) :: r228)
  | 74 -> One (S (T T_DOT) :: r41)
  | 244 -> One (S (T T_DOT) :: r191)
  | 443 -> One (S (T T_DOT) :: r342)
  | 476 -> One (S (T T_DOT) :: r358)
  | 583 -> One (S (T T_DOT) :: r439)
  | 1026 -> One (S (T T_DOT) :: r708)
  | 1181 -> One (S (T T_DOT) :: r824)
  | 1193 -> One (S (T T_DOT) :: r833)
  | 170 -> One (S (T T_COLON) :: r141)
  | 425 -> One (S (T T_COLON) :: r317)
  | 1109 -> One (S (T T_COLON) :: r772)
  | 540 -> One (S (T T_BARRBRACKET) :: r404)
  | 641 -> One (S (T T_BARRBRACKET) :: r487)
  | 862 -> One (S (T T_BARRBRACKET) :: r642)
  | 181 | 1232 -> One (S (T T_BAR) :: r155)
  | 231 -> One (S (T T_BAR) :: r184)
  | 379 -> One (S (N N_structure) :: r261)
  | 1424 -> One (S (N N_structure) :: r978)
  | 390 -> One (S (N N_pattern) :: r274)
  | 400 | 559 | 675 | 926 -> One (S (N N_pattern) :: r281)
  | 556 -> One (S (N N_pattern) :: r421)
  | 576 -> One (S (N N_pattern) :: r434)
  | 578 -> One (S (N N_pattern) :: r435)
  | 581 -> One (S (N N_pattern) :: r436)
  | 589 -> One (S (N N_pattern) :: r442)
  | 594 -> One (S (N N_pattern) :: r443)
  | 812 -> One (S (N N_pattern) :: r630)
  | 817 -> One (S (N N_pattern) :: r631)
  | 819 -> One (S (N N_pattern) :: r632)
  | 821 -> One (S (N N_pattern) :: r633)
  | 1073 -> One (S (N N_pattern) :: r725)
  | 417 -> One (S (N N_module_type) :: r307)
  | 418 -> One (S (N N_module_type) :: r309)
  | 473 -> One (S (N N_module_type) :: r356)
  | 519 -> One (S (N N_module_type) :: r386)
  | 981 -> One (S (N N_module_type) :: r691)
  | 495 -> One (S (N N_module_expr) :: r363)
  | 666 -> One (S (N N_let_pattern) :: r538)
  | 646 -> One (S (N N_expr) :: r495)
  | 650 -> One (S (N N_expr) :: r507)
  | 715 -> One (S (N N_expr) :: r583)
  | 732 -> One (S (N N_expr) :: r591)
  | 746 -> One (S (N N_expr) :: r598)
  | 748 -> One (S (N N_expr) :: r599)
  | 750 -> One (S (N N_expr) :: r600)
  | 755 -> One (S (N N_expr) :: r601)
  | 757 -> One (S (N N_expr) :: r602)
  | 759 -> One (S (N N_expr) :: r603)
  | 761 -> One (S (N N_expr) :: r604)
  | 763 -> One (S (N N_expr) :: r605)
  | 765 -> One (S (N N_expr) :: r606)
  | 767 -> One (S (N N_expr) :: r607)
  | 769 -> One (S (N N_expr) :: r608)
  | 771 -> One (S (N N_expr) :: r609)
  | 773 -> One (S (N N_expr) :: r610)
  | 775 -> One (S (N N_expr) :: r611)
  | 777 -> One (S (N N_expr) :: r612)
  | 779 -> One (S (N N_expr) :: r613)
  | 781 -> One (S (N N_expr) :: r614)
  | 783 -> One (S (N N_expr) :: r615)
  | 785 -> One (S (N N_expr) :: r616)
  | 787 -> One (S (N N_expr) :: r617)
  | 789 -> One (S (N N_expr) :: r618)
  | 791 -> One (S (N N_expr) :: r619)
  | 794 -> One (S (N N_expr) :: r620)
  | 796 -> One (S (N N_expr) :: r621)
  | 835 -> One (S (N N_expr) :: r638)
  | 840 -> One (S (N N_expr) :: r639)
  | 843 -> One (S (N N_expr) :: r640)
  | 898 -> One (S (N N_expr) :: r662)
  | 956 -> One (S (N N_expr) :: r679)
  | 634 -> One (Sub (r1) :: r467)
  | 661 -> One (Sub (r1) :: r524)
  | 918 -> One (Sub (r1) :: r668)
  | 1075 -> One (Sub (r1) :: r726)
  | 1528 -> One (Sub (r1) :: r1009)
  | 1530 -> One (Sub (r1) :: r1010)
  | 2 -> One (Sub (r10) :: r12)
  | 55 -> One (Sub (r10) :: r13)
  | 89 -> One (Sub (r10) :: r52)
  | 329 -> One (Sub (r10) :: r234)
  | 800 -> One (Sub (r10) :: r623)
  | 1071 -> One (Sub (r10) :: r724)
  | 1077 -> One (Sub (r10) :: r729)
  | 70 -> One (Sub (r34) :: r35)
  | 649 -> One (Sub (r34) :: r505)
  | 690 -> One (Sub (r34) :: r563)
  | 711 -> One (Sub (r34) :: r575)
  | 724 -> One (Sub (r34) :: r589)
  | 726 -> One (Sub (r34) :: r590)
  | 121 -> One (Sub (r37) :: r76)
  | 188 -> One (Sub (r37) :: r158)
  | 265 -> One (Sub (r37) :: r195)
  | 596 -> One (Sub (r53) :: r444)
  | 823 -> One (Sub (r53) :: r634)
  | 214 -> One (Sub (r57) :: r176)
  | 297 -> One (Sub (r57) :: r222)
  | 932 -> One (Sub (r57) :: r675)
  | 1225 -> One (Sub (r59) :: r857)
  | 1229 -> One (Sub (r59) :: r860)
  | 130 -> One (Sub (r61) :: r88)
  | 163 -> One (Sub (r61) :: r131)
  | 218 -> One (Sub (r61) :: r177)
  | 224 -> One (Sub (r63) :: r180)
  | 268 -> One (Sub (r65) :: r196)
  | 553 -> One (Sub (r65) :: r420)
  | 610 -> One (Sub (r65) :: r446)
  | 626 -> One (Sub (r65) :: r462)
  | 668 -> One (Sub (r65) :: r539)
  | 807 -> One (Sub (r65) :: r629)
  | 890 -> One (Sub (r65) :: r658)
  | 894 -> One (Sub (r65) :: r661)
  | 1148 -> One (Sub (r65) :: r799)
  | 1485 -> One (Sub (r65) :: r1000)
  | 93 -> One (Sub (r72) :: r74)
  | 146 -> One (Sub (r78) :: r115)
  | 245 -> One (Sub (r78) :: r192)
  | 368 -> One (Sub (r78) :: r260)
  | 406 -> One (Sub (r90) :: r286)
  | 529 -> One (Sub (r90) :: r390)
  | 1055 -> One (Sub (r90) :: r717)
  | 1058 -> One (Sub (r90) :: r719)
  | 1061 -> One (Sub (r90) :: r721)
  | 151 -> One (Sub (r110) :: r116)
  | 143 -> One (Sub (r112) :: r114)
  | 275 -> One (Sub (r119) :: r201)
  | 159 -> One (Sub (r125) :: r126)
  | 321 -> One (Sub (r125) :: r229)
  | 202 -> One (Sub (r144) :: r166)
  | 180 -> One (Sub (r146) :: r152)
  | 192 -> One (Sub (r162) :: r164)
  | 210 -> One (Sub (r174) :: r175)
  | 239 -> One (Sub (r187) :: r189)
  | 280 -> One (Sub (r203) :: r204)
  | 283 -> One (Sub (r205) :: r221)
  | 720 -> One (Sub (r205) :: r587)
  | 1328 -> One (Sub (r205) :: r920)
  | 1348 -> One (Sub (r205) :: r937)
  | 281 -> One (Sub (r213) :: r215)
  | 322 -> One (Sub (r213) :: r231)
  | 1118 -> One (Sub (r262) :: r774)
  | 392 -> One (Sub (r276) :: r277)
  | 988 -> One (Sub (r296) :: r692)
  | 469 -> One (Sub (r322) :: r355)
  | 429 -> One (Sub (r324) :: r325)
  | 438 -> One (Sub (r335) :: r340)
  | 431 -> One (Sub (r337) :: r339)
  | 1140 -> One (Sub (r337) :: r791)
  | 446 -> One (Sub (r344) :: r347)
  | 452 -> One (Sub (r351) :: r352)
  | 523 -> One (Sub (r387) :: r388)
  | 541 -> One (Sub (r406) :: r409)
  | 542 -> One (Sub (r416) :: r418)
  | 930 -> One (Sub (r424) :: r672)
  | 571 -> One (Sub (r432) :: r433)
  | 621 -> One (Sub (r453) :: r454)
  | 1016 -> One (Sub (r453) :: r699)
  | 622 -> One (Sub (r456) :: r457)
  | 631 -> One (Sub (r456) :: r463)
  | 623 -> One (Sub (r459) :: r461)
  | 632 -> One (Sub (r459) :: r466)
  | 647 -> One (Sub (r502) :: r504)
  | 882 -> One (Sub (r502) :: r655)
  | 937 -> One (Sub (r529) :: r676)
  | 664 -> One (Sub (r534) :: r535)
  | 676 -> One (Sub (r541) :: r544)
  | 927 -> One (Sub (r541) :: r671)
  | 1020 -> One (Sub (r541) :: r704)
  | 1355 -> One (Sub (r541) :: r945)
  | 717 -> One (Sub (r585) :: r586)
  | 722 -> One (Sub (r585) :: r588)
  | 875 -> One (Sub (r651) :: r653)
  | 950 -> One (Sub (r677) :: r678)
  | 1107 -> One (Sub (r764) :: r766)
  | 1354 -> One (Sub (r816) :: r940)
  | 1190 -> One (Sub (r828) :: r831)
  | 1376 -> One (Sub (r828) :: r958)
  | 1397 -> One (Sub (r843) :: r968)
  | 1414 -> One (Sub (r843) :: r976)
  | 1374 -> One (Sub (r893) :: r955)
  | 1418 -> One (Sub (r896) :: r977)
  | 1317 -> One (Sub (r911) :: r913)
  | 1339 -> One (Sub (r928) :: r930)
  | 798 -> One (r0)
  | 1543 -> One (r2)
  | 1542 -> One (r3)
  | 1541 -> One (r4)
  | 1540 -> One (r5)
  | 1539 -> One (r6)
  | 53 -> One (r7)
  | 54 -> One (r9)
  | 1538 -> One (r11)
  | 57 -> One (r12)
  | 56 -> One (r13)
  | 1466 -> One (r14)
  | 1537 -> One (r16)
  | 1536 -> One (r17)
  | 59 -> One (r18)
  | 58 -> One (r19)
  | 1535 -> One (r20)
  | 1534 -> One (r21)
  | 1533 -> One (r22)
  | 1532 -> One (r23)
  | 61 -> One (r24)
  | 62 -> One (r25)
  | 1524 -> One (r26)
  | 64 -> One (r27)
  | 1523 -> One (r28)
  | 1522 -> One (r29)
  | 1521 -> One (r30)
  | 1520 -> One (r31)
  | 66 -> One (r32)
  | 69 -> One (r33)
  | 1519 -> One (r35)
  | 73 -> One (r36)
  | 78 -> One (r38)
  | 72 -> One (r39)
  | 77 -> One (r40)
  | 75 -> One (r41)
  | 76 -> One (r42)
  | 80 -> One (r43)
  | 82 -> One (r44)
  | 1042 -> One (r45)
  | 1041 -> One (r46)
  | 83 -> One (r47)
  | 86 -> One (r48)
  | 88 | 648 | 904 -> One (r49)
  | 1518 -> One (r50)
  | 1517 -> One (r51)
  | 90 -> One (r52)
  | 119 -> One (r54)
  | 187 -> One (r56)
  | 209 -> One (r58)
  | 208 -> One (r60)
  | 217 -> One (r62)
  | 262 -> One (r64)
  | 1516 -> One (r66)
  | 1515 -> One (r67)
  | 118 -> One (r68)
  | 117 -> One (r69)
  | 92 -> One (r70)
  | 114 -> One (r71)
  | 116 -> One (r73)
  | 115 -> One (r74)
  | 120 | 134 -> One (r75)
  | 122 -> One (r76)
  | 125 -> One (r77)
  | 126 -> One (r79)
  | 123 -> One (r80)
  | 1514 -> One (r81)
  | 1513 -> One (r82)
  | 1512 -> One (r83)
  | 129 -> One (r84)
  | 128 -> One (r85)
  | 1511 -> One (r86)
  | 1510 -> One (r87)
  | 1509 -> One (r88)
  | 532 -> One (r89)
  | 1508 -> One (r91)
  | 1507 -> One (r92)
  | 133 -> One (r93)
  | 132 -> One (r94)
  | 1506 -> One (r95)
  | 1505 -> One (r96)
  | 136 -> One (r97)
  | 1493 -> One (r98)
  | 328 -> One (r99)
  | 327 -> One (r100)
  | 155 -> One (r101)
  | 154 | 334 -> One (r102)
  | 140 | 333 -> One (r103)
  | 138 | 332 -> One (r104)
  | 137 | 331 -> One (r105)
  | 145 -> One (r106)
  | 148 -> One (r108)
  | 144 -> One (r109)
  | 153 -> One (r111)
  | 150 -> One (r113)
  | 149 -> One (r114)
  | 147 -> One (r115)
  | 152 -> One (r116)
  | 312 -> One (r117)
  | 274 -> One (r118)
  | 310 -> One (r120)
  | 309 -> One (r121)
  | 308 -> One (r122)
  | 307 -> One (r124)
  | 305 -> One (r126)
  | 161 -> One (r127)
  | 162 | 177 | 1228 -> One (r128)
  | 259 -> One (r129)
  | 258 -> One (r130)
  | 257 -> One (r131)
  | 166 -> One (r132)
  | 165 | 442 -> One (r133)
  | 169 -> One (r134)
  | 253 -> One (r135)
  | 252 -> One (r137)
  | 251 -> One (r138)
  | 250 -> One (r139)
  | 249 -> One (r140)
  | 171 -> One (r141)
  | 199 | 1233 -> One (r143)
  | 228 -> One (r145)
  | 238 -> One (r147)
  | 237 -> One (r148)
  | 176 -> One (r149)
  | 179 -> One (r150)
  | 236 -> One (r151)
  | 235 -> One (r152)
  | 201 -> One (r153)
  | 200 -> One (r154)
  | 182 -> One (r155)
  | 184 -> One (r156)
  | 183 -> One (r157)
  | 189 -> One (r158)
  | 198 | 1238 -> One (r159)
  | 197 | 1237 -> One (r160)
  | 191 | 1236 -> One (r161)
  | 194 -> One (r163)
  | 193 -> One (r164)
  | 196 -> One (r165)
  | 203 -> One (r166)
  | 227 -> One (r167)
  | 216 -> One (r168)
  | 226 -> One (r170)
  | 223 -> One (r171)
  | 207 -> One (r172)
  | 211 -> One (r173)
  | 213 -> One (r175)
  | 215 -> One (r176)
  | 219 -> One (r177)
  | 222 -> One (r178)
  | 221 -> One (r179)
  | 225 -> One (r180)
  | 230 -> One (r181)
  | 234 -> One (r182)
  | 233 -> One (r183)
  | 232 -> One (r184)
  | 243 -> One (r186)
  | 241 -> One (r188)
  | 240 -> One (r189)
  | 248 -> One (r190)
  | 247 -> One (r191)
  | 246 -> One (r192)
  | 261 -> One (r193)
  | 264 -> One (r194)
  | 266 -> One (r195)
  | 269 -> One (r196)
  | 271 -> One (r197)
  | 273 -> One (r198)
  | 303 -> One (r199)
  | 302 -> One (r200)
  | 279 -> One (r201)
  | 295 -> One (r202)
  | 296 -> One (r204)
  | 294 -> One (r212)
  | 291 -> One (r214)
  | 290 -> One (r215)
  | 289 -> One (r216)
  | 288 -> One (r217)
  | 287 -> One (r218)
  | 286 -> One (r219)
  | 285 -> One (r220)
  | 284 -> One (r221)
  | 298 -> One (r222)
  | 301 -> One (r223)
  | 300 -> One (r224)
  | 316 -> One (r225)
  | 315 -> One (r226)
  | 314 -> One (r227)
  | 320 -> One (r228)
  | 325 -> One (r229)
  | 324 -> One (r230)
  | 323 -> One (r231)
  | 1492 -> One (r232)
  | 1491 -> One (r233)
  | 330 -> One (r234)
  | 364 -> One (r235)
  | 363 -> One (r236)
  | 1503 -> One (r237)
  | 358 -> One (r239)
  | 352 -> One (r241)
  | 351 -> One (r242)
  | 337 -> One (r243)
  | 336 -> One (r244)
  | 350 -> One (r245)
  | 349 -> One (r246)
  | 1498 -> One (r247)
  | 1497 -> One (r248)
  | 344 -> One (r249)
  | 348 -> One (r251)
  | 347 -> One (r252)
  | 340 -> One (r253)
  | 343 -> One (r255)
  | 354 -> One (r256)
  | 362 -> One (r257)
  | 361 -> One (r258)
  | 360 -> One (r259)
  | 369 -> One (r260)
  | 1490 -> One (r261)
  | 387 -> One (r263)
  | 386 -> One (r264)
  | 385 -> One (r265)
  | 384 -> One (r266)
  | 1316 -> One (r267)
  | 1315 -> One (r268)
  | 1489 -> One (r270)
  | 1488 -> One (r271)
  | 389 -> One (r272)
  | 1484 -> One (r273)
  | 1483 -> One (r274)
  | 391 -> One (r275)
  | 393 -> One (r277)
  | 395 -> One (r278)
  | 398 -> One (r279)
  | 609 -> One (r280)
  | 608 -> One (r281)
  | 405 -> One (r282)
  | 404 -> One (r283)
  | 403 -> One (r284)
  | 1475 -> One (r285)
  | 1474 -> One (r286)
  | 1473 -> One (r287)
  | 410 -> One (r288)
  | 409 -> One (r289)
  | 408 -> One (r290)
  | 1472 -> One (r291)
  | 1471 -> One (r292)
  | 412 -> One (r293)
  | 1064 -> One (r294)
  | 494 -> One (r295)
  | 1070 -> One (r297)
  | 1069 -> One (r298)
  | 1068 -> One (r299)
  | 1067 -> One (r300)
  | 491 -> One (r302)
  | 490 -> One (r303)
  | 416 -> One (r304)
  | 415 -> One (r305)
  | 414 -> One (r306)
  | 489 -> One (r307)
  | 488 -> One (r308)
  | 487 -> One (r309)
  | 484 -> One (r310)
  | 483 -> One (r311)
  | 482 -> One (r312)
  | 420 -> One (r313)
  | 424 -> One (r314)
  | 472 -> One (r315)
  | 428 -> One (r316)
  | 426 -> One (r317)
  | 464 -> One (r318)
  | 463 -> One (r319)
  | 462 -> One (r320)
  | 461 -> One (r321)
  | 471 -> One (r323)
  | 468 -> One (r325)
  | 459 -> One (r326)
  | 458 -> One (r327)
  | 457 -> One (r328)
  | 441 -> One (r329)
  | 434 -> One (r330)
  | 433 -> One (r331)
  | 435 -> One (r333)
  | 432 -> One (r334)
  | 440 -> One (r336)
  | 437 -> One (r338)
  | 436 -> One (r339)
  | 439 -> One (r340)
  | 445 -> One (r341)
  | 444 -> One (r342)
  | 447 -> One (r343)
  | 451 -> One (r345)
  | 450 -> One (r346)
  | 449 -> One (r347)
  | 455 -> One (r348)
  | 454 -> One (r349)
  | 453 -> One (r350)
  | 456 -> One (r352)
  | 467 -> One (r353)
  | 466 -> One (r354)
  | 470 -> One (r355)
  | 474 -> One (r356)
  | 479 -> One (r357)
  | 477 -> One (r358)
  | 1066 -> One (r359)
  | 1065 -> One (r360)
  | 493 -> One (r361)
  | 518 -> One (r362)
  | 517 -> One (r363)
  | 1054 -> One (r364)
  | 1053 -> One (r365)
  | 497 -> One (r366)
  | 1052 -> One (r367)
  | 1051 -> One (r368)
  | 1050 -> One (r369)
  | 1049 -> One (r370)
  | 501 -> One (r371)
  | 1048 -> One (r372)
  | 1047 -> One (r373)
  | 1046 -> One (r374)
  | 1045 -> One (r375)
  | 503 -> One (r376)
  | 528 -> One (r377)
  | 527 -> One (r378)
  | 508 -> One (r379)
  | 514 -> One (r380)
  | 512 -> One (r381)
  | 511 -> One (r382)
  | 510 -> One (r383)
  | 516 -> One (r384)
  | 521 -> One (r385)
  | 520 -> One (r386)
  | 531 -> One (r389)
  | 530 -> One (r390)
  | 593 -> One (r391)
  | 592 -> One (r392)
  | 826 -> One (r394)
  | 816 -> One (r396)
  | 815 -> One (r397)
  | 814 -> One (r398)
  | 1033 -> One (r400)
  | 1032 -> One (r401)
  | 539 -> One (r402)
  | 537 -> One (r403)
  | 617 -> One (r404)
  | 605 -> One (r405)
  | 604 -> One (r407)
  | 603 -> One (r408)
  | 600 -> One (r409)
  | 543 -> One (r410)
  | 555 -> One (r412)
  | 552 -> One (r413)
  | 546 -> One (r415)
  | 545 -> One (r417)
  | 544 -> One (r418)
  | 550 -> One (r419)
  | 554 -> One (r420)
  | 616 -> One (r421)
  | 567 | 806 -> One (r423)
  | 613 -> One (r425)
  | 558 -> One (r426)
  | 561 -> One (r427)
  | 607 -> One (r428)
  | 564 -> One (r429)
  | 573 -> One (r431)
  | 572 -> One (r433)
  | 577 -> One (r434)
  | 579 -> One (r435)
  | 582 -> One (r436)
  | 599 -> One (r437)
  | 598 -> One (r438)
  | 584 -> One (r439)
  | 586 -> One (r440)
  | 588 -> One (r441)
  | 590 -> One (r442)
  | 595 -> One (r443)
  | 597 -> One (r444)
  | 612 -> One (r445)
  | 611 -> One (r446)
  | 620 -> One (r447)
  | 619 -> One (r448)
  | 1011 -> One (r449)
  | 1010 -> One (r450)
  | 1015 -> One (r452)
  | 1031 -> One (r454)
  | 624 -> One (r455)
  | 630 -> One (r457)
  | 625 -> One (r458)
  | 629 -> One (r460)
  | 628 -> One (r461)
  | 627 -> One (r462)
  | 1005 -> One (r463)
  | 1004 -> One (r464)
  | 1003 -> One (r465)
  | 633 -> One (r466)
  | 1002 -> One (r467)
  | 997 -> One (r468)
  | 996 -> One (r469)
  | 995 -> One (r470)
  | 994 -> One (r472)
  | 993 -> One (r473)
  | 992 -> One (r474)
  | 991 -> One (r475)
  | 990 -> One (r476)
  | 1001 -> One (r477)
  | 1000 -> One (r478)
  | 999 -> One (r479)
  | 998 -> One (r480)
  | 1375 -> One (r481)
  | 977 -> One (r482)
  | 640 -> One (r483)
  | 639 -> One (r484)
  | 638 -> One (r485)
  | 637 -> One (r486)
  | 973 -> One (r487)
  | 869 -> One (r488)
  | 972 -> One (r490)
  | 971 -> One (r491)
  | 970 -> One (r492)
  | 644 -> One (r493)
  | 645 -> One (r494)
  | 967 -> One (r495)
  | 897 -> One (r496)
  | 889 -> One (r497)
  | 886 -> One (r499)
  | 905 -> One (r501)
  | 966 -> One (r503)
  | 965 -> One (r504)
  | 964 -> One (r505)
  | 963 -> One (r506)
  | 962 -> One (r507)
  | 961 -> One (r508)
  | 652 -> One (r509)
  | 958 -> One (r510)
  | 654 -> One (r511)
  | 955 -> One (r512)
  | 954 -> One (r513)
  | 953 -> One (r514)
  | 656 -> One (r515)
  | 949 -> One (r516)
  | 659 -> One (r517)
  | 658 -> One (r518)
  | 948 -> One (r519)
  | 946 -> One (r520)
  | 660 -> One (r521)
  | 945 -> One (r522)
  | 944 -> One (r523)
  | 943 -> One (r524)
  | 936 -> One (r525)
  | 925 -> One (r527)
  | 681 -> One (r528)
  | 942 -> One (r530)
  | 941 -> One (r531)
  | 663 -> One (r532)
  | 665 -> One (r533)
  | 674 -> One (r535)
  | 672 -> One (r536)
  | 671 -> One (r537)
  | 670 -> One (r538)
  | 669 -> One (r539)
  | 677 -> One (r540)
  | 940 -> One (r542)
  | 680 -> One (r543)
  | 679 -> One (r544)
  | 917 -> One (r545)
  | 916 -> One (r546)
  | 915 -> One (r547)
  | 914 -> One (r548)
  | 685 -> One (r549)
  | 684 -> One (r550)
  | 683 -> One (r551)
  | 908 -> One (r552)
  | 913 -> One (r554)
  | 912 -> One (r555)
  | 911 -> One (r556)
  | 910 -> One (r557)
  | 909 -> One (r558)
  | 906 -> One (r559)
  | 689 -> One (r560)
  | 688 -> One (r561)
  | 687 -> One (r562)
  | 692 -> One (r563)
  | 697 -> One (r564)
  | 696 -> One (r565)
  | 695 | 903 -> One (r566)
  | 902 -> One (r567)
  | 703 -> One (r568)
  | 702 -> One (r569)
  | 701 -> One (r570)
  | 700 -> One (r571)
  | 699 -> One (r572)
  | 710 -> One (r573)
  | 709 -> One (r574)
  | 712 -> One (r575)
  | 839 | 858 -> One (r576)
  | 838 | 857 -> One (r577)
  | 837 | 856 -> One (r578)
  | 713 | 728 -> One (r579)
  | 731 | 852 -> One (r580)
  | 730 | 851 -> One (r581)
  | 714 | 729 -> One (r582)
  | 850 -> One (r583)
  | 718 -> One (r584)
  | 719 -> One (r586)
  | 721 -> One (r587)
  | 723 -> One (r588)
  | 725 -> One (r589)
  | 727 -> One (r590)
  | 831 -> One (r591)
  | 737 -> One (r592)
  | 736 -> One (r593)
  | 741 -> One (r594)
  | 740 -> One (r595)
  | 793 -> One (r596)
  | 744 -> One (r597)
  | 747 -> One (r598)
  | 749 -> One (r599)
  | 751 -> One (r600)
  | 756 -> One (r601)
  | 758 -> One (r602)
  | 760 -> One (r603)
  | 762 -> One (r604)
  | 764 -> One (r605)
  | 766 -> One (r606)
  | 768 -> One (r607)
  | 770 -> One (r608)
  | 772 -> One (r609)
  | 774 -> One (r610)
  | 776 -> One (r611)
  | 778 -> One (r612)
  | 780 -> One (r613)
  | 782 -> One (r614)
  | 784 -> One (r615)
  | 786 -> One (r616)
  | 788 -> One (r617)
  | 790 -> One (r618)
  | 792 -> One (r619)
  | 795 -> One (r620)
  | 797 -> One (r621)
  | 802 -> One (r622)
  | 801 -> One (r623)
  | 828 -> One (r624)
  | 827 -> One (r625)
  | 805 -> One (r626)
  | 810 -> One (r627)
  | 809 -> One (r628)
  | 808 -> One (r629)
  | 813 -> One (r630)
  | 818 -> One (r631)
  | 820 -> One (r632)
  | 822 -> One (r633)
  | 824 -> One (r634)
  | 834 | 855 -> One (r635)
  | 833 | 854 -> One (r636)
  | 832 | 853 -> One (r637)
  | 836 -> One (r638)
  | 841 -> One (r639)
  | 844 -> One (r640)
  | 861 -> One (r641)
  | 863 -> One (r642)
  | 868 -> One (r643)
  | 867 -> One (r644)
  | 871 -> One (r645)
  | 874 -> One (r646)
  | 873 -> One (r647)
  | 881 -> One (r649)
  | 878 -> One (r650)
  | 877 -> One (r652)
  | 876 -> One (r653)
  | 901 -> One (r654)
  | 900 -> One (r655)
  | 885 -> One (r656)
  | 884 -> One (r657)
  | 891 -> One (r658)
  | 893 -> One (r659)
  | 892 | 1019 -> One (r660)
  | 895 -> One (r661)
  | 899 -> One (r662)
  | 924 -> One (r663)
  | 923 -> One (r664)
  | 922 -> One (r665)
  | 921 -> One (r666)
  | 920 -> One (r667)
  | 919 -> One (r668)
  | 939 -> One (r669)
  | 929 -> One (r670)
  | 928 -> One (r671)
  | 931 -> One (r672)
  | 935 -> One (r673)
  | 934 -> One (r674)
  | 933 -> One (r675)
  | 938 -> One (r676)
  | 951 -> One (r678)
  | 957 -> One (r679)
  | 960 -> One (r680)
  | 969 -> One (r681)
  | 976 -> One (r682)
  | 975 -> One (r683)
  | 987 -> One (r684)
  | 986 -> One (r685)
  | 985 -> One (r686)
  | 980 -> One (r687)
  | 979 -> One (r688)
  | 984 -> One (r689)
  | 983 -> One (r690)
  | 982 -> One (r691)
  | 989 -> One (r692)
  | 1018 -> One (r693)
  | 1009 -> One (r694)
  | 1008 -> One (r695)
  | 1007 -> One (r696)
  | 1014 -> One (r697)
  | 1013 -> One (r698)
  | 1017 -> One (r699)
  | 1025 -> One (r700)
  | 1024 -> One (r701)
  | 1023 -> One (r702)
  | 1022 -> One (r703)
  | 1021 -> One (r704)
  | 1030 -> One (r705)
  | 1029 -> One (r706)
  | 1028 -> One (r707)
  | 1027 -> One (r708)
  | 1040 -> One (r709)
  | 1039 -> One (r710)
  | 1038 -> One (r711)
  | 1037 -> One (r712)
  | 1036 -> One (r713)
  | 1035 -> One (r714)
  | 1044 -> One (r715)
  | 1057 -> One (r716)
  | 1056 -> One (r717)
  | 1060 -> One (r718)
  | 1059 -> One (r719)
  | 1063 -> One (r720)
  | 1062 -> One (r721)
  | 1470 -> One (r722)
  | 1469 -> One (r723)
  | 1072 -> One (r724)
  | 1074 -> One (r725)
  | 1076 -> One (r726)
  | 1468 -> One (r727)
  | 1467 -> One (r728)
  | 1078 -> One (r729)
  | 1082 -> One (r730)
  | 1081 -> One (r731)
  | 1080 -> One (r732)
  | 1089 -> One (r733)
  | 1092 -> One (r735)
  | 1091 -> One (r736)
  | 1088 -> One (r737)
  | 1087 -> One (r738)
  | 1086 -> One (r739)
  | 1085 -> One (r740)
  | 1084 -> One (r741)
  | 1099 -> One (r742)
  | 1098 -> One (r743)
  | 1097 -> One (r744)
  | 1096 -> One (r745)
  | 1102 -> One (r748)
  | 1101 -> One (r749)
  | 1100 -> One (r750)
  | 1134 -> One (r751)
  | 1133 -> One (r752)
  | 1132 -> One (r753)
  | 1303 -> One (r754)
  | 1302 -> One (r755)
  | 1127 -> One (r756)
  | 1126 -> One (r757)
  | 1125 -> One (r758)
  | 1124 -> One (r759)
  | 1123 -> One (r760)
  | 1106 -> One (r761)
  | 1114 -> One (r762)
  | 1113 -> One (r763)
  | 1122 -> One (r765)
  | 1121 -> One (r766)
  | 1117 -> One (r767)
  | 1116 -> One (r768)
  | 1115 -> One (r769)
  | 1112 -> One (r770)
  | 1111 -> One (r771)
  | 1110 -> One (r772)
  | 1120 -> One (r773)
  | 1119 -> One (r774)
  | 1131 -> One (r775)
  | 1130 -> One (r776)
  | 1129 -> One (r777)
  | 1189 -> One (r778)
  | 1198 -> One (r780)
  | 1214 -> One (r782)
  | 1213 -> One (r783)
  | 1145 -> One (r784)
  | 1144 -> One (r785)
  | 1143 -> One (r786)
  | 1139 -> One (r787)
  | 1137 -> One (r788)
  | 1136 -> One (r789)
  | 1142 -> One (r790)
  | 1141 -> One (r791)
  | 1154 -> One (r792)
  | 1153 -> One (r793)
  | 1152 -> One (r795)
  | 1151 -> One (r796)
  | 1147 -> One (r797)
  | 1150 -> One (r798)
  | 1149 -> One (r799)
  | 1171 -> One (r800)
  | 1170 -> One (r801)
  | 1169 -> One (r802)
  | 1168 -> One (r804)
  | 1167 -> One (r805)
  | 1156 -> One (r806)
  | 1161 -> One (r807)
  | 1160 -> One (r808)
  | 1159 -> One (r809)
  | 1158 -> One (r810)
  | 1166 -> One (r811)
  | 1165 -> One (r812)
  | 1164 -> One (r813)
  | 1163 -> One (r814)
  | 1186 -> One (r815)
  | 1185 -> One (r817)
  | 1184 -> One (r818)
  | 1180 -> One (r819)
  | 1179 -> One (r820)
  | 1178 -> One (r821)
  | 1173 -> One (r822)
  | 1183 -> One (r823)
  | 1182 -> One (r824)
  | 1200 -> One (r825)
  | 1199 -> One (r826)
  | 1188 -> One (r827)
  | 1196 -> One (r829)
  | 1192 -> One (r830)
  | 1191 -> One (r831)
  | 1195 -> One (r832)
  | 1194 -> One (r833)
  | 1206 -> One (r834)
  | 1205 -> One (r835)
  | 1204 -> One (r836)
  | 1208 -> One (r838)
  | 1207 -> One (r839)
  | 1203 -> One (r840)
  | 1210 -> One (r841)
  | 1241 -> One (r842)
  | 1246 -> One (r844)
  | 1245 -> One (r845)
  | 1219 -> One (r846)
  | 1218 -> One (r847)
  | 1217 -> One (r848)
  | 1216 -> One (r849)
  | 1244 -> One (r850)
  | 1224 -> One (r851)
  | 1223 -> One (r852)
  | 1222 -> One (r853)
  | 1221 -> One (r854)
  | 1243 -> One (r855)
  | 1227 -> One (r856)
  | 1226 -> One (r857)
  | 1242 -> One (r858)
  | 1231 -> One (r859)
  | 1230 -> One (r860)
  | 1240 -> One (r861)
  | 1235 -> One (r862)
  | 1255 -> One (r863)
  | 1254 -> One (r864)
  | 1253 -> One (r865)
  | 1252 -> One (r866)
  | 1251 -> One (r867)
  | 1250 -> One (r868)
  | 1259 -> One (r869)
  | 1269 -> One (r870)
  | 1268 -> One (r871)
  | 1267 -> One (r872)
  | 1266 -> One (r873)
  | 1265 -> One (r874)
  | 1278 -> One (r875)
  | 1288 -> One (r876)
  | 1287 -> One (r877)
  | 1286 -> One (r878)
  | 1285 -> One (r879)
  | 1284 -> One (r880)
  | 1283 -> One (r881)
  | 1282 -> One (r882)
  | 1299 -> One (r883)
  | 1298 -> One (r884)
  | 1297 -> One (r885)
  | 1296 -> One (r886)
  | 1295 -> One (r887)
  | 1294 -> One (r888)
  | 1293 -> One (r889)
  | 1389 -> One (r890)
  | 1387 -> One (r892)
  | 1413 -> One (r894)
  | 1310 -> One (r895)
  | 1421 -> One (r897)
  | 1420 -> One (r898)
  | 1309 -> One (r899)
  | 1308 -> One (r900)
  | 1307 -> One (r901)
  | 1314 -> One (r902)
  | 1313 -> One (r903)
  | 1312 -> One (r904)
  | 1335 -> One (r905)
  | 1334 -> One (r906)
  | 1333 -> One (r907)
  | 1332 -> One (r908)
  | 1321 -> One (r909)
  | 1320 -> One (r910)
  | 1319 -> One (r912)
  | 1318 -> One (r913)
  | 1326 -> One (r914)
  | 1325 -> One (r915)
  | 1324 -> One (r916)
  | 1323 -> One (r917)
  | 1331 -> One (r918)
  | 1330 -> One (r919)
  | 1329 -> One (r920)
  | 1338 -> One (r921)
  | 1337 -> One (r922)
  | 1364 -> One (r923)
  | 1353 -> One (r924)
  | 1352 -> One (r925)
  | 1341 -> One (r926)
  | 1340 -> One (r927)
  | 1366 -> One (r929)
  | 1365 -> One (r930)
  | 1346 -> One (r931)
  | 1345 -> One (r932)
  | 1344 -> One (r933)
  | 1343 -> One (r934)
  | 1351 -> One (r935)
  | 1350 -> One (r936)
  | 1349 -> One (r937)
  | 1363 -> One (r938)
  | 1362 -> One (r939)
  | 1361 -> One (r940)
  | 1360 -> One (r941)
  | 1359 -> One (r942)
  | 1358 -> One (r943)
  | 1357 -> One (r944)
  | 1356 -> One (r945)
  | 1370 -> One (r946)
  | 1369 -> One (r947)
  | 1368 -> One (r948)
  | 1404 -> One (r949)
  | 1403 -> One (r950)
  | 1400 -> One (r951)
  | 1373 -> One (r952)
  | 1372 -> One (r953)
  | 1396 -> One (r954)
  | 1395 -> One (r955)
  | 1379 -> One (r956)
  | 1378 -> One (r957)
  | 1377 -> One (r958)
  | 1392 -> One (r959)
  | 1383 -> One (r960)
  | 1382 -> One (r961)
  | 1394 -> One (r963)
  | 1381 -> One (r964)
  | 1390 -> One (r965)
  | 1385 -> One (r966)
  | 1399 -> One (r967)
  | 1398 -> One (r968)
  | 1402 -> One (r969)
  | 1408 -> One (r970)
  | 1407 -> One (r971)
  | 1406 -> One (r972)
  | 1410 -> One (r973)
  | 1417 -> One (r974)
  | 1416 -> One (r975)
  | 1415 -> One (r976)
  | 1419 -> One (r977)
  | 1460 -> One (r978)
  | 1427 -> One (r979)
  | 1437 -> One (r980)
  | 1436 -> One (r981)
  | 1435 -> One (r982)
  | 1434 -> One (r983)
  | 1447 -> One (r984)
  | 1457 -> One (r985)
  | 1456 -> One (r986)
  | 1455 -> One (r987)
  | 1454 -> One (r988)
  | 1453 -> One (r989)
  | 1452 -> One (r990)
  | 1463 -> One (r991)
  | 1462 -> One (r992)
  | 1482 -> One (r993)
  | 1481 -> One (r994)
  | 1480 -> One (r995)
  | 1479 -> One (r996)
  | 1478 -> One (r997)
  | 1477 -> One (r998)
  | 1487 -> One (r999)
  | 1486 -> One (r1000)
  | 1502 -> One (r1001)
  | 1500 -> One (r1003)
  | 1499 -> One (r1004)
  | 1496 -> One (r1005)
  | 1495 -> One (r1006)
  | 1527 -> One (r1007)
  | 1526 -> One (r1008)
  | 1529 -> One (r1009)
  | 1531 -> One (r1010)
  | 1545 -> One (r1011)
  | 1549 -> One (r1012)
  | 1553 -> One (r1013)
  | 694 -> Select (function
    | -1 -> [R 97]
    | _ -> r567)
  | 413 -> Select (function
    | -1 -> S (T T_TYPE) :: r306
    | _ -> R 191 :: r301)
  | 1093 -> Select (function
    | -1 -> r753
    | _ -> R 191 :: r747)
  | 526 -> Select (function
    | 514 | 517 | 523 | 527 | 700 | 984 | 1064 | 1081 | 1473 -> r388
    | _ -> [R 312])
  | 525 -> Select (function
    | 514 | 517 | 523 | 527 | 700 | 984 | 1064 | 1081 | 1473 -> [R 389]
    | _ -> [R 313])
  | 492 -> Select (function
    | -1 -> S (T T_UIDENT) :: r361
    | _ -> r301)
  | 504 -> Select (function
    | -1 -> S (T T_RPAREN) :: r48
    | _ -> r47)
  | 642 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r255
    | _ -> Sub (r489) :: r492)
  | 635 -> Select (function
    | 59 | 90 | 330 | 379 | 412 | 1072 | 1078 | 1424 -> r481
    | _ -> S (T T_EXCEPTION) :: r476)
  | 172 -> Select (function
    | 1019 -> r80
    | _ -> Sub (r78) :: r142)
  | 338 -> Select (function
    | 351 -> r248
    | _ -> Sub (r119) :: r254)
  | 535 -> Select (function
    | -1 -> r49
    | _ -> r133)
  | 173 -> Select (function
    | 1019 -> r79
    | _ -> r142)
  | 339 -> Select (function
    | 337 -> r254
    | _ -> r247)
  | 1095 -> Select (function
    | -1 -> r751
    | _ -> r746)
  | 1094 -> Select (function
    | -1 -> r752
    | _ -> r747)
  | _ -> raise Not_found
