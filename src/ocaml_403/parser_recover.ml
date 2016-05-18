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
    | MenhirInterpreter.T MenhirInterpreter.T_LABEL -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_comma_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern -> default_pattern ()
    | MenhirInterpreter.N MenhirInterpreter.N_parse_expression -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parent_binder -> raise Not_found
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
  [|0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;2;1;2;1;2;1;1;2;1;2;1;1;1;1;1;2;1;1;2;3;3;3;1;2;1;2;1;2;1;1;2;1;1;1;2;1;2;1;1;1;1;1;1;1;2;3;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;3;3;4;1;1;1;2;1;1;1;2;1;2;3;1;1;2;3;1;1;2;1;2;1;3;1;1;1;1;1;1;2;2;2;3;2;3;1;4;5;1;1;1;2;1;2;1;2;1;1;2;1;2;3;1;2;1;2;1;1;2;1;1;2;2;1;2;1;2;1;1;1;2;3;2;1;2;3;4;2;3;2;3;1;3;4;2;3;1;2;1;3;1;1;2;1;1;3;2;3;1;1;2;3;2;3;4;4;2;3;5;2;1;2;3;2;3;4;5;3;4;3;4;4;5;6;2;1;1;2;3;2;3;3;4;5;6;1;1;2;1;3;4;5;2;3;1;2;3;4;5;4;2;3;2;1;1;2;1;1;1;1;1;2;1;1;1;1;2;3;4;5;6;7;2;3;1;2;1;1;2;1;2;2;3;4;3;4;3;3;2;1;1;2;3;1;2;2;3;4;5;2;3;1;4;4;5;6;7;5;2;6;7;1;2;1;2;3;4;5;6;7;1;2;3;1;1;2;1;1;2;4;5;3;4;8;9;1;2;2;2;1;1;1;2;3;4;2;3;1;1;1;1;2;3;3;3;3;3;1;3;2;3;1;1;1;1;1;2;3;4;5;1;2;1;1;1;2;1;2;2;1;2;2;1;1;2;3;4;5;5;1;2;3;4;1;2;1;2;3;4;1;1;1;2;1;1;1;2;2;3;1;4;2;1;1;1;1;2;2;2;3;2;3;1;2;1;1;2;3;3;1;2;4;5;6;2;1;2;3;3;3;4;5;1;1;2;3;4;2;3;4;3;2;3;1;5;2;3;2;1;2;3;3;1;1;3;4;5;2;1;2;3;2;5;6;2;3;1;1;2;3;1;1;1;2;1;2;1;1;1;2;3;1;2;3;4;5;2;3;3;4;2;1;1;4;5;5;6;7;1;1;1;1;1;2;1;3;1;1;1;1;2;3;1;2;3;1;4;3;1;1;2;2;3;1;2;1;1;1;1;1;2;1;1;1;1;1;2;3;1;1;2;3;2;3;2;1;2;1;1;2;3;2;3;2;3;3;3;4;5;2;3;2;3;3;1;1;4;2;2;3;3;4;1;2;2;3;4;1;1;2;1;1;2;3;3;4;2;1;2;3;1;1;2;3;4;5;6;1;1;1;2;1;2;1;1;1;1;1;2;1;2;1;2;1;2;3;1;2;1;2;1;1;2;1;2;3;3;4;5;1;2;3;4;1;2;5;6;1;1;2;3;4;1;2;3;4;1;1;2;1;1;2;3;4;4;5;6;7;8;9;1;1;1;1;1;2;3;2;3;2;3;1;1;1;1;2;2;3;1;2;1;2;1;2;2;3;4;5;6;1;2;1;2;3;1;1;2;3;1;1;2;1;2;3;2;3;2;3;2;1;2;1;2;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;3;2;3;2;3;1;2;3;1;2;1;2;3;4;5;1;2;3;3;4;2;2;7;3;4;5;6;7;3;4;5;6;7;3;4;5;2;1;2;1;1;2;4;5;3;4;5;3;4;5;3;4;5;3;4;4;2;3;5;6;1;3;4;4;5;6;3;4;5;1;2;3;2;3;1;2;3;1;2;3;1;1;2;1;2;3;4;1;2;3;4;4;5;1;2;3;1;5;1;1;6;7;8;9;10;5;6;7;8;4;5;6;7;8;9;10;2;1;2;3;4;1;2;1;2;3;4;1;1;2;5;7;3;4;3;4;5;2;3;3;4;2;3;1;3;4;5;6;7;3;3;4;3;2;3;2;2;3;3;2;3;2;3;4;2;2;3;4;7;2;3;4;1;2;3;4;5;6;7;1;2;2;3;4;5;2;4;5;2;1;2;3;4;1;2;1;2;3;1;1;2;5;2;3;4;5;6;7;8;3;4;5;6;7;2;4;5;2;3;4;5;6;7;8;2;3;3;4;3;4;5;6;3;4;5;6;4;5;5;6;7;5;6;7;7;8;9;2;3;3;4;5;2;4;5;3;4;5;6;1;2;1;2;3;4;1;2;1;2;3;4;1;2;3;4;5;6;1;2;7;8;1;2;3;4;5;6;7;4;5;6;1;1;1;2;3;1;2;3;4;5;1;2;6;2;3;4;5;6;4;5;3;4;5;6;7;1;2;3;4;1;2;3;1;2;3;1;4;1;2;3;5;6;7;1;2;1;2;3;3;4;1;2;1;2;1;2;3;4;5;1;2;3;4;5;3;4;1;2;3;1;2;1;2;1;2;3;4;5;1;2;3;6;7;1;1;2;1;1;2;3;1;2;3;4;1;1;3;4;2;1;2;1;2;3;3;4;1;2;1;2;8;9;2;3;4;5;6;1;2;3;4;5;1;2;3;1;2;3;4;1;1;1;2;1;2;3;1;3;1;5;4;6;7;8;1;1;1;2;3;4;5;6;7;2;1;1;2;1;1;1;1;1;2;3;4;5;6;2;1;1;1;1;1;1;1;2;1;1;1;2;3;4;5;6;7;8;2;1;1;1;2;3;4;5;6;7;8;2;1;2;2;2;1;2;3;4;5;1;1;2;3;4;1;2;1;2;3;1;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;5;6;7;1;1;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;8;9;10;11;6;7;8;5;2;3;1;2;3;4;1;2;3;1;1;1;2;3;4;1;2;1;2;1;2;1;1;2;1;3;2;3;2;3;2;3;3;4;5;4;1;2;5;6;1;2;3;4;1;2;1;2;2;1;2;3;4;1;2;6;7;1;1;1;1;1;2;1;1;1;1;1;1;2;3;4;5;2;1;1;1;1;1;1;1;1;2;1;1;1;1;2;3;4;5;6;7;2;1;2;1;2;3;1;1;1;3;4;3;4;3;4;5;6;7;2;3;4;5;6;7;8;2;3;3;4;5;3;4;2;3;4;8;5;6;7;1;2;8;9;2;1;1;1;3;4;4;5;2;3;4;4;5;6;5;6;3;4;2;3;4;5;6;3;4;5;6;7;8;4;5;3;4;5;6;3;4;3;2;3;4;5;6;1;2;1;0;1;2;1;0;1;2;1;|]

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
  let r0 = [R 442] in
  let r1 = S (N N_expr) :: r0 in
  let r2 = [R 133] in
  let r3 = S (T T_DONE) :: r2 in
  let r4 = Sub (r1) :: r3 in
  let r5 = S (T T_DO) :: r4 in
  let r6 = Sub (r1) :: r5 in
  let r7 = [R 556] in
  let r8 = S (T T_AND) :: r7 in
  let r9 = [R 14] in
  let r10 = Sub (r8) :: r9 in
  let r11 = [R 192] in
  let r12 = R 17 :: r11 in
  let r13 = [R 15] in
  let r14 = [R 406] in
  let r15 = S (N N_structure) :: r14 in
  let r16 = [R 16] in
  let r17 = S (T T_RBRACKET) :: r16 in
  let r18 = Sub (r15) :: r17 in
  let r19 = [R 151] in
  let r20 = S (T T_DONE) :: r19 in
  let r21 = Sub (r1) :: r20 in
  let r22 = S (T T_DO) :: r21 in
  let r23 = Sub (r1) :: r22 in
  let r24 = [R 293] in
  let r25 = [R 129] in
  let r26 = Sub (r1) :: r25 in
  let r27 = [R 145] in
  let r28 = S (N N_match_cases) :: r27 in
  let r29 = R 364 :: r28 in
  let r30 = S (T T_WITH) :: r29 in
  let r31 = Sub (r1) :: r30 in
  let r32 = [R 534] in
  let r33 = S (T T_QUESTIONQUESTION) :: r32 in
  let r34 = [R 522] in
  let r35 = [R 48] in
  let r36 = S (T T_LIDENT) :: r35 in
  let r37 = [R 524] in
  let r38 = Sub (r36) :: r37 in
  let r39 = [R 49] in
  let r40 = S (T T_LIDENT) :: r39 in
  let r41 = [R 294] in
  let r42 = [R 191] in
  let r43 = [R 18] in
  let r44 = [R 684] in
  let r45 = S (T T_error) :: r44 in
  let r46 = [R 99] in
  let r47 = [R 193] in
  let r48 = S (T T_RBRACKET) :: r47 in
  let r49 = Sub (r15) :: r48 in
  let r50 = S (T T_LPAREN) :: r45 in
  let r51 = [R 477] in
  let r52 = S (T T_UNDERSCORE) :: r51 in
  let r53 = [R 474] in
  let r54 = Sub (r52) :: r53 in
  let r55 = [R 495] in
  let r56 = Sub (r54) :: r55 in
  let r57 = [R 114] in
  let r58 = Sub (r56) :: r57 in
  let r59 = [R 123] in
  let r60 = Sub (r58) :: r59 in
  let r61 = [R 112] in
  let r62 = Sub (r60) :: r61 in
  let r63 = [R 692] in
  let r64 = R 416 :: r63 in
  let r65 = Sub (r62) :: r64 in
  let r66 = S (T T_COLON) :: r65 in
  let r67 = Sub (r50) :: r66 in
  let r68 = [R 685] in
  let r69 = [R 682] in
  let r70 = [R 290] in
  let r71 = [R 483] in
  let r72 = [R 220] in
  let r73 = S (T T_LIDENT) :: r72 in
  let r74 = [R 476] in
  let r75 = Sub (r73) :: r74 in
  let r76 = [R 115] in
  let r77 = Sub (r58) :: r76 in
  let r78 = S (T T_MINUSGREATER) :: r77 in
  let r79 = Sub (r58) :: r78 in
  let r80 = S (T T_COLON) :: r79 in
  let r81 = [R 116] in
  let r82 = Sub (r58) :: r81 in
  let r83 = S (T T_MINUSGREATER) :: r82 in
  let r84 = [R 384] in
  let r85 = S (N N_module_type) :: r84 in
  let r86 = [R 493] in
  let r87 = S (T T_RPAREN) :: r86 in
  let r88 = Sub (r85) :: r87 in
  let r89 = R 190 :: r88 in
  let r90 = [R 318] in
  let r91 = S (T T_END) :: r90 in
  let r92 = R 451 :: r91 in
  let r93 = [R 654] in
  let r94 = R 416 :: r93 in
  let r95 = R 105 :: r94 in
  let r96 = R 657 :: r95 in
  let r97 = S (T T_LIDENT) :: r96 in
  let r98 = R 377 :: r97 in
  let r99 = R 336 :: r98 in
  let r100 = R 190 :: r99 in
  let r101 = [R 381] in
  let r102 = S (T T_UNDERSCORE) :: r101 in
  let r103 = [R 374] in
  let r104 = Sub (r102) :: r103 in
  let r105 = R 676 :: r104 in
  let r106 = [R 375] in
  let r107 = Sub (r105) :: r106 in
  let r108 = [R 379] in
  let r109 = S (T T_RPAREN) :: r108 in
  let r110 = [R 380] in
  let r111 = [R 376] in
  let r112 = [R 662] in
  let r113 = [R 95] in
  let r114 = S (T T_FALSE) :: r113 in
  let r115 = [R 108] in
  let r116 = R 17 :: r115 in
  let r117 = R 215 :: r116 in
  let r118 = Sub (r114) :: r117 in
  let r119 = [R 109] in
  let r120 = Sub (r118) :: r119 in
  let r121 = [R 661] in
  let r122 = [R 93] in
  let r123 = [R 667] in
  let r124 = [R 117] in
  let r125 = Sub (r58) :: r124 in
  let r126 = S (T T_MINUSGREATER) :: r125 in
  let r127 = [R 482] in
  let r128 = [R 224] in
  let r129 = [R 481] in
  let r130 = [R 413] in
  let r131 = Sub (r60) :: r130 in
  let r132 = [R 201] in
  let r133 = R 17 :: r132 in
  let r134 = S (T T_SEMI) :: r133 in
  let r135 = R 17 :: r134 in
  let r136 = Sub (r131) :: r135 in
  let r137 = [R 679] in
  let r138 = [R 439] in
  let r139 = Sub (r54) :: r138 in
  let r140 = [R 440] in
  let r141 = Sub (r139) :: r140 in
  let r142 = [R 491] in
  let r143 = S (T T_RBRACKET) :: r142 in
  let r144 = Sub (r141) :: r143 in
  let r145 = [R 490] in
  let r146 = [R 489] in
  let r147 = S (T T_RBRACKET) :: r146 in
  let r148 = [R 487] in
  let r149 = S (T T_RBRACKET) :: r148 in
  let r150 = Sub (r141) :: r149 in
  let r151 = [R 333] in
  let r152 = Sub (r73) :: r151 in
  let r153 = [R 484] in
  let r154 = [R 668] in
  let r155 = S (T T_LIDENT) :: r154 in
  let r156 = S (T T_DOT) :: r155 in
  let r157 = S (T T_UIDENT) :: r70 in
  let r158 = [R 292] in
  let r159 = S (T T_RPAREN) :: r158 in
  let r160 = [R 291] in
  let r161 = [R 441] in
  let r162 = [R 643] in
  let r163 = [R 5] in
  let r164 = Sub (r60) :: r163 in
  let r165 = [R 642] in
  let r166 = R 17 :: r165 in
  let r167 = Sub (r164) :: r166 in
  let r168 = [R 121] in
  let r169 = Sub (r54) :: r168 in
  let r170 = [R 496] in
  let r171 = [R 122] in
  let r172 = [R 118] in
  let r173 = [R 124] in
  let r174 = Sub (r73) :: r173 in
  let r175 = [R 6] in
  let r176 = [R 486] in
  let r177 = [R 488] in
  let r178 = S (T T_RBRACKET) :: r177 in
  let r179 = Sub (r141) :: r178 in
  let r180 = S (T T_BACKQUOTE) :: r152 in
  let r181 = [R 334] in
  let r182 = Sub (r180) :: r181 in
  let r183 = [R 492] in
  let r184 = S (T T_RBRACKET) :: r183 in
  let r185 = [R 414] in
  let r186 = Sub (r60) :: r185 in
  let r187 = [R 680] in
  let r188 = [R 94] in
  let r189 = [R 475] in
  let r190 = [R 485] in
  let r191 = [R 120] in
  let r192 = [R 119] in
  let r193 = [R 92] in
  let r194 = [R 19] in
  let r195 = R 17 :: r194 in
  let r196 = R 215 :: r195 in
  let r197 = [R 106] in
  let r198 = Sub (r169) :: r197 in
  let r199 = [R 216] in
  let r200 = S (T T_LIDENT) :: r128 in
  let r201 = [R 225] in
  let r202 = R 17 :: r201 in
  let r203 = Sub (r131) :: r202 in
  let r204 = S (T T_COLON) :: r203 in
  let r205 = Sub (r200) :: r204 in
  let r206 = R 331 :: r205 in
  let r207 = [R 227] in
  let r208 = Sub (r206) :: r207 in
  let r209 = [R 107] in
  let r210 = S (T T_RBRACE) :: r209 in
  let r211 = [R 226] in
  let r212 = R 17 :: r211 in
  let r213 = S (T T_SEMI) :: r212 in
  let r214 = R 17 :: r213 in
  let r215 = Sub (r131) :: r214 in
  let r216 = S (T T_COLON) :: r215 in
  let r217 = [R 218] in
  let r218 = [R 217] in
  let r219 = Sub (r54) :: r218 in
  let r220 = [R 663] in
  let r221 = S (T T_RBRACE) :: r220 in
  let r222 = Sub (r208) :: r221 in
  let r223 = [R 665] in
  let r224 = [R 664] in
  let r225 = [R 666] in
  let r226 = S (T T_RBRACE) :: r225 in
  let r227 = [R 415] in
  let r228 = S (T T_RBRACKET) :: r227 in
  let r229 = Sub (r15) :: r228 in
  let r230 = [R 194] in
  let r231 = R 17 :: r230 in
  let r232 = R 215 :: r231 in
  let r233 = Sub (r114) :: r232 in
  let r234 = [R 607] in
  let r235 = Sub (r233) :: r234 in
  let r236 = [R 614] in
  let r237 = R 416 :: r236 in
  let r238 = Sub (r235) :: r237 in
  let r239 = R 421 :: r238 in
  let r240 = [R 20] in
  let r241 = R 17 :: r240 in
  let r242 = R 215 :: r241 in
  let r243 = Sub (r114) :: r242 in
  let r244 = [R 100] in
  let r245 = S (T T_FALSE) :: r244 in
  let r246 = [R 21] in
  let r247 = R 17 :: r246 in
  let r248 = Sub (r245) :: r247 in
  let r249 = S (T T_EQUAL) :: r248 in
  let r250 = [R 98] in
  let r251 = [R 417] in
  let r252 = [R 195] in
  let r253 = R 17 :: r252 in
  let r254 = Sub (r245) :: r253 in
  let r255 = [R 644] in
  let r256 = [R 638] in
  let r257 = S (T T_UIDENT) :: r24 in
  let r258 = [R 338] in
  let r259 = R 416 :: r258 in
  let r260 = Sub (r257) :: r259 in
  let r261 = R 190 :: r260 in
  let r262 = [R 74] in
  let r263 = R 41 :: r262 in
  let r264 = R 52 :: r263 in
  let r265 = [R 184] in
  let r266 = S (T T_END) :: r265 in
  let r267 = Sub (r264) :: r266 in
  let r268 = [R 50] in
  let r269 = S (T T_RPAREN) :: r268 in
  let r270 = [R 539] in
  let r271 = S (T T_LIDENT) :: r123 in
  let r272 = [R 544] in
  let r273 = [R 472] in
  let r274 = [R 470] in
  let r275 = [R 551] in
  let r276 = S (T T_RPAREN) :: r275 in
  let r277 = [R 552] in
  let r278 = S (T T_RPAREN) :: r277 in
  let r279 = [R 322] in
  let r280 = S (N N_module_expr) :: r279 in
  let r281 = R 17 :: r280 in
  let r282 = S (T T_OF) :: r281 in
  let r283 = [R 305] in
  let r284 = S (T T_END) :: r283 in
  let r285 = S (N N_structure) :: r284 in
  let r286 = [R 297] in
  let r287 = S (N N_module_expr) :: r286 in
  let r288 = S (T T_EQUAL) :: r287 in
  let r289 = [R 430] in
  let r290 = R 416 :: r289 in
  let r291 = Sub (r288) :: r290 in
  let r292 = S (T T_UIDENT) :: r291 in
  let r293 = S (T T_REC) :: r292 in
  let r294 = [R 326] in
  let r295 = R 416 :: r294 in
  let r296 = R 327 :: r295 in
  let r297 = Sub (r73) :: r296 in
  let r298 = R 190 :: r297 in
  let r299 = [R 328] in
  let r300 = [R 323] in
  let r301 = S (T T_RPAREN) :: r300 in
  let r302 = [R 319] in
  let r303 = S (N N_module_type) :: r302 in
  let r304 = S (T T_MINUSGREATER) :: r303 in
  let r305 = S (N N_functor_args) :: r304 in
  let r306 = [R 209] in
  let r307 = [R 210] in
  let r308 = S (T T_RPAREN) :: r307 in
  let r309 = S (N N_module_type) :: r308 in
  let r310 = [R 701] in
  let r311 = Sub (r157) :: r310 in
  let r312 = S (T T_COLONEQUAL) :: r311 in
  let r313 = S (T T_UIDENT) :: r312 in
  let r314 = S (T T_MODULE) :: r313 in
  let r315 = [R 702] in
  let r316 = Sub (r314) :: r315 in
  let r317 = [R 321] in
  let r318 = [R 699] in
  let r319 = Sub (r60) :: r318 in
  let r320 = S (T T_COLONEQUAL) :: r319 in
  let r321 = Sub (r200) :: r320 in
  let r322 = [R 675] in
  let r323 = Sub (r73) :: r322 in
  let r324 = S (T T_QUOTE) :: r323 in
  let r325 = [R 669] in
  let r326 = Sub (r324) :: r325 in
  let r327 = R 676 :: r326 in
  let r328 = [R 670] in
  let r329 = Sub (r327) :: r328 in
  let r330 = [R 674] in
  let r331 = S (T T_RPAREN) :: r330 in
  let r332 = [R 671] in
  let r333 = [R 238] in
  let r334 = S (T T_LIDENT) :: r333 in
  let r335 = [R 704] in
  let r336 = S (T T_EQUAL) :: r335 in
  let r337 = [R 698] in
  let r338 = R 105 :: r337 in
  let r339 = Sub (r60) :: r338 in
  let r340 = [R 102] in
  let r341 = Sub (r62) :: r340 in
  let r342 = S (T T_EQUAL) :: r341 in
  let r343 = Sub (r62) :: r342 in
  let r344 = [R 104] in
  let r345 = [R 700] in
  let r346 = Sub (r157) :: r345 in
  let r347 = [R 703] in
  let r348 = [R 320] in
  let r349 = [R 330] in
  let r350 = Sub (r73) :: r349 in
  let r351 = [R 296] in
  let r352 = R 416 :: r351 in
  let r353 = Sub (r288) :: r352 in
  let r354 = [R 310] in
  let r355 = S (T T_RPAREN) :: r354 in
  let r356 = [R 311] in
  let r357 = S (T T_RPAREN) :: r356 in
  let r358 = S (N N_expr) :: r357 in
  let r359 = [R 128] in
  let r360 = S (N N_match_cases) :: r359 in
  let r361 = R 364 :: r360 in
  let r362 = S (T T_WITH) :: r361 in
  let r363 = Sub (r1) :: r362 in
  let r364 = [R 144] in
  let r365 = S (N N_match_cases) :: r364 in
  let r366 = R 364 :: r365 in
  let r367 = S (T T_WITH) :: r366 in
  let r368 = Sub (r1) :: r367 in
  let r369 = [R 530] in
  let r370 = S (T T_RPAREN) :: r369 in
  let r371 = [R 306] in
  let r372 = S (N N_module_expr) :: r371 in
  let r373 = S (T T_MINUSGREATER) :: r372 in
  let r374 = S (N N_functor_args) :: r373 in
  let r375 = [R 308] in
  let r376 = [R 307] in
  let r377 = [R 531] in
  let r378 = S (T T_RPAREN) :: r377 in
  let r379 = [R 265] in
  let r380 = Sub (r1) :: r379 in
  let r381 = S (T T_EQUAL) :: r380 in
  let r382 = S (N N_pattern) :: r381 in
  let r383 = [R 273] in
  let r384 = R 416 :: r383 in
  let r385 = Sub (r382) :: r384 in
  let r386 = R 428 :: r385 in
  let r387 = [R 548] in
  let r388 = [R 402] in
  let r389 = S (N N_pattern) :: r388 in
  let r390 = [R 546] in
  let r391 = S (T T_RBRACKET) :: r390 in
  let r392 = R 370 :: r391 in
  let r393 = [R 237] in
  let r394 = S (T T_LIDENT) :: r393 in
  let r395 = [R 256] in
  let r396 = R 369 :: r395 in
  let r397 = Sub (r394) :: r396 in
  let r398 = [R 257] in
  let r399 = Sub (r397) :: r398 in
  let r400 = [R 545] in
  let r401 = S (T T_RBRACE) :: r400 in
  let r402 = [R 259] in
  let r403 = [R 368] in
  let r404 = [R 255] in
  let r405 = S (T T_UNDERSCORE) :: r270 in
  let r406 = [R 538] in
  let r407 = Sub (r405) :: r406 in
  let r408 = [R 396] in
  let r409 = Sub (r407) :: r408 in
  let r410 = [R 87] in
  let r411 = [R 397] in
  let r412 = S (N N_pattern) :: r411 in
  let r413 = S (T T_INT) :: r410 in
  let r414 = [R 469] in
  let r415 = Sub (r413) :: r414 in
  let r416 = [R 541] in
  let r417 = [R 399] in
  let r418 = [R 393] in
  let r419 = [R 392] in
  let r420 = [R 391] in
  let r421 = [R 549] in
  let r422 = [R 400] in
  let r423 = [R 550] in
  let r424 = S (T T_RPAREN) :: r423 in
  let r425 = [R 395] in
  let r426 = [R 389] in
  let r427 = [R 547] in
  let r428 = S (T T_BARRBRACKET) :: r427 in
  let r429 = [R 239] in
  let r430 = S (T T_LIDENT) :: r429 in
  let r431 = [R 247] in
  let r432 = [R 235] in
  let r433 = Sub (r430) :: r432 in
  let r434 = [R 246] in
  let r435 = S (T T_RPAREN) :: r434 in
  let r436 = [R 236] in
  let r437 = [R 243] in
  let r438 = [R 242] in
  let r439 = S (T T_RPAREN) :: r438 in
  let r440 = R 366 :: r439 in
  let r441 = [R 367] in
  let r442 = [R 261] in
  let r443 = R 416 :: r442 in
  let r444 = Sub (r382) :: r443 in
  let r445 = R 428 :: r444 in
  let r446 = R 190 :: r445 in
  let r447 = [R 140] in
  let r448 = Sub (r1) :: r447 in
  let r449 = S (T T_IN) :: r448 in
  let r450 = Sub (r257) :: r449 in
  let r451 = R 190 :: r450 in
  let r452 = R 382 :: r451 in
  let r453 = [R 516] in
  let r454 = [R 188] in
  let r455 = S (N N_expr) :: r454 in
  let r456 = [R 519] in
  let r457 = S (T T_RBRACKET) :: r456 in
  let r458 = R 370 :: r457 in
  let r459 = [R 526] in
  let r460 = [R 198] in
  let r461 = [R 197] in
  let r462 = [R 251] in
  let r463 = R 373 :: r462 in
  let r464 = Sub (r394) :: r463 in
  let r465 = [R 252] in
  let r466 = Sub (r464) :: r465 in
  let r467 = [R 437] in
  let r468 = Sub (r466) :: r467 in
  let r469 = [R 513] in
  let r470 = S (T T_RBRACE) :: r469 in
  let r471 = [R 498] in
  let r472 = [R 497] in
  let r473 = S (T T_GREATERDOT) :: r472 in
  let r474 = [R 183] in
  let r475 = Sub (r33) :: r474 in
  let r476 = [R 505] in
  let r477 = S (T T_END) :: r476 in
  let r478 = [R 150] in
  let r479 = S (N N_expr) :: r478 in
  let r480 = S (T T_THEN) :: r479 in
  let r481 = Sub (r1) :: r480 in
  let r482 = [R 141] in
  let r483 = S (N N_match_cases) :: r482 in
  let r484 = R 364 :: r483 in
  let r485 = [R 278] in
  let r486 = S (T T_DOT) :: r485 in
  let r487 = S (T T_MINUSGREATER) :: r486 in
  let r488 = [R 277] in
  let r489 = Sub (r1) :: r488 in
  let r490 = S (T T_MINUSGREATER) :: r489 in
  let r491 = [R 249] in
  let r492 = Sub (r407) :: r491 in
  let r493 = [R 205] in
  let r494 = Sub (r1) :: r493 in
  let r495 = S (T T_MINUSGREATER) :: r494 in
  let r496 = [R 142] in
  let r497 = Sub (r495) :: r496 in
  let r498 = Sub (r492) :: r497 in
  let r499 = [R 405] in
  let r500 = S (T T_UNDERSCORE) :: r499 in
  let r501 = [R 245] in
  let r502 = [R 244] in
  let r503 = S (T T_RPAREN) :: r502 in
  let r504 = R 366 :: r503 in
  let r505 = [R 270] in
  let r506 = [R 271] in
  let r507 = S (T T_LIDENT) :: r506 in
  let r508 = [R 143] in
  let r509 = Sub (r495) :: r508 in
  let r510 = S (T T_RPAREN) :: r509 in
  let r511 = [R 135] in
  let r512 = S (T T_DONE) :: r511 in
  let r513 = Sub (r1) :: r512 in
  let r514 = S (T T_DO) :: r513 in
  let r515 = Sub (r1) :: r514 in
  let r516 = S (T T_IN) :: r515 in
  let r517 = S (N N_pattern) :: r516 in
  let r518 = [R 126] in
  let r519 = S (T T_DOWNTO) :: r518 in
  let r520 = [R 152] in
  let r521 = S (T T_DONE) :: r520 in
  let r522 = Sub (r1) :: r521 in
  let r523 = S (T T_DO) :: r522 in
  let r524 = Sub (r1) :: r523 in
  let r525 = Sub (r519) :: r524 in
  let r526 = Sub (r1) :: r525 in
  let r527 = S (T T_EQUAL) :: r526 in
  let r528 = S (N N_pattern) :: r527 in
  let r529 = [R 523] in
  let r530 = [R 509] in
  let r531 = S (T T_RPAREN) :: r530 in
  let r532 = S (T T_LPAREN) :: r531 in
  let r533 = S (T T_DOT) :: r532 in
  let r534 = [R 532] in
  let r535 = S (T T_RPAREN) :: r534 in
  let r536 = Sub (r85) :: r535 in
  let r537 = S (T T_COLON) :: r536 in
  let r538 = [R 182] in
  let r539 = Sub (r33) :: r538 in
  let r540 = [R 529] in
  let r541 = [R 512] in
  let r542 = S (T T_RBRACE) :: r541 in
  let r543 = S (N N_expr) :: r542 in
  let r544 = S (T T_LBRACE) :: r543 in
  let r545 = [R 510] in
  let r546 = S (T T_RPAREN) :: r545 in
  let r547 = Sub (r1) :: r546 in
  let r548 = [R 175] in
  let r549 = [R 234] in
  let r550 = S (T T_LIDENT) :: r549 in
  let r551 = [R 231] in
  let r552 = [R 528] in
  let r553 = [R 232] in
  let r554 = [R 233] in
  let r555 = [R 230] in
  let r556 = [R 178] in
  let r557 = [R 127] in
  let r558 = Sub (r1) :: r557 in
  let r559 = [R 138] in
  let r560 = Sub (r1) :: r559 in
  let r561 = [R 181] in
  let r562 = S (N N_expr) :: r561 in
  let r563 = [R 186] in
  let r564 = [R 165] in
  let r565 = [R 159] in
  let r566 = [R 176] in
  let r567 = [R 162] in
  let r568 = [R 166] in
  let r569 = [R 158] in
  let r570 = [R 161] in
  let r571 = [R 160] in
  let r572 = [R 170] in
  let r573 = [R 164] in
  let r574 = [R 163] in
  let r575 = [R 168] in
  let r576 = [R 157] in
  let r577 = [R 156] in
  let r578 = [R 153] in
  let r579 = [R 155] in
  let r580 = [R 169] in
  let r581 = [R 167] in
  let r582 = [R 171] in
  let r583 = [R 172] in
  let r584 = [R 173] in
  let r585 = [R 187] in
  let r586 = [R 174] in
  let r587 = [R 10] in
  let r588 = R 416 :: r587 in
  let r589 = Sub (r382) :: r588 in
  let r590 = [R 266] in
  let r591 = Sub (r1) :: r590 in
  let r592 = S (T T_EQUAL) :: r591 in
  let r593 = [R 511] in
  let r594 = S (T T_RBRACKET) :: r593 in
  let r595 = Sub (r1) :: r594 in
  let r596 = [R 179] in
  let r597 = [R 180] in
  let r598 = [R 177] in
  let r599 = [R 508] in
  let r600 = [R 518] in
  let r601 = [R 517] in
  let r602 = S (T T_BARRBRACKET) :: r601 in
  let r603 = [R 521] in
  let r604 = [R 520] in
  let r605 = S (T T_RBRACKET) :: r604 in
  let r606 = Sub (r200) :: r460 in
  let r607 = [R 199] in
  let r608 = R 370 :: r607 in
  let r609 = Sub (r606) :: r608 in
  let r610 = [R 527] in
  let r611 = S (T T_GREATERRBRACE) :: r610 in
  let r612 = [R 514] in
  let r613 = S (T T_RBRACE) :: r612 in
  let r614 = [R 436] in
  let r615 = Sub (r466) :: r614 in
  let r616 = [R 653] in
  let r617 = [R 651] in
  let r618 = Sub (r62) :: r617 in
  let r619 = [R 652] in
  let r620 = [R 250] in
  let r621 = [R 134] in
  let r622 = S (T T_DONE) :: r621 in
  let r623 = Sub (r1) :: r622 in
  let r624 = S (T T_DO) :: r623 in
  let r625 = Sub (r1) :: r624 in
  let r626 = Sub (r519) :: r625 in
  let r627 = [R 208] in
  let r628 = Sub (r495) :: r627 in
  let r629 = S (T T_RPAREN) :: r628 in
  let r630 = [R 248] in
  let r631 = [R 206] in
  let r632 = Sub (r1) :: r631 in
  let r633 = S (T T_MINUSGREATER) :: r632 in
  let r634 = [R 207] in
  let r635 = S (N N_pattern) :: r487 in
  let r636 = [R 280] in
  let r637 = [R 149] in
  let r638 = [R 504] in
  let r639 = [R 525] in
  let r640 = [R 515] in
  let r641 = S (T T_BARRBRACKET) :: r640 in
  let r642 = [R 139] in
  let r643 = Sub (r1) :: r642 in
  let r644 = S (T T_IN) :: r643 in
  let r645 = Sub (r288) :: r644 in
  let r646 = S (T T_UIDENT) :: r645 in
  let r647 = [R 298] in
  let r648 = S (N N_module_expr) :: r647 in
  let r649 = S (T T_EQUAL) :: r648 in
  let r650 = [R 299] in
  let r651 = [R 615] in
  let r652 = Sub (r1) :: r651 in
  let r653 = S (T T_EQUAL) :: r652 in
  let r654 = [R 203] in
  let r655 = Sub (r653) :: r654 in
  let r656 = [R 617] in
  let r657 = Sub (r655) :: r656 in
  let r658 = S (T T_RPAREN) :: r657 in
  let r659 = Sub (r507) :: r658 in
  let r660 = [R 204] in
  let r661 = Sub (r1) :: r660 in
  let r662 = [R 616] in
  let r663 = [R 264] in
  let r664 = Sub (r1) :: r663 in
  let r665 = S (T T_EQUAL) :: r664 in
  let r666 = Sub (r62) :: r665 in
  let r667 = S (T T_DOT) :: r666 in
  let r668 = [R 263] in
  let r669 = Sub (r1) :: r668 in
  let r670 = S (T T_EQUAL) :: r669 in
  let r671 = Sub (r62) :: r670 in
  let r672 = [R 154] in
  let r673 = S (T T_RPAREN) :: r672 in
  let r674 = S (N N_expr) :: r673 in
  let r675 = S (T T_COMMA) :: r674 in
  let r676 = S (N N_expr) :: r675 in
  let r677 = S (T T_LPAREN) :: r676 in
  let r678 = [R 503] in
  let r679 = [R 506] in
  let r680 = [R 314] in
  let r681 = S (T T_RPAREN) :: r680 in
  let r682 = [R 312] in
  let r683 = S (T T_RPAREN) :: r682 in
  let r684 = [R 313] in
  let r685 = S (T T_RPAREN) :: r684 in
  let r686 = [R 309] in
  let r687 = S (T T_RPAREN) :: r686 in
  let r688 = [R 223] in
  let r689 = S (T T_RBRACKET) :: r688 in
  let r690 = Sub (r15) :: r689 in
  let r691 = [R 409] in
  let r692 = [R 410] in
  let r693 = [R 202] in
  let r694 = S (T T_RBRACKET) :: r693 in
  let r695 = Sub (r15) :: r694 in
  let r696 = [R 613] in
  let r697 = R 416 :: r696 in
  let r698 = S (N N_module_expr) :: r697 in
  let r699 = [R 419] in
  let r700 = S (T T_STRING) :: r699 in
  let r701 = [R 418] in
  let r702 = R 416 :: r701 in
  let r703 = Sub (r700) :: r702 in
  let r704 = S (T T_EQUAL) :: r703 in
  let r705 = Sub (r62) :: r704 in
  let r706 = S (T T_COLON) :: r705 in
  let r707 = Sub (r50) :: r706 in
  let r708 = [R 606] in
  let r709 = R 416 :: r708 in
  let r710 = R 17 :: r709 in
  let r711 = Sub (r245) :: r710 in
  let r712 = S (T T_EQUAL) :: r711 in
  let r713 = Sub (r114) :: r712 in
  let r714 = [R 445] in
  let r715 = R 416 :: r714 in
  let r716 = R 17 :: r715 in
  let r717 = R 215 :: r716 in
  let r718 = Sub (r114) :: r717 in
  let r719 = R 190 :: r718 in
  let r720 = [R 407] in
  let r721 = [R 452] in
  let r722 = [R 433] in
  let r723 = R 416 :: r722 in
  let r724 = S (N N_module_type) :: r723 in
  let r725 = S (T T_COLON) :: r724 in
  let r726 = S (T T_UIDENT) :: r725 in
  let r727 = S (T T_REC) :: r726 in
  let r728 = [R 301] in
  let r729 = S (N N_module_type) :: r728 in
  let r730 = S (T T_COLON) :: r729 in
  let r731 = [R 300] in
  let r732 = R 416 :: r731 in
  let r733 = [R 303] in
  let r734 = Sub (r730) :: r733 in
  let r735 = [R 302] in
  let r736 = Sub (r730) :: r735 in
  let r737 = S (T T_RPAREN) :: r736 in
  let r738 = S (N N_module_type) :: r737 in
  let r739 = [R 295] in
  let r740 = R 416 :: r739 in
  let r741 = [R 449] in
  let r742 = R 416 :: r741 in
  let r743 = S (N N_module_type) :: r742 in
  let r744 = [R 85] in
  let r745 = S (T T_LIDENT) :: r744 in
  let r746 = [R 65] in
  let r747 = Sub (r745) :: r746 in
  let r748 = [R 80] in
  let r749 = R 416 :: r748 in
  let r750 = Sub (r747) :: r749 in
  let r751 = S (T T_EQUAL) :: r750 in
  let r752 = S (T T_LIDENT) :: r751 in
  let r753 = R 83 :: r752 in
  let r754 = R 696 :: r753 in
  let r755 = R 190 :: r754 in
  let r756 = [R 84] in
  let r757 = S (T T_RBRACKET) :: r756 in
  let r758 = [R 55] in
  let r759 = R 62 :: r758 in
  let r760 = R 54 :: r759 in
  let r761 = [R 66] in
  let r762 = S (T T_END) :: r761 in
  let r763 = Sub (r760) :: r762 in
  let r764 = [R 53] in
  let r765 = S (T T_RPAREN) :: r764 in
  let r766 = [R 695] in
  let r767 = Sub (r62) :: r766 in
  let r768 = S (T T_COLON) :: r767 in
  let r769 = Sub (r200) :: r768 in
  let r770 = [R 57] in
  let r771 = R 416 :: r770 in
  let r772 = Sub (r769) :: r771 in
  let r773 = [R 693] in
  let r774 = Sub (r62) :: r773 in
  let r775 = S (T T_COLON) :: r774 in
  let r776 = Sub (r200) :: r775 in
  let r777 = [R 694] in
  let r778 = Sub (r62) :: r777 in
  let r779 = S (T T_COLON) :: r778 in
  let r780 = Sub (r200) :: r779 in
  let r781 = [R 411] in
  let r782 = Sub (r62) :: r781 in
  let r783 = [R 58] in
  let r784 = R 416 :: r783 in
  let r785 = Sub (r782) :: r784 in
  let r786 = S (T T_COLON) :: r785 in
  let r787 = Sub (r200) :: r786 in
  let r788 = R 423 :: r787 in
  let r789 = [R 412] in
  let r790 = Sub (r62) :: r789 in
  let r791 = [R 56] in
  let r792 = R 416 :: r791 in
  let r793 = Sub (r747) :: r792 in
  let r794 = Sub (r62) :: r192 in
  let r795 = [R 64] in
  let r796 = Sub (r745) :: r795 in
  let r797 = S (T T_RBRACKET) :: r796 in
  let r798 = [R 86] in
  let r799 = S (T T_LIDENT) :: r798 in
  let r800 = [R 103] in
  let r801 = Sub (r62) :: r800 in
  let r802 = S (T T_EQUAL) :: r801 in
  let r803 = Sub (r62) :: r802 in
  let r804 = [R 59] in
  let r805 = R 416 :: r804 in
  let r806 = Sub (r803) :: r805 in
  let r807 = [R 60] in
  let r808 = [R 75] in
  let r809 = Sub (r747) :: r808 in
  let r810 = [R 25] in
  let r811 = R 416 :: r810 in
  let r812 = Sub (r809) :: r811 in
  let r813 = S (T T_COLON) :: r812 in
  let r814 = S (T T_LIDENT) :: r813 in
  let r815 = R 83 :: r814 in
  let r816 = [R 76] in
  let r817 = Sub (r809) :: r816 in
  let r818 = S (T T_MINUSGREATER) :: r817 in
  let r819 = Sub (r56) :: r818 in
  let r820 = S (T T_COLON) :: r819 in
  let r821 = [R 77] in
  let r822 = Sub (r809) :: r821 in
  let r823 = S (T T_MINUSGREATER) :: r822 in
  let r824 = [R 78] in
  let r825 = Sub (r809) :: r824 in
  let r826 = S (T T_MINUSGREATER) :: r825 in
  let r827 = [R 79] in
  let r828 = Sub (r809) :: r827 in
  let r829 = [R 13] in
  let r830 = R 416 :: r829 in
  let r831 = R 105 :: r830 in
  let r832 = R 657 :: r831 in
  let r833 = S (T T_LIDENT) :: r832 in
  let r834 = R 377 :: r833 in
  let r835 = [R 453] in
  let r836 = [R 12] in
  let r837 = R 416 :: r836 in
  let r838 = S (N N_module_type) :: r837 in
  let r839 = S (T T_COLON) :: r838 in
  let r840 = S (T T_UIDENT) :: r839 in
  let r841 = [R 467] in
  let r842 = [R 9] in
  let r843 = R 416 :: r842 in
  let r844 = Sub (r747) :: r843 in
  let r845 = S (T T_EQUAL) :: r844 in
  let r846 = S (T T_LIDENT) :: r845 in
  let r847 = R 83 :: r846 in
  let r848 = R 696 :: r847 in
  let r849 = [R 8] in
  let r850 = R 416 :: r849 in
  let r851 = Sub (r809) :: r850 in
  let r852 = S (T T_COLON) :: r851 in
  let r853 = S (T T_LIDENT) :: r852 in
  let r854 = R 83 :: r853 in
  let r855 = R 696 :: r854 in
  let r856 = [R 70] in
  let r857 = Sub (r36) :: r856 in
  let r858 = [R 28] in
  let r859 = Sub (r857) :: r858 in
  let r860 = [R 43] in
  let r861 = Sub (r859) :: r860 in
  let r862 = S (T T_EQUAL) :: r861 in
  let r863 = [R 22] in
  let r864 = R 416 :: r863 in
  let r865 = Sub (r862) :: r864 in
  let r866 = S (T T_LIDENT) :: r865 in
  let r867 = R 83 :: r866 in
  let r868 = [R 71] in
  let r869 = S (T T_END) :: r868 in
  let r870 = Sub (r264) :: r869 in
  let r871 = [R 690] in
  let r872 = Sub (r1) :: r871 in
  let r873 = S (T T_EQUAL) :: r872 in
  let r874 = Sub (r200) :: r873 in
  let r875 = R 331 :: r874 in
  let r876 = R 17 :: r875 in
  let r877 = R 382 :: r876 in
  let r878 = [R 35] in
  let r879 = R 416 :: r878 in
  let r880 = [R 689] in
  let r881 = Sub (r62) :: r880 in
  let r882 = S (T T_COLON) :: r881 in
  let r883 = Sub (r200) :: r882 in
  let r884 = [R 688] in
  let r885 = Sub (r62) :: r884 in
  let r886 = S (T T_COLON) :: r885 in
  let r887 = [R 691] in
  let r888 = Sub (r1) :: r887 in
  let r889 = [R 287] in
  let r890 = Sub (r653) :: r889 in
  let r891 = Sub (r200) :: r890 in
  let r892 = R 421 :: r891 in
  let r893 = R 17 :: r892 in
  let r894 = R 382 :: r893 in
  let r895 = [R 36] in
  let r896 = R 416 :: r895 in
  let r897 = [R 286] in
  let r898 = Sub (r782) :: r897 in
  let r899 = S (T T_COLON) :: r898 in
  let r900 = Sub (r200) :: r899 in
  let r901 = [R 285] in
  let r902 = Sub (r782) :: r901 in
  let r903 = S (T T_COLON) :: r902 in
  let r904 = [R 288] in
  let r905 = Sub (r1) :: r904 in
  let r906 = S (T T_EQUAL) :: r905 in
  let r907 = [R 289] in
  let r908 = Sub (r1) :: r907 in
  let r909 = S (T T_EQUAL) :: r908 in
  let r910 = Sub (r62) :: r909 in
  let r911 = S (T T_DOT) :: r910 in
  let r912 = [R 38] in
  let r913 = R 416 :: r912 in
  let r914 = Sub (r1) :: r913 in
  let r915 = [R 34] in
  let r916 = R 416 :: r915 in
  let r917 = R 386 :: r916 in
  let r918 = Sub (r859) :: r917 in
  let r919 = R 17 :: r918 in
  let r920 = [R 73] in
  let r921 = S (T T_RPAREN) :: r920 in
  let r922 = [R 69] in
  let r923 = Sub (r36) :: r922 in
  let r924 = S (T T_RBRACKET) :: r923 in
  let r925 = [R 46] in
  let r926 = Sub (r859) :: r925 in
  let r927 = S (T T_MINUSGREATER) :: r926 in
  let r928 = Sub (r492) :: r927 in
  let r929 = [R 29] in
  let r930 = Sub (r928) :: r929 in
  let r931 = [R 31] in
  let r932 = Sub (r859) :: r931 in
  let r933 = [R 72] in
  let r934 = S (T T_RPAREN) :: r933 in
  let r935 = [R 385] in
  let r936 = [R 37] in
  let r937 = R 416 :: r936 in
  let r938 = Sub (r803) :: r937 in
  let r939 = [R 39] in
  let r940 = [R 44] in
  let r941 = Sub (r859) :: r940 in
  let r942 = S (T T_EQUAL) :: r941 in
  let r943 = [R 45] in
  let r944 = [R 619] in
  let r945 = [R 639] in
  let r946 = [R 11] in
  let r947 = R 416 :: r946 in
  let r948 = Sub (r288) :: r947 in
  let r949 = S (T T_UIDENT) :: r948 in
  let r950 = [R 635] in
  let r951 = [R 7] in
  let r952 = R 416 :: r951 in
  let r953 = Sub (r862) :: r952 in
  let r954 = S (T T_LIDENT) :: r953 in
  let r955 = R 83 :: r954 in
  let r956 = R 696 :: r955 in
  let r957 = [R 618] in
  let r958 = R 637 :: r957 in
  let r959 = [R 394] in
  let r960 = S (T T_RPAREN) :: r959 in
  let r961 = S (N N_pattern) :: r960 in
  let r962 = S (T T_COMMA) :: r961 in
  let r963 = S (N N_pattern) :: r962 in
  let r964 = S (T T_LPAREN) :: r963 in
  let r965 = [R 51] in
  let r966 = S (T T_RPAREN) :: r965 in
  let r967 = [R 446] in
  let r968 = Sub (r233) :: r967 in
  let r969 = [R 450] in
  let r970 = R 416 :: r969 in
  let r971 = Sub (r968) :: r970 in
  let r972 = R 421 :: r971 in
  let r973 = [R 130] in
  let r974 = S (N N_match_cases) :: r973 in
  let r975 = [R 132] in
  let r976 = [R 131] in
  let r977 = [R 221] in
  let r978 = [R 222] in
  let r979 = [R 387] in
  function
  | 0 | 1516 | 1520 -> Nothing
  | 1515 -> One ([R 0])
  | 1519 -> One ([R 1])
  | 1523 -> One ([R 2])
  | 385 -> One ([R 3])
  | 384 -> One ([R 4])
  | 79 -> One (R 17 :: r42)
  | 81 -> One (R 17 :: r43)
  | 139 -> One (R 17 :: r92)
  | 208 -> One (R 17 :: r162)
  | 415 -> One (R 17 :: r285)
  | 423 -> One (R 17 :: r305)
  | 500 -> One (R 17 :: r358)
  | 513 -> One (R 17 :: r374)
  | 789 -> One (R 17 :: r589)
  | 1115 -> One (R 17 :: r763)
  | 1124 -> One (R 17 :: r772)
  | 1141 -> One (R 17 :: r788)
  | 1156 -> One (R 17 :: r793)
  | 1171 -> One (R 17 :: r806)
  | 1218 -> One (R 17 :: r834)
  | 1233 -> One (R 17 :: r840)
  | 1250 -> One (R 17 :: r848)
  | 1261 -> One (R 17 :: r855)
  | 1280 -> One (R 17 :: r870)
  | 1336 -> One (R 17 :: r914)
  | 1349 -> One (R 17 :: r930)
  | 1374 -> One (R 17 :: r938)
  | 1402 -> One (R 17 :: r949)
  | 1420 -> One (R 17 :: r956)
  | 1428 -> One ([R 23])
  | 1427 -> One ([R 24])
  | 1270 -> One ([R 26])
  | 1269 -> One ([R 27])
  | 1357 -> One ([R 30])
  | 1360 -> One ([R 32])
  | 1355 -> One ([R 33])
  | 1380 -> One ([R 40])
  | 1381 -> One ([R 42])
  | 1362 -> One ([R 47])
  | 1180 -> One ([R 61])
  | 1181 -> One ([R 63])
  | 1170 -> One ([R 67])
  | 1166 -> One ([R 68])
  | 1259 -> One ([R 81])
  | 1258 -> One ([R 82])
  | 563 -> One ([R 88])
  | 68 -> One ([R 89])
  | 560 -> One ([R 90])
  | 161 | 280 -> One ([R 91])
  | 162 -> One ([R 96])
  | 349 -> One ([R 97])
  | 67 -> One ([R 101])
  | 315 -> One ([R 110])
  | 310 -> One ([R 111])
  | 274 -> One ([R 113])
  | 879 -> One ([R 125])
  | 704 -> One ([R 136])
  | 817 -> One ([R 137])
  | 733 -> One ([R 146])
  | 742 -> One ([R 147])
  | 722 -> One ([R 148])
  | 740 -> One ([R 185])
  | 838 -> One ([R 189])
  | 1 -> One (R 190 :: r6)
  | 60 -> One (R 190 :: r23)
  | 63 -> One (R 190 :: r26)
  | 65 -> One (R 190 :: r31)
  | 71 -> One (R 190 :: r38)
  | 92 -> One (R 190 :: r67)
  | 392 -> One (R 190 :: r267)
  | 504 -> One (R 190 :: r363)
  | 506 -> One (R 190 :: r368)
  | 534 -> One (R 190 :: r386)
  | 555 -> One (R 190 :: r409)
  | 561 -> One (R 190 :: r412)
  | 639 -> One (R 190 :: r475)
  | 641 -> One (R 190 :: r477)
  | 643 -> One (R 190 :: r481)
  | 645 -> One (R 190 :: r484)
  | 650 -> One (R 190 :: r498)
  | 670 -> One (R 190 :: r517)
  | 674 -> One (R 190 :: r528)
  | 696 -> One (R 190 :: r539)
  | 950 -> One (R 190 :: r646)
  | 1048 -> One (R 190 :: r698)
  | 1052 -> One (R 190 :: r707)
  | 1074 -> One (R 190 :: r727)
  | 1097 -> One (R 190 :: r743)
  | 852 -> One ([R 200])
  | 427 -> One ([R 211])
  | 426 -> One ([R 212])
  | 489 -> One ([R 213])
  | 490 -> One ([R 214])
  | 128 | 482 -> One ([R 219])
  | 296 -> One ([R 228])
  | 297 -> One ([R 229])
  | 818 -> One ([R 240])
  | 820 -> One ([R 241])
  | 860 -> One ([R 253])
  | 859 -> One ([R 254])
  | 545 -> One ([R 258])
  | 549 -> One ([R 260])
  | 995 -> One ([R 262])
  | 730 -> One ([R 267])
  | 801 -> One ([R 268])
  | 655 -> One ([R 269])
  | 666 -> One ([R 272])
  | 726 -> One ([R 274])
  | 802 -> One ([R 275])
  | 920 -> One ([R 276])
  | 924 -> One ([R 279])
  | 259 -> One ([R 281])
  | 258 -> One ([R 282])
  | 260 -> One ([R 283])
  | 171 -> One ([R 284])
  | 523 -> One ([R 304])
  | 522 -> One ([R 315])
  | 524 -> One ([R 316])
  | 431 -> One ([R 317])
  | 485 -> One ([R 324])
  | 479 -> One ([R 325])
  | 484 -> One ([R 329])
  | 1126 -> One (R 331 :: r776)
  | 1291 -> One (R 331 :: r883)
  | 286 | 1296 -> One ([R 332])
  | 246 -> One ([R 335])
  | 143 -> One ([R 337])
  | 88 | 95 -> One ([R 339])
  | 110 -> One ([R 340])
  | 109 -> One ([R 341])
  | 108 -> One ([R 342])
  | 107 -> One ([R 343])
  | 106 -> One ([R 344])
  | 86 -> One ([R 345])
  | 115 | 692 -> One ([R 346])
  | 98 | 405 | 510 -> One ([R 347])
  | 97 | 509 -> One ([R 348])
  | 104 | 532 | 558 -> One ([R 349])
  | 103 | 531 -> One ([R 350])
  | 85 -> One ([R 351])
  | 112 -> One ([R 352])
  | 105 -> One ([R 353])
  | 111 -> One ([R 354])
  | 100 -> One ([R 355])
  | 114 -> One ([R 356])
  | 116 -> One ([R 357])
  | 117 -> One ([R 358])
  | 113 -> One ([R 359])
  | 96 -> One ([R 360])
  | 99 -> One ([R 361])
  | 210 -> One ([R 362])
  | 209 -> One (R 363 :: r167)
  | 178 -> One (R 364 :: r144)
  | 1494 -> One (R 364 :: r974)
  | 179 -> One ([R 365])
  | 546 -> One (R 370 :: r402)
  | 606 -> One (R 370 :: r428)
  | 836 -> One (R 370 :: r602)
  | 844 -> One (R 370 :: r605)
  | 946 -> One (R 370 :: r641)
  | 547 | 600 | 837 | 851 -> One ([R 371])
  | 868 -> One ([R 372])
  | 371 -> One ([R 378])
  | 386 -> One (R 382 :: r261)
  | 1340 -> One (R 382 :: r919)
  | 387 -> One ([R 383])
  | 566 -> One ([R 388])
  | 571 -> One ([R 390])
  | 576 -> One ([R 398])
  | 586 -> One ([R 401])
  | 601 -> One ([R 403])
  | 661 -> One ([R 404])
  | 1273 -> One ([R 408])
  | 357 -> One (R 416 :: r251)
  | 1178 -> One (R 416 :: r807)
  | 1246 -> One (R 416 :: r841)
  | 1378 -> One (R 416 :: r939)
  | 1415 -> One (R 416 :: r950)
  | 1430 -> One (R 416 :: r958)
  | 1059 -> One ([R 420])
  | 1311 -> One (R 421 :: r900)
  | 323 | 1316 -> One ([R 422])
  | 1145 -> One ([R 424])
  | 1143 -> One ([R 425])
  | 1146 -> One ([R 426])
  | 1144 -> One ([R 427])
  | 536 -> One ([R 429])
  | 1408 -> One ([R 431])
  | 1407 -> One ([R 432])
  | 1240 -> One ([R 434])
  | 1239 -> One ([R 435])
  | 190 -> One ([R 438])
  | 787 -> One ([R 443])
  | 788 -> One ([R 444])
  | 1473 -> One ([R 447])
  | 1470 -> One ([R 448])
  | 1072 -> One (R 451 :: r720)
  | 1073 -> One (R 451 :: r721)
  | 1227 -> One (R 451 :: r835)
  | 1216 -> One ([R 454])
  | 1241 -> One ([R 455])
  | 1217 -> One ([R 456])
  | 1229 -> One ([R 457])
  | 1231 -> One ([R 458])
  | 1244 -> One ([R 459])
  | 1245 -> One ([R 460])
  | 1232 -> One ([R 461])
  | 1243 -> One ([R 462])
  | 1242 -> One ([R 463])
  | 1230 -> One ([R 464])
  | 1260 -> One ([R 465])
  | 1249 -> One ([R 466])
  | 1248 -> One ([R 468])
  | 403 -> One ([R 471])
  | 400 -> One ([R 473])
  | 189 -> One ([R 478])
  | 194 -> One ([R 479])
  | 271 -> One ([R 480])
  | 216 | 1208 -> One ([R 494])
  | 679 -> One ([R 499])
  | 695 -> One ([R 500])
  | 694 | 741 -> One ([R 501])
  | 681 | 721 -> One ([R 502])
  | 814 | 831 -> One ([R 507])
  | 693 -> One ([R 533])
  | 821 -> One ([R 535])
  | 819 -> One ([R 536])
  | 564 | 609 -> One ([R 537])
  | 567 -> One ([R 540])
  | 597 -> One ([R 542])
  | 596 -> One ([R 543])
  | 579 -> One ([R 553])
  | 28 -> One ([R 554])
  | 8 -> One ([R 555])
  | 52 -> One ([R 557])
  | 51 -> One ([R 558])
  | 50 -> One ([R 559])
  | 49 -> One ([R 560])
  | 48 -> One ([R 561])
  | 47 -> One ([R 562])
  | 46 -> One ([R 563])
  | 45 -> One ([R 564])
  | 44 -> One ([R 565])
  | 43 -> One ([R 566])
  | 42 -> One ([R 567])
  | 41 -> One ([R 568])
  | 40 -> One ([R 569])
  | 39 -> One ([R 570])
  | 38 -> One ([R 571])
  | 37 -> One ([R 572])
  | 36 -> One ([R 573])
  | 35 -> One ([R 574])
  | 34 -> One ([R 575])
  | 33 -> One ([R 576])
  | 32 -> One ([R 577])
  | 31 -> One ([R 578])
  | 30 -> One ([R 579])
  | 29 -> One ([R 580])
  | 27 -> One ([R 581])
  | 26 -> One ([R 582])
  | 25 -> One ([R 583])
  | 24 -> One ([R 584])
  | 23 -> One ([R 585])
  | 22 -> One ([R 586])
  | 21 -> One ([R 587])
  | 20 -> One ([R 588])
  | 19 -> One ([R 589])
  | 18 -> One ([R 590])
  | 17 -> One ([R 591])
  | 16 -> One ([R 592])
  | 15 -> One ([R 593])
  | 14 -> One ([R 594])
  | 13 -> One ([R 595])
  | 12 -> One ([R 596])
  | 11 -> One ([R 597])
  | 10 -> One ([R 598])
  | 9 -> One ([R 599])
  | 7 -> One ([R 600])
  | 6 -> One ([R 601])
  | 5 -> One ([R 602])
  | 4 -> One ([R 603])
  | 3 -> One ([R 604])
  | 1400 -> One ([R 605])
  | 370 -> One ([R 608])
  | 361 -> One ([R 609])
  | 369 -> One ([R 610])
  | 360 -> One ([R 611])
  | 359 -> One ([R 612])
  | 1394 -> One ([R 620])
  | 1413 | 1433 -> One ([R 621])
  | 1414 | 1434 -> One ([R 622])
  | 1409 -> One ([R 623])
  | 1391 -> One ([R 624])
  | 1392 -> One ([R 625])
  | 1397 -> One ([R 626])
  | 1399 -> One ([R 627])
  | 1412 -> One ([R 628])
  | 1401 -> One ([R 629])
  | 1411 -> One ([R 630])
  | 1410 -> One ([R 631])
  | 1419 -> One ([R 632])
  | 1418 -> One ([R 633])
  | 1398 -> One ([R 634])
  | 1417 -> One ([R 636])
  | 1395 -> One (R 637 :: r945)
  | 503 -> One ([R 640])
  | 502 -> One ([R 641])
  | 375 -> One ([R 645])
  | 376 -> One ([R 646])
  | 378 -> One ([R 647])
  | 380 -> One ([R 648])
  | 377 -> One ([R 649])
  | 374 -> One ([R 650])
  | 1226 -> One ([R 655])
  | 1225 -> One ([R 656])
  | 321 -> One ([R 658])
  | 308 -> One ([R 659])
  | 330 -> One ([R 660])
  | 434 -> One (R 672 :: r321)
  | 464 -> One ([R 673])
  | 145 -> One ([R 677])
  | 146 -> One ([R 678])
  | 89 | 636 | 876 -> One ([R 681])
  | 119 -> One ([R 683])
  | 379 -> One ([R 686])
  | 382 -> One ([R 687])
  | 1131 -> One (R 696 :: r780)
  | 1184 -> One (R 696 :: r815)
  | 1275 -> One (R 696 :: r867)
  | 1107 -> One ([R 697])
  | 452 -> One ([R 705])
  | 101 | 406 | 511 | 686 -> One (S (T T_error) :: r68)
  | 855 -> One (S (T T_WITH) :: r615)
  | 350 | 381 -> One (S (T T_UIDENT) :: r41)
  | 199 -> One (S (T T_UIDENT) :: r160)
  | 407 -> One (S (T T_UIDENT) :: r276)
  | 411 -> One (S (T T_TYPE) :: r282)
  | 970 -> One (S (T T_TYPE) :: r659)
  | 1104 | 1274 -> One (S (T T_TYPE) :: r755)
  | 345 -> One (S (T T_RPAREN) :: r46)
  | 118 -> One (S (T T_RPAREN) :: r69)
  | 164 | 281 -> One (S (T T_RPAREN) :: r122)
  | 264 -> One (S (T T_RPAREN) :: r188)
  | 267 -> One (S (T T_RPAREN) :: r189)
  | 425 -> One (S (T T_RPAREN) :: r306)
  | 518 -> One (S (T T_RPAREN) :: r375)
  | 520 -> One (S (T T_RPAREN) :: r376)
  | 583 -> One (S (T T_RPAREN) :: r421)
  | 832 -> One (S (T T_RPAREN) :: r599)
  | 998 -> One (S (T T_RPAREN) :: r677)
  | 1005 -> One (S (T T_RPAREN) :: r678)
  | 1007 -> One (S (T T_RPAREN) :: r679)
  | 1077 -> One (S (T T_RPAREN) :: r734)
  | 1445 -> One (S (T T_RPAREN) :: r964)
  | 182 -> One (S (T T_RBRACKET) :: r145)
  | 233 -> One (S (T T_RBRACKET) :: r176)
  | 276 | 282 -> One (S (T T_RBRACKET) :: r193)
  | 346 -> One (S (T T_RBRACKET) :: r250)
  | 842 -> One (S (T T_RBRACKET) :: r603)
  | 224 -> One (S (T T_QUOTE) :: r174)
  | 339 -> One (S (T T_PLUSEQ) :: r239)
  | 1463 -> One (S (T T_PLUSEQ) :: r972)
  | 135 -> One (S (T T_MODULE) :: r89)
  | 303 -> One (S (T T_MINUSGREATER) :: r219)
  | 1203 -> One (S (T T_MINUSGREATER) :: r828)
  | 131 -> One (S (T T_LIDENT) :: r80)
  | 1189 -> One (S (T T_LIDENT) :: r820)
  | 1370 -> One (S (T T_LIDENT) :: r935)
  | 731 -> One (S (T T_LESSMINUS) :: r562)
  | 317 -> One (S (T T_LBRACE) :: r222)
  | 398 -> One (S (T T_INT) :: r273)
  | 401 -> One (S (T T_INT) :: r274)
  | 723 -> One (S (T T_IN) :: r558)
  | 727 -> One (S (T T_IN) :: r560)
  | 1353 -> One (S (T T_IN) :: r932)
  | 631 -> One (S (T T_GREATERRBRACE) :: r459)
  | 940 -> One (S (T T_GREATERRBRACE) :: r639)
  | 168 -> One (S (T T_GREATER) :: r127)
  | 172 -> One (S (T T_GREATER) :: r129)
  | 363 -> One (S (T T_EQUAL) :: r254)
  | 469 -> One (S (T T_EQUAL) :: r346)
  | 976 -> One (S (T T_EQUAL) :: r661)
  | 1305 -> One (S (T T_EQUAL) :: r888)
  | 1513 -> One (S (T T_EOF) :: r977)
  | 1517 -> One (S (T T_EOF) :: r978)
  | 1521 -> One (S (T T_EOF) :: r979)
  | 931 -> One (S (T T_END) :: r638)
  | 160 -> One (S (T T_DOTDOT) :: r112)
  | 322 -> One (S (T T_DOTDOT) :: r223)
  | 74 -> One (S (T T_DOT) :: r40)
  | 248 -> One (S (T T_DOT) :: r186)
  | 447 -> One (S (T T_DOT) :: r334)
  | 480 -> One (S (T T_DOT) :: r350)
  | 990 -> One (S (T T_DOT) :: r671)
  | 1150 -> One (S (T T_DOT) :: r790)
  | 1162 -> One (S (T T_DOT) :: r799)
  | 174 -> One (S (T T_COLON) :: r136)
  | 429 -> One (S (T T_COLON) :: r309)
  | 1078 -> One (S (T T_COLON) :: r738)
  | 538 -> One (S (T T_BARRBRACKET) :: r387)
  | 629 -> One (S (T T_BARRBRACKET) :: r453)
  | 834 -> One (S (T T_BARRBRACKET) :: r600)
  | 185 | 1201 -> One (S (T T_BAR) :: r150)
  | 235 -> One (S (T T_BAR) :: r179)
  | 383 -> One (S (N N_structure) :: r256)
  | 1393 -> One (S (N N_structure) :: r944)
  | 394 -> One (S (N N_pattern) :: r269)
  | 554 -> One (S (N N_pattern) :: r404)
  | 572 -> One (S (N N_pattern) :: r417)
  | 574 -> One (S (N N_pattern) :: r418)
  | 577 -> One (S (N N_pattern) :: r419)
  | 580 -> One (S (N N_pattern) :: r420)
  | 585 -> One (S (N N_pattern) :: r422)
  | 591 -> One (S (N N_pattern) :: r425)
  | 1042 -> One (S (N N_pattern) :: r691)
  | 421 -> One (S (N N_module_type) :: r299)
  | 422 -> One (S (N N_module_type) :: r301)
  | 477 -> One (S (N N_module_type) :: r348)
  | 953 -> One (S (N N_module_type) :: r649)
  | 1030 -> One (S (N N_module_type) :: r687)
  | 499 -> One (S (N N_module_expr) :: r355)
  | 512 -> One (S (N N_module_expr) :: r370)
  | 687 -> One (S (N N_module_expr) :: r537)
  | 654 -> One (S (N N_let_pattern) :: r504)
  | 634 -> One (S (N N_expr) :: r461)
  | 638 -> One (S (N N_expr) :: r473)
  | 703 -> One (S (N N_expr) :: r548)
  | 720 -> One (S (N N_expr) :: r556)
  | 734 -> One (S (N N_expr) :: r563)
  | 736 -> One (S (N N_expr) :: r564)
  | 738 -> One (S (N N_expr) :: r565)
  | 743 -> One (S (N N_expr) :: r566)
  | 745 -> One (S (N N_expr) :: r567)
  | 747 -> One (S (N N_expr) :: r568)
  | 749 -> One (S (N N_expr) :: r569)
  | 751 -> One (S (N N_expr) :: r570)
  | 753 -> One (S (N N_expr) :: r571)
  | 755 -> One (S (N N_expr) :: r572)
  | 757 -> One (S (N N_expr) :: r573)
  | 759 -> One (S (N N_expr) :: r574)
  | 761 -> One (S (N N_expr) :: r575)
  | 763 -> One (S (N N_expr) :: r576)
  | 765 -> One (S (N N_expr) :: r577)
  | 767 -> One (S (N N_expr) :: r578)
  | 769 -> One (S (N N_expr) :: r579)
  | 771 -> One (S (N N_expr) :: r580)
  | 773 -> One (S (N N_expr) :: r581)
  | 775 -> One (S (N N_expr) :: r582)
  | 777 -> One (S (N N_expr) :: r583)
  | 779 -> One (S (N N_expr) :: r584)
  | 782 -> One (S (N N_expr) :: r585)
  | 784 -> One (S (N N_expr) :: r586)
  | 807 -> One (S (N N_expr) :: r596)
  | 812 -> One (S (N N_expr) :: r597)
  | 815 -> One (S (N N_expr) :: r598)
  | 870 -> One (S (N N_expr) :: r620)
  | 928 -> One (S (N N_expr) :: r637)
  | 622 -> One (Sub (r1) :: r441)
  | 649 -> One (Sub (r1) :: r490)
  | 890 -> One (Sub (r1) :: r626)
  | 1044 -> One (Sub (r1) :: r692)
  | 1497 -> One (Sub (r1) :: r975)
  | 1499 -> One (Sub (r1) :: r976)
  | 2 -> One (Sub (r10) :: r12)
  | 55 -> One (Sub (r10) :: r13)
  | 58 -> One (Sub (r10) :: r18)
  | 90 -> One (Sub (r10) :: r49)
  | 333 -> One (Sub (r10) :: r229)
  | 1040 -> One (Sub (r10) :: r690)
  | 1046 -> One (Sub (r10) :: r695)
  | 70 -> One (Sub (r33) :: r34)
  | 637 -> One (Sub (r33) :: r471)
  | 678 -> One (Sub (r33) :: r529)
  | 699 -> One (Sub (r33) :: r540)
  | 712 -> One (Sub (r33) :: r554)
  | 714 -> One (Sub (r33) :: r555)
  | 125 -> One (Sub (r36) :: r71)
  | 192 -> One (Sub (r36) :: r153)
  | 269 -> One (Sub (r36) :: r190)
  | 593 -> One (Sub (r50) :: r426)
  | 218 -> One (Sub (r54) :: r171)
  | 301 -> One (Sub (r54) :: r217)
  | 904 -> One (Sub (r54) :: r633)
  | 1194 -> One (Sub (r56) :: r823)
  | 1198 -> One (Sub (r56) :: r826)
  | 134 -> One (Sub (r58) :: r83)
  | 167 -> One (Sub (r58) :: r126)
  | 222 -> One (Sub (r58) :: r172)
  | 228 -> One (Sub (r60) :: r175)
  | 272 -> One (Sub (r62) :: r191)
  | 551 -> One (Sub (r62) :: r403)
  | 588 -> One (Sub (r62) :: r424)
  | 614 -> One (Sub (r62) :: r436)
  | 656 -> One (Sub (r62) :: r505)
  | 792 -> One (Sub (r62) :: r592)
  | 862 -> One (Sub (r62) :: r616)
  | 866 -> One (Sub (r62) :: r619)
  | 1117 -> One (Sub (r62) :: r765)
  | 1454 -> One (Sub (r62) :: r966)
  | 150 -> One (Sub (r73) :: r110)
  | 249 -> One (Sub (r73) :: r187)
  | 372 -> One (Sub (r73) :: r255)
  | 410 -> One (Sub (r85) :: r278)
  | 527 -> One (Sub (r85) :: r378)
  | 1019 -> One (Sub (r85) :: r681)
  | 1022 -> One (Sub (r85) :: r683)
  | 1025 -> One (Sub (r85) :: r685)
  | 155 -> One (Sub (r105) :: r111)
  | 147 -> One (Sub (r107) :: r109)
  | 279 -> One (Sub (r114) :: r196)
  | 163 -> One (Sub (r120) :: r121)
  | 325 -> One (Sub (r120) :: r224)
  | 206 -> One (Sub (r139) :: r161)
  | 184 -> One (Sub (r141) :: r147)
  | 196 -> One (Sub (r157) :: r159)
  | 214 -> One (Sub (r169) :: r170)
  | 243 -> One (Sub (r182) :: r184)
  | 284 -> One (Sub (r198) :: r199)
  | 287 -> One (Sub (r200) :: r216)
  | 708 -> One (Sub (r200) :: r552)
  | 1297 -> One (Sub (r200) :: r886)
  | 1317 -> One (Sub (r200) :: r903)
  | 285 -> One (Sub (r208) :: r210)
  | 326 -> One (Sub (r208) :: r226)
  | 1087 -> One (Sub (r257) :: r740)
  | 396 -> One (Sub (r271) :: r272)
  | 960 -> One (Sub (r288) :: r650)
  | 473 -> One (Sub (r314) :: r347)
  | 433 -> One (Sub (r316) :: r317)
  | 442 -> One (Sub (r327) :: r332)
  | 435 -> One (Sub (r329) :: r331)
  | 1109 -> One (Sub (r329) :: r757)
  | 450 -> One (Sub (r336) :: r339)
  | 456 -> One (Sub (r343) :: r344)
  | 539 -> One (Sub (r389) :: r392)
  | 540 -> One (Sub (r399) :: r401)
  | 902 -> One (Sub (r407) :: r630)
  | 568 -> One (Sub (r415) :: r416)
  | 610 -> One (Sub (r430) :: r431)
  | 619 -> One (Sub (r430) :: r437)
  | 611 -> One (Sub (r433) :: r435)
  | 620 -> One (Sub (r433) :: r440)
  | 635 -> One (Sub (r468) :: r470)
  | 854 -> One (Sub (r468) :: r613)
  | 909 -> One (Sub (r495) :: r634)
  | 652 -> One (Sub (r500) :: r501)
  | 664 -> One (Sub (r507) :: r510)
  | 899 -> One (Sub (r507) :: r629)
  | 984 -> One (Sub (r507) :: r667)
  | 1324 -> One (Sub (r507) :: r911)
  | 705 -> One (Sub (r550) :: r551)
  | 710 -> One (Sub (r550) :: r553)
  | 847 -> One (Sub (r609) :: r611)
  | 922 -> One (Sub (r635) :: r636)
  | 980 -> One (Sub (r655) :: r662)
  | 1076 -> One (Sub (r730) :: r732)
  | 1323 -> One (Sub (r782) :: r906)
  | 1159 -> One (Sub (r794) :: r797)
  | 1345 -> One (Sub (r794) :: r924)
  | 1366 -> One (Sub (r809) :: r934)
  | 1383 -> One (Sub (r809) :: r942)
  | 1343 -> One (Sub (r859) :: r921)
  | 1387 -> One (Sub (r862) :: r943)
  | 1286 -> One (Sub (r877) :: r879)
  | 1308 -> One (Sub (r894) :: r896)
  | 786 -> One (r0)
  | 1512 -> One (r2)
  | 1511 -> One (r3)
  | 1510 -> One (r4)
  | 1509 -> One (r5)
  | 1508 -> One (r6)
  | 53 -> One (r7)
  | 54 -> One (r9)
  | 1507 -> One (r11)
  | 57 -> One (r12)
  | 56 -> One (r13)
  | 1435 -> One (r14)
  | 1506 -> One (r16)
  | 1505 -> One (r17)
  | 59 -> One (r18)
  | 1504 -> One (r19)
  | 1503 -> One (r20)
  | 1502 -> One (r21)
  | 1501 -> One (r22)
  | 61 -> One (r23)
  | 62 -> One (r24)
  | 1493 -> One (r25)
  | 64 -> One (r26)
  | 1492 -> One (r27)
  | 1491 -> One (r28)
  | 1490 -> One (r29)
  | 1489 -> One (r30)
  | 66 -> One (r31)
  | 69 -> One (r32)
  | 1488 -> One (r34)
  | 73 -> One (r35)
  | 78 -> One (r37)
  | 72 -> One (r38)
  | 77 -> One (r39)
  | 75 -> One (r40)
  | 76 -> One (r41)
  | 80 -> One (r42)
  | 82 -> One (r43)
  | 84 -> One (r44)
  | 83 | 94 | 404 | 508 | 557 | 663 | 898 -> One (r45)
  | 87 -> One (r46)
  | 1487 -> One (r47)
  | 1486 -> One (r48)
  | 91 -> One (r49)
  | 123 -> One (r51)
  | 191 -> One (r53)
  | 213 -> One (r55)
  | 212 -> One (r57)
  | 221 -> One (r59)
  | 266 -> One (r61)
  | 1485 -> One (r63)
  | 1484 -> One (r64)
  | 122 -> One (r65)
  | 121 -> One (r66)
  | 93 -> One (r67)
  | 102 -> One (r68)
  | 120 -> One (r69)
  | 124 | 138 -> One (r70)
  | 126 -> One (r71)
  | 129 -> One (r72)
  | 130 -> One (r74)
  | 127 -> One (r75)
  | 1483 -> One (r76)
  | 1482 -> One (r77)
  | 1481 -> One (r78)
  | 133 -> One (r79)
  | 132 -> One (r80)
  | 1480 -> One (r81)
  | 1479 -> One (r82)
  | 1478 -> One (r83)
  | 530 -> One (r84)
  | 1477 -> One (r86)
  | 1476 -> One (r87)
  | 137 -> One (r88)
  | 136 -> One (r89)
  | 1475 -> One (r90)
  | 1474 -> One (r91)
  | 140 -> One (r92)
  | 1462 -> One (r93)
  | 332 -> One (r94)
  | 331 -> One (r95)
  | 159 -> One (r96)
  | 158 | 338 -> One (r97)
  | 144 | 337 -> One (r98)
  | 142 | 336 -> One (r99)
  | 141 | 335 -> One (r100)
  | 149 -> One (r101)
  | 152 -> One (r103)
  | 148 -> One (r104)
  | 157 -> One (r106)
  | 154 -> One (r108)
  | 153 -> One (r109)
  | 151 -> One (r110)
  | 156 -> One (r111)
  | 316 -> One (r112)
  | 278 -> One (r113)
  | 314 -> One (r115)
  | 313 -> One (r116)
  | 312 -> One (r117)
  | 311 -> One (r119)
  | 309 -> One (r121)
  | 165 -> One (r122)
  | 166 | 181 | 1197 -> One (r123)
  | 263 -> One (r124)
  | 262 -> One (r125)
  | 261 -> One (r126)
  | 170 -> One (r127)
  | 169 | 446 -> One (r128)
  | 173 -> One (r129)
  | 257 -> One (r130)
  | 256 -> One (r132)
  | 255 -> One (r133)
  | 254 -> One (r134)
  | 253 -> One (r135)
  | 175 -> One (r136)
  | 203 | 1202 -> One (r138)
  | 232 -> One (r140)
  | 242 -> One (r142)
  | 241 -> One (r143)
  | 180 -> One (r144)
  | 183 -> One (r145)
  | 240 -> One (r146)
  | 239 -> One (r147)
  | 205 -> One (r148)
  | 204 -> One (r149)
  | 186 -> One (r150)
  | 188 -> One (r151)
  | 187 -> One (r152)
  | 193 -> One (r153)
  | 202 | 1207 -> One (r154)
  | 201 | 1206 -> One (r155)
  | 195 | 1205 -> One (r156)
  | 198 -> One (r158)
  | 197 -> One (r159)
  | 200 -> One (r160)
  | 207 -> One (r161)
  | 231 -> One (r162)
  | 220 -> One (r163)
  | 230 -> One (r165)
  | 227 -> One (r166)
  | 211 -> One (r167)
  | 215 -> One (r168)
  | 217 -> One (r170)
  | 219 -> One (r171)
  | 223 -> One (r172)
  | 226 -> One (r173)
  | 225 -> One (r174)
  | 229 -> One (r175)
  | 234 -> One (r176)
  | 238 -> One (r177)
  | 237 -> One (r178)
  | 236 -> One (r179)
  | 247 -> One (r181)
  | 245 -> One (r183)
  | 244 -> One (r184)
  | 252 -> One (r185)
  | 251 -> One (r186)
  | 250 -> One (r187)
  | 265 -> One (r188)
  | 268 -> One (r189)
  | 270 -> One (r190)
  | 273 -> One (r191)
  | 275 -> One (r192)
  | 277 -> One (r193)
  | 307 -> One (r194)
  | 306 -> One (r195)
  | 283 -> One (r196)
  | 299 -> One (r197)
  | 300 -> One (r199)
  | 298 -> One (r207)
  | 295 -> One (r209)
  | 294 -> One (r210)
  | 293 -> One (r211)
  | 292 -> One (r212)
  | 291 -> One (r213)
  | 290 -> One (r214)
  | 289 -> One (r215)
  | 288 -> One (r216)
  | 302 -> One (r217)
  | 305 -> One (r218)
  | 304 -> One (r219)
  | 320 -> One (r220)
  | 319 -> One (r221)
  | 318 -> One (r222)
  | 324 -> One (r223)
  | 329 -> One (r224)
  | 328 -> One (r225)
  | 327 -> One (r226)
  | 1461 -> One (r227)
  | 1460 -> One (r228)
  | 334 -> One (r229)
  | 368 -> One (r230)
  | 367 -> One (r231)
  | 1472 -> One (r232)
  | 362 -> One (r234)
  | 356 -> One (r236)
  | 355 -> One (r237)
  | 341 -> One (r238)
  | 340 -> One (r239)
  | 354 -> One (r240)
  | 353 -> One (r241)
  | 1467 -> One (r242)
  | 1466 -> One (r243)
  | 348 -> One (r244)
  | 352 -> One (r246)
  | 351 -> One (r247)
  | 344 -> One (r248)
  | 347 -> One (r250)
  | 358 -> One (r251)
  | 366 -> One (r252)
  | 365 -> One (r253)
  | 364 -> One (r254)
  | 373 -> One (r255)
  | 1459 -> One (r256)
  | 391 -> One (r258)
  | 390 -> One (r259)
  | 389 -> One (r260)
  | 388 -> One (r261)
  | 1285 -> One (r262)
  | 1284 -> One (r263)
  | 1458 -> One (r265)
  | 1457 -> One (r266)
  | 393 -> One (r267)
  | 1453 -> One (r268)
  | 1452 -> One (r269)
  | 395 -> One (r270)
  | 397 -> One (r272)
  | 399 -> One (r273)
  | 402 -> One (r274)
  | 409 -> One (r275)
  | 408 -> One (r276)
  | 1444 -> One (r277)
  | 1443 -> One (r278)
  | 1442 -> One (r279)
  | 414 -> One (r280)
  | 413 -> One (r281)
  | 412 -> One (r282)
  | 1441 -> One (r283)
  | 1440 -> One (r284)
  | 416 -> One (r285)
  | 1033 -> One (r286)
  | 498 -> One (r287)
  | 1039 -> One (r289)
  | 1038 -> One (r290)
  | 1037 -> One (r291)
  | 1036 -> One (r292)
  | 495 -> One (r294)
  | 494 -> One (r295)
  | 420 -> One (r296)
  | 419 -> One (r297)
  | 418 -> One (r298)
  | 493 -> One (r299)
  | 492 -> One (r300)
  | 491 -> One (r301)
  | 488 -> One (r302)
  | 487 -> One (r303)
  | 486 -> One (r304)
  | 424 -> One (r305)
  | 428 -> One (r306)
  | 476 -> One (r307)
  | 432 -> One (r308)
  | 430 -> One (r309)
  | 468 -> One (r310)
  | 467 -> One (r311)
  | 466 -> One (r312)
  | 465 -> One (r313)
  | 475 -> One (r315)
  | 472 -> One (r317)
  | 463 -> One (r318)
  | 462 -> One (r319)
  | 461 -> One (r320)
  | 445 -> One (r321)
  | 438 -> One (r322)
  | 437 -> One (r323)
  | 439 -> One (r325)
  | 436 -> One (r326)
  | 444 -> One (r328)
  | 441 -> One (r330)
  | 440 -> One (r331)
  | 443 -> One (r332)
  | 449 -> One (r333)
  | 448 -> One (r334)
  | 451 -> One (r335)
  | 455 -> One (r337)
  | 454 -> One (r338)
  | 453 -> One (r339)
  | 459 -> One (r340)
  | 458 -> One (r341)
  | 457 -> One (r342)
  | 460 -> One (r344)
  | 471 -> One (r345)
  | 470 -> One (r346)
  | 474 -> One (r347)
  | 478 -> One (r348)
  | 483 -> One (r349)
  | 481 -> One (r350)
  | 1035 -> One (r351)
  | 1034 -> One (r352)
  | 497 -> One (r353)
  | 1029 -> One (r354)
  | 1028 -> One (r355)
  | 1018 -> One (r356)
  | 1017 -> One (r357)
  | 501 -> One (r358)
  | 1016 -> One (r359)
  | 1015 -> One (r360)
  | 1014 -> One (r361)
  | 1013 -> One (r362)
  | 505 -> One (r363)
  | 1012 -> One (r364)
  | 1011 -> One (r365)
  | 1010 -> One (r366)
  | 1009 -> One (r367)
  | 507 -> One (r368)
  | 526 -> One (r369)
  | 525 -> One (r370)
  | 517 -> One (r371)
  | 516 -> One (r372)
  | 515 -> One (r373)
  | 514 -> One (r374)
  | 519 -> One (r375)
  | 521 -> One (r376)
  | 529 -> One (r377)
  | 528 -> One (r378)
  | 798 -> One (r379)
  | 797 -> One (r380)
  | 796 -> One (r381)
  | 997 -> One (r383)
  | 996 -> One (r384)
  | 537 -> One (r385)
  | 535 -> One (r386)
  | 605 -> One (r387)
  | 604 -> One (r388)
  | 603 -> One (r390)
  | 602 -> One (r391)
  | 599 -> One (r392)
  | 541 -> One (r393)
  | 553 -> One (r395)
  | 550 -> One (r396)
  | 544 -> One (r398)
  | 543 -> One (r400)
  | 542 -> One (r401)
  | 548 -> One (r402)
  | 552 -> One (r403)
  | 598 -> One (r404)
  | 565 | 791 -> One (r406)
  | 595 -> One (r408)
  | 556 -> One (r409)
  | 559 -> One (r410)
  | 582 -> One (r411)
  | 562 -> One (r412)
  | 570 -> One (r414)
  | 569 -> One (r416)
  | 573 -> One (r417)
  | 575 -> One (r418)
  | 578 -> One (r419)
  | 581 -> One (r420)
  | 584 -> One (r421)
  | 587 -> One (r422)
  | 590 -> One (r423)
  | 589 -> One (r424)
  | 592 -> One (r425)
  | 594 -> One (r426)
  | 608 -> One (r427)
  | 607 -> One (r428)
  | 612 -> One (r429)
  | 618 -> One (r431)
  | 613 -> One (r432)
  | 617 -> One (r434)
  | 616 -> One (r435)
  | 615 -> One (r436)
  | 969 -> One (r437)
  | 968 -> One (r438)
  | 967 -> One (r439)
  | 621 -> One (r440)
  | 966 -> One (r441)
  | 965 -> One (r442)
  | 964 -> One (r443)
  | 963 -> One (r444)
  | 962 -> One (r445)
  | 1344 -> One (r446)
  | 949 -> One (r447)
  | 628 -> One (r448)
  | 627 -> One (r449)
  | 626 -> One (r450)
  | 625 -> One (r451)
  | 624 -> One (r452)
  | 945 -> One (r453)
  | 841 -> One (r454)
  | 944 -> One (r456)
  | 943 -> One (r457)
  | 942 -> One (r458)
  | 632 -> One (r459)
  | 633 -> One (r460)
  | 939 -> One (r461)
  | 869 -> One (r462)
  | 861 -> One (r463)
  | 858 -> One (r465)
  | 877 -> One (r467)
  | 938 -> One (r469)
  | 937 -> One (r470)
  | 936 -> One (r471)
  | 935 -> One (r472)
  | 934 -> One (r473)
  | 933 -> One (r474)
  | 640 -> One (r475)
  | 930 -> One (r476)
  | 642 -> One (r477)
  | 927 -> One (r478)
  | 926 -> One (r479)
  | 925 -> One (r480)
  | 644 -> One (r481)
  | 921 -> One (r482)
  | 647 -> One (r483)
  | 646 -> One (r484)
  | 919 -> One (r485)
  | 918 -> One (r486)
  | 648 -> One (r487)
  | 917 -> One (r488)
  | 916 -> One (r489)
  | 915 -> One (r490)
  | 908 -> One (r491)
  | 897 -> One (r493)
  | 669 -> One (r494)
  | 914 -> One (r496)
  | 913 -> One (r497)
  | 651 -> One (r498)
  | 653 -> One (r499)
  | 662 -> One (r501)
  | 660 -> One (r502)
  | 659 -> One (r503)
  | 658 -> One (r504)
  | 657 -> One (r505)
  | 665 -> One (r506)
  | 912 -> One (r508)
  | 668 -> One (r509)
  | 667 -> One (r510)
  | 889 -> One (r511)
  | 888 -> One (r512)
  | 887 -> One (r513)
  | 886 -> One (r514)
  | 673 -> One (r515)
  | 672 -> One (r516)
  | 671 -> One (r517)
  | 880 -> One (r518)
  | 885 -> One (r520)
  | 884 -> One (r521)
  | 883 -> One (r522)
  | 882 -> One (r523)
  | 881 -> One (r524)
  | 878 -> One (r525)
  | 677 -> One (r526)
  | 676 -> One (r527)
  | 675 -> One (r528)
  | 680 -> One (r529)
  | 685 -> One (r530)
  | 684 -> One (r531)
  | 683 | 875 -> One (r532)
  | 874 -> One (r533)
  | 691 -> One (r534)
  | 690 -> One (r535)
  | 689 -> One (r536)
  | 688 -> One (r537)
  | 698 -> One (r538)
  | 697 -> One (r539)
  | 700 -> One (r540)
  | 811 | 830 -> One (r541)
  | 810 | 829 -> One (r542)
  | 809 | 828 -> One (r543)
  | 701 | 716 -> One (r544)
  | 719 | 824 -> One (r545)
  | 718 | 823 -> One (r546)
  | 702 | 717 -> One (r547)
  | 822 -> One (r548)
  | 706 -> One (r549)
  | 707 -> One (r551)
  | 709 -> One (r552)
  | 711 -> One (r553)
  | 713 -> One (r554)
  | 715 -> One (r555)
  | 803 -> One (r556)
  | 725 -> One (r557)
  | 724 -> One (r558)
  | 729 -> One (r559)
  | 728 -> One (r560)
  | 781 -> One (r561)
  | 732 -> One (r562)
  | 735 -> One (r563)
  | 737 -> One (r564)
  | 739 -> One (r565)
  | 744 -> One (r566)
  | 746 -> One (r567)
  | 748 -> One (r568)
  | 750 -> One (r569)
  | 752 -> One (r570)
  | 754 -> One (r571)
  | 756 -> One (r572)
  | 758 -> One (r573)
  | 760 -> One (r574)
  | 762 -> One (r575)
  | 764 -> One (r576)
  | 766 -> One (r577)
  | 768 -> One (r578)
  | 770 -> One (r579)
  | 772 -> One (r580)
  | 774 -> One (r581)
  | 776 -> One (r582)
  | 778 -> One (r583)
  | 780 -> One (r584)
  | 783 -> One (r585)
  | 785 -> One (r586)
  | 800 -> One (r587)
  | 799 -> One (r588)
  | 790 -> One (r589)
  | 795 -> One (r590)
  | 794 -> One (r591)
  | 793 -> One (r592)
  | 806 | 827 -> One (r593)
  | 805 | 826 -> One (r594)
  | 804 | 825 -> One (r595)
  | 808 -> One (r596)
  | 813 -> One (r597)
  | 816 -> One (r598)
  | 833 -> One (r599)
  | 835 -> One (r600)
  | 840 -> One (r601)
  | 839 -> One (r602)
  | 843 -> One (r603)
  | 846 -> One (r604)
  | 845 -> One (r605)
  | 853 -> One (r607)
  | 850 -> One (r608)
  | 849 -> One (r610)
  | 848 -> One (r611)
  | 873 -> One (r612)
  | 872 -> One (r613)
  | 857 -> One (r614)
  | 856 -> One (r615)
  | 863 -> One (r616)
  | 865 -> One (r617)
  | 864 | 983 -> One (r618)
  | 867 -> One (r619)
  | 871 -> One (r620)
  | 896 -> One (r621)
  | 895 -> One (r622)
  | 894 -> One (r623)
  | 893 -> One (r624)
  | 892 -> One (r625)
  | 891 -> One (r626)
  | 911 -> One (r627)
  | 901 -> One (r628)
  | 900 -> One (r629)
  | 903 -> One (r630)
  | 907 -> One (r631)
  | 906 -> One (r632)
  | 905 -> One (r633)
  | 910 -> One (r634)
  | 923 -> One (r636)
  | 929 -> One (r637)
  | 932 -> One (r638)
  | 941 -> One (r639)
  | 948 -> One (r640)
  | 947 -> One (r641)
  | 959 -> One (r642)
  | 958 -> One (r643)
  | 957 -> One (r644)
  | 952 -> One (r645)
  | 951 -> One (r646)
  | 956 -> One (r647)
  | 955 -> One (r648)
  | 954 -> One (r649)
  | 961 -> One (r650)
  | 975 -> One (r651)
  | 974 -> One (r652)
  | 979 -> One (r654)
  | 982 -> One (r656)
  | 973 -> One (r657)
  | 972 -> One (r658)
  | 971 -> One (r659)
  | 978 -> One (r660)
  | 977 -> One (r661)
  | 981 -> One (r662)
  | 989 -> One (r663)
  | 988 -> One (r664)
  | 987 -> One (r665)
  | 986 -> One (r666)
  | 985 -> One (r667)
  | 994 -> One (r668)
  | 993 -> One (r669)
  | 992 -> One (r670)
  | 991 -> One (r671)
  | 1004 -> One (r672)
  | 1003 -> One (r673)
  | 1002 -> One (r674)
  | 1001 -> One (r675)
  | 1000 -> One (r676)
  | 999 -> One (r677)
  | 1006 -> One (r678)
  | 1008 -> One (r679)
  | 1021 -> One (r680)
  | 1020 -> One (r681)
  | 1024 -> One (r682)
  | 1023 -> One (r683)
  | 1027 -> One (r684)
  | 1026 -> One (r685)
  | 1032 -> One (r686)
  | 1031 -> One (r687)
  | 1439 -> One (r688)
  | 1438 -> One (r689)
  | 1041 -> One (r690)
  | 1043 -> One (r691)
  | 1045 -> One (r692)
  | 1437 -> One (r693)
  | 1436 -> One (r694)
  | 1047 -> One (r695)
  | 1051 -> One (r696)
  | 1050 -> One (r697)
  | 1049 -> One (r698)
  | 1058 -> One (r699)
  | 1061 -> One (r701)
  | 1060 -> One (r702)
  | 1057 -> One (r703)
  | 1056 -> One (r704)
  | 1055 -> One (r705)
  | 1054 -> One (r706)
  | 1053 -> One (r707)
  | 1068 -> One (r708)
  | 1067 -> One (r709)
  | 1066 -> One (r710)
  | 1065 -> One (r711)
  | 1071 -> One (r714)
  | 1070 -> One (r715)
  | 1069 -> One (r716)
  | 1103 -> One (r717)
  | 1102 -> One (r718)
  | 1101 -> One (r719)
  | 1272 -> One (r720)
  | 1271 -> One (r721)
  | 1096 -> One (r722)
  | 1095 -> One (r723)
  | 1094 -> One (r724)
  | 1093 -> One (r725)
  | 1092 -> One (r726)
  | 1075 -> One (r727)
  | 1083 -> One (r728)
  | 1082 -> One (r729)
  | 1091 -> One (r731)
  | 1090 -> One (r732)
  | 1086 -> One (r733)
  | 1085 -> One (r734)
  | 1084 -> One (r735)
  | 1081 -> One (r736)
  | 1080 -> One (r737)
  | 1079 -> One (r738)
  | 1089 -> One (r739)
  | 1088 -> One (r740)
  | 1100 -> One (r741)
  | 1099 -> One (r742)
  | 1098 -> One (r743)
  | 1158 -> One (r744)
  | 1167 -> One (r746)
  | 1183 -> One (r748)
  | 1182 -> One (r749)
  | 1114 -> One (r750)
  | 1113 -> One (r751)
  | 1112 -> One (r752)
  | 1108 -> One (r753)
  | 1106 -> One (r754)
  | 1105 -> One (r755)
  | 1111 -> One (r756)
  | 1110 -> One (r757)
  | 1123 -> One (r758)
  | 1122 -> One (r759)
  | 1121 -> One (r761)
  | 1120 -> One (r762)
  | 1116 -> One (r763)
  | 1119 -> One (r764)
  | 1118 -> One (r765)
  | 1140 -> One (r766)
  | 1139 -> One (r767)
  | 1138 -> One (r768)
  | 1137 -> One (r770)
  | 1136 -> One (r771)
  | 1125 -> One (r772)
  | 1130 -> One (r773)
  | 1129 -> One (r774)
  | 1128 -> One (r775)
  | 1127 -> One (r776)
  | 1135 -> One (r777)
  | 1134 -> One (r778)
  | 1133 -> One (r779)
  | 1132 -> One (r780)
  | 1155 -> One (r781)
  | 1154 -> One (r783)
  | 1153 -> One (r784)
  | 1149 -> One (r785)
  | 1148 -> One (r786)
  | 1147 -> One (r787)
  | 1142 -> One (r788)
  | 1152 -> One (r789)
  | 1151 -> One (r790)
  | 1169 -> One (r791)
  | 1168 -> One (r792)
  | 1157 -> One (r793)
  | 1165 -> One (r795)
  | 1161 -> One (r796)
  | 1160 -> One (r797)
  | 1164 -> One (r798)
  | 1163 -> One (r799)
  | 1175 -> One (r800)
  | 1174 -> One (r801)
  | 1173 -> One (r802)
  | 1177 -> One (r804)
  | 1176 -> One (r805)
  | 1172 -> One (r806)
  | 1179 -> One (r807)
  | 1210 -> One (r808)
  | 1215 -> One (r810)
  | 1214 -> One (r811)
  | 1188 -> One (r812)
  | 1187 -> One (r813)
  | 1186 -> One (r814)
  | 1185 -> One (r815)
  | 1213 -> One (r816)
  | 1193 -> One (r817)
  | 1192 -> One (r818)
  | 1191 -> One (r819)
  | 1190 -> One (r820)
  | 1212 -> One (r821)
  | 1196 -> One (r822)
  | 1195 -> One (r823)
  | 1211 -> One (r824)
  | 1200 -> One (r825)
  | 1199 -> One (r826)
  | 1209 -> One (r827)
  | 1204 -> One (r828)
  | 1224 -> One (r829)
  | 1223 -> One (r830)
  | 1222 -> One (r831)
  | 1221 -> One (r832)
  | 1220 -> One (r833)
  | 1219 -> One (r834)
  | 1228 -> One (r835)
  | 1238 -> One (r836)
  | 1237 -> One (r837)
  | 1236 -> One (r838)
  | 1235 -> One (r839)
  | 1234 -> One (r840)
  | 1247 -> One (r841)
  | 1257 -> One (r842)
  | 1256 -> One (r843)
  | 1255 -> One (r844)
  | 1254 -> One (r845)
  | 1253 -> One (r846)
  | 1252 -> One (r847)
  | 1251 -> One (r848)
  | 1268 -> One (r849)
  | 1267 -> One (r850)
  | 1266 -> One (r851)
  | 1265 -> One (r852)
  | 1264 -> One (r853)
  | 1263 -> One (r854)
  | 1262 -> One (r855)
  | 1358 -> One (r856)
  | 1356 -> One (r858)
  | 1382 -> One (r860)
  | 1279 -> One (r861)
  | 1390 -> One (r863)
  | 1389 -> One (r864)
  | 1278 -> One (r865)
  | 1277 -> One (r866)
  | 1276 -> One (r867)
  | 1283 -> One (r868)
  | 1282 -> One (r869)
  | 1281 -> One (r870)
  | 1304 -> One (r871)
  | 1303 -> One (r872)
  | 1302 -> One (r873)
  | 1301 -> One (r874)
  | 1290 -> One (r875)
  | 1289 -> One (r876)
  | 1288 -> One (r878)
  | 1287 -> One (r879)
  | 1295 -> One (r880)
  | 1294 -> One (r881)
  | 1293 -> One (r882)
  | 1292 -> One (r883)
  | 1300 -> One (r884)
  | 1299 -> One (r885)
  | 1298 -> One (r886)
  | 1307 -> One (r887)
  | 1306 -> One (r888)
  | 1333 -> One (r889)
  | 1322 -> One (r890)
  | 1321 -> One (r891)
  | 1310 -> One (r892)
  | 1309 -> One (r893)
  | 1335 -> One (r895)
  | 1334 -> One (r896)
  | 1315 -> One (r897)
  | 1314 -> One (r898)
  | 1313 -> One (r899)
  | 1312 -> One (r900)
  | 1320 -> One (r901)
  | 1319 -> One (r902)
  | 1318 -> One (r903)
  | 1332 -> One (r904)
  | 1331 -> One (r905)
  | 1330 -> One (r906)
  | 1329 -> One (r907)
  | 1328 -> One (r908)
  | 1327 -> One (r909)
  | 1326 -> One (r910)
  | 1325 -> One (r911)
  | 1339 -> One (r912)
  | 1338 -> One (r913)
  | 1337 -> One (r914)
  | 1373 -> One (r915)
  | 1372 -> One (r916)
  | 1369 -> One (r917)
  | 1342 -> One (r918)
  | 1341 -> One (r919)
  | 1365 -> One (r920)
  | 1364 -> One (r921)
  | 1348 -> One (r922)
  | 1347 -> One (r923)
  | 1346 -> One (r924)
  | 1361 -> One (r925)
  | 1352 -> One (r926)
  | 1351 -> One (r927)
  | 1363 -> One (r929)
  | 1350 -> One (r930)
  | 1359 -> One (r931)
  | 1354 -> One (r932)
  | 1368 -> One (r933)
  | 1367 -> One (r934)
  | 1371 -> One (r935)
  | 1377 -> One (r936)
  | 1376 -> One (r937)
  | 1375 -> One (r938)
  | 1379 -> One (r939)
  | 1386 -> One (r940)
  | 1385 -> One (r941)
  | 1384 -> One (r942)
  | 1388 -> One (r943)
  | 1429 -> One (r944)
  | 1396 -> One (r945)
  | 1406 -> One (r946)
  | 1405 -> One (r947)
  | 1404 -> One (r948)
  | 1403 -> One (r949)
  | 1416 -> One (r950)
  | 1426 -> One (r951)
  | 1425 -> One (r952)
  | 1424 -> One (r953)
  | 1423 -> One (r954)
  | 1422 -> One (r955)
  | 1421 -> One (r956)
  | 1432 -> One (r957)
  | 1431 -> One (r958)
  | 1451 -> One (r959)
  | 1450 -> One (r960)
  | 1449 -> One (r961)
  | 1448 -> One (r962)
  | 1447 -> One (r963)
  | 1446 -> One (r964)
  | 1456 -> One (r965)
  | 1455 -> One (r966)
  | 1471 -> One (r967)
  | 1469 -> One (r969)
  | 1468 -> One (r970)
  | 1465 -> One (r971)
  | 1464 -> One (r972)
  | 1496 -> One (r973)
  | 1495 -> One (r974)
  | 1498 -> One (r975)
  | 1500 -> One (r976)
  | 1514 -> One (r977)
  | 1518 -> One (r978)
  | 1522 -> One (r979)
  | 682 -> Select (function
    | -1 | -1 -> [R 97]
    | _ -> r533)
  | 417 -> Select (function
    | -1 -> S (T T_TYPE) :: r298
    | _ -> R 190 :: r293)
  | 1062 -> Select (function
    | -1 -> r719
    | _ -> R 190 :: r713)
  | 533 -> Select (function
    | -1 | -1 -> [R 681]
    | _ -> r128)
  | 496 -> Select (function
    | -1 -> S (T T_UIDENT) :: r353
    | _ -> r293)
  | 630 -> Select (function
    | -1 | -1 -> S (T T_RBRACKET) :: r250
    | _ -> Sub (r455) :: r458)
  | 623 -> Select (function
    | -1 | -1 | 59 | 91 | 334 | 383 | 416 | 1041 | 1047 | 1393 -> r446
    | _ -> S (T T_OPEN) :: r452)
  | 176 -> Select (function
    | 983 -> r75
    | _ -> Sub (r73) :: r137)
  | 342 -> Select (function
    | 355 -> r243
    | _ -> Sub (r114) :: r249)
  | 177 -> Select (function
    | 983 -> r74
    | _ -> r137)
  | 343 -> Select (function
    | 341 -> r249
    | _ -> r242)
  | 1064 -> Select (function
    | -1 -> r717
    | _ -> r712)
  | 1063 -> Select (function
    | -1 -> r718
    | _ -> r713)
  | _ -> raise Not_found
