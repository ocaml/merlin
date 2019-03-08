open Parser_raw

module Default = struct

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
    | MenhirInterpreter.T MenhirInterpreter.T_LETOP -> raise Not_found
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
    | MenhirInterpreter.T MenhirInterpreter.T_HASHOP -> ""
    | MenhirInterpreter.T MenhirInterpreter.T_HASH -> ()
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
    | MenhirInterpreter.T MenhirInterpreter.T_ANDOP -> raise Not_found
    | MenhirInterpreter.T MenhirInterpreter.T_AND -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_AMPERSAND -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_AMPERAMPER -> ()
    | MenhirInterpreter.N MenhirInterpreter.N_with_type_binder -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_with_constraint -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_virtual_with_private_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_virtual_with_mutable_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_virtual_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_value_description -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_value -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_val_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_val_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_use_file -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_variance -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_variable -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_parameters -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_parameter -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_kind -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_type_constraint -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_tuple_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_toplevel_phrase -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_toplevel_directive -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_tag_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_subtractive -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_structure_item -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_structure -> []
    | MenhirInterpreter.N MenhirInterpreter.N_strict_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_str_exception_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_single_attr_id -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_pattern_not_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_simple_delimited_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_signed_constant -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_signature_item -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_signature -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_sig_exception_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_seq_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_record_expr_field_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_pattern_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_object_expr_field_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_separated_or_terminated_nonempty_list_SEMI_expr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_row_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nontrivial_llist_STAR_atomic_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nontrivial_llist_COMMA_expr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nontrivial_llist_COMMA_core_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_STAR_atomic_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_COMMA_type_parameter_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_COMMA_core_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_BAR_row_field_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_AND_with_constraint_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_typevar_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_name_tag_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_labeled_simple_expr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_llist_functor_arg_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_llist_preceded_CONSTRAINT_constrain__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_bar_llist_extension_constructor_declaration_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_bar_llist_extension_constructor_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_bar_llist_constructor_declaration_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_record_expr_content -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_rec_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_private_virtual_flags -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_private_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_primitive_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_post_item_attribute -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_possibly_poly_core_type_no_attr_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_possibly_poly_core_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_payload -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_var -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_no_exn -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_gen -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_comma_list_pattern_no_exn_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern_comma_list_pattern_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_pattern -> default_pattern ()
    | MenhirInterpreter.N MenhirInterpreter.N_parse_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_expression -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_core_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_paren_module_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_optlabel -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_type_constraint_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_seq_expr__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_pattern__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_module_type__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_expr__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_COLON_core_type__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_AS_mkrhs_LIDENT___ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_SEMI_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_BAR_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_ampersand -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_operator -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_open_description -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_open_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_nonempty_type_kind -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_nonempty_list_raw_string_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_nonempty_list_mkrhs_LIDENT__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_name_tag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mutable_virtual_flags -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mutable_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mty_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_type_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_type -> default_module_type ()
    | MenhirInterpreter.N MenhirInterpreter.N_module_subst -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_expr -> default_module_expr ()
    | MenhirInterpreter.N MenhirInterpreter.N_module_declaration_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_binding_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mod_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mod_ext_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_method_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_meth_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_match_cases -> []
    | MenhirInterpreter.N MenhirInterpreter.N_match_case -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lwt_bindings -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_lwt_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_listx_SEMI_record_pat_field_UNDERSCORE_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_use_file_element_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_text_str_structure_item__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_text_cstr_class_field__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_text_csig_class_sig_field__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_structure_element_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_signature_element_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_post_item_attribute_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_generic_and_type_declaration_type_subst_kind__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_generic_and_type_declaration_type_kind__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_attribute_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_and_module_declaration_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_and_module_binding_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_and_class_type_declaration_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_and_class_description_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_list_and_class_declaration_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_letop_bindings -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_letop_binding_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_pattern -> default_pattern ()
    | MenhirInterpreter.N MenhirInterpreter.N_let_bindings_no_ext_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_bindings_ext_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_binding_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_labeled_simple_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_labeled_simple_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_let_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_declaration_semi -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_item_extension -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_interface -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_implementation -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generic_type_declaration_nonrec_flag_type_kind_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generic_type_declaration_no_nonrec_flag_type_subst_kind_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generic_constructor_declaration_epsilon_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generic_constructor_declaration_BAR_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generalized_constructor_arguments -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_functor_args -> []
    | MenhirInterpreter.N MenhirInterpreter.N_functor_arg_name -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_functor_arg -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_function_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_fun_def -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_fun_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_formal_class_parameters -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_floating_attribute -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_rebind_epsilon_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_rebind_BAR_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_ext -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_expr -> default_expr ()
    | MenhirInterpreter.N MenhirInterpreter.N_direction_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_core_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constructor_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constructor_arguments -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constrain_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constr_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constr_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constant -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_clty_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_type_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_simple_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_signature -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_sig_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_self_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_self_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_fun_def -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_fun_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_class_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_attribute -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_attr_id -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_atomic_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_and_let_binding -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_alias_type -> raise Not_found
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
  [|0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;2;2;1;2;1;2;3;1;1;1;2;3;1;2;3;1;1;1;1;1;2;3;1;1;2;3;3;4;1;2;1;2;1;2;3;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;2;2;1;2;1;2;3;1;1;1;1;1;1;1;1;2;3;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;4;2;3;4;2;3;4;1;1;1;1;1;1;2;3;3;1;4;5;1;1;1;1;1;2;1;2;3;1;1;2;3;4;1;1;2;1;2;3;4;5;1;1;1;1;2;1;2;3;4;1;2;3;4;1;2;1;2;3;1;1;1;2;3;1;2;3;1;2;1;2;3;1;4;1;1;2;2;1;2;2;1;1;1;1;1;2;1;2;2;2;3;4;5;6;6;1;1;2;1;2;3;1;4;1;1;1;1;1;2;2;1;2;3;2;3;5;6;1;1;1;1;1;2;1;2;1;1;1;2;1;2;1;2;1;2;3;4;5;6;1;1;1;1;1;2;1;1;2;1;2;2;1;1;2;2;1;2;1;1;2;1;2;1;2;3;3;4;2;3;2;3;1;3;2;3;2;1;2;3;4;1;2;3;3;1;1;3;4;2;3;1;2;1;3;4;2;1;3;2;3;4;5;1;2;1;2;1;2;3;2;3;4;5;3;4;3;4;4;5;6;2;1;2;3;2;3;4;5;4;2;3;2;3;2;3;1;2;2;1;1;2;3;1;2;3;2;3;4;5;6;7;1;3;4;1;2;1;1;2;1;1;1;1;2;1;1;2;3;1;2;3;2;1;1;2;3;4;2;3;4;1;1;2;1;1;2;2;1;2;3;1;2;3;1;2;1;2;3;4;5;6;4;4;3;4;5;3;3;1;7;8;1;2;1;1;2;3;4;1;2;3;1;1;2;3;1;2;3;1;1;1;1;2;2;1;2;2;2;3;3;4;5;6;6;1;2;3;4;1;2;1;2;3;4;5;6;7;8;1;2;1;1;2;1;1;2;1;2;3;4;5;1;1;1;1;2;1;1;1;2;2;3;1;2;1;2;3;4;1;5;2;1;2;1;1;2;3;3;1;2;4;5;4;5;6;2;3;4;5;1;1;2;3;4;2;3;4;1;3;2;3;2;3;2;1;2;3;3;1;1;1;4;5;3;4;1;5;2;1;2;3;4;1;2;3;4;1;2;1;2;3;1;2;3;1;2;3;4;4;5;6;1;2;2;3;4;1;2;3;4;2;3;2;3;4;5;1;1;1;2;3;2;1;1;2;1;2;1;1;2;3;1;3;2;3;2;1;2;3;4;1;2;3;3;5;1;5;2;3;4;4;5;6;2;3;4;5;2;1;1;2;3;1;4;2;3;3;4;2;3;3;4;1;3;2;3;2;3;2;1;2;3;3;1;1;2;3;3;2;3;3;2;6;1;2;7;2;3;4;1;1;2;3;4;1;5;2;3;1;2;3;1;1;1;2;3;4;1;1;1;1;2;3;1;1;1;1;2;3;2;3;3;1;1;2;2;1;1;1;2;1;2;3;3;1;2;3;1;1;2;1;1;1;1;2;1;1;4;1;1;2;3;1;1;1;2;3;3;4;4;1;2;3;1;1;1;2;3;2;3;3;2;1;2;1;1;2;4;4;5;4;5;5;2;3;3;2;3;3;2;3;1;2;2;3;3;3;3;4;2;3;3;1;2;3;3;1;2;2;3;3;2;3;4;5;1;6;5;1;2;3;1;1;2;1;1;1;1;1;4;1;1;2;3;1;1;1;2;3;4;4;4;5;4;5;5;3;4;2;3;3;3;1;1;1;2;3;1;1;1;2;3;2;3;3;2;1;2;1;2;4;2;3;2;3;3;3;4;5;2;3;3;2;3;3;4;1;1;2;3;4;5;1;2;3;1;2;3;4;5;6;7;1;1;2;3;1;1;2;1;1;1;2;1;2;3;3;4;2;1;2;3;1;1;1;1;1;2;3;4;5;6;7;1;1;2;2;3;3;1;1;1;1;1;1;2;3;1;2;3;1;2;3;1;1;1;2;3;1;2;3;1;1;2;1;2;3;1;1;2;1;2;3;3;4;5;1;2;4;5;1;2;6;7;1;1;2;3;4;5;1;2;3;4;5;1;1;1;2;1;1;2;3;4;1;1;4;5;6;7;8;9;9;10;2;2;1;1;1;1;1;1;2;3;4;1;2;3;4;2;3;2;3;1;2;3;1;1;1;2;1;2;1;2;2;3;2;3;1;2;3;1;1;2;3;4;1;2;3;4;5;6;7;1;1;2;3;4;5;6;7;8;9;1;2;1;2;1;1;1;1;2;3;1;1;2;3;1;2;3;2;3;2;1;2;1;2;2;3;4;5;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;1;2;1;1;2;3;4;1;2;1;2;1;2;2;1;2;2;3;4;1;2;3;1;1;1;2;5;1;2;3;2;3;3;3;4;4;5;5;2;3;4;5;6;7;8;3;4;5;6;7;2;3;4;2;1;2;3;4;5;1;2;3;2;3;3;2;3;3;2;3;3;2;3;3;2;1;1;2;3;3;4;2;2;3;5;6;1;1;7;8;9;10;11;1;2;3;4;5;6;7;8;9;10;11;1;2;3;4;1;1;1;2;1;1;2;3;4;4;5;6;7;8;9;9;10;1;1;1;1;1;2;3;4;2;3;2;3;1;1;1;2;1;2;1;2;2;3;2;3;4;5;5;1;2;1;2;1;2;3;1;2;3;1;2;3;2;3;2;1;2;1;2;2;3;4;5;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;2;3;3;3;4;5;5;3;4;5;5;3;4;5;6;7;7;5;6;7;7;5;6;7;7;3;1;2;2;3;4;5;5;6;7;3;4;5;5;6;7;3;4;5;5;6;7;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;8;9;5;6;7;8;9;5;6;7;8;9;3;4;5;1;2;2;1;2;4;5;3;4;5;3;4;5;4;5;5;3;4;4;5;5;1;2;3;3;4;4;5;5;3;4;5;5;3;1;2;3;1;1;2;2;1;2;2;3;4;1;2;3;4;5;1;4;5;5;1;2;3;3;4;4;5;5;3;4;4;5;5;3;4;5;5;3;4;5;5;3;3;4;5;5;2;1;2;3;4;1;2;3;4;1;2;5;8;4;5;5;2;3;3;2;1;2;3;4;1;4;5;5;3;4;5;5;3;4;5;5;3;4;5;6;7;7;5;6;7;7;5;6;7;7;3;1;2;2;3;4;5;5;6;7;3;4;5;5;6;7;3;4;5;5;6;7;2;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;3;4;5;6;7;8;9;5;6;7;8;9;5;6;7;8;9;3;4;5;1;2;2;1;2;4;5;6;7;8;4;5;3;4;5;3;4;5;5;1;2;3;2;3;4;2;3;1;4;5;5;6;7;8;9;10;11;6;7;8;9;5;6;7;8;9;10;11;2;1;2;3;4;1;2;3;4;1;2;5;8;4;5;3;4;5;2;3;3;2;1;2;3;4;1;4;5;6;7;8;2;3;2;4;4;4;5;5;4;2;3;2;2;3;3;2;3;3;2;3;3;8;3;4;5;6;7;2;3;4;5;1;2;3;4;6;7;8;1;2;2;3;4;5;6;7;8;9;2;3;4;5;6;2;2;3;4;2;2;3;3;2;4;5;2;2;3;2;3;3;4;5;6;2;2;3;3;8;3;4;5;6;7;2;3;4;5;6;7;8;2;3;4;5;6;7;8;9;4;5;6;6;5;6;7;2;1;2;3;1;1;2;1;2;2;3;4;5;2;3;4;5;4;5;6;1;1;2;1;3;4;1;2;3;4;1;2;1;2;3;1;1;2;5;5;6;7;8;9;10;11;1;2;3;6;7;8;1;5;2;3;1;1;2;1;2;2;3;4;1;2;1;2;3;5;1;2;3;4;5;6;7;8;9;10;5;6;7;4;1;2;1;2;1;2;1;2;3;4;5;1;2;3;4;5;6;7;1;2;8;9;1;2;3;4;5;1;1;1;2;3;6;7;8;5;6;7;1;1;2;3;4;5;6;7;8;9;10;6;7;8;9;10;2;1;1;4;5;6;7;8;9;6;7;8;5;6;7;8;9;1;1;2;3;4;5;6;2;3;4;5;1;2;3;4;5;6;7;5;6;6;7;1;2;5;6;1;2;4;5;6;7;8;1;2;3;4;5;6;7;9;1;2;3;4;5;1;2;3;4;1;2;3;4;1;5;1;2;3;6;7;8;1;2;1;2;3;3;1;2;1;2;1;2;3;4;5;6;7;1;2;1;2;1;2;3;4;5;6;7;1;2;1;2;3;4;5;6;1;1;2;3;1;2;3;4;1;2;3;1;2;3;1;2;1;2;3;3;4;4;5;5;1;2;1;1;2;1;2;1;1;2;3;4;5;6;7;2;3;4;5;6;1;2;3;4;2;3;1;1;2;1;2;1;2;1;1;2;1;3;2;2;3;2;3;7;3;4;5;6;2;3;4;5;2;3;3;3;1;2;3;1;2;1;1;2;3;4;5;6;1;2;3;1;2;3;4;1;1;7;2;3;4;5;6;3;4;1;2;3;4;4;5;5;1;2;1;1;2;1;2;3;4;1;1;1;2;1;2;3;1;2;3;1;4;1;3;5;4;5;5;4;1;2;5;6;2;3;4;5;4;5;5;1;2;3;3;3;4;5;5;1;2;3;4;5;6;1;2;3;4;1;2;1;2;1;2;1;1;2;1;3;2;3;2;3;7;3;4;5;6;2;1;1;2;3;4;1;2;3;4;5;6;1;1;2;3;1;2;3;4;1;1;7;2;3;4;5;6;1;1;2;1;2;3;1;2;3;1;4;1;3;5;2;3;4;1;2;7;8;1;2;3;4;5;6;7;8;9;1;1;1;1;1;1;1;1;1;2;1;1;2;1;1;2;3;4;5;6;7;8;2;1;1;1;1;2;3;3;4;1;1;1;4;5;5;9;10;1;2;3;4;5;6;7;8;9;11;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9;10;1;1;1;1;1;1;1;2;1;2;1;2;1;1;2;3;4;5;6;7;8;9;2;1;1;2;2;1;2;3;4;3;4;3;4;5;6;6;2;2;3;3;2;2;3;4;5;6;6;7;7;8;2;3;2;3;3;3;4;4;5;5;5;6;6;7;5;6;6;7;7;8;9;1;2;3;4;1;5;2;3;2;3;3;3;4;5;5;2;2;1;2;1;2;3;3;3;4;5;5;2;5;6;4;5;6;7;1;2;3;4;5;6;8;3;4;2;3;4;3;4;9;6;7;8;1;1;2;3;1;2;1;1;2;1;1;2;3;1;2;3;2;1;3;1;1;1;2;3;4;5;1;2;3;4;2;3;4;9;10;2;2;1;1;1;1;1;2;3;4;2;3;4;5;6;7;8;5;6;7;8;9;1;1;2;3;4;5;6;2;3;4;5;1;2;3;4;5;1;2;6;7;2;3;4;1;2;1;2;3;4;5;6;1;2;4;5;6;7;1;2;3;4;5;6;8;1;2;3;4;1;2;3;4;5;6;7;8;3;4;5;6;7;2;3;4;2;1;2;3;4;5;1;2;3;5;6;1;2;3;4;5;6;1;2;3;4;5;1;2;3;4;5;6;7;1;2;8;9;1;2;3;4;5;6;7;8;5;6;7;1;2;3;4;5;6;7;8;9;10;1;2;3;4;5;6;7;8;9;11;2;3;4;5;6;1;2;1;2;3;4;1;2;7;8;1;2;3;4;5;6;7;8;9;1;1;1;1;1;1;1;1;1;1;2;3;4;2;1;2;1;2;1;2;1;1;2;3;4;5;6;7;8;2;1;1;2;3;4;1;2;1;3;4;4;1;2;3;1;5;7;8;8;2;3;3;3;4;4;5;5;2;2;3;3;2;3;4;4;5;6;6;4;5;6;7;8;5;6;4;5;5;5;6;6;7;5;6;6;7;7;8;9;5;6;2;3;4;5;2;3;4;2;3;4;9;6;7;8;1;2;3;4;5;9;10;2;2;1;1;1;1;1;2;3;4;4;5;5;6;7;8;9;3;4;3;4;5;6;7;2;2;3;3;4;5;2;3;4;5;4;2;3;1;2;3;4;2;1;2;1;1;2;1;1;2;2;1;1;2;3;1;2;1;2;3;4;5;6;4;4;3;4;5;3;3;1;7;8;9;10;6;7;8;9;10;2;1;1;4;5;6;7;8;9;5;6;7;8;9;1;1;2;3;4;5;6;2;3;4;5;1;2;3;4;5;6;7;5;6;6;7;1;2;5;6;1;2;4;5;6;7;8;1;2;3;4;5;6;7;9;1;2;3;4;5;1;2;3;4;1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9;10;1;1;1;1;1;1;1;2;1;2;1;2;1;1;2;3;4;5;6;7;8;9;2;1;1;2;3;4;4;1;7;8;8;5;6;3;4;5;6;4;5;6;4;5;6;7;3;4;4;5;5;6;6;7;3;4;4;5;6;6;7;3;4;3;4;5;6;6;4;5;6;7;2;3;4;5;6;6;7;7;8;2;3;3;3;4;2;4;5;6;6;4;5;6;7;8;5;6;4;5;6;7;3;4;3;4;5;6;7;1;2;1;0;1;2;1;0;1;1;2;0;1;2;1;0;1;2;1;0;1;2;3;3;3;3;3;3;2;1;1;1;2;1;2;1;2;3;1;2;0;1;1;1;2;2;2;3;4;2;1;1;2;3;4;1;2;|]

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
  | T_HASH -> true
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
  let r0 = [R 571] in
  let r1 = S (N N_expr) :: r0 in
  let r2 = [R 125] in
  let r3 = S (T T_DONE) :: r2 in
  let r4 = Sub (r1) :: r3 in
  let r5 = S (T T_DO) :: r4 in
  let r6 = Sub (r1) :: r5 in
  let r7 = R 277 :: r6 in
  let r8 = R 189 :: r7 in
  let r9 = [R 699] in
  let r10 = S (T T_AND) :: r9 in
  let r11 = [R 32] in
  let r12 = Sub (r10) :: r11 in
  let r13 = [R 190] in
  let r14 = Sub (r12) :: r13 in
  let r15 = [R 33] in
  let r16 = Sub (r12) :: r15 in
  let r17 = [R 492] in
  let r18 = S (N N_structure) :: r17 in
  let r19 = [R 34] in
  let r20 = S (T T_RBRACKET) :: r19 in
  let r21 = Sub (r18) :: r20 in
  let r22 = Sub (r12) :: r21 in
  let r23 = [R 141] in
  let r24 = S (T T_DONE) :: r23 in
  let r25 = Sub (r1) :: r24 in
  let r26 = S (T T_DO) :: r25 in
  let r27 = Sub (r1) :: r26 in
  let r28 = R 277 :: r27 in
  let r29 = R 189 :: r28 in
  let r30 = [R 332] in
  let r31 = [R 121] in
  let r32 = Sub (r1) :: r31 in
  let r33 = R 277 :: r32 in
  let r34 = R 189 :: r33 in
  let r35 = [R 137] in
  let r36 = S (N N_match_cases) :: r35 in
  let r37 = S (T T_WITH) :: r36 in
  let r38 = Sub (r1) :: r37 in
  let r39 = R 277 :: r38 in
  let r40 = R 189 :: r39 in
  let r41 = [R 653] in
  let r42 = S (T T_QUESTIONQUESTION) :: r41 in
  let r43 = [R 641] in
  let r44 = Sub (r42) :: r43 in
  let r45 = [R 56] in
  let r46 = S (T T_LIDENT) :: r45 in
  let r47 = [R 633] in
  let r48 = Sub (r46) :: r47 in
  let r49 = R 277 :: r48 in
  let r50 = R 189 :: r49 in
  let r51 = [R 57] in
  let r52 = S (T T_LIDENT) :: r51 in
  let r53 = S (T T_DOT) :: r52 in
  let r54 = [R 333] in
  let r55 = [R 278] in
  let r56 = R 277 :: r55 in
  let r57 = [R 812] in
  let r58 = S (T T_error) :: r57 in
  let r59 = [R 145] in
  let r60 = S (T T_END) :: r59 in
  let r61 = R 294 :: r60 in
  let r62 = R 60 :: r61 in
  let r63 = R 277 :: r62 in
  let r64 = R 189 :: r63 in
  let r65 = S (T T_LPAREN) :: r58 in
  let r66 = [R 15] in
  let r67 = S (T T_UNDERSCORE) :: r66 in
  let r68 = [R 787] in
  let r69 = Sub (r67) :: r68 in
  let r70 = [R 203] in
  let r71 = Sub (r69) :: r70 in
  let r72 = [R 9] in
  let r73 = Sub (r71) :: r72 in
  let r74 = [R 115] in
  let r75 = Sub (r73) :: r74 in
  let r76 = [R 821] in
  let r77 = R 283 :: r76 in
  let r78 = Sub (r75) :: r77 in
  let r79 = S (T T_COLON) :: r78 in
  let r80 = Sub (r65) :: r79 in
  let r81 = R 277 :: r80 in
  let r82 = R 189 :: r81 in
  let r83 = [R 813] in
  let r84 = S (T T_error) :: r83 in
  let r85 = [R 397] in
  let r86 = S (T T_RPAREN) :: r85 in
  let r87 = [R 399] in
  let r88 = [R 401] in
  let r89 = [R 810] in
  let r90 = S (T T_RPAREN) :: r89 in
  let r91 = [R 328] in
  let r92 = [R 223] in
  let r93 = S (T T_LIDENT) :: r92 in
  let r94 = [R 14] in
  let r95 = Sub (r93) :: r94 in
  let r96 = [R 446] in
  let r97 = S (T T_COLON) :: r96 in
  let r98 = [R 13] in
  let r99 = S (T T_RPAREN) :: r98 in
  let r100 = S (N N_module_type) :: r99 in
  let r101 = R 277 :: r100 in
  let r102 = R 189 :: r101 in
  let r103 = S (T T_MODULE) :: r102 in
  let r104 = [R 576] in
  let r105 = R 285 :: r104 in
  let r106 = [R 350] in
  let r107 = S (T T_END) :: r106 in
  let r108 = Sub (r105) :: r107 in
  let r109 = R 277 :: r108 in
  let r110 = [R 353] in
  let r111 = S (N N_module_expr) :: r110 in
  let r112 = R 277 :: r111 in
  let r113 = S (T T_OF) :: r112 in
  let r114 = S (T T_TYPE) :: r113 in
  let r115 = [R 339] in
  let r116 = S (T T_END) :: r115 in
  let r117 = S (N N_structure) :: r116 in
  let r118 = R 277 :: r117 in
  let r119 = [R 120] in
  let r120 = S (N N_match_cases) :: r119 in
  let r121 = S (T T_WITH) :: r120 in
  let r122 = Sub (r1) :: r121 in
  let r123 = R 277 :: r122 in
  let r124 = R 189 :: r123 in
  let r125 = [R 136] in
  let r126 = S (N N_match_cases) :: r125 in
  let r127 = S (T T_WITH) :: r126 in
  let r128 = Sub (r1) :: r127 in
  let r129 = R 277 :: r128 in
  let r130 = R 189 :: r129 in
  let r131 = [R 176] in
  let r132 = S (N N_expr) :: r131 in
  let r133 = [R 750] in
  let r134 = Sub (r1) :: r133 in
  let r135 = S (T T_EQUAL) :: r134 in
  let r136 = [R 249] in
  let r137 = Sub (r135) :: r136 in
  let r138 = Sub (r65) :: r137 in
  let r139 = [R 307] in
  let r140 = R 283 :: r139 in
  let r141 = Sub (r138) :: r140 in
  let r142 = R 510 :: r141 in
  let r143 = R 277 :: r142 in
  let r144 = R 189 :: r143 in
  let r145 = [R 679] in
  let r146 = [R 598] in
  let r147 = S (T T_INT) :: r146 in
  let r148 = [R 596] in
  let r149 = S (T T_INT) :: r148 in
  let r150 = [R 105] in
  let r151 = [R 677] in
  let r152 = S (T T_RPAREN) :: r151 in
  let r153 = S (T T_UIDENT) :: r152 in
  let r154 = R 277 :: r153 in
  let r155 = [R 678] in
  let r156 = S (T T_RPAREN) :: r155 in
  let r157 = S (N N_module_type) :: r156 in
  let r158 = [R 796] in
  let r159 = S (T T_LIDENT) :: r158 in
  let r160 = [R 100] in
  let r161 = S (T T_FALSE) :: r160 in
  let r162 = [R 217] in
  let r163 = R 277 :: r162 in
  let r164 = R 212 :: r163 in
  let r165 = Sub (r161) :: r164 in
  let r166 = [R 523] in
  let r167 = Sub (r165) :: r166 in
  let r168 = [R 583] in
  let r169 = R 283 :: r168 in
  let r170 = Sub (r167) :: r169 in
  let r171 = R 503 :: r170 in
  let r172 = S (T T_PLUSEQ) :: r171 in
  let r173 = Sub (r159) :: r172 in
  let r174 = R 799 :: r173 in
  let r175 = R 277 :: r174 in
  let r176 = R 189 :: r175 in
  let r177 = [R 584] in
  let r178 = R 283 :: r177 in
  let r179 = Sub (r167) :: r178 in
  let r180 = R 503 :: r179 in
  let r181 = S (T T_PLUSEQ) :: r180 in
  let r182 = Sub (r159) :: r181 in
  let r183 = R 799 :: r182 in
  let r184 = [R 803] in
  let r185 = S (T T_UNDERSCORE) :: r184 in
  let r186 = [R 798] in
  let r187 = Sub (r185) :: r186 in
  let r188 = R 804 :: r187 in
  let r189 = [R 547] in
  let r190 = Sub (r188) :: r189 in
  let r191 = [R 801] in
  let r192 = S (T T_RPAREN) :: r191 in
  let r193 = [R 802] in
  let r194 = [R 548] in
  let r195 = [R 382] in
  let r196 = S (T T_DOTDOT) :: r195 in
  let r197 = [R 795] in
  let r198 = Sub (r196) :: r197 in
  let r199 = [R 383] in
  let r200 = S (T T_DOTDOT) :: r199 in
  let r201 = [R 98] in
  let r202 = S (T T_RPAREN) :: r201 in
  let r203 = [R 205] in
  let r204 = Sub (r71) :: r203 in
  let r205 = S (T T_MINUSGREATER) :: r204 in
  let r206 = Sub (r69) :: r205 in
  let r207 = S (T T_COLON) :: r206 in
  let r208 = [R 20] in
  let r209 = S (T T_GREATER) :: r208 in
  let r210 = [R 499] in
  let r211 = Sub (r73) :: r210 in
  let r212 = [R 318] in
  let r213 = R 277 :: r212 in
  let r214 = Sub (r211) :: r213 in
  let r215 = [R 534] in
  let r216 = Sub (r93) :: r215 in
  let r217 = [R 191] in
  let r218 = S (T T_RBRACKET) :: r217 in
  let r219 = Sub (r18) :: r218 in
  let r220 = Sub (r12) :: r219 in
  let r221 = [R 517] in
  let r222 = Sub (r165) :: r221 in
  let r223 = [R 762] in
  let r224 = R 283 :: r223 in
  let r225 = Sub (r222) :: r224 in
  let r226 = R 503 :: r225 in
  let r227 = S (T T_PLUSEQ) :: r226 in
  let r228 = Sub (r159) :: r227 in
  let r229 = R 799 :: r228 in
  let r230 = R 277 :: r229 in
  let r231 = R 189 :: r230 in
  let r232 = [R 220] in
  let r233 = R 283 :: r232 in
  let r234 = R 526 :: r233 in
  let r235 = R 794 :: r234 in
  let r236 = S (T T_LIDENT) :: r235 in
  let r237 = R 799 :: r236 in
  let r238 = R 277 :: r237 in
  let r239 = R 189 :: r238 in
  let r240 = [R 763] in
  let r241 = R 283 :: r240 in
  let r242 = Sub (r222) :: r241 in
  let r243 = R 503 :: r242 in
  let r244 = S (T T_PLUSEQ) :: r243 in
  let r245 = Sub (r159) :: r244 in
  let r246 = R 799 :: r245 in
  let r247 = [R 221] in
  let r248 = R 283 :: r247 in
  let r249 = R 526 :: r248 in
  let r250 = R 794 :: r249 in
  let r251 = S (T T_LIDENT) :: r250 in
  let r252 = R 799 :: r251 in
  let r253 = [R 558] in
  let r254 = Sub (r75) :: r253 in
  let r255 = [R 543] in
  let r256 = Sub (r254) :: r255 in
  let r257 = [R 29] in
  let r258 = S (T T_RBRACKET) :: r257 in
  let r259 = Sub (r256) :: r258 in
  let r260 = R 427 :: r259 in
  let r261 = [R 28] in
  let r262 = S (T T_RBRACKET) :: r261 in
  let r263 = [R 27] in
  let r264 = S (T T_RBRACKET) :: r263 in
  let r265 = Sub (r256) :: r264 in
  let r266 = [R 371] in
  let r267 = Sub (r93) :: r266 in
  let r268 = S (T T_BACKQUOTE) :: r267 in
  let r269 = [R 775] in
  let r270 = R 277 :: r269 in
  let r271 = Sub (r268) :: r270 in
  let r272 = [R 24] in
  let r273 = S (T T_RBRACKET) :: r272 in
  let r274 = Sub (r271) :: r273 in
  let r275 = [R 21] in
  let r276 = Sub (r46) :: r275 in
  let r277 = [R 25] in
  let r278 = S (T T_RBRACKET) :: r277 in
  let r279 = Sub (r256) :: r278 in
  let r280 = [R 206] in
  let r281 = Sub (r71) :: r280 in
  let r282 = [R 555] in
  let r283 = Sub (r67) :: r282 in
  let r284 = [R 331] in
  let r285 = S (T T_error) :: r284 in
  let r286 = [R 797] in
  let r287 = S (T T_LIDENT) :: r286 in
  let r288 = S (T T_DOT) :: r287 in
  let r289 = [R 330] in
  let r290 = S (T T_RPAREN) :: r289 in
  let r291 = [R 329] in
  let r292 = S (T T_UIDENT) :: r291 in
  let r293 = [R 22] in
  let r294 = Sub (r46) :: r293 in
  let r295 = [R 204] in
  let r296 = Sub (r71) :: r295 in
  let r297 = S (T T_MINUSGREATER) :: r296 in
  let r298 = Sub (r69) :: r297 in
  let r299 = [R 556] in
  let r300 = Sub (r67) :: r299 in
  let r301 = [R 544] in
  let r302 = [R 539] in
  let r303 = Sub (r73) :: r302 in
  let r304 = [R 774] in
  let r305 = R 277 :: r304 in
  let r306 = Sub (r303) :: r305 in
  let r307 = [R 540] in
  let r308 = [R 10] in
  let r309 = Sub (r93) :: r308 in
  let r310 = S (T T_QUOTE) :: r309 in
  let r311 = [R 26] in
  let r312 = S (T T_RBRACKET) :: r311 in
  let r313 = Sub (r256) :: r312 in
  let r314 = S (T T_BAR) :: r313 in
  let r315 = [R 532] in
  let r316 = Sub (r268) :: r315 in
  let r317 = [R 30] in
  let r318 = S (T T_RBRACKET) :: r317 in
  let r319 = Sub (r316) :: r318 in
  let r320 = [R 99] in
  let r321 = S (T T_RPAREN) :: r320 in
  let r322 = [R 18] in
  let r323 = Sub (r159) :: r322 in
  let r324 = S (T T_RPAREN) :: r323 in
  let r325 = [R 23] in
  let r326 = Sub (r46) :: r325 in
  let r327 = [R 551] in
  let r328 = [R 12] in
  let r329 = S (T T_RPAREN) :: r328 in
  let r330 = [R 552] in
  let r331 = [R 97] in
  let r332 = S (T T_RBRACKET) :: r331 in
  let r333 = [R 227] in
  let r334 = R 277 :: r333 in
  let r335 = Sub (r211) :: r334 in
  let r336 = S (T T_COLON) :: r335 in
  let r337 = S (T T_LIDENT) :: r336 in
  let r338 = R 364 :: r337 in
  let r339 = [R 229] in
  let r340 = Sub (r338) :: r339 in
  let r341 = [R 387] in
  let r342 = S (T T_RBRACE) :: r341 in
  let r343 = Sub (r340) :: r342 in
  let r344 = [R 228] in
  let r345 = R 277 :: r344 in
  let r346 = S (T T_SEMI) :: r345 in
  let r347 = R 277 :: r346 in
  let r348 = Sub (r211) :: r347 in
  let r349 = S (T T_COLON) :: r348 in
  let r350 = [R 500] in
  let r351 = Sub (r73) :: r350 in
  let r352 = [R 535] in
  let r353 = [R 216] in
  let r354 = R 277 :: r353 in
  let r355 = R 212 :: r354 in
  let r356 = [R 110] in
  let r357 = Sub (r67) :: r356 in
  let r358 = [R 213] in
  let r359 = Sub (r357) :: r358 in
  let r360 = [R 112] in
  let r361 = S (T T_RBRACE) :: r360 in
  let r362 = Sub (r340) :: r361 in
  let r363 = [R 111] in
  let r364 = Sub (r67) :: r363 in
  let r365 = S (T T_STAR) :: r364 in
  let r366 = [R 215] in
  let r367 = Sub (r67) :: r366 in
  let r368 = [R 214] in
  let r369 = Sub (r67) :: r368 in
  let r370 = S (T T_MINUSGREATER) :: r369 in
  let r371 = Sub (r161) :: r355 in
  let r372 = [R 386] in
  let r373 = S (T T_RBRACE) :: r372 in
  let r374 = Sub (r340) :: r373 in
  let r375 = [R 384] in
  let r376 = S (T T_DOTDOT) :: r375 in
  let r377 = [R 385] in
  let r378 = S (T T_DOTDOT) :: r377 in
  let r379 = [R 389] in
  let r380 = S (T T_RBRACE) :: r379 in
  let r381 = Sub (r340) :: r380 in
  let r382 = [R 388] in
  let r383 = S (T T_RBRACE) :: r382 in
  let r384 = Sub (r340) :: r383 in
  let r385 = [R 501] in
  let r386 = S (T T_RBRACKET) :: r385 in
  let r387 = Sub (r18) :: r386 in
  let r388 = Sub (r12) :: r387 in
  let r389 = [R 289] in
  let r390 = R 288 :: r389 in
  let r391 = [R 390] in
  let r392 = R 283 :: r391 in
  let r393 = S (N N_module_expr) :: r392 in
  let r394 = R 277 :: r393 in
  let r395 = R 189 :: r394 in
  let r396 = [R 391] in
  let r397 = R 283 :: r396 in
  let r398 = S (N N_module_expr) :: r397 in
  let r399 = R 277 :: r398 in
  let r400 = R 189 :: r399 in
  let r401 = [R 449] in
  let r402 = S (T T_RPAREN) :: r401 in
  let r403 = S (N N_module_expr) :: r402 in
  let r404 = [R 451] in
  let r405 = S (T T_RPAREN) :: r404 in
  let r406 = S (N N_expr) :: r405 in
  let r407 = R 277 :: r406 in
  let r408 = [R 262] in
  let r409 = Sub (r135) :: r408 in
  let r410 = Sub (r65) :: r409 in
  let r411 = [R 265] in
  let r412 = Sub (r410) :: r411 in
  let r413 = [R 174] in
  let r414 = Sub (r1) :: r413 in
  let r415 = S (T T_IN) :: r414 in
  let r416 = Sub (r412) :: r415 in
  let r417 = [R 605] in
  let r418 = S (T T_BARRBRACKET) :: r417 in
  let r419 = [R 495] in
  let r420 = [R 104] in
  let r421 = S (T T_RBRACKET) :: r420 in
  let r422 = [R 565] in
  let r423 = S (N N_pattern) :: r422 in
  let r424 = [R 602] in
  let r425 = S (T T_RBRACKET) :: r424 in
  let r426 = Sub (r423) :: r425 in
  let r427 = [R 334] in
  let r428 = S (N N_module_expr) :: r427 in
  let r429 = S (T T_EQUAL) :: r428 in
  let r430 = [R 765] in
  let r431 = R 283 :: r430 in
  let r432 = Sub (r429) :: r431 in
  let r433 = S (T T_UIDENT) :: r432 in
  let r434 = R 277 :: r433 in
  let r435 = R 189 :: r434 in
  let r436 = [R 361] in
  let r437 = R 283 :: r436 in
  let r438 = R 437 :: r437 in
  let r439 = Sub (r93) :: r438 in
  let r440 = R 277 :: r439 in
  let r441 = R 189 :: r440 in
  let r442 = [R 438] in
  let r443 = S (N N_module_type) :: r442 in
  let r444 = [R 354] in
  let r445 = S (T T_RPAREN) :: r444 in
  let r446 = S (N N_module_type) :: r445 in
  let r447 = [R 352] in
  let r448 = S (N N_module_type) :: r447 in
  let r449 = S (T T_MINUSGREATER) :: r448 in
  let r450 = S (N N_functor_args) :: r449 in
  let r451 = R 277 :: r450 in
  let r452 = [R 207] in
  let r453 = [R 208] in
  let r454 = S (T T_RPAREN) :: r453 in
  let r455 = S (N N_module_type) :: r454 in
  let r456 = S (T T_UIDENT) :: r30 in
  let r457 = S (T T_UIDENT) :: r91 in
  let r458 = [R 832] in
  let r459 = Sub (r457) :: r458 in
  let r460 = S (T T_EQUAL) :: r459 in
  let r461 = Sub (r456) :: r460 in
  let r462 = S (T T_MODULE) :: r461 in
  let r463 = [R 541] in
  let r464 = Sub (r462) :: r463 in
  let r465 = [R 359] in
  let r466 = Sub (r464) :: r465 in
  let r467 = [R 234] in
  let r468 = S (T T_LIDENT) :: r467 in
  let r469 = [R 831] in
  let r470 = Sub (r73) :: r469 in
  let r471 = S (T T_COLONEQUAL) :: r470 in
  let r472 = Sub (r468) :: r471 in
  let r473 = R 799 :: r472 in
  let r474 = [R 235] in
  let r475 = S (T T_LIDENT) :: r474 in
  let r476 = S (T T_DOT) :: r475 in
  let r477 = [R 830] in
  let r478 = R 526 :: r477 in
  let r479 = Sub (r73) :: r478 in
  let r480 = [R 527] in
  let r481 = Sub (r75) :: r480 in
  let r482 = S (T T_EQUAL) :: r481 in
  let r483 = Sub (r75) :: r482 in
  let r484 = S (T T_UIDENT) :: r54 in
  let r485 = [R 833] in
  let r486 = Sub (r457) :: r485 in
  let r487 = [R 542] in
  let r488 = Sub (r462) :: r487 in
  let r489 = [R 358] in
  let r490 = S (N N_module_type) :: r489 in
  let r491 = [R 363] in
  let r492 = Sub (r93) :: r491 in
  let r493 = S (T T_DOT) :: r492 in
  let r494 = [R 19] in
  let r495 = S (T T_GREATER) :: r494 in
  let r496 = [R 284] in
  let r497 = R 283 :: r496 in
  let r498 = [R 341] in
  let r499 = S (N N_module_expr) :: r498 in
  let r500 = S (T T_MINUSGREATER) :: r499 in
  let r501 = S (N N_functor_args) :: r500 in
  let r502 = R 277 :: r501 in
  let r503 = [R 346] in
  let r504 = S (T T_RPAREN) :: r503 in
  let r505 = [R 662] in
  let r506 = S (T T_BARRBRACKET) :: r505 in
  let r507 = [R 58] in
  let r508 = S (T T_RPAREN) :: r507 in
  let r509 = [R 303] in
  let r510 = R 439 :: r509 in
  let r511 = R 433 :: r510 in
  let r512 = Sub (r468) :: r511 in
  let r513 = [R 600] in
  let r514 = S (T T_RBRACE) :: r513 in
  let r515 = Sub (r512) :: r514 in
  let r516 = [R 434] in
  let r517 = [R 440] in
  let r518 = S (T T_UNDERSCORE) :: r145 in
  let r519 = [R 674] in
  let r520 = Sub (r518) :: r519 in
  let r521 = [R 480] in
  let r522 = Sub (r520) :: r521 in
  let r523 = R 277 :: r522 in
  let r524 = R 189 :: r523 in
  let r525 = [R 92] in
  let r526 = [R 684] in
  let r527 = Sub (r159) :: r526 in
  let r528 = [R 107] in
  let r529 = S (T T_INT) :: r525 in
  let r530 = [R 595] in
  let r531 = Sub (r529) :: r530 in
  let r532 = [R 681] in
  let r533 = Sub (r531) :: r532 in
  let r534 = [R 686] in
  let r535 = S (T T_RBRACKET) :: r534 in
  let r536 = S (T T_LBRACKET) :: r535 in
  let r537 = S (T T_DOT) :: r536 in
  let r538 = [R 687] in
  let r539 = S (T T_RPAREN) :: r538 in
  let r540 = [R 470] in
  let r541 = S (N N_pattern) :: r540 in
  let r542 = R 277 :: r541 in
  let r543 = R 189 :: r542 in
  let r544 = [R 471] in
  let r545 = S (N N_pattern) :: r544 in
  let r546 = [R 461] in
  let r547 = S (N N_pattern) :: r546 in
  let r548 = [R 479] in
  let r549 = S (N N_pattern) :: r548 in
  let r550 = [R 478] in
  let r551 = S (N N_pattern) :: r550 in
  let r552 = [R 103] in
  let r553 = S (T T_RPAREN) :: r552 in
  let r554 = [R 688] in
  let r555 = S (T T_RPAREN) :: r554 in
  let r556 = [R 472] in
  let r557 = S (N N_pattern) :: r556 in
  let r558 = [R 468] in
  let r559 = S (N N_pattern) :: r558 in
  let r560 = [R 465] in
  let r561 = S (T T_error) :: r560 in
  let r562 = [R 604] in
  let r563 = S (T T_BARRBRACKET) :: r562 in
  let r564 = [R 305] in
  let r565 = [R 59] in
  let r566 = S (T T_RPAREN) :: r565 in
  let r567 = [R 817] in
  let r568 = Sub (r1) :: r567 in
  let r569 = S (T T_EQUAL) :: r568 in
  let r570 = S (T T_LIDENT) :: r569 in
  let r571 = R 364 :: r570 in
  let r572 = R 277 :: r571 in
  let r573 = [R 45] in
  let r574 = R 283 :: r573 in
  let r575 = [R 818] in
  let r576 = Sub (r1) :: r575 in
  let r577 = S (T T_EQUAL) :: r576 in
  let r578 = S (T T_LIDENT) :: r577 in
  let r579 = R 364 :: r578 in
  let r580 = [R 131] in
  let r581 = Sub (r1) :: r580 in
  let r582 = S (T T_IN) :: r581 in
  let r583 = S (N N_module_expr) :: r582 in
  let r584 = R 277 :: r583 in
  let r585 = R 189 :: r584 in
  let r586 = S (T T_OPEN) :: r585 in
  let r587 = [R 255] in
  let r588 = R 283 :: r587 in
  let r589 = Sub (r138) :: r588 in
  let r590 = R 510 :: r589 in
  let r591 = R 277 :: r590 in
  let r592 = R 189 :: r591 in
  let r593 = [R 132] in
  let r594 = Sub (r1) :: r593 in
  let r595 = S (T T_IN) :: r594 in
  let r596 = S (N N_module_expr) :: r595 in
  let r597 = R 277 :: r596 in
  let r598 = R 189 :: r597 in
  let r599 = [R 559] in
  let r600 = S (N N_expr) :: r599 in
  let r601 = [R 666] in
  let r602 = S (T T_RBRACKET) :: r601 in
  let r603 = Sub (r600) :: r602 in
  let r604 = [R 246] in
  let r605 = [R 232] in
  let r606 = S (T T_LIDENT) :: r605 in
  let r607 = [R 245] in
  let r608 = S (T T_RPAREN) :: r607 in
  let r609 = [R 233] in
  let r610 = [R 242] in
  let r611 = [R 241] in
  let r612 = S (T T_RPAREN) :: r611 in
  let r613 = R 441 :: r612 in
  let r614 = [R 442] in
  let r615 = [R 645] in
  let r616 = S (T T_GREATERRBRACE) :: r615 in
  let r617 = [R 562] in
  let r618 = R 435 :: r617 in
  let r619 = [R 436] in
  let r620 = [R 643] in
  let r621 = S (T T_GREATERRBRACE) :: r620 in
  let r622 = [R 568] in
  let r623 = R 435 :: r622 in
  let r624 = R 443 :: r623 in
  let r625 = Sub (r468) :: r624 in
  let r626 = [R 512] in
  let r627 = Sub (r625) :: r626 in
  let r628 = [R 656] in
  let r629 = S (T T_RBRACE) :: r628 in
  let r630 = Sub (r627) :: r629 in
  let r631 = [R 608] in
  let r632 = Sub (r42) :: r631 in
  let r633 = [R 607] in
  let r634 = S (T T_GREATERDOT) :: r633 in
  let r635 = S (N N_expr) :: r634 in
  let r636 = [R 144] in
  let r637 = Sub (r42) :: r636 in
  let r638 = R 277 :: r637 in
  let r639 = R 189 :: r638 in
  let r640 = [R 631] in
  let r641 = S (T T_END) :: r640 in
  let r642 = R 277 :: r641 in
  let r643 = R 189 :: r642 in
  let r644 = [R 140] in
  let r645 = S (N N_expr) :: r644 in
  let r646 = S (T T_THEN) :: r645 in
  let r647 = Sub (r1) :: r646 in
  let r648 = R 277 :: r647 in
  let r649 = R 189 :: r648 in
  let r650 = [R 133] in
  let r651 = S (N N_match_cases) :: r650 in
  let r652 = R 277 :: r651 in
  let r653 = R 189 :: r652 in
  let r654 = [R 310] in
  let r655 = Sub (r1) :: r654 in
  let r656 = S (T T_MINUSGREATER) :: r655 in
  let r657 = S (N N_pattern) :: r656 in
  let r658 = [R 537] in
  let r659 = Sub (r657) :: r658 in
  let r660 = [R 311] in
  let r661 = Sub (r1) :: r660 in
  let r662 = S (T T_MINUSGREATER) :: r661 in
  let r663 = Sub (r1) :: r662 in
  let r664 = [R 248] in
  let r665 = Sub (r520) :: r664 in
  let r666 = [R 199] in
  let r667 = Sub (r1) :: r666 in
  let r668 = S (T T_MINUSGREATER) :: r667 in
  let r669 = [R 134] in
  let r670 = Sub (r668) :: r669 in
  let r671 = Sub (r665) :: r670 in
  let r672 = R 277 :: r671 in
  let r673 = R 189 :: r672 in
  let r674 = [R 491] in
  let r675 = S (T T_UNDERSCORE) :: r674 in
  let r676 = [R 244] in
  let r677 = [R 243] in
  let r678 = S (T T_RPAREN) :: r677 in
  let r679 = R 441 :: r678 in
  let r680 = [R 261] in
  let r681 = [R 372] in
  let r682 = S (T T_LIDENT) :: r681 in
  let r683 = [R 135] in
  let r684 = Sub (r668) :: r683 in
  let r685 = S (T T_RPAREN) :: r684 in
  let r686 = Sub (r682) :: r685 in
  let r687 = [R 127] in
  let r688 = S (T T_DONE) :: r687 in
  let r689 = Sub (r1) :: r688 in
  let r690 = S (T T_DO) :: r689 in
  let r691 = Sub (r1) :: r690 in
  let r692 = S (T T_IN) :: r691 in
  let r693 = S (N N_pattern) :: r692 in
  let r694 = R 277 :: r693 in
  let r695 = R 189 :: r694 in
  let r696 = [R 118] in
  let r697 = S (T T_DOWNTO) :: r696 in
  let r698 = [R 142] in
  let r699 = S (T T_DONE) :: r698 in
  let r700 = Sub (r1) :: r699 in
  let r701 = S (T T_DO) :: r700 in
  let r702 = Sub (r1) :: r701 in
  let r703 = Sub (r697) :: r702 in
  let r704 = Sub (r1) :: r703 in
  let r705 = S (T T_EQUAL) :: r704 in
  let r706 = S (N N_pattern) :: r705 in
  let r707 = R 277 :: r706 in
  let r708 = R 189 :: r707 in
  let r709 = [R 642] in
  let r710 = Sub (r42) :: r709 in
  let r711 = [R 654] in
  let r712 = S (T T_RPAREN) :: r711 in
  let r713 = S (T T_LPAREN) :: r712 in
  let r714 = S (T T_DOT) :: r713 in
  let r715 = [R 672] in
  let r716 = S (T T_error) :: r715 in
  let r717 = S (T T_COLON) :: r716 in
  let r718 = S (N N_module_expr) :: r717 in
  let r719 = R 277 :: r718 in
  let r720 = [R 671] in
  let r721 = S (T T_RPAREN) :: r720 in
  let r722 = [R 263] in
  let r723 = Sub (r1) :: r722 in
  let r724 = S (T T_EQUAL) :: r723 in
  let r725 = [R 143] in
  let r726 = Sub (r42) :: r725 in
  let r727 = R 277 :: r726 in
  let r728 = R 189 :: r727 in
  let r729 = [R 651] in
  let r730 = Sub (r42) :: r729 in
  let r731 = [R 616] in
  let r732 = S (T T_RBRACKET) :: r731 in
  let r733 = S (N N_expr) :: r732 in
  let r734 = S (T T_LBRACKET) :: r733 in
  let r735 = [R 618] in
  let r736 = S (T T_RPAREN) :: r735 in
  let r737 = S (N N_expr) :: r736 in
  let r738 = [R 171] in
  let r739 = S (N N_expr) :: r738 in
  let r740 = [R 238] in
  let r741 = S (T T_LIDENT) :: r740 in
  let r742 = [R 239] in
  let r743 = S (T T_LIDENT) :: r742 in
  let r744 = [R 240] in
  let r745 = Sub (r42) :: r744 in
  let r746 = [R 650] in
  let r747 = S (T T_LIDENT) :: r746 in
  let r748 = [R 628] in
  let r749 = S (T T_RBRACE) :: r748 in
  let r750 = S (N N_expr) :: r749 in
  let r751 = S (T T_LBRACE) :: r750 in
  let r752 = [R 612] in
  let r753 = S (T T_RPAREN) :: r752 in
  let r754 = Sub (r1) :: r753 in
  let r755 = [R 553] in
  let r756 = S (N N_expr) :: r755 in
  let r757 = [R 119] in
  let r758 = Sub (r1) :: r757 in
  let r759 = S (T T_IN) :: r758 in
  let r760 = [R 173] in
  let r761 = Sub (r1) :: r760 in
  let r762 = S (T T_IN) :: r761 in
  let r763 = [R 161] in
  let r764 = S (N N_expr) :: r763 in
  let r765 = [R 155] in
  let r766 = S (N N_expr) :: r765 in
  let r767 = [R 172] in
  let r768 = S (N N_expr) :: r767 in
  let r769 = [R 574] in
  let r770 = Sub (r1) :: r769 in
  let r771 = Sub (r12) :: r770 in
  let r772 = [R 158] in
  let r773 = S (N N_expr) :: r772 in
  let r774 = [R 162] in
  let r775 = S (N N_expr) :: r774 in
  let r776 = [R 154] in
  let r777 = S (N N_expr) :: r776 in
  let r778 = [R 157] in
  let r779 = S (N N_expr) :: r778 in
  let r780 = [R 156] in
  let r781 = S (N N_expr) :: r780 in
  let r782 = [R 166] in
  let r783 = S (N N_expr) :: r782 in
  let r784 = [R 160] in
  let r785 = S (N N_expr) :: r784 in
  let r786 = [R 159] in
  let r787 = S (N N_expr) :: r786 in
  let r788 = [R 164] in
  let r789 = S (N N_expr) :: r788 in
  let r790 = [R 153] in
  let r791 = S (N N_expr) :: r790 in
  let r792 = [R 152] in
  let r793 = S (N N_expr) :: r792 in
  let r794 = [R 175] in
  let r795 = S (N N_expr) :: r794 in
  let r796 = [R 151] in
  let r797 = S (N N_expr) :: r796 in
  let r798 = [R 165] in
  let r799 = S (N N_expr) :: r798 in
  let r800 = [R 163] in
  let r801 = S (N N_expr) :: r800 in
  let r802 = [R 167] in
  let r803 = S (N N_expr) :: r802 in
  let r804 = [R 168] in
  let r805 = S (N N_expr) :: r804 in
  let r806 = [R 169] in
  let r807 = S (N N_expr) :: r806 in
  let r808 = [R 554] in
  let r809 = S (N N_expr) :: r808 in
  let r810 = [R 170] in
  let r811 = S (N N_expr) :: r810 in
  let r812 = [R 11] in
  let r813 = R 283 :: r812 in
  let r814 = Sub (r138) :: r813 in
  let r815 = R 277 :: r814 in
  let r816 = [R 197] in
  let r817 = Sub (r135) :: r816 in
  let r818 = [R 752] in
  let r819 = Sub (r817) :: r818 in
  let r820 = S (T T_RPAREN) :: r819 in
  let r821 = Sub (r682) :: r820 in
  let r822 = S (T T_TYPE) :: r821 in
  let r823 = [R 247] in
  let r824 = [R 793] in
  let r825 = S (T T_error) :: r824 in
  let r826 = [R 792] in
  let r827 = S (T T_error) :: r826 in
  let r828 = [R 790] in
  let r829 = Sub (r75) :: r828 in
  let r830 = [R 198] in
  let r831 = Sub (r1) :: r830 in
  let r832 = S (T T_EQUAL) :: r831 in
  let r833 = [R 751] in
  let r834 = Sub (r817) :: r833 in
  let r835 = [R 106] in
  let r836 = S (T T_RPAREN) :: r835 in
  let r837 = [R 675] in
  let r838 = S (T T_RPAREN) :: r837 in
  let r839 = [R 694] in
  let r840 = S (T T_error) :: r839 in
  let r841 = [R 692] in
  let r842 = S (T T_RPAREN) :: r841 in
  let r843 = [R 252] in
  let r844 = Sub (r1) :: r843 in
  let r845 = S (T T_EQUAL) :: r844 in
  let r846 = Sub (r75) :: r845 in
  let r847 = S (T T_DOT) :: r846 in
  let r848 = Sub (r682) :: r847 in
  let r849 = [R 251] in
  let r850 = Sub (r1) :: r849 in
  let r851 = S (T T_EQUAL) :: r850 in
  let r852 = Sub (r75) :: r851 in
  let r853 = S (T T_DOT) :: r852 in
  let r854 = [R 250] in
  let r855 = Sub (r1) :: r854 in
  let r856 = S (T T_EQUAL) :: r855 in
  let r857 = [R 254] in
  let r858 = Sub (r1) :: r857 in
  let r859 = S (T T_EQUAL) :: r858 in
  let r860 = Sub (r75) :: r859 in
  let r861 = [R 253] in
  let r862 = Sub (r1) :: r861 in
  let r863 = S (T T_EQUAL) :: r862 in
  let r864 = [R 475] in
  let r865 = [R 481] in
  let r866 = [R 488] in
  let r867 = [R 485] in
  let r868 = [R 474] in
  let r869 = [R 126] in
  let r870 = S (T T_DONE) :: r869 in
  let r871 = Sub (r1) :: r870 in
  let r872 = S (T T_DO) :: r871 in
  let r873 = Sub (r1) :: r872 in
  let r874 = Sub (r697) :: r873 in
  let r875 = Sub (r1) :: r874 in
  let r876 = [R 614] in
  let r877 = S (T T_RBRACKET) :: r876 in
  let r878 = Sub (r1) :: r877 in
  let r879 = [R 622] in
  let r880 = S (T T_RBRACKET) :: r879 in
  let r881 = S (N N_expr) :: r880 in
  let r882 = S (T T_LBRACKET) :: r881 in
  let r883 = [R 624] in
  let r884 = S (T T_RPAREN) :: r883 in
  let r885 = S (N N_expr) :: r884 in
  let r886 = [R 626] in
  let r887 = S (T T_RBRACE) :: r886 in
  let r888 = S (N N_expr) :: r887 in
  let r889 = [R 237] in
  let r890 = Sub (r42) :: r889 in
  let r891 = [R 182] in
  let r892 = S (N N_expr) :: r891 in
  let r893 = [R 181] in
  let r894 = S (N N_expr) :: r893 in
  let r895 = [R 620] in
  let r896 = S (T T_RBRACE) :: r895 in
  let r897 = S (N N_expr) :: r896 in
  let r898 = [R 183] in
  let r899 = S (N N_expr) :: r898 in
  let r900 = [R 178] in
  let r901 = S (N N_expr) :: r900 in
  let r902 = [R 179] in
  let r903 = S (N N_expr) :: r902 in
  let r904 = [R 180] in
  let r905 = S (N N_expr) :: r904 in
  let r906 = [R 185] in
  let r907 = S (N N_expr) :: r906 in
  let r908 = [R 184] in
  let r909 = S (N N_expr) :: r908 in
  let r910 = [R 186] in
  let r911 = S (N N_expr) :: r910 in
  let r912 = [R 177] in
  let r913 = S (N N_expr) :: r912 in
  let r914 = [R 647] in
  let r915 = S (T T_RPAREN) :: r914 in
  let r916 = [R 664] in
  let r917 = S (T T_BARRBRACKET) :: r916 in
  let r918 = [R 663] in
  let r919 = S (T T_BARRBRACKET) :: r918 in
  let r920 = [R 669] in
  let r921 = S (T T_RBRACKET) :: r920 in
  let r922 = [R 668] in
  let r923 = S (T T_RBRACKET) :: r922 in
  let r924 = S (T T_LIDENT) :: r618 in
  let r925 = [R 648] in
  let r926 = S (T T_GREATERRBRACE) :: r925 in
  let r927 = Sub (r924) :: r926 in
  let r928 = [R 658] in
  let r929 = S (T T_RBRACE) :: r928 in
  let r930 = Sub (r627) :: r929 in
  let r931 = [R 513] in
  let r932 = Sub (r625) :: r931 in
  let r933 = [R 630] in
  let r934 = S (T T_END) :: r933 in
  let r935 = [R 202] in
  let r936 = Sub (r668) :: r935 in
  let r937 = S (T T_RPAREN) :: r936 in
  let r938 = Sub (r682) :: r937 in
  let r939 = [R 200] in
  let r940 = Sub (r1) :: r939 in
  let r941 = S (T T_MINUSGREATER) :: r940 in
  let r942 = Sub (r67) :: r941 in
  let r943 = [R 201] in
  let r944 = Sub (r668) :: r943 in
  let r945 = [R 538] in
  let r946 = Sub (r657) :: r945 in
  let r947 = [R 139] in
  let r948 = S (N N_expr) :: r947 in
  let r949 = [R 264] in
  let r950 = Sub (r1) :: r949 in
  let r951 = [R 266] in
  let r952 = [R 129] in
  let r953 = Sub (r1) :: r952 in
  let r954 = S (T T_IN) :: r953 in
  let r955 = Sub (r429) :: r954 in
  let r956 = S (T T_UIDENT) :: r955 in
  let r957 = R 277 :: r956 in
  let r958 = R 189 :: r957 in
  let r959 = [R 335] in
  let r960 = S (N N_module_expr) :: r959 in
  let r961 = S (T T_EQUAL) :: r960 in
  let r962 = S (N N_module_type) :: r961 in
  let r963 = [R 336] in
  let r964 = Sub (r429) :: r963 in
  let r965 = [R 130] in
  let r966 = Sub (r1) :: r965 in
  let r967 = S (T T_IN) :: r966 in
  let r968 = R 277 :: r967 in
  let r969 = R 212 :: r968 in
  let r970 = Sub (r161) :: r969 in
  let r971 = R 277 :: r970 in
  let r972 = R 189 :: r971 in
  let r973 = [R 660] in
  let r974 = S (T T_BARRBRACKET) :: r973 in
  let r975 = [R 820] in
  let r976 = Sub (r1) :: r975 in
  let r977 = [R 816] in
  let r978 = Sub (r75) :: r977 in
  let r979 = S (T T_COLON) :: r978 in
  let r980 = [R 819] in
  let r981 = Sub (r1) :: r980 in
  let r982 = [R 322] in
  let r983 = Sub (r135) :: r982 in
  let r984 = S (T T_LIDENT) :: r983 in
  let r985 = R 503 :: r984 in
  let r986 = R 277 :: r985 in
  let r987 = [R 46] in
  let r988 = R 283 :: r987 in
  let r989 = [R 323] in
  let r990 = Sub (r135) :: r989 in
  let r991 = S (T T_LIDENT) :: r990 in
  let r992 = R 503 :: r991 in
  let r993 = [R 497] in
  let r994 = Sub (r75) :: r993 in
  let r995 = [R 325] in
  let r996 = Sub (r1) :: r995 in
  let r997 = S (T T_EQUAL) :: r996 in
  let r998 = [R 327] in
  let r999 = Sub (r1) :: r998 in
  let r1000 = S (T T_EQUAL) :: r999 in
  let r1001 = Sub (r75) :: r1000 in
  let r1002 = S (T T_DOT) :: r1001 in
  let r1003 = [R 498] in
  let r1004 = Sub (r75) :: r1003 in
  let r1005 = S (T T_DOT) :: r1004 in
  let r1006 = [R 321] in
  let r1007 = Sub (r994) :: r1006 in
  let r1008 = S (T T_COLON) :: r1007 in
  let r1009 = [R 324] in
  let r1010 = Sub (r1) :: r1009 in
  let r1011 = S (T T_EQUAL) :: r1010 in
  let r1012 = [R 326] in
  let r1013 = Sub (r1) :: r1012 in
  let r1014 = S (T T_EQUAL) :: r1013 in
  let r1015 = Sub (r75) :: r1014 in
  let r1016 = S (T T_DOT) :: r1015 in
  let r1017 = [R 226] in
  let r1018 = S (T T_RBRACKET) :: r1017 in
  let r1019 = Sub (r18) :: r1018 in
  let r1020 = Sub (r12) :: r1019 in
  let r1021 = [R 194] in
  let r1022 = S (T T_RBRACKET) :: r1021 in
  let r1023 = Sub (r18) :: r1022 in
  let r1024 = Sub (r12) :: r1023 in
  let r1025 = [R 771] in
  let r1026 = R 283 :: r1025 in
  let r1027 = S (N N_module_expr) :: r1026 in
  let r1028 = R 277 :: r1027 in
  let r1029 = R 189 :: r1028 in
  let r1030 = [R 374] in
  let r1031 = S (T T_STRING) :: r1030 in
  let r1032 = [R 502] in
  let r1033 = R 283 :: r1032 in
  let r1034 = Sub (r1031) :: r1033 in
  let r1035 = S (T T_EQUAL) :: r1034 in
  let r1036 = Sub (r75) :: r1035 in
  let r1037 = S (T T_COLON) :: r1036 in
  let r1038 = Sub (r65) :: r1037 in
  let r1039 = R 277 :: r1038 in
  let r1040 = R 189 :: r1039 in
  let r1041 = S (T T_FALSE) :: r528 in
  let r1042 = [R 749] in
  let r1043 = R 283 :: r1042 in
  let r1044 = R 277 :: r1043 in
  let r1045 = Sub (r1041) :: r1044 in
  let r1046 = S (T T_EQUAL) :: r1045 in
  let r1047 = Sub (r161) :: r1046 in
  let r1048 = R 277 :: r1047 in
  let r1049 = R 189 :: r1048 in
  let r1050 = [R 575] in
  let r1051 = R 283 :: r1050 in
  let r1052 = R 277 :: r1051 in
  let r1053 = R 212 :: r1052 in
  let r1054 = Sub (r161) :: r1053 in
  let r1055 = R 277 :: r1054 in
  let r1056 = R 189 :: r1055 in
  let r1057 = S (T T_RPAREN) :: r150 in
  let r1058 = S (T T_COLONCOLON) :: r553 in
  let r1059 = S (T T_LPAREN) :: r1058 in
  let r1060 = [R 493] in
  let r1061 = [R 219] in
  let r1062 = R 283 :: r1061 in
  let r1063 = R 526 :: r1062 in
  let r1064 = Sub (r196) :: r1063 in
  let r1065 = [R 218] in
  let r1066 = R 283 :: r1065 in
  let r1067 = R 526 :: r1066 in
  let r1068 = Sub (r196) :: r1067 in
  let r1069 = [R 286] in
  let r1070 = R 285 :: r1069 in
  let r1071 = [R 392] in
  let r1072 = R 283 :: r1071 in
  let r1073 = Sub (r457) :: r1072 in
  let r1074 = R 277 :: r1073 in
  let r1075 = R 189 :: r1074 in
  let r1076 = [R 393] in
  let r1077 = R 283 :: r1076 in
  let r1078 = Sub (r457) :: r1077 in
  let r1079 = R 277 :: r1078 in
  let r1080 = R 189 :: r1079 in
  let r1081 = [R 349] in
  let r1082 = S (T T_error) :: r1081 in
  let r1083 = S (T T_COLONEQUAL) :: r1082 in
  let r1084 = S (T T_UIDENT) :: r1083 in
  let r1085 = R 277 :: r1084 in
  let r1086 = [R 337] in
  let r1087 = S (N N_module_type) :: r1086 in
  let r1088 = S (T T_COLON) :: r1087 in
  let r1089 = [R 586] in
  let r1090 = R 283 :: r1089 in
  let r1091 = Sub (r1088) :: r1090 in
  let r1092 = S (T T_UIDENT) :: r1091 in
  let r1093 = R 277 :: r1092 in
  let r1094 = R 189 :: r1093 in
  let r1095 = [R 587] in
  let r1096 = R 283 :: r1095 in
  let r1097 = Sub (r456) :: r1096 in
  let r1098 = [R 348] in
  let r1099 = R 283 :: r1098 in
  let r1100 = [R 338] in
  let r1101 = Sub (r1088) :: r1100 in
  let r1102 = [R 589] in
  let r1103 = R 275 :: r1102 in
  let r1104 = R 283 :: r1103 in
  let r1105 = S (N N_module_type) :: r1104 in
  let r1106 = S (T T_COLON) :: r1105 in
  let r1107 = S (T T_UIDENT) :: r1106 in
  let r1108 = [R 276] in
  let r1109 = R 275 :: r1108 in
  let r1110 = R 283 :: r1109 in
  let r1111 = S (N N_module_type) :: r1110 in
  let r1112 = S (T T_COLON) :: r1111 in
  let r1113 = S (T T_UIDENT) :: r1112 in
  let r1114 = R 277 :: r1113 in
  let r1115 = [R 592] in
  let r1116 = R 283 :: r1115 in
  let r1117 = S (N N_module_type) :: r1116 in
  let r1118 = R 277 :: r1117 in
  let r1119 = R 189 :: r1118 in
  let r1120 = [R 90] in
  let r1121 = S (T T_LIDENT) :: r1120 in
  let r1122 = [R 69] in
  let r1123 = Sub (r1121) :: r1122 in
  let r1124 = [R 85] in
  let r1125 = Sub (r1123) :: r1124 in
  let r1126 = [R 593] in
  let r1127 = R 269 :: r1126 in
  let r1128 = R 283 :: r1127 in
  let r1129 = Sub (r1125) :: r1128 in
  let r1130 = S (T T_COLON) :: r1129 in
  let r1131 = S (T T_LIDENT) :: r1130 in
  let r1132 = R 195 :: r1131 in
  let r1133 = R 822 :: r1132 in
  let r1134 = R 277 :: r1133 in
  let r1135 = R 189 :: r1134 in
  let r1136 = [R 89] in
  let r1137 = R 271 :: r1136 in
  let r1138 = R 283 :: r1137 in
  let r1139 = Sub (r1123) :: r1138 in
  let r1140 = S (T T_EQUAL) :: r1139 in
  let r1141 = S (T T_LIDENT) :: r1140 in
  let r1142 = R 195 :: r1141 in
  let r1143 = R 822 :: r1142 in
  let r1144 = R 277 :: r1143 in
  let r1145 = R 189 :: r1144 in
  let r1146 = [R 196] in
  let r1147 = S (T T_RBRACKET) :: r1146 in
  let r1148 = [R 72] in
  let r1149 = S (T T_END) :: r1148 in
  let r1150 = R 292 :: r1149 in
  let r1151 = R 62 :: r1150 in
  let r1152 = R 277 :: r1151 in
  let r1153 = [R 61] in
  let r1154 = S (T T_RPAREN) :: r1153 in
  let r1155 = [R 64] in
  let r1156 = R 283 :: r1155 in
  let r1157 = Sub (r75) :: r1156 in
  let r1158 = S (T T_COLON) :: r1157 in
  let r1159 = S (T T_LIDENT) :: r1158 in
  let r1160 = R 366 :: r1159 in
  let r1161 = [R 65] in
  let r1162 = R 283 :: r1161 in
  let r1163 = Sub (r994) :: r1162 in
  let r1164 = S (T T_COLON) :: r1163 in
  let r1165 = S (T T_LIDENT) :: r1164 in
  let r1166 = R 505 :: r1165 in
  let r1167 = [R 79] in
  let r1168 = Sub (r46) :: r1167 in
  let r1169 = [R 35] in
  let r1170 = Sub (r1168) :: r1169 in
  let r1171 = [R 51] in
  let r1172 = Sub (r1170) :: r1171 in
  let r1173 = S (T T_EQUAL) :: r1172 in
  let r1174 = [R 769] in
  let r1175 = R 267 :: r1174 in
  let r1176 = R 283 :: r1175 in
  let r1177 = Sub (r1173) :: r1176 in
  let r1178 = S (T T_LIDENT) :: r1177 in
  let r1179 = R 195 :: r1178 in
  let r1180 = R 822 :: r1179 in
  let r1181 = R 277 :: r1180 in
  let r1182 = R 189 :: r1181 in
  let r1183 = [R 81] in
  let r1184 = S (T T_error) :: r1183 in
  let r1185 = R 294 :: r1184 in
  let r1186 = R 60 :: r1185 in
  let r1187 = R 277 :: r1186 in
  let r1188 = [R 48] in
  let r1189 = R 283 :: r1188 in
  let r1190 = Sub (r1) :: r1189 in
  let r1191 = [R 43] in
  let r1192 = R 283 :: r1191 in
  let r1193 = R 431 :: r1192 in
  let r1194 = Sub (r1170) :: r1193 in
  let r1195 = [R 44] in
  let r1196 = R 283 :: r1195 in
  let r1197 = R 431 :: r1196 in
  let r1198 = Sub (r1170) :: r1197 in
  let r1199 = [R 109] in
  let r1200 = Sub (r75) :: r1199 in
  let r1201 = S (T T_EQUAL) :: r1200 in
  let r1202 = Sub (r75) :: r1201 in
  let r1203 = [R 47] in
  let r1204 = R 283 :: r1203 in
  let r1205 = Sub (r1202) :: r1204 in
  let r1206 = [R 49] in
  let r1207 = [R 295] in
  let r1208 = [R 77] in
  let r1209 = S (T T_RPAREN) :: r1208 in
  let r1210 = Sub (r1170) :: r1209 in
  let r1211 = [R 38] in
  let r1212 = Sub (r1170) :: r1211 in
  let r1213 = S (T T_IN) :: r1212 in
  let r1214 = Sub (r456) :: r1213 in
  let r1215 = R 277 :: r1214 in
  let r1216 = S (T T_OPEN) :: r1215 in
  let r1217 = [R 258] in
  let r1218 = R 283 :: r1217 in
  let r1219 = Sub (r138) :: r1218 in
  let r1220 = R 510 :: r1219 in
  let r1221 = R 277 :: r1220 in
  let r1222 = [R 39] in
  let r1223 = Sub (r1170) :: r1222 in
  let r1224 = S (T T_IN) :: r1223 in
  let r1225 = Sub (r456) :: r1224 in
  let r1226 = R 277 :: r1225 in
  let r1227 = [R 545] in
  let r1228 = Sub (r75) :: r1227 in
  let r1229 = [R 80] in
  let r1230 = Sub (r46) :: r1229 in
  let r1231 = S (T T_RBRACKET) :: r1230 in
  let r1232 = Sub (r1228) :: r1231 in
  let r1233 = [R 546] in
  let r1234 = [R 54] in
  let r1235 = Sub (r1170) :: r1234 in
  let r1236 = S (T T_MINUSGREATER) :: r1235 in
  let r1237 = Sub (r665) :: r1236 in
  let r1238 = [R 36] in
  let r1239 = Sub (r1237) :: r1238 in
  let r1240 = R 277 :: r1239 in
  let r1241 = [R 37] in
  let r1242 = Sub (r1170) :: r1241 in
  let r1243 = S (T T_IN) :: r1242 in
  let r1244 = [R 257] in
  let r1245 = R 283 :: r1244 in
  let r1246 = Sub (r138) :: r1245 in
  let r1247 = [R 82] in
  let r1248 = S (T T_RPAREN) :: r1247 in
  let r1249 = Sub (r1125) :: r1248 in
  let r1250 = [R 63] in
  let r1251 = R 283 :: r1250 in
  let r1252 = Sub (r1123) :: r1251 in
  let r1253 = [R 75] in
  let r1254 = Sub (r1123) :: r1253 in
  let r1255 = S (T T_IN) :: r1254 in
  let r1256 = Sub (r456) :: r1255 in
  let r1257 = R 277 :: r1256 in
  let r1258 = S (T T_OPEN) :: r1257 in
  let r1259 = [R 76] in
  let r1260 = Sub (r1123) :: r1259 in
  let r1261 = S (T T_IN) :: r1260 in
  let r1262 = Sub (r456) :: r1261 in
  let r1263 = R 277 :: r1262 in
  let r1264 = [R 70] in
  let r1265 = Sub (r1121) :: r1264 in
  let r1266 = S (T T_RBRACKET) :: r1265 in
  let r1267 = Sub (r1228) :: r1266 in
  let r1268 = [R 91] in
  let r1269 = S (T T_LIDENT) :: r1268 in
  let r1270 = S (T T_DOT) :: r1269 in
  let r1271 = [R 66] in
  let r1272 = R 283 :: r1271 in
  let r1273 = Sub (r1202) :: r1272 in
  let r1274 = [R 67] in
  let r1275 = [R 293] in
  let r1276 = [R 87] in
  let r1277 = Sub (r1125) :: r1276 in
  let r1278 = S (T T_MINUSGREATER) :: r1277 in
  let r1279 = Sub (r69) :: r1278 in
  let r1280 = S (T T_COLON) :: r1279 in
  let r1281 = [R 88] in
  let r1282 = Sub (r1125) :: r1281 in
  let r1283 = S (T T_MINUSGREATER) :: r1282 in
  let r1284 = [R 86] in
  let r1285 = Sub (r1125) :: r1284 in
  let r1286 = S (T T_MINUSGREATER) :: r1285 in
  let r1287 = Sub (r69) :: r1286 in
  let r1288 = [R 432] in
  let r1289 = [R 52] in
  let r1290 = Sub (r1170) :: r1289 in
  let r1291 = S (T T_EQUAL) :: r1290 in
  let r1292 = Sub (r1125) :: r1291 in
  let r1293 = [R 53] in
  let r1294 = Sub (r1173) :: r1293 in
  let r1295 = [R 268] in
  let r1296 = R 267 :: r1295 in
  let r1297 = R 283 :: r1296 in
  let r1298 = Sub (r1173) :: r1297 in
  let r1299 = S (T T_LIDENT) :: r1298 in
  let r1300 = R 195 :: r1299 in
  let r1301 = R 822 :: r1300 in
  let r1302 = R 277 :: r1301 in
  let r1303 = [R 291] in
  let r1304 = R 288 :: r1303 in
  let r1305 = [R 757] in
  let r1306 = R 283 :: r1305 in
  let r1307 = [R 761] in
  let r1308 = R 279 :: r1307 in
  let r1309 = [R 280] in
  let r1310 = R 279 :: r1309 in
  let r1311 = R 283 :: r1310 in
  let r1312 = R 526 :: r1311 in
  let r1313 = R 794 :: r1312 in
  let r1314 = S (T T_LIDENT) :: r1313 in
  let r1315 = R 799 :: r1314 in
  let r1316 = R 277 :: r1315 in
  let r1317 = [R 754] in
  let r1318 = R 288 :: r1317 in
  let r1319 = R 283 :: r1318 in
  let r1320 = [R 272] in
  let r1321 = R 271 :: r1320 in
  let r1322 = R 283 :: r1321 in
  let r1323 = Sub (r1123) :: r1322 in
  let r1324 = S (T T_EQUAL) :: r1323 in
  let r1325 = S (T T_LIDENT) :: r1324 in
  let r1326 = R 195 :: r1325 in
  let r1327 = R 822 :: r1326 in
  let r1328 = R 277 :: r1327 in
  let r1329 = [R 270] in
  let r1330 = R 269 :: r1329 in
  let r1331 = R 283 :: r1330 in
  let r1332 = Sub (r1125) :: r1331 in
  let r1333 = S (T T_COLON) :: r1332 in
  let r1334 = S (T T_LIDENT) :: r1333 in
  let r1335 = R 195 :: r1334 in
  let r1336 = R 822 :: r1335 in
  let r1337 = R 277 :: r1336 in
  let r1338 = [R 287] in
  let r1339 = R 285 :: r1338 in
  let r1340 = [R 577] in
  let r1341 = R 283 :: r1340 in
  let r1342 = [R 581] in
  let r1343 = R 279 :: r1342 in
  let r1344 = [R 582] in
  let r1345 = R 281 :: r1344 in
  let r1346 = [R 282] in
  let r1347 = R 281 :: r1346 in
  let r1348 = R 283 :: r1347 in
  let r1349 = R 526 :: r1348 in
  let r1350 = Sub (r196) :: r1349 in
  let r1351 = S (T T_COLONEQUAL) :: r1350 in
  let r1352 = S (T T_LIDENT) :: r1351 in
  let r1353 = R 799 :: r1352 in
  let r1354 = R 277 :: r1353 in
  let r1355 = [R 634] in
  let r1356 = S (T T_RPAREN) :: r1355 in
  let r1357 = S (N N_module_expr) :: r1356 in
  let r1358 = R 277 :: r1357 in
  let r1359 = [R 636] in
  let r1360 = S (T T_error) :: r1359 in
  let r1361 = [R 635] in
  let r1362 = S (T T_RPAREN) :: r1361 in
  let r1363 = [R 609] in
  let r1364 = S (T T_RPAREN) :: r1363 in
  let r1365 = [R 611] in
  let r1366 = S (T T_RPAREN) :: r1365 in
  let r1367 = [R 456] in
  let r1368 = S (T T_error) :: r1367 in
  let r1369 = [R 454] in
  let r1370 = S (T T_RPAREN) :: r1369 in
  let r1371 = [R 455] in
  let r1372 = S (T T_error) :: r1371 in
  let r1373 = [R 452] in
  let r1374 = S (T T_RPAREN) :: r1373 in
  let r1375 = [R 453] in
  let r1376 = S (T T_RPAREN) :: r1375 in
  let r1377 = S (N N_module_type) :: r1376 in
  let r1378 = [R 447] in
  let r1379 = S (T T_RPAREN) :: r1378 in
  let r1380 = S (N N_module_type) :: r1379 in
  let r1381 = [R 766] in
  let r1382 = R 273 :: r1381 in
  let r1383 = R 283 :: r1382 in
  let r1384 = Sub (r429) :: r1383 in
  let r1385 = S (T T_UIDENT) :: r1384 in
  let r1386 = [R 274] in
  let r1387 = R 273 :: r1386 in
  let r1388 = R 283 :: r1387 in
  let r1389 = Sub (r429) :: r1388 in
  let r1390 = S (T T_UIDENT) :: r1389 in
  let r1391 = R 277 :: r1390 in
  let r1392 = [R 496] in
  let r1393 = [R 192] in
  let r1394 = R 277 :: r1393 in
  let r1395 = Sub (r1041) :: r1394 in
  let r1396 = [R 193] in
  let r1397 = R 277 :: r1396 in
  let r1398 = Sub (r1041) :: r1397 in
  let r1399 = [R 290] in
  let r1400 = R 288 :: r1399 in
  let r1401 = R 283 :: r1400 in
  let r1402 = [R 122] in
  let r1403 = S (N N_match_cases) :: r1402 in
  let r1404 = [R 124] in
  let r1405 = Sub (r1) :: r1404 in
  let r1406 = [R 123] in
  let r1407 = Sub (r1) :: r1406 in
  let r1408 = [R 316] in
  let r1409 = [R 224] in
  let r1410 = [R 225] in
  let r1411 = [R 458] in
  let r1412 = [R 459] in
  let r1413 = [R 460] in
  let r1414 = [R 776] in
  let r1415 = [R 785] in
  let r1416 = [R 297] in
  let r1417 = [R 783] in
  let r1418 = S (T T_SEMISEMI) :: r1417 in
  let r1419 = [R 784] in
  let r1420 = [R 299] in
  let r1421 = [R 302] in
  let r1422 = [R 301] in
  let r1423 = [R 300] in
  let r1424 = R 298 :: r1423 in
  let r1425 = [R 808] in
  let r1426 = S (T T_EOF) :: r1425 in
  let r1427 = R 298 :: r1426 in
  let r1428 = [R 807] in
  function
  | 0 | 3159 | 3163 | 3167 | 3171 | 3175 | 3196 -> Nothing
  | 3158 -> One ([R 0])
  | 3162 -> One ([R 1])
  | 3164 -> One ([R 2])
  | 3170 -> One ([R 3])
  | 3174 -> One ([R 4])
  | 3186 -> One ([R 5])
  | 3206 -> One ([R 6])
  | 93 -> One ([R 7])
  | 92 -> One ([R 8])
  | 340 | 665 -> One ([R 16])
  | 358 | 678 -> One ([R 17])
  | 354 | 674 -> One ([R 31])
  | 2182 | 2304 -> One ([R 40])
  | 2179 | 2301 -> One ([R 41])
  | 2177 | 2299 -> One ([R 42])
  | 2146 -> One ([R 50])
  | 2185 | 2306 -> One ([R 55])
  | 2238 -> One ([R 68])
  | 2219 | 2255 | 2333 | 2350 -> One ([R 71])
  | 2234 | 2406 -> One ([R 73])
  | 2222 | 2336 -> One ([R 74])
  | 2197 | 2277 -> One ([R 78])
  | 2261 | 2281 -> One ([R 83])
  | 2143 | 2274 -> One ([R 84])
  | 795 | 879 -> One ([R 93])
  | 73 | 267 -> One ([R 94])
  | 793 | 877 -> One ([R 95])
  | 301 | 323 | 443 | 2563 -> One ([R 96])
  | 302 | 324 -> One ([R 101])
  | 1980 | 2584 -> One ([R 102])
  | 72 | 266 -> One ([R 108])
  | 442 | 2926 -> One ([R 113])
  | 463 | 2928 -> One ([R 114])
  | 388 | 700 -> One ([R 116])
  | 1281 -> One ([R 117])
  | 1090 | 1333 -> One ([R 128])
  | 2838 | 3136 -> One ([R 138])
  | 2474 | 3113 -> One ([R 146])
  | 1494 | 1683 -> One ([R 147])
  | 1126 | 1347 -> One ([R 148])
  | 1147 | 1364 -> One ([R 149])
  | 1129 | 1350 -> One ([R 150])
  | 1145 | 1362 -> One ([R 187])
  | 64 | 504 -> One ([R 188])
  | 561 -> One ([R 209])
  | 560 -> One ([R 210])
  | 567 -> One ([R 211])
  | 204 | 225 | 609 | 724 -> One ([R 222])
  | 437 -> One ([R 230])
  | 438 -> One ([R 231])
  | 1493 | 1682 -> One ([R 236])
  | 1276 | 2789 -> One ([R 256])
  | 2183 -> One ([R 259])
  | 1023 -> One ([R 260])
  | 940 -> One (R 277 :: r579)
  | 1888 -> One (R 277 :: r992)
  | 2090 -> One (R 277 :: r1160)
  | 2101 -> One (R 277 :: r1166)
  | 2124 -> One (R 277 :: r1190)
  | 2128 -> One (R 277 :: r1194)
  | 2129 -> One (R 277 :: r1198)
  | 2134 -> One (R 277 :: r1205)
  | 2203 -> One (R 277 :: r1252)
  | 2229 -> One (R 277 :: r1273)
  | 2905 -> One (R 277 :: r1408)
  | 2144 -> One (R 283 :: r1206)
  | 2236 -> One (R 283 :: r1274)
  | 3191 -> One (R 283 :: r1418)
  | 3202 -> One (R 283 :: r1424)
  | 3207 -> One (R 283 :: r1427)
  | 2239 -> One (R 292 :: r1275)
  | 2147 -> One (R 294 :: r1207)
  | 3189 -> One (R 296 :: r1416)
  | 3197 -> One (R 298 :: r1420)
  | 3198 -> One (R 298 :: r1421)
  | 3199 -> One (R 298 :: r1422)
  | 866 -> One ([R 304])
  | 870 -> One ([R 306])
  | 1136 | 2786 -> One ([R 308])
  | 1277 | 2785 -> One ([R 309])
  | 1591 | 1749 -> One ([R 312])
  | 1594 | 1752 -> One ([R 313])
  | 2907 -> One ([R 314])
  | 636 -> One ([R 315])
  | 635 -> One ([R 317])
  | 634 -> One ([R 319])
  | 631 -> One ([R 320])
  | 2811 | 3094 -> One ([R 340])
  | 747 | 2515 -> One ([R 342])
  | 1068 | 2527 -> One ([R 343])
  | 1069 | 2528 -> One ([R 344])
  | 1067 | 2526 -> One ([R 345])
  | 1070 | 2530 -> One ([R 347])
  | 3074 | 3102 -> One ([R 351])
  | 729 | 732 -> One ([R 355])
  | 606 | 721 -> One ([R 356])
  | 571 | 618 -> One ([R 357])
  | 612 | 727 -> One ([R 360])
  | 611 | 726 -> One ([R 362])
  | 421 | 1874 -> One ([R 365])
  | 2094 -> One ([R 367])
  | 2092 -> One ([R 368])
  | 2095 -> One ([R 369])
  | 2093 -> One ([R 370])
  | 1034 -> One ([R 373])
  | 1970 | 2714 -> One ([R 375])
  | 478 | 2941 -> One ([R 376])
  | 468 | 2933 -> One ([R 377])
  | 491 | 2954 -> One ([R 378])
  | 469 | 2934 -> One ([R 379])
  | 490 | 2953 -> One ([R 380])
  | 485 | 2948 -> One ([R 381])
  | 159 | 761 -> One ([R 394])
  | 169 | 1073 -> One ([R 395])
  | 192 -> One ([R 396])
  | 182 -> One ([R 398])
  | 185 -> One ([R 400])
  | 188 -> One ([R 402])
  | 176 -> One ([R 403])
  | 191 | 1320 -> One ([R 404])
  | 175 -> One ([R 405])
  | 174 -> One ([R 406])
  | 173 -> One ([R 407])
  | 172 -> One ([R 408])
  | 171 -> One ([R 409])
  | 162 | 269 | 1058 -> One ([R 410])
  | 161 | 1057 -> One ([R 411])
  | 160 -> One ([R 412])
  | 168 | 1072 | 1222 -> One ([R 413])
  | 167 | 1071 -> One ([R 414])
  | 158 -> One ([R 415])
  | 163 -> One ([R 416])
  | 178 -> One ([R 417])
  | 170 -> One ([R 418])
  | 177 -> One ([R 419])
  | 164 -> One ([R 420])
  | 190 -> One ([R 421])
  | 193 -> One ([R 422])
  | 194 -> One ([R 423])
  | 189 -> One ([R 424])
  | 375 -> One ([R 425])
  | 374 -> One (R 426 :: r306)
  | 329 -> One ([R 428])
  | 867 -> One (R 429 :: r564)
  | 868 -> One ([R 430])
  | 1539 -> One ([R 444])
  | 210 -> One ([R 445])
  | 2524 | 2536 -> One ([R 448])
  | 2520 | 2532 -> One ([R 450])
  | 2498 | 2848 -> One ([R 457])
  | 824 | 918 -> One ([R 462])
  | 817 | 911 -> One ([R 463])
  | 849 | 937 -> One ([R 464])
  | 818 | 912 -> One ([R 466])
  | 822 | 916 -> One ([R 467])
  | 840 | 933 -> One ([R 469])
  | 837 | 927 -> One ([R 473])
  | 1258 -> One ([R 476])
  | 816 | 910 | 1074 -> One ([R 477])
  | 1269 -> One ([R 482])
  | 1270 -> One ([R 483])
  | 1268 -> One ([R 484])
  | 1271 -> One ([R 486])
  | 1261 -> One ([R 487])
  | 1264 -> One ([R 489])
  | 1029 -> One ([R 490])
  | 2466 -> One ([R 494])
  | 1890 | 1926 -> One ([R 504])
  | 2105 -> One ([R 506])
  | 2103 -> One ([R 507])
  | 2106 -> One ([R 508])
  | 2104 -> One ([R 509])
  | 2192 -> One (R 510 :: r1246)
  | 254 -> One ([R 511])
  | 466 | 2931 -> One ([R 514])
  | 467 | 2932 -> One ([R 515])
  | 465 | 2930 -> One ([R 516])
  | 2602 | 2888 -> One ([R 518])
  | 2601 | 2887 -> One ([R 519])
  | 2603 | 2889 -> One ([R 520])
  | 2598 | 2884 -> One ([R 521])
  | 2599 | 2885 -> One ([R 522])
  | 2007 | 2966 -> One ([R 524])
  | 2005 | 2964 -> One ([R 525])
  | 613 -> One ([R 528])
  | 568 -> One ([R 529])
  | 1496 | 1685 -> One ([R 530])
  | 1495 | 1684 -> One ([R 531])
  | 403 -> One ([R 533])
  | 1598 | 1756 -> One ([R 536])
  | 367 -> One ([R 557])
  | 1515 -> One ([R 560])
  | 1516 -> One ([R 561])
  | 1817 -> One ([R 563])
  | 1818 -> One ([R 564])
  | 856 -> One ([R 566])
  | 857 -> One ([R 567])
  | 1542 -> One ([R 569])
  | 1543 -> One ([R 570])
  | 1150 | 1367 -> One ([R 572])
  | 1154 | 1371 -> One ([R 573])
  | 2461 | 3070 -> One ([R 578])
  | 2438 | 3047 -> One ([R 579])
  | 2441 | 3050 -> One ([R 580])
  | 2440 | 3049 -> One ([R 585])
  | 2444 | 3053 -> One ([R 588])
  | 2443 | 3052 -> One ([R 590])
  | 2442 | 3051 -> One ([R 591])
  | 2462 | 3071 -> One ([R 594])
  | 262 | 516 -> One ([R 597])
  | 259 | 271 -> One ([R 599])
  | 773 | 785 -> One ([R 601])
  | 853 | 902 -> One ([R 603])
  | 862 | 2831 -> One ([R 606])
  | 2493 | 3129 -> One ([R 610])
  | 1345 | 1600 -> One ([R 613])
  | 1415 | 1604 -> One ([R 615])
  | 1447 | 1636 -> One ([R 617])
  | 1441 | 1630 -> One ([R 619])
  | 1453 | 1642 -> One ([R 621])
  | 1429 | 1618 -> One ([R 623])
  | 1425 | 1614 -> One ([R 625])
  | 1433 | 1622 -> One ([R 627])
  | 1419 | 1608 -> One ([R 629])
  | 1572 | 1768 -> One ([R 632])
  | 1049 | 1304 -> One ([R 637])
  | 1132 | 1323 -> One ([R 638])
  | 1131 | 1146 | 1322 | 1363 -> One ([R 639])
  | 1052 | 1128 | 1307 | 1349 -> One ([R 640])
  | 990 | 1821 -> One ([R 644])
  | 1435 | 1490 | 1624 | 1679 -> One ([R 646])
  | 1524 | 1563 -> One ([R 649])
  | 1130 | 1321 -> One ([R 652])
  | 1507 | 1711 -> One ([R 655])
  | 1775 | 1778 -> One ([R 657])
  | 1546 | 1567 -> One ([R 659])
  | 1837 | 2478 -> One ([R 661])
  | 1512 | 1554 -> One ([R 665])
  | 1781 | 1830 -> One ([R 667])
  | 1520 | 1559 -> One ([R 670])
  | 798 | 880 -> One ([R 673])
  | 801 | 883 -> One ([R 676])
  | 802 | 884 -> One ([R 680])
  | 859 | 906 -> One ([R 682])
  | 806 | 888 -> One ([R 683])
  | 858 | 904 -> One ([R 685])
  | 834 | 897 -> One ([R 689])
  | 811 | 892 -> One ([R 690])
  | 1226 | 2822 -> One ([R 691])
  | 1231 | 2827 -> One ([R 693])
  | 2819 | 3078 -> One ([R 695])
  | 827 | 905 -> One ([R 696])
  | 28 | 121 -> One ([R 697])
  | 8 | 101 -> One ([R 698])
  | 52 | 145 -> One ([R 700])
  | 51 | 144 -> One ([R 701])
  | 50 | 143 -> One ([R 702])
  | 49 | 142 -> One ([R 703])
  | 48 | 141 -> One ([R 704])
  | 47 | 140 -> One ([R 705])
  | 46 | 139 -> One ([R 706])
  | 45 | 138 -> One ([R 707])
  | 44 | 137 -> One ([R 708])
  | 43 | 136 -> One ([R 709])
  | 42 | 135 -> One ([R 710])
  | 41 | 134 -> One ([R 711])
  | 40 | 133 -> One ([R 712])
  | 39 | 132 -> One ([R 713])
  | 38 | 131 -> One ([R 714])
  | 37 | 130 -> One ([R 715])
  | 36 | 129 -> One ([R 716])
  | 35 | 128 -> One ([R 717])
  | 34 | 127 -> One ([R 718])
  | 33 | 126 -> One ([R 719])
  | 32 | 125 -> One ([R 720])
  | 31 | 124 -> One ([R 721])
  | 30 | 123 -> One ([R 722])
  | 29 | 122 -> One ([R 723])
  | 27 | 120 -> One ([R 724])
  | 26 | 119 -> One ([R 725])
  | 25 | 118 -> One ([R 726])
  | 24 | 117 -> One ([R 727])
  | 23 | 116 -> One ([R 728])
  | 22 | 115 -> One ([R 729])
  | 21 | 114 -> One ([R 730])
  | 20 | 113 -> One ([R 731])
  | 19 | 112 -> One ([R 732])
  | 18 | 111 -> One ([R 733])
  | 17 | 110 -> One ([R 734])
  | 16 | 109 -> One ([R 735])
  | 15 | 108 -> One ([R 736])
  | 14 | 107 -> One ([R 737])
  | 13 | 106 -> One ([R 738])
  | 12 | 105 -> One ([R 739])
  | 11 | 104 -> One ([R 740])
  | 10 | 103 -> One ([R 741])
  | 9 | 102 -> One ([R 742])
  | 7 | 100 -> One ([R 743])
  | 6 | 99 -> One ([R 744])
  | 5 | 98 -> One ([R 745])
  | 4 | 97 -> One ([R 746])
  | 3 | 96 -> One ([R 747])
  | 2375 | 2776 -> One ([R 748])
  | 2403 | 2816 -> One ([R 753])
  | 2379 | 2402 | 2780 | 2807 -> One ([R 755])
  | 2381 | 2404 | 2788 | 2809 -> One ([R 756])
  | 2394 | 2802 -> One ([R 758])
  | 2376 | 2777 -> One ([R 759])
  | 2371 | 2772 -> One ([R 760])
  | 2374 | 2775 -> One ([R 764])
  | 2378 | 2779 -> One ([R 767])
  | 2377 | 2778 -> One ([R 768])
  | 2395 | 2803 -> One ([R 770])
  | 242 -> One ([R 772])
  | 241 -> One ([R 773])
  | 3179 -> One ([R 777])
  | 3180 -> One ([R 778])
  | 3182 -> One ([R 779])
  | 3183 -> One ([R 780])
  | 3181 -> One ([R 781])
  | 3178 -> One ([R 782])
  | 3185 -> One ([R 786])
  | 343 | 668 -> One ([R 788])
  | 1211 | 1536 -> One ([R 789])
  | 1208 | 1533 -> One ([R 791])
  | 592 -> One ([R 800])
  | 284 -> One ([R 805])
  | 286 -> One ([R 806])
  | 198 | 249 | 546 | 960 | 993 | 1550 -> One ([R 809])
  | 196 | 847 -> One ([R 811])
  | 1050 | 1305 -> One ([R 814])
  | 1569 | 1570 -> One ([R 815])
  | 2076 -> One ([R 823])
  | 1872 -> One ([R 824])
  | 1875 -> One ([R 825])
  | 1873 -> One ([R 826])
  | 1924 -> One ([R 827])
  | 1927 -> One ([R 828])
  | 1925 -> One ([R 829])
  | 581 -> One ([R 834])
  | 582 -> One ([R 835])
  | 1266 -> One (S (T T_error) :: r867)
  | 1527 -> One (S (T T_WITH) :: r932)
  | 3187 -> One (S (T T_SEMISEMI) :: r1415)
  | 3194 -> One (S (T T_SEMISEMI) :: r1419)
  | 559 -> One (S (T T_RPAREN) :: r452)
  | 183 -> One (S (T T_RBRACKET) :: r87)
  | 186 -> One (S (T T_RBRACE) :: r88)
  | 179 -> One (S (T T_LPAREN) :: r86)
  | 207 -> One (S (T T_LIDENT) :: r97)
  | 422 -> One (S (T T_LIDENT) :: r349)
  | 964 -> One (S (T T_LIDENT) :: r604)
  | 972 -> One (S (T T_LIDENT) :: r610)
  | 1876 -> One (S (T T_LIDENT) :: r979)
  | 1928 -> One (S (T T_LIDENT) :: r1008)
  | 2264 -> One (S (T T_LIDENT) :: r1288)
  | 1701 -> One (S (T T_EQUAL) :: r950)
  | 1864 -> One (S (T T_EQUAL) :: r976)
  | 1884 -> One (S (T T_EQUAL) :: r981)
  | 3156 -> One (S (T T_EOF) :: r1409)
  | 3160 -> One (S (T T_EOF) :: r1410)
  | 3165 -> One (S (T T_EOF) :: r1411)
  | 3168 -> One (S (T T_EOF) :: r1412)
  | 3172 -> One (S (T T_EOF) :: r1413)
  | 3211 -> One (S (T T_EOF) :: r1428)
  | 425 -> One (S (T T_DOT) :: r351)
  | 310 -> One (S (T T_COLON) :: r214)
  | 563 -> One (S (T T_COLON) :: r455)
  | 543 -> One (S (N N_pattern) :: r419)
  | 766 -> One (S (N N_pattern) :: r508)
  | 779 -> One (S (N N_pattern) :: r517)
  | 1257 -> One (S (N N_pattern) :: r864)
  | 1260 -> One (S (N N_pattern) :: r865)
  | 1263 -> One (S (N N_pattern) :: r866)
  | 1272 -> One (S (N N_pattern) :: r868)
  | 1022 -> One (S (N N_let_pattern) :: r679)
  | 978 -> One (S (N N_expr) :: r619)
  | 975 -> One (Sub (r1) :: r614)
  | 2555 -> One (Sub (r1) :: r1392)
  | 2153 -> One (Sub (r12) :: r1221)
  | 378 -> One (Sub (r73) :: r307)
  | 412 -> One (Sub (r75) :: r327)
  | 416 -> One (Sub (r75) :: r330)
  | 776 -> One (Sub (r75) :: r516)
  | 929 -> One (Sub (r75) :: r566)
  | 967 -> One (Sub (r75) :: r609)
  | 1024 -> One (Sub (r75) :: r680)
  | 1075 -> One (Sub (r75) :: r724)
  | 2086 -> One (Sub (r75) :: r1154)
  | 2168 -> One (Sub (r75) :: r1233)
  | 290 -> One (Sub (r93) :: r193)
  | 426 -> One (Sub (r93) :: r352)
  | 3176 -> One (Sub (r93) :: r1414)
  | 1989 -> One (Sub (r105) :: r1060)
  | 1707 -> One (Sub (r138) :: r951)
  | 296 -> One (Sub (r188) :: r194)
  | 287 -> One (Sub (r190) :: r192)
  | 2078 -> One (Sub (r190) :: r1147)
  | 371 -> One (Sub (r254) :: r301)
  | 1202 -> One (Sub (r520) :: r823)
  | 939 -> One (Sub (r572) :: r574)
  | 965 -> One (Sub (r606) :: r608)
  | 973 -> One (Sub (r606) :: r613)
  | 1020 -> One (Sub (r675) :: r676)
  | 1907 -> One (Sub (r682) :: r1002)
  | 1941 -> One (Sub (r682) :: r1016)
  | 1887 -> One (Sub (r986) :: r988)
  | 1906 -> One (Sub (r994) :: r997)
  | 1940 -> One (Sub (r994) :: r1011)
  | 1140 | 1357 -> One (r0)
  | 3118 | 3155 -> One (r2)
  | 3117 | 3154 -> One (r3)
  | 3116 | 3153 -> One (r4)
  | 3115 | 3152 -> One (r5)
  | 91 | 3151 -> One (r6)
  | 58 | 90 -> One (r7)
  | 1 | 89 -> One (r8)
  | 53 | 146 -> One (r9)
  | 54 | 147 -> One (r11)
  | 57 | 150 -> One (r13)
  | 2 | 95 -> One (r14)
  | 56 | 149 -> One (r15)
  | 55 | 148 -> One (r16)
  | 2396 -> One (r17)
  | 3109 | 3150 -> One (r19)
  | 3108 | 3149 -> One (r20)
  | 60 | 153 -> One (r21)
  | 59 | 152 -> One (r22)
  | 3092 | 3148 -> One (r23)
  | 3091 | 3147 -> One (r24)
  | 3090 | 3146 -> One (r25)
  | 3089 | 3145 -> One (r26)
  | 63 | 240 -> One (r27)
  | 62 | 239 -> One (r28)
  | 61 | 238 -> One (r29)
  | 65 | 265 -> One (r30)
  | 2840 | 3138 -> One (r31)
  | 68 | 507 -> One (r32)
  | 67 | 506 -> One (r33)
  | 66 | 505 -> One (r34)
  | 2839 | 3137 -> One (r35)
  | 2837 | 3135 -> One (r36)
  | 2836 | 3134 -> One (r37)
  | 71 | 510 -> One (r38)
  | 70 | 509 -> One (r39)
  | 69 | 508 -> One (r40)
  | 74 | 754 -> One (r41)
  | 2480 | 3133 -> One (r43)
  | 75 | 755 -> One (r44)
  | 79 | 660 -> One (r45)
  | 84 | 759 -> One (r47)
  | 78 | 758 -> One (r48)
  | 77 | 757 -> One (r49)
  | 76 | 756 -> One (r50)
  | 83 | 663 -> One (r51)
  | 81 | 662 -> One (r52)
  | 80 | 661 -> One (r53)
  | 82 | 809 -> One (r54)
  | 86 | 2150 -> One (r55)
  | 85 | 2149 -> One (r56)
  | 88 | 518 -> One (r57)
  | 87 | 157 | 263 | 517 | 760 | 844 | 1031 | 1112 | 1575 | 1731 -> One (r58)
  | 2475 | 3114 -> One (r59)
  | 2473 | 3112 -> One (r60)
  | 938 | 3111 -> One (r61)
  | 765 | 3110 -> One (r62)
  | 151 | 764 -> One (r63)
  | 94 | 763 -> One (r64)
  | 201 | 223 -> One (r66)
  | 363 | 683 -> One (r68)
  | 341 | 666 -> One (r70)
  | 379 | 688 -> One (r72)
  | 389 | 701 -> One (r74)
  | 3100 | 3107 -> One (r76)
  | 3099 | 3106 -> One (r77)
  | 200 | 222 -> One (r78)
  | 199 | 221 -> One (r79)
  | 156 | 220 -> One (r80)
  | 155 | 219 -> One (r81)
  | 154 | 218 -> One (r82)
  | 166 | 521 -> One (r83)
  | 165 | 272 | 520 | 845 | 1059 | 1312 | 2481 | 3119 -> One (r84)
  | 181 -> One (r85)
  | 180 -> One (r86)
  | 184 -> One (r87)
  | 187 -> One (r88)
  | 197 | 848 -> One (r89)
  | 195 | 846 -> One (r90)
  | 202 | 215 | 278 | 705 -> One (r91)
  | 205 | 226 -> One (r92)
  | 206 | 227 -> One (r94)
  | 203 | 224 -> One (r95)
  | 209 -> One (r96)
  | 208 -> One (r97)
  | 3098 | 3105 -> One (r98)
  | 3097 | 3104 -> One (r99)
  | 214 | 231 -> One (r100)
  | 213 | 230 -> One (r101)
  | 212 | 229 -> One (r102)
  | 211 | 228 -> One (r103)
  | 2465 | 3076 -> One (r104)
  | 3075 | 3103 -> One (r106)
  | 3073 | 3101 -> One (r107)
  | 217 | 280 -> One (r108)
  | 216 | 279 -> One (r109)
  | 2817 | 3096 -> One (r110)
  | 235 | 530 -> One (r111)
  | 234 | 529 -> One (r112)
  | 233 | 528 -> One (r113)
  | 232 | 527 -> One (r114)
  | 2812 | 3095 -> One (r115)
  | 2810 | 3093 -> One (r116)
  | 237 | 532 -> One (r117)
  | 236 | 531 -> One (r118)
  | 1862 | 3088 -> One (r119)
  | 1861 | 3087 -> One (r120)
  | 1860 | 3086 -> One (r121)
  | 245 | 947 -> One (r122)
  | 244 | 946 -> One (r123)
  | 243 | 945 -> One (r124)
  | 1834 | 3085 -> One (r125)
  | 1833 | 3084 -> One (r126)
  | 1832 | 3083 -> One (r127)
  | 248 | 958 -> One (r128)
  | 247 | 957 -> One (r129)
  | 246 | 956 -> One (r130)
  | 1828 | 3082 -> One (r131)
  | 250 | 961 -> One (r132)
  | 1205 | 1898 -> One (r133)
  | 1204 | 1897 -> One (r134)
  | 1248 | 2684 -> One (r136)
  | 1197 | 2668 -> One (r137)
  | 2694 | 3081 -> One (r139)
  | 2693 | 3080 -> One (r140)
  | 255 | 2667 -> One (r141)
  | 253 | 2666 -> One (r142)
  | 252 | 2665 -> One (r143)
  | 251 | 2664 -> One (r144)
  | 256 | 264 -> One (r145)
  | 258 | 270 -> One (r146)
  | 257 | 513 -> One (r147)
  | 261 | 515 -> One (r148)
  | 260 | 514 -> One (r149)
  | 268 | 519 -> One (r150)
  | 276 | 525 -> One (r151)
  | 275 | 524 -> One (r152)
  | 274 | 523 -> One (r153)
  | 273 | 522 -> One (r154)
  | 2820 | 3079 -> One (r155)
  | 2818 | 3077 -> One (r156)
  | 277 | 526 -> One (r157)
  | 299 | 308 | 538 | 670 | 2611 | 2968 -> One (r158)
  | 440 | 2569 -> One (r160)
  | 472 | 2609 -> One (r162)
  | 471 | 2608 -> One (r163)
  | 470 | 2604 | 2890 | 2935 -> One (r164)
  | 2006 | 2965 -> One (r166)
  | 2021 | 2977 -> One (r168)
  | 2020 | 2976 -> One (r169)
  | 2019 | 2975 -> One (r170)
  | 2018 | 2974 -> One (r171)
  | 2017 | 2973 -> One (r172)
  | 2967 -> One (r173)
  | 283 -> One (r174)
  | 282 -> One (r175)
  | 281 -> One (r176)
  | 2004 | 2963 -> One (r177)
  | 2003 | 2962 -> One (r178)
  | 2002 | 2961 -> One (r179)
  | 2001 | 2960 -> One (r180)
  | 2000 | 2959 -> One (r181)
  | 298 -> One (r182)
  | 285 -> One (r183)
  | 289 -> One (r184)
  | 292 -> One (r186)
  | 288 -> One (r187)
  | 293 -> One (r189)
  | 295 -> One (r191)
  | 294 -> One (r192)
  | 291 -> One (r193)
  | 297 -> One (r194)
  | 476 | 2939 -> One (r195)
  | 477 | 2940 -> One (r197)
  | 300 | 322 -> One (r198)
  | 441 | 2925 -> One (r199)
  | 303 | 325 -> One (r200)
  | 305 | 327 -> One (r201)
  | 304 | 326 | 444 | 2564 -> One (r202)
  | 687 | 2913 -> One (r203)
  | 628 | 2912 -> One (r204)
  | 627 | 2911 -> One (r205)
  | 307 | 626 -> One (r206)
  | 625 -> One (r207)
  | 630 | 2908 -> One (r208)
  | 309 | 629 -> One (r209)
  | 434 -> One (r210)
  | 2904 -> One (r212)
  | 2903 -> One (r213)
  | 311 -> One (r214)
  | 1932 -> One (r215)
  | 1931 -> One (r216)
  | 2553 | 2902 -> One (r217)
  | 2552 | 2901 -> One (r218)
  | 315 | 548 -> One (r219)
  | 314 | 547 -> One (r220)
  | 2600 | 2886 -> One (r221)
  | 2619 | 2900 -> One (r223)
  | 2618 | 2899 -> One (r224)
  | 2617 | 2898 -> One (r225)
  | 2616 | 2897 -> One (r226)
  | 2615 | 2896 -> One (r227)
  | 2610 -> One (r228)
  | 535 -> One (r229)
  | 534 -> One (r230)
  | 533 -> One (r231)
  | 2016 | 2614 -> One (r232)
  | 2015 | 2613 -> One (r233)
  | 2014 | 2612 -> One (r234)
  | 2597 | 2883 -> One (r240)
  | 2596 | 2882 -> One (r241)
  | 2562 | 2876 -> One (r242)
  | 2561 | 2875 -> One (r243)
  | 2560 | 2874 -> One (r244)
  | 537 -> One (r245)
  | 536 -> One (r246)
  | 2559 | 2873 -> One (r247)
  | 493 | 540 -> One (r248)
  | 492 | 539 -> One (r249)
  | 387 -> One (r253)
  | 368 -> One (r255)
  | 399 | 640 -> One (r257)
  | 398 | 639 -> One (r258)
  | 330 | 638 -> One (r259)
  | 328 | 637 -> One (r260)
  | 332 | 645 -> One (r261)
  | 331 | 644 -> One (r262)
  | 397 | 648 -> One (r263)
  | 396 | 647 -> One (r264)
  | 333 | 646 -> One (r265)
  | 339 | 797 -> One (r266)
  | 338 | 796 -> One (r267)
  | 386 -> One (r269)
  | 373 -> One (r270)
  | 391 | 654 -> One (r272)
  | 390 | 653 -> One (r273)
  | 334 | 649 -> One (r274)
  | 336 | 664 -> One (r275)
  | 335 | 659 -> One (r276)
  | 370 | 652 -> One (r277)
  | 369 | 651 -> One (r278)
  | 337 | 650 -> One (r279)
  | 366 | 686 -> One (r280)
  | 342 | 667 -> One (r281)
  | 355 | 675 -> One (r282)
  | 344 | 669 -> One (r283)
  | 347 | 708 -> One (r284)
  | 346 | 707 -> One (r285)
  | 353 | 673 -> One (r286)
  | 352 | 672 -> One (r287)
  | 671 -> One (r288)
  | 349 | 710 -> One (r289)
  | 348 | 709 -> One (r290)
  | 351 | 712 -> One (r291)
  | 350 | 711 -> One (r292)
  | 357 | 677 -> One (r293)
  | 356 | 676 -> One (r294)
  | 362 | 682 -> One (r295)
  | 361 | 681 -> One (r296)
  | 360 | 680 -> One (r297)
  | 359 | 679 -> One (r298)
  | 365 | 685 -> One (r299)
  | 364 | 684 -> One (r300)
  | 372 -> One (r301)
  | 385 -> One (r302)
  | 384 -> One (r304)
  | 377 -> One (r305)
  | 376 -> One (r306)
  | 380 -> One (r307)
  | 383 | 692 -> One (r308)
  | 382 | 691 -> One (r309)
  | 381 | 690 -> One (r310)
  | 395 | 658 -> One (r311)
  | 394 | 657 -> One (r312)
  | 393 | 656 -> One (r313)
  | 392 | 655 -> One (r314)
  | 404 -> One (r315)
  | 402 | 643 -> One (r317)
  | 401 | 642 -> One (r318)
  | 400 | 641 -> One (r319)
  | 406 | 2566 -> One (r320)
  | 405 | 2565 -> One (r321)
  | 411 | 2918 -> One (r322)
  | 408 | 2915 -> One (r323)
  | 407 | 2914 -> One (r324)
  | 410 | 2917 -> One (r325)
  | 409 | 2916 -> One (r326)
  | 413 -> One (r327)
  | 415 | 2920 -> One (r328)
  | 414 | 2919 -> One (r329)
  | 417 -> One (r330)
  | 419 | 2568 -> One (r331)
  | 418 | 445 | 2567 | 2921 -> One (r332)
  | 439 -> One (r339)
  | 436 | 2924 -> One (r341)
  | 435 | 2923 -> One (r342)
  | 420 | 2922 -> One (r343)
  | 433 -> One (r344)
  | 432 -> One (r345)
  | 431 -> One (r346)
  | 430 -> One (r347)
  | 424 -> One (r348)
  | 423 -> One (r349)
  | 429 -> One (r350)
  | 428 -> One (r351)
  | 427 -> One (r352)
  | 462 | 2595 -> One (r353)
  | 461 | 2594 -> One (r354)
  | 446 | 2571 | 2878 | 2927 -> One (r355)
  | 455 | 2580 -> One (r356)
  | 454 | 2579 -> One (r358)
  | 447 | 2572 -> One (r359)
  | 450 | 2575 -> One (r360)
  | 449 | 2574 -> One (r361)
  | 448 | 2573 -> One (r362)
  | 453 | 2578 -> One (r363)
  | 452 | 2577 -> One (r364)
  | 451 | 2576 -> One (r365)
  | 460 | 2593 -> One (r366)
  | 456 | 2589 -> One (r367)
  | 459 | 2592 -> One (r368)
  | 458 | 2591 -> One (r369)
  | 457 | 2590 -> One (r370)
  | 464 | 2570 | 2877 | 2929 -> One (r371)
  | 475 | 2938 -> One (r372)
  | 474 | 2937 -> One (r373)
  | 473 | 2936 -> One (r374)
  | 489 | 2952 -> One (r375)
  | 479 | 2942 -> One (r376)
  | 484 | 2947 -> One (r377)
  | 480 | 2943 -> One (r378)
  | 483 | 2946 -> One (r379)
  | 482 | 2945 -> One (r380)
  | 481 | 2944 -> One (r381)
  | 488 | 2951 -> One (r382)
  | 487 | 2950 -> One (r383)
  | 486 | 2949 -> One (r384)
  | 2558 | 2872 -> One (r385)
  | 2557 | 2871 -> One (r386)
  | 495 | 542 -> One (r387)
  | 494 | 541 -> One (r388)
  | 2808 | 2870 -> One (r389)
  | 496 | 2620 -> One (r390)
  | 2630 | 2866 -> One (r391)
  | 2629 | 2865 -> One (r392)
  | 2628 | 2864 -> One (r393)
  | 2627 | 2863 -> One (r394)
  | 497 | 2621 -> One (r395)
  | 2626 | 2862 -> One (r396)
  | 2625 | 2861 -> One (r397)
  | 500 | 2624 -> One (r398)
  | 499 | 2623 -> One (r399)
  | 498 | 2622 -> One (r400)
  | 2521 | 2533 -> One (r401)
  | 2519 | 2531 -> One (r402)
  | 501 | 751 -> One (r403)
  | 2499 | 2849 -> One (r404)
  | 2497 | 2847 -> One (r405)
  | 503 | 753 -> One (r406)
  | 502 | 752 -> One (r407)
  | 1827 -> One (r408)
  | 963 -> One (r409)
  | 1709 -> One (r411)
  | 1706 | 2835 -> One (r413)
  | 1705 | 2834 -> One (r414)
  | 1704 | 2833 -> One (r415)
  | 511 | 962 -> One (r416)
  | 860 | 2829 -> One (r417)
  | 512 | 780 -> One (r418)
  | 2554 -> One (r419)
  | 545 | 782 -> One (r420)
  | 1979 | 2583 -> One (r421)
  | 855 -> One (r422)
  | 854 | 903 -> One (r424)
  | 852 | 901 -> One (r425)
  | 781 -> One (r426)
  | 2538 | 2644 -> One (r427)
  | 742 | 2643 -> One (r428)
  | 2540 | 2650 -> One (r430)
  | 2539 | 2649 -> One (r431)
  | 741 | 2642 -> One (r432)
  | 740 | 2641 -> One (r433)
  | 739 | 2640 -> One (r434)
  | 549 | 2631 -> One (r435)
  | 738 | 2639 -> One (r436)
  | 735 | 2638 -> One (r437)
  | 553 | 2635 -> One (r438)
  | 552 | 2634 -> One (r439)
  | 551 | 2633 -> One (r440)
  | 550 | 2632 -> One (r441)
  | 734 | 2637 -> One (r442)
  | 554 | 2636 -> One (r443)
  | 730 | 733 -> One (r444)
  | 728 | 731 -> One (r445)
  | 555 | 556 -> One (r446)
  | 572 | 619 -> One (r447)
  | 570 | 617 -> One (r448)
  | 569 | 616 -> One (r449)
  | 558 | 566 -> One (r450)
  | 557 | 565 -> One (r451)
  | 562 -> One (r452)
  | 615 -> One (r453)
  | 614 -> One (r454)
  | 564 -> One (r455)
  | 596 | 706 -> One (r458)
  | 595 | 704 -> One (r459)
  | 594 | 703 -> One (r460)
  | 593 | 702 -> One (r461)
  | 600 | 715 -> One (r463)
  | 601 | 716 -> One (r465)
  | 573 | 620 -> One (r466)
  | 576 | 768 -> One (r467)
  | 584 | 689 -> One (r469)
  | 583 | 624 -> One (r470)
  | 580 | 623 -> One (r471)
  | 575 | 622 -> One (r472)
  | 574 | 621 -> One (r473)
  | 579 | 771 -> One (r474)
  | 578 | 770 -> One (r475)
  | 577 | 769 | 1421 | 1473 | 1610 | 1662 -> One (r476)
  | 587 | 695 -> One (r477)
  | 586 | 694 -> One (r478)
  | 585 | 693 -> One (r479)
  | 591 | 699 -> One (r480)
  | 590 | 698 -> One (r481)
  | 589 | 697 -> One (r482)
  | 588 | 696 -> One (r483)
  | 597 | 2529 | 3184 -> One (r484)
  | 599 | 714 -> One (r485)
  | 598 | 713 -> One (r486)
  | 603 | 718 -> One (r487)
  | 602 | 717 -> One (r488)
  | 605 | 720 -> One (r489)
  | 604 | 719 -> One (r490)
  | 610 | 725 -> One (r491)
  | 608 | 723 -> One (r492)
  | 607 | 722 -> One (r493)
  | 633 | 2910 -> One (r494)
  | 632 | 2909 -> One (r495)
  | 737 | 1869 -> One (r496)
  | 736 | 1868 -> One (r497)
  | 748 | 2516 -> One (r498)
  | 746 | 2514 -> One (r499)
  | 745 | 2513 -> One (r500)
  | 744 | 2512 -> One (r501)
  | 743 | 2511 -> One (r502)
  | 750 | 2518 -> One (r503)
  | 749 | 2517 -> One (r504)
  | 1835 | 2476 -> One (r505)
  | 762 | 955 -> One (r506)
  | 925 -> One (r507)
  | 924 -> One (r508)
  | 865 -> One (r509)
  | 778 -> One (r510)
  | 775 -> One (r511)
  | 774 | 786 -> One (r513)
  | 772 | 784 -> One (r514)
  | 767 | 783 -> One (r515)
  | 777 -> One (r516)
  | 864 -> One (r517)
  | 799 | 881 | 1249 | 2685 -> One (r519)
  | 800 | 882 -> One (r521)
  | 789 | 873 -> One (r522)
  | 788 | 872 -> One (r523)
  | 787 | 871 -> One (r524)
  | 790 | 874 -> One (r525)
  | 792 | 876 -> One (r526)
  | 791 | 875 -> One (r527)
  | 794 | 878 -> One (r528)
  | 805 | 887 -> One (r530)
  | 804 | 886 -> One (r532)
  | 803 | 885 -> One (r533)
  | 851 | 900 -> One (r534)
  | 850 | 899 -> One (r535)
  | 808 | 890 -> One (r536)
  | 807 -> One (r537)
  | 812 | 893 -> One (r538)
  | 810 | 891 -> One (r539)
  | 830 | 923 -> One (r540)
  | 815 | 909 -> One (r541)
  | 814 | 908 -> One (r542)
  | 813 | 907 -> One (r543)
  | 820 | 914 -> One (r544)
  | 819 | 913 -> One (r545)
  | 823 | 917 -> One (r546)
  | 821 | 915 -> One (r547)
  | 826 | 920 -> One (r548)
  | 825 -> One (r549)
  | 829 | 922 -> One (r550)
  | 828 -> One (r551)
  | 832 | 895 -> One (r552)
  | 831 | 894 -> One (r553)
  | 835 | 898 -> One (r554)
  | 833 | 896 -> One (r555)
  | 838 | 928 -> One (r556)
  | 836 | 926 -> One (r557)
  | 841 | 934 -> One (r558)
  | 839 | 932 -> One (r559)
  | 843 | 936 -> One (r560)
  | 842 | 935 -> One (r561)
  | 863 | 2832 -> One (r562)
  | 861 | 2830 -> One (r563)
  | 869 -> One (r564)
  | 931 -> One (r565)
  | 930 -> One (r566)
  | 1883 -> One (r567)
  | 1882 -> One (r568)
  | 1881 -> One (r569)
  | 1880 -> One (r570)
  | 1871 -> One (r571)
  | 1870 -> One (r573)
  | 1867 -> One (r574)
  | 1863 -> One (r575)
  | 944 -> One (r576)
  | 943 -> One (r577)
  | 942 -> One (r578)
  | 941 -> One (r579)
  | 1788 | 1844 -> One (r580)
  | 1787 | 1843 -> One (r581)
  | 1786 | 1842 -> One (r582)
  | 1785 | 1841 -> One (r583)
  | 1784 | 1840 -> One (r584)
  | 949 | 980 -> One (r585)
  | 979 -> One (r586)
  | 1814 | 2700 -> One (r587)
  | 1813 | 2699 -> One (r588)
  | 1812 | 2698 -> One (r589)
  | 1811 | 2697 -> One (r590)
  | 1810 | 2696 -> One (r591)
  | 2373 | 2774 -> One (r592)
  | 1783 | 1839 -> One (r593)
  | 954 | 985 -> One (r594)
  | 953 | 984 -> One (r595)
  | 952 | 983 -> One (r596)
  | 951 | 982 -> One (r597)
  | 950 | 981 -> One (r598)
  | 1514 -> One (r599)
  | 1782 | 1831 -> One (r601)
  | 1780 | 1829 -> One (r602)
  | 986 -> One (r603)
  | 971 -> One (r604)
  | 966 -> One (r605)
  | 970 -> One (r607)
  | 969 -> One (r608)
  | 968 -> One (r609)
  | 1826 -> One (r610)
  | 1825 -> One (r611)
  | 1824 -> One (r612)
  | 974 -> One (r613)
  | 1823 -> One (r614)
  | 988 | 1819 -> One (r615)
  | 976 | 987 -> One (r616)
  | 1816 -> One (r617)
  | 977 -> One (r618)
  | 1815 -> One (r619)
  | 991 | 1822 -> One (r620)
  | 989 | 1820 -> One (r621)
  | 1541 -> One (r622)
  | 1540 -> One (r623)
  | 1530 -> One (r624)
  | 1544 -> One (r626)
  | 1776 | 1779 -> One (r628)
  | 1774 | 1777 -> One (r629)
  | 992 | 994 -> One (r630)
  | 1764 | 1773 -> One (r631)
  | 995 | 1006 -> One (r632)
  | 1763 | 1772 -> One (r633)
  | 1762 | 1771 -> One (r634)
  | 996 | 1007 -> One (r635)
  | 1765 | 1770 -> One (r636)
  | 999 | 1005 -> One (r637)
  | 998 | 1004 -> One (r638)
  | 997 | 1003 -> One (r639)
  | 1302 | 1766 -> One (r640)
  | 1002 | 1301 -> One (r641)
  | 1001 | 1300 -> One (r642)
  | 1000 | 1299 -> One (r643)
  | 1689 | 1759 -> One (r644)
  | 1688 | 1758 -> One (r645)
  | 1687 | 1757 -> One (r646)
  | 1010 | 1088 -> One (r647)
  | 1009 | 1087 -> One (r648)
  | 1008 | 1086 -> One (r649)
  | 1597 | 1755 -> One (r650)
  | 1013 | 1103 -> One (r651)
  | 1012 | 1102 -> One (r652)
  | 1011 | 1101 -> One (r653)
  | 1592 | 1750 -> One (r654)
  | 1590 | 1748 -> One (r655)
  | 1015 | 1105 -> One (r656)
  | 1593 | 1751 -> One (r658)
  | 1014 | 1104 -> One (r659)
  | 1589 | 1747 -> One (r660)
  | 1108 | 1746 -> One (r661)
  | 1107 | 1745 -> One (r662)
  | 1016 | 1106 -> One (r663)
  | 1218 -> One (r664)
  | 1574 | 1730 -> One (r666)
  | 1037 | 1116 -> One (r667)
  | 1588 | 1744 -> One (r669)
  | 1587 | 1743 -> One (r670)
  | 1019 | 1111 -> One (r671)
  | 1018 | 1110 -> One (r672)
  | 1017 | 1109 -> One (r673)
  | 1021 -> One (r674)
  | 1030 -> One (r676)
  | 1028 -> One (r677)
  | 1027 -> One (r678)
  | 1026 -> One (r679)
  | 1025 -> One (r680)
  | 1033 -> One (r681)
  | 1586 | 1742 -> One (r683)
  | 1036 | 1115 -> One (r684)
  | 1035 | 1114 -> One (r685)
  | 1032 | 1113 -> One (r686)
  | 1125 | 1722 -> One (r687)
  | 1124 | 1721 -> One (r688)
  | 1123 | 1720 -> One (r689)
  | 1122 | 1719 -> One (r690)
  | 1042 | 1121 -> One (r691)
  | 1041 | 1120 -> One (r692)
  | 1040 | 1119 -> One (r693)
  | 1039 | 1118 -> One (r694)
  | 1038 | 1117 -> One (r695)
  | 1282 -> One (r696)
  | 1298 | 1718 -> One (r698)
  | 1297 | 1717 -> One (r699)
  | 1296 | 1716 -> One (r700)
  | 1295 | 1715 -> One (r701)
  | 1294 | 1714 -> One (r702)
  | 1293 | 1713 -> One (r703)
  | 1047 | 1292 -> One (r704)
  | 1046 | 1291 -> One (r705)
  | 1045 | 1290 -> One (r706)
  | 1044 | 1289 -> One (r707)
  | 1043 | 1288 -> One (r708)
  | 1051 | 1306 -> One (r709)
  | 1048 | 1303 -> One (r710)
  | 1056 | 1311 -> One (r711)
  | 1055 | 1310 -> One (r712)
  | 1054 | 1309 | 1549 -> One (r713)
  | 1308 | 1548 -> One (r714)
  | 1064 | 1317 -> One (r715)
  | 1063 | 1316 -> One (r716)
  | 1062 | 1315 -> One (r717)
  | 1061 | 1314 -> One (r718)
  | 1060 | 1313 -> One (r719)
  | 1066 | 1319 -> One (r720)
  | 1065 | 1318 -> One (r721)
  | 1700 -> One (r722)
  | 1077 -> One (r723)
  | 1076 -> One (r724)
  | 1081 | 1327 -> One (r725)
  | 1080 | 1326 -> One (r726)
  | 1079 | 1325 -> One (r727)
  | 1078 | 1324 -> One (r728)
  | 1083 | 1329 -> One (r729)
  | 1082 | 1328 -> One (r730)
  | 1448 | 1502 | 1637 | 1696 -> One (r731)
  | 1446 | 1501 | 1635 | 1695 -> One (r732)
  | 1445 | 1500 | 1634 | 1694 -> One (r733)
  | 1084 | 1330 | 1438 | 1627 -> One (r734)
  | 1442 | 1499 | 1631 | 1693 -> One (r735)
  | 1440 | 1498 | 1629 | 1692 -> One (r736)
  | 1085 | 1331 | 1439 | 1628 -> One (r737)
  | 1497 | 1686 -> One (r738)
  | 1089 | 1332 -> One (r739)
  | 1092 | 1335 -> One (r740)
  | 1091 | 1334 -> One (r741)
  | 1094 | 1337 -> One (r742)
  | 1093 | 1336 -> One (r743)
  | 1096 | 1339 -> One (r744)
  | 1095 | 1338 -> One (r745)
  | 1098 | 1341 -> One (r746)
  | 1097 | 1340 -> One (r747)
  | 1420 | 1470 | 1609 | 1659 -> One (r748)
  | 1418 | 1469 | 1607 | 1658 -> One (r749)
  | 1417 | 1468 | 1606 | 1657 -> One (r750)
  | 1099 | 1342 | 1457 | 1646 -> One (r751)
  | 1346 | 1460 | 1601 | 1649 -> One (r752)
  | 1344 | 1459 | 1599 | 1648 -> One (r753)
  | 1100 | 1343 | 1458 | 1647 -> One (r754)
  | 1278 | 1412 -> One (r755)
  | 1127 | 1348 -> One (r756)
  | 1135 | 1353 -> One (r757)
  | 1134 | 1352 -> One (r758)
  | 1133 | 1351 -> One (r759)
  | 1139 | 1356 -> One (r760)
  | 1138 | 1355 -> One (r761)
  | 1137 | 1354 -> One (r762)
  | 1142 | 1359 -> One (r763)
  | 1141 | 1358 -> One (r764)
  | 1144 | 1361 -> One (r765)
  | 1143 | 1360 -> One (r766)
  | 1149 | 1366 -> One (r767)
  | 1148 | 1365 -> One (r768)
  | 1153 | 1370 -> One (r769)
  | 1152 | 1369 -> One (r770)
  | 1151 | 1368 -> One (r771)
  | 1156 | 1373 -> One (r772)
  | 1155 | 1372 -> One (r773)
  | 1158 | 1375 -> One (r774)
  | 1157 | 1374 -> One (r775)
  | 1160 | 1377 -> One (r776)
  | 1159 | 1376 -> One (r777)
  | 1162 | 1379 -> One (r778)
  | 1161 | 1378 -> One (r779)
  | 1164 | 1381 -> One (r780)
  | 1163 | 1380 -> One (r781)
  | 1166 | 1383 -> One (r782)
  | 1165 | 1382 -> One (r783)
  | 1168 | 1385 -> One (r784)
  | 1167 | 1384 -> One (r785)
  | 1170 | 1387 -> One (r786)
  | 1169 | 1386 -> One (r787)
  | 1172 | 1389 -> One (r788)
  | 1171 | 1388 -> One (r789)
  | 1174 | 1391 -> One (r790)
  | 1173 | 1390 -> One (r791)
  | 1176 | 1393 -> One (r792)
  | 1175 | 1392 -> One (r793)
  | 1178 | 1395 -> One (r794)
  | 1177 | 1394 -> One (r795)
  | 1180 | 1397 -> One (r796)
  | 1179 | 1396 -> One (r797)
  | 1182 | 1399 -> One (r798)
  | 1181 | 1398 -> One (r799)
  | 1184 | 1401 -> One (r800)
  | 1183 | 1400 -> One (r801)
  | 1186 | 1403 -> One (r802)
  | 1185 | 1402 -> One (r803)
  | 1188 | 1405 -> One (r804)
  | 1187 | 1404 -> One (r805)
  | 1190 | 1407 -> One (r806)
  | 1189 | 1406 -> One (r807)
  | 1192 | 1409 -> One (r808)
  | 1191 | 1408 -> One (r809)
  | 1194 | 1411 -> One (r810)
  | 1193 | 1410 -> One (r811)
  | 1275 | 2784 -> One (r812)
  | 1274 | 2783 -> One (r813)
  | 1196 | 2782 -> One (r814)
  | 1195 | 2781 -> One (r815)
  | 1217 | 1902 -> One (r816)
  | 1221 | 1905 -> One (r818)
  | 1201 | 1896 -> One (r819)
  | 1200 | 1895 -> One (r820)
  | 1199 | 1894 -> One (r821)
  | 1198 | 1893 -> One (r822)
  | 1203 -> One (r823)
  | 1207 | 1532 -> One (r824)
  | 1206 | 1531 -> One (r825)
  | 1210 | 1535 -> One (r826)
  | 1209 | 1233 | 1534 | 2669 -> One (r827)
  | 1213 | 1538 -> One (r828)
  | 1212 | 1537 -> One (r829)
  | 1216 | 1901 -> One (r830)
  | 1215 | 1900 -> One (r831)
  | 1214 | 1899 -> One (r832)
  | 1220 | 1904 -> One (r833)
  | 1219 | 1903 -> One (r834)
  | 1224 | 2491 -> One (r835)
  | 1223 | 2490 -> One (r836)
  | 1227 | 2823 -> One (r837)
  | 1225 | 2821 -> One (r838)
  | 1229 | 2825 -> One (r839)
  | 1228 | 2824 -> One (r840)
  | 1232 | 2828 -> One (r841)
  | 1230 | 2826 -> One (r842)
  | 1239 | 2675 -> One (r843)
  | 1238 | 2674 -> One (r844)
  | 1237 | 2673 -> One (r845)
  | 1236 | 2672 -> One (r846)
  | 1235 | 2671 -> One (r847)
  | 1234 | 2670 -> One (r848)
  | 1244 | 2680 -> One (r849)
  | 1243 | 2679 -> One (r850)
  | 1242 | 2678 -> One (r851)
  | 1241 | 2677 -> One (r852)
  | 1240 | 2676 -> One (r853)
  | 1247 | 2683 -> One (r854)
  | 1246 | 2682 -> One (r855)
  | 1245 | 2681 -> One (r856)
  | 1253 | 2689 -> One (r857)
  | 1252 | 2688 -> One (r858)
  | 1251 | 2687 -> One (r859)
  | 1250 | 2686 -> One (r860)
  | 1256 | 2692 -> One (r861)
  | 1255 | 2691 -> One (r862)
  | 1254 | 2690 -> One (r863)
  | 1259 -> One (r864)
  | 1262 -> One (r865)
  | 1265 -> One (r866)
  | 1267 -> One (r867)
  | 1273 -> One (r868)
  | 1287 | 1729 -> One (r869)
  | 1286 | 1728 -> One (r870)
  | 1285 | 1727 -> One (r871)
  | 1284 | 1726 -> One (r872)
  | 1283 | 1725 -> One (r873)
  | 1280 | 1724 -> One (r874)
  | 1279 | 1723 -> One (r875)
  | 1416 | 1465 | 1605 | 1654 -> One (r876)
  | 1414 | 1464 | 1603 | 1653 -> One (r877)
  | 1413 | 1463 | 1602 | 1652 -> One (r878)
  | 1430 | 1482 | 1619 | 1671 -> One (r879)
  | 1428 | 1481 | 1617 | 1670 -> One (r880)
  | 1427 | 1480 | 1616 | 1669 -> One (r881)
  | 1422 | 1474 | 1611 | 1663 -> One (r882)
  | 1426 | 1477 | 1615 | 1666 -> One (r883)
  | 1424 | 1476 | 1613 | 1665 -> One (r884)
  | 1423 | 1475 | 1612 | 1664 -> One (r885)
  | 1434 | 1487 | 1623 | 1676 -> One (r886)
  | 1432 | 1486 | 1621 | 1675 -> One (r887)
  | 1431 | 1485 | 1620 | 1674 -> One (r888)
  | 1437 | 1626 -> One (r889)
  | 1436 | 1625 -> One (r890)
  | 1444 | 1633 -> One (r891)
  | 1443 | 1632 -> One (r892)
  | 1450 | 1639 -> One (r893)
  | 1449 | 1638 -> One (r894)
  | 1454 | 1505 | 1643 | 1699 -> One (r895)
  | 1452 | 1504 | 1641 | 1698 -> One (r896)
  | 1451 | 1503 | 1640 | 1697 -> One (r897)
  | 1456 | 1645 -> One (r898)
  | 1455 | 1644 -> One (r899)
  | 1462 | 1651 -> One (r900)
  | 1461 | 1650 -> One (r901)
  | 1467 | 1656 -> One (r902)
  | 1466 | 1655 -> One (r903)
  | 1472 | 1661 -> One (r904)
  | 1471 | 1660 -> One (r905)
  | 1479 | 1668 -> One (r906)
  | 1478 | 1667 -> One (r907)
  | 1484 | 1673 -> One (r908)
  | 1483 | 1672 -> One (r909)
  | 1489 | 1678 -> One (r910)
  | 1488 | 1677 -> One (r911)
  | 1492 | 1681 -> One (r912)
  | 1491 | 1680 -> One (r913)
  | 1508 | 1712 -> One (r914)
  | 1506 | 1710 -> One (r915)
  | 1510 | 1552 -> One (r916)
  | 1509 | 1551 -> One (r917)
  | 1513 | 1555 -> One (r918)
  | 1511 | 1553 -> One (r919)
  | 1518 | 1557 -> One (r920)
  | 1517 | 1556 -> One (r921)
  | 1521 | 1560 -> One (r922)
  | 1519 | 1558 -> One (r923)
  | 1525 | 1564 -> One (r925)
  | 1523 | 1562 -> One (r926)
  | 1522 | 1561 -> One (r927)
  | 1547 | 1568 -> One (r928)
  | 1545 | 1566 -> One (r929)
  | 1526 | 1565 -> One (r930)
  | 1529 -> One (r931)
  | 1528 -> One (r932)
  | 1573 | 1769 -> One (r933)
  | 1571 | 1767 -> One (r934)
  | 1585 | 1741 -> One (r935)
  | 1578 | 1734 -> One (r936)
  | 1577 | 1733 -> One (r937)
  | 1576 | 1732 -> One (r938)
  | 1582 | 1738 -> One (r939)
  | 1581 | 1737 -> One (r940)
  | 1580 | 1736 -> One (r941)
  | 1579 | 1735 -> One (r942)
  | 1584 | 1740 -> One (r943)
  | 1583 | 1739 -> One (r944)
  | 1596 | 1754 -> One (r945)
  | 1595 | 1753 -> One (r946)
  | 1691 | 1761 -> One (r947)
  | 1690 | 1760 -> One (r948)
  | 1703 -> One (r949)
  | 1702 -> One (r950)
  | 1708 -> One (r951)
  | 1799 | 1851 -> One (r952)
  | 1798 | 1850 -> One (r953)
  | 1797 | 1849 -> One (r954)
  | 1792 | 1848 -> One (r955)
  | 1791 | 1847 -> One (r956)
  | 1790 | 1846 -> One (r957)
  | 1789 | 1845 -> One (r958)
  | 1796 | 2648 -> One (r959)
  | 1795 | 2647 -> One (r960)
  | 1794 | 2646 -> One (r961)
  | 1793 | 2645 -> One (r962)
  | 1801 | 2652 -> One (r963)
  | 1800 | 2651 -> One (r964)
  | 1809 | 1859 -> One (r965)
  | 1808 | 1858 -> One (r966)
  | 1807 | 1857 -> One (r967)
  | 1806 | 1856 -> One (r968)
  | 1805 | 1855 -> One (r969)
  | 1804 | 1854 -> One (r970)
  | 1803 | 1853 -> One (r971)
  | 1802 | 1852 -> One (r972)
  | 1838 | 2479 -> One (r973)
  | 1836 | 2477 -> One (r974)
  | 1866 -> One (r975)
  | 1865 -> One (r976)
  | 1879 -> One (r977)
  | 1878 -> One (r978)
  | 1877 -> One (r979)
  | 1886 -> One (r980)
  | 1885 -> One (r981)
  | 1950 -> One (r982)
  | 1939 -> One (r983)
  | 1938 -> One (r984)
  | 1923 -> One (r985)
  | 1922 -> One (r987)
  | 1921 -> One (r988)
  | 1920 -> One (r989)
  | 1892 -> One (r990)
  | 1891 -> One (r991)
  | 1889 -> One (r992)
  | 1919 | 1937 -> One (r993)
  | 1918 -> One (r995)
  | 1917 -> One (r996)
  | 1916 -> One (r997)
  | 1912 -> One (r998)
  | 1911 -> One (r999)
  | 1910 -> One (r1000)
  | 1909 -> One (r1001)
  | 1908 -> One (r1002)
  | 1915 | 1935 -> One (r1003)
  | 1914 | 1934 -> One (r1004)
  | 1913 | 1933 -> One (r1005)
  | 1936 -> One (r1006)
  | 1930 -> One (r1007)
  | 1929 -> One (r1008)
  | 1949 -> One (r1009)
  | 1948 -> One (r1010)
  | 1947 -> One (r1011)
  | 1946 -> One (r1012)
  | 1945 -> One (r1013)
  | 1944 -> One (r1014)
  | 1943 -> One (r1015)
  | 1942 -> One (r1016)
  | 2470 | 2472 -> One (r1017)
  | 2469 | 2471 -> One (r1018)
  | 1952 | 1954 -> One (r1019)
  | 1951 | 1953 -> One (r1020)
  | 2401 | 2468 -> One (r1021)
  | 2400 | 2467 -> One (r1022)
  | 1956 | 2113 -> One (r1023)
  | 1955 | 2112 -> One (r1024)
  | 1961 | 2705 -> One (r1025)
  | 1960 | 2704 -> One (r1026)
  | 1959 | 2703 -> One (r1027)
  | 1958 | 2702 -> One (r1028)
  | 1957 | 2701 -> One (r1029)
  | 1969 | 2713 -> One (r1030)
  | 1972 | 2716 -> One (r1032)
  | 1971 | 2715 -> One (r1033)
  | 1968 | 2712 -> One (r1034)
  | 1967 | 2711 -> One (r1035)
  | 1966 | 2710 -> One (r1036)
  | 1965 | 2709 -> One (r1037)
  | 1964 | 2708 -> One (r1038)
  | 1963 | 2707 -> One (r1039)
  | 1962 | 2706 -> One (r1040)
  | 1985 | 2724 -> One (r1042)
  | 1984 | 2723 -> One (r1043)
  | 1983 | 2722 -> One (r1044)
  | 1977 | 2721 -> One (r1045)
  | 2720 -> One (r1046)
  | 2719 -> One (r1047)
  | 2718 -> One (r1048)
  | 2717 -> One (r1049)
  | 1988 | 2727 -> One (r1050)
  | 1987 | 2726 -> One (r1051)
  | 1986 | 2725 -> One (r1052)
  | 2071 | 3027 -> One (r1053)
  | 2070 | 3026 -> One (r1054)
  | 2069 | 3025 -> One (r1055)
  | 2068 | 3024 -> One (r1056)
  | 1978 | 2582 -> One (r1057)
  | 1982 | 2586 -> One (r1058)
  | 1981 | 2585 -> One (r1059)
  | 2464 -> One (r1060)
  | 1999 | 2958 -> One (r1061)
  | 1998 | 2957 -> One (r1062)
  | 1997 | 2956 -> One (r1063)
  | 1996 | 2955 -> One (r1064)
  | 2013 | 2972 -> One (r1065)
  | 2012 | 2971 -> One (r1066)
  | 2011 | 2970 -> One (r1067)
  | 2010 | 2969 -> One (r1068)
  | 2463 | 3072 -> One (r1069)
  | 2022 | 2978 -> One (r1070)
  | 2032 | 2988 -> One (r1071)
  | 2031 | 2987 -> One (r1072)
  | 2030 | 2986 -> One (r1073)
  | 2029 | 2985 -> One (r1074)
  | 2023 | 2979 -> One (r1075)
  | 2028 | 2984 -> One (r1076)
  | 2027 | 2983 -> One (r1077)
  | 2026 | 2982 -> One (r1078)
  | 2025 | 2981 -> One (r1079)
  | 2024 | 2980 -> One (r1080)
  | 2041 | 2997 -> One (r1081)
  | 2040 | 2996 -> One (r1082)
  | 2045 | 3001 -> One (r1086)
  | 2044 | 3000 -> One (r1087)
  | 2047 | 3003 -> One (r1089)
  | 2046 | 3002 -> One (r1090)
  | 2992 -> One (r1091)
  | 2991 -> One (r1092)
  | 2990 -> One (r1093)
  | 2989 -> One (r1094)
  | 2039 | 2995 -> One (r1095)
  | 2038 | 2994 -> One (r1096)
  | 2037 | 2993 -> One (r1097)
  | 2043 | 2999 -> One (r1098)
  | 2042 | 2998 -> One (r1099)
  | 2049 | 3005 -> One (r1100)
  | 2048 | 3004 -> One (r1101)
  | 2062 | 3018 -> One (r1102)
  | 2054 | 3010 -> One (r1103)
  | 2053 | 3009 -> One (r1104)
  | 2052 | 3008 -> One (r1105)
  | 2051 | 3007 -> One (r1106)
  | 2050 | 3006 -> One (r1107)
  | 2061 | 3017 -> One (r1108)
  | 2060 | 3016 -> One (r1109)
  | 2059 | 3015 -> One (r1110)
  | 2058 | 3014 -> One (r1111)
  | 2057 | 3013 -> One (r1112)
  | 2056 | 3012 -> One (r1113)
  | 2055 | 3011 -> One (r1114)
  | 2067 | 3023 -> One (r1115)
  | 2066 | 3022 -> One (r1116)
  | 2065 | 3021 -> One (r1117)
  | 2064 | 3020 -> One (r1118)
  | 2063 | 3019 -> One (r1119)
  | 2205 | 2325 -> One (r1120)
  | 2220 | 2334 -> One (r1122)
  | 2257 | 2352 -> One (r1124)
  | 2437 | 3046 -> One (r1126)
  | 2427 | 3036 -> One (r1127)
  | 2426 | 3035 -> One (r1128)
  | 2425 | 3034 -> One (r1129)
  | 2424 | 3033 -> One (r1130)
  | 2423 | 3032 -> One (r1131)
  | 2422 | 3031 -> One (r1132)
  | 2421 | 3030 -> One (r1133)
  | 2420 | 3029 -> One (r1134)
  | 2072 | 3028 -> One (r1135)
  | 2419 | 2747 -> One (r1136)
  | 2409 | 2737 -> One (r1137)
  | 2408 | 2736 -> One (r1138)
  | 2083 | 2735 -> One (r1139)
  | 2082 | 2734 -> One (r1140)
  | 2081 | 2733 -> One (r1141)
  | 2077 | 2732 -> One (r1142)
  | 2075 | 2731 -> One (r1143)
  | 2074 | 2730 -> One (r1144)
  | 2073 | 2729 -> One (r1145)
  | 2080 -> One (r1146)
  | 2079 -> One (r1147)
  | 2235 | 2407 -> One (r1148)
  | 2233 | 2405 -> One (r1149)
  | 2089 | 2202 -> One (r1150)
  | 2085 | 2201 -> One (r1151)
  | 2084 | 2200 -> One (r1152)
  | 2088 -> One (r1153)
  | 2087 -> One (r1154)
  | 2100 -> One (r1155)
  | 2099 -> One (r1156)
  | 2098 -> One (r1157)
  | 2097 -> One (r1158)
  | 2096 -> One (r1159)
  | 2091 -> One (r1160)
  | 2111 -> One (r1161)
  | 2110 -> One (r1162)
  | 2109 -> One (r1163)
  | 2108 -> One (r1164)
  | 2107 -> One (r1165)
  | 2102 -> One (r1166)
  | 2180 | 2302 -> One (r1167)
  | 2178 | 2300 -> One (r1169)
  | 2313 | 2754 -> One (r1171)
  | 2120 | 2753 -> One (r1172)
  | 2370 | 2771 -> One (r1174)
  | 2361 | 2762 -> One (r1175)
  | 2360 | 2761 -> One (r1176)
  | 2119 | 2752 -> One (r1177)
  | 2118 | 2751 -> One (r1178)
  | 2117 | 2750 -> One (r1179)
  | 2116 | 2749 -> One (r1180)
  | 2115 | 2748 -> One (r1181)
  | 2728 -> One (r1182)
  | 2142 | 2273 -> One (r1183)
  | 2141 | 2272 -> One (r1184)
  | 2123 | 2133 -> One (r1185)
  | 2122 | 2132 -> One (r1186)
  | 2121 | 2131 -> One (r1187)
  | 2127 -> One (r1188)
  | 2126 -> One (r1189)
  | 2125 -> One (r1190)
  | 2271 -> One (r1191)
  | 2270 -> One (r1192)
  | 2269 -> One (r1193)
  | 2268 -> One (r1194)
  | 2267 -> One (r1195)
  | 2266 -> One (r1196)
  | 2263 -> One (r1197)
  | 2130 -> One (r1198)
  | 2138 -> One (r1199)
  | 2137 -> One (r1200)
  | 2136 -> One (r1201)
  | 2140 -> One (r1203)
  | 2139 -> One (r1204)
  | 2135 -> One (r1205)
  | 2145 -> One (r1206)
  | 2148 -> One (r1207)
  | 2198 | 2278 -> One (r1208)
  | 2196 | 2276 -> One (r1209)
  | 2151 | 2275 -> One (r1210)
  | 2191 | 2312 -> One (r1211)
  | 2190 | 2311 -> One (r1212)
  | 2189 | 2310 -> One (r1213)
  | 2188 | 2309 -> One (r1214)
  | 2159 | 2284 -> One (r1215)
  | 2152 | 2283 -> One (r1216)
  | 2158 -> One (r1217)
  | 2157 -> One (r1218)
  | 2156 -> One (r1219)
  | 2155 -> One (r1220)
  | 2154 -> One (r1221)
  | 2187 | 2308 -> One (r1222)
  | 2163 | 2288 -> One (r1223)
  | 2162 | 2287 -> One (r1224)
  | 2161 | 2286 -> One (r1225)
  | 2160 | 2285 -> One (r1226)
  | 2170 | 2246 -> One (r1227)
  | 2167 | 2292 -> One (r1229)
  | 2166 | 2291 -> One (r1230)
  | 2165 | 2290 -> One (r1231)
  | 2164 | 2289 -> One (r1232)
  | 2169 -> One (r1233)
  | 2184 | 2305 -> One (r1234)
  | 2174 | 2296 -> One (r1235)
  | 2173 | 2295 -> One (r1236)
  | 2186 | 2307 -> One (r1238)
  | 2172 | 2294 -> One (r1239)
  | 2171 | 2293 -> One (r1240)
  | 2181 | 2303 -> One (r1241)
  | 2176 | 2298 -> One (r1242)
  | 2175 | 2297 -> One (r1243)
  | 2195 -> One (r1244)
  | 2194 -> One (r1245)
  | 2193 -> One (r1246)
  | 2262 | 2282 -> One (r1247)
  | 2260 | 2280 -> One (r1248)
  | 2199 | 2279 -> One (r1249)
  | 2228 -> One (r1250)
  | 2227 -> One (r1251)
  | 2204 -> One (r1252)
  | 2226 | 2340 -> One (r1253)
  | 2225 | 2339 -> One (r1254)
  | 2224 | 2338 -> One (r1255)
  | 2223 | 2337 -> One (r1256)
  | 2207 | 2320 -> One (r1257)
  | 2206 | 2319 -> One (r1258)
  | 2221 | 2335 -> One (r1259)
  | 2211 | 2324 -> One (r1260)
  | 2210 | 2323 -> One (r1261)
  | 2209 | 2322 -> One (r1262)
  | 2208 | 2321 -> One (r1263)
  | 2218 | 2332 -> One (r1264)
  | 2214 | 2328 -> One (r1265)
  | 2213 | 2327 -> One (r1266)
  | 2212 | 2245 | 2326 | 2341 -> One (r1267)
  | 2217 | 2254 | 2331 | 2349 -> One (r1268)
  | 2216 | 2253 | 2330 | 2348 -> One (r1269)
  | 2215 | 2252 | 2329 | 2347 -> One (r1270)
  | 2232 -> One (r1271)
  | 2231 -> One (r1272)
  | 2230 -> One (r1273)
  | 2237 -> One (r1274)
  | 2240 -> One (r1275)
  | 2259 | 2354 -> One (r1276)
  | 2244 | 2318 -> One (r1277)
  | 2243 | 2317 -> One (r1278)
  | 2242 | 2316 -> One (r1279)
  | 2241 | 2315 -> One (r1280)
  | 2258 | 2353 -> One (r1281)
  | 2248 | 2343 -> One (r1282)
  | 2247 | 2342 -> One (r1283)
  | 2256 | 2351 -> One (r1284)
  | 2251 | 2346 -> One (r1285)
  | 2250 | 2345 -> One (r1286)
  | 2249 | 2344 -> One (r1287)
  | 2265 -> One (r1288)
  | 2357 | 2758 -> One (r1289)
  | 2356 | 2757 -> One (r1290)
  | 2355 | 2756 -> One (r1291)
  | 2314 | 2755 -> One (r1292)
  | 2359 | 2760 -> One (r1293)
  | 2358 | 2759 -> One (r1294)
  | 2369 | 2770 -> One (r1295)
  | 2368 | 2769 -> One (r1296)
  | 2367 | 2768 -> One (r1297)
  | 2366 | 2767 -> One (r1298)
  | 2365 | 2766 -> One (r1299)
  | 2364 | 2765 -> One (r1300)
  | 2363 | 2764 -> One (r1301)
  | 2362 | 2763 -> One (r1302)
  | 2380 | 2787 -> One (r1303)
  | 2372 | 2773 -> One (r1304)
  | 2383 | 2791 -> One (r1305)
  | 2382 | 2790 -> One (r1306)
  | 2393 | 2801 -> One (r1307)
  | 2384 | 2792 -> One (r1308)
  | 2392 | 2800 -> One (r1309)
  | 2391 | 2799 -> One (r1310)
  | 2390 | 2798 -> One (r1311)
  | 2389 | 2797 -> One (r1312)
  | 2388 | 2796 -> One (r1313)
  | 2387 | 2795 -> One (r1314)
  | 2386 | 2794 -> One (r1315)
  | 2385 | 2793 -> One (r1316)
  | 2399 | 2815 -> One (r1317)
  | 2398 | 2814 -> One (r1318)
  | 2397 | 2813 -> One (r1319)
  | 2418 | 2746 -> One (r1320)
  | 2417 | 2745 -> One (r1321)
  | 2416 | 2744 -> One (r1322)
  | 2415 | 2743 -> One (r1323)
  | 2414 | 2742 -> One (r1324)
  | 2413 | 2741 -> One (r1325)
  | 2412 | 2740 -> One (r1326)
  | 2411 | 2739 -> One (r1327)
  | 2410 | 2738 -> One (r1328)
  | 2436 | 3045 -> One (r1329)
  | 2435 | 3044 -> One (r1330)
  | 2434 | 3043 -> One (r1331)
  | 2433 | 3042 -> One (r1332)
  | 2432 | 3041 -> One (r1333)
  | 2431 | 3040 -> One (r1334)
  | 2430 | 3039 -> One (r1335)
  | 2429 | 3038 -> One (r1336)
  | 2428 | 3037 -> One (r1337)
  | 2445 | 3054 -> One (r1338)
  | 2439 | 3048 -> One (r1339)
  | 2447 | 3056 -> One (r1340)
  | 2446 | 3055 -> One (r1341)
  | 2449 | 3058 -> One (r1342)
  | 2448 | 3057 -> One (r1343)
  | 2460 | 3069 -> One (r1344)
  | 2450 | 3059 -> One (r1345)
  | 2459 | 3068 -> One (r1346)
  | 2458 | 3067 -> One (r1347)
  | 2457 | 3066 -> One (r1348)
  | 2456 | 3065 -> One (r1349)
  | 2455 | 3064 -> One (r1350)
  | 2454 | 3063 -> One (r1351)
  | 2453 | 3062 -> One (r1352)
  | 2452 | 3061 -> One (r1353)
  | 2451 | 3060 -> One (r1354)
  | 2485 | 3123 -> One (r1355)
  | 2484 | 3122 -> One (r1356)
  | 2483 | 3121 -> One (r1357)
  | 2482 | 3120 -> One (r1358)
  | 2487 | 3125 -> One (r1359)
  | 2486 | 3124 -> One (r1360)
  | 2489 | 3127 -> One (r1361)
  | 2488 | 3126 -> One (r1362)
  | 2494 | 3130 -> One (r1363)
  | 2492 | 3128 -> One (r1364)
  | 2496 | 3132 -> One (r1365)
  | 2495 | 3131 -> One (r1366)
  | 2501 | 2851 -> One (r1367)
  | 2500 | 2850 -> One (r1368)
  | 2503 | 2853 -> One (r1369)
  | 2502 | 2852 -> One (r1370)
  | 2505 | 2855 -> One (r1371)
  | 2504 | 2854 -> One (r1372)
  | 2507 | 2857 -> One (r1373)
  | 2506 | 2856 -> One (r1374)
  | 2510 | 2860 -> One (r1375)
  | 2509 | 2859 -> One (r1376)
  | 2508 | 2858 -> One (r1377)
  | 2525 | 2537 -> One (r1378)
  | 2523 | 2535 -> One (r1379)
  | 2522 | 2534 -> One (r1380)
  | 2551 | 2663 -> One (r1381)
  | 2544 | 2656 -> One (r1382)
  | 2543 | 2655 -> One (r1383)
  | 2542 | 2654 -> One (r1384)
  | 2541 | 2653 -> One (r1385)
  | 2550 | 2662 -> One (r1386)
  | 2549 | 2661 -> One (r1387)
  | 2548 | 2660 -> One (r1388)
  | 2547 | 2659 -> One (r1389)
  | 2546 | 2658 -> One (r1390)
  | 2545 | 2657 -> One (r1391)
  | 2556 -> One (r1392)
  | 2588 | 2881 -> One (r1393)
  | 2587 | 2880 -> One (r1394)
  | 2581 | 2879 -> One (r1395)
  | 2607 | 2893 -> One (r1396)
  | 2606 | 2892 -> One (r1397)
  | 2605 | 2891 -> One (r1398)
  | 2806 | 2869 -> One (r1399)
  | 2805 | 2868 -> One (r1400)
  | 2804 | 2867 -> One (r1401)
  | 2842 | 3140 -> One (r1402)
  | 2841 | 3139 -> One (r1403)
  | 2844 | 3142 -> One (r1404)
  | 2843 | 3141 -> One (r1405)
  | 2846 | 3144 -> One (r1406)
  | 2845 | 3143 -> One (r1407)
  | 2906 -> One (r1408)
  | 3157 -> One (r1409)
  | 3161 -> One (r1410)
  | 3166 -> One (r1411)
  | 3169 -> One (r1412)
  | 3173 -> One (r1413)
  | 3177 -> One (r1414)
  | 3188 -> One (r1415)
  | 3190 -> One (r1416)
  | 3193 -> One (r1417)
  | 3192 -> One (r1418)
  | 3195 -> One (r1419)
  | 3205 -> One (r1420)
  | 3201 -> One (r1421)
  | 3200 -> One (r1422)
  | 3204 -> One (r1423)
  | 3203 -> One (r1424)
  | 3210 -> One (r1425)
  | 3209 -> One (r1426)
  | 3208 -> One (r1427)
  | 3212 -> One (r1428)
  | 889 -> Select (function
    | -1 -> [R 102]
    | _ -> r537)
  | 1053 -> Select (function
    | -1 -> [R 102]
    | _ -> r714)
  | 2033 -> Select (function
    | -1 -> R 189 :: r1085
    | _ -> r1094)
  | 921 -> Select (function
    | -1 -> [R 682]
    | _ -> r551)
  | 919 -> Select (function
    | -1 -> [R 683]
    | _ -> r549)
  | 2114 -> Select (function
    | -1 -> S (T T_TYPE) :: r1145
    | _ -> r1182)
  | 345 -> Select (function
    | -1 -> S (T T_LPAREN) :: r285
    | _ -> r288)
  | 313 -> Select (function
    | 1233 | 2669 -> r94
    | _ -> r215)
  | 312 -> Select (function
    | 1233 | 2669 -> r95
    | _ -> r216)
  | 306 -> Select (function
    | -1 -> r158
    | _ -> r207)
  | 2895 -> Select (function
    | -1 -> r235
    | _ -> r158)
  | 321 -> Select (function
    | -1 -> r250
    | _ -> r158)
  | 2009 -> Select (function
    | -1 -> r235
    | _ -> r158)
  | 1995 -> Select (function
    | -1 -> r250
    | _ -> r158)
  | 2008 -> Select (function
    | -1 -> r236
    | _ -> r173)
  | 1992 -> Select (function
    | -1 -> r237
    | _ -> r174)
  | 1991 -> Select (function
    | -1 -> r238
    | _ -> r175)
  | 1990 -> Select (function
    | -1 -> r239
    | _ -> r176)
  | 1994 -> Select (function
    | -1 -> r251
    | _ -> r182)
  | 1993 -> Select (function
    | -1 -> r252
    | _ -> r183)
  | 2894 -> Select (function
    | -1 -> r236
    | _ -> r228)
  | 318 -> Select (function
    | -1 -> r237
    | _ -> r229)
  | 317 -> Select (function
    | -1 -> r238
    | _ -> r230)
  | 316 -> Select (function
    | -1 -> r239
    | _ -> r231)
  | 320 -> Select (function
    | -1 -> r251
    | _ -> r245)
  | 319 -> Select (function
    | -1 -> r252
    | _ -> r246)
  | 544 -> Select (function
    | -1 -> r421
    | _ -> r426)
  | 959 -> Select (function
    | -1 -> r421
    | _ -> r603)
  | 948 -> Select (function
    | 60 | 153 | 315 | 495 | 542 | 548 | 1952 | 1954 | 1956 | 2113 -> r592
    | _ -> r586)
  | 2695 -> Select (function
    | 2620 -> r586
    | _ -> r592)
  | 1976 -> Select (function
    | -1 -> r1053
    | _ -> r1046)
  | 1975 -> Select (function
    | -1 -> r1054
    | _ -> r1047)
  | 1974 -> Select (function
    | -1 -> r1055
    | _ -> r1048)
  | 1973 -> Select (function
    | -1 -> r1056
    | _ -> r1049)
  | 2036 -> Select (function
    | -1 -> r1083
    | _ -> r1091)
  | 2035 -> Select (function
    | -1 -> r1084
    | _ -> r1092)
  | 2034 -> Select (function
    | -1 -> r1085
    | _ -> r1093)
  | _ -> raise Not_found
