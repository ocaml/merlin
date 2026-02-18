open Parser_raw

module Default = struct

  open Parsetree
  open Ast_helper

  let default_loc = ref Location.none

  let default_expr () =
    let id = Location.mkloc Ast_helper.hole_txt !default_loc in
    Exp.mk ~loc:!default_loc (Pexp_extension (id, PStr []))

  let default_pattern () = Pat.any ~loc:!default_loc ()

  let default_module_expr () = Mod.structure ~loc:!default_loc []
  let default_module_type () = Mty.signature ~loc:!default_loc []

  let value (type a) : a MenhirInterpreter.symbol -> a = function
    | MenhirInterpreter.T MenhirInterpreter.T_error -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WITH -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WHILE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_WHEN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_VIRTUAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_VAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_UNDERSCORE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_UIDENT -> "_"
    | MenhirInterpreter.T MenhirInterpreter.T_TYPE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TRY -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TRUE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TO -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_TILDE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_THEN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_STRUCT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_STRING -> ("", Location.none, None)
    | MenhirInterpreter.T MenhirInterpreter.T_STAR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_SIG -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_SEMISEMI -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_SEMI -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_RPAREN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_REC -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_RBRACKET -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_RBRACE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_QUOTED_STRING_ITEM -> ("", Location.none, "", Location.none, None)
    | MenhirInterpreter.T MenhirInterpreter.T_QUOTED_STRING_EXPR -> ("", Location.none, "", Location.none, None)
    | MenhirInterpreter.T MenhirInterpreter.T_QUOTE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_QUESTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PRIVATE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_PREFIXOP -> "!+"
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
    | MenhirInterpreter.T MenhirInterpreter.T_METAOCAML_ESCAPE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_METAOCAML_BRACKET_OPEN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_METAOCAML_BRACKET_CLOSE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_MATCH -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LPAREN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_LIDENT -> "_"
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
    | MenhirInterpreter.T MenhirInterpreter.T_GREATER -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FUNCTOR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FUNCTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FUN -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FOR -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_FLOAT -> ("0.",None)
    | MenhirInterpreter.T MenhirInterpreter.T_FALSE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EXTERNAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EXCEPTION -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EQUAL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EOL -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EOF -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_END -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_ELSE -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_EFFECT -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOWNTO -> ()
    | MenhirInterpreter.T MenhirInterpreter.T_DOTOP -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_val_extra_ident -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_simple_param_pattern -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nontrivial_llist_COMMA_core_type_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_nonempty_concat_fun_param_as_list_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_llist_preceded_CONSTRAINT_constrain__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_labeled_tuple_pattern_pattern_no_exn_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_labeled_tuple_pattern_pattern_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_reversed_labeled_tuple_body -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_pattern -> default_pattern ()
    | MenhirInterpreter.N MenhirInterpreter.N_parse_val_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_mty_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_module_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_module_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_mod_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_mod_ext_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_expression -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_core_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_constr_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_parse_any_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_paren_module_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_optlabel -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_type_constraint_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_seq_expr__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_pattern__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_module_type__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_EQUAL_expr__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_COLON_core_type__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_COLON_atomic_type__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_preceded_AS_mkrhs_LIDENT___ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_SEMI_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_option_BAR_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_opt_ampersand -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_operator -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_open_description -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_open_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_object_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_nonempty_type_kind -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_nonempty_list_raw_string_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_nonempty_list_mkrhs_LIDENT__ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_name_tag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mutable_virtual_flags -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mutable_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mty_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_type_subst -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_type_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_type -> default_module_type ()
    | MenhirInterpreter.N MenhirInterpreter.N_module_subst -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_name -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_expr -> default_module_expr ()
    | MenhirInterpreter.N MenhirInterpreter.N_module_declaration_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_module_binding_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mod_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mod_ext_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_longident_val_ident_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_longident_UIDENT_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_longident_LIDENT_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident_ident_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident___anonymous_43_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident_UIDENT_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_mk_longident_mod_ext_longident_LIDENT_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_method_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_meth_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_match_case -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_local_structure_item -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_let_binding_body_no_punning -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_let_binding_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_labeled_tuple_pattern_pattern_no_exn_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_labeled_tuple_pattern_pattern_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_labeled_tuple_pat_element_list_pattern_no_exn_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_labeled_tuple_pat_element_list_pattern_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_labeled_simple_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_let_pattern -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_declaration_semi -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_label_declaration -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_item_extension -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_interface -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_index_mod -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_implementation -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generic_type_declaration_nonrec_flag_type_kind_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generic_type_declaration_no_nonrec_flag_type_subst_kind_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generic_constructor_declaration_epsilon_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generic_constructor_declaration_BAR_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_generalized_constructor_arguments -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_functor_args -> []
    | MenhirInterpreter.N MenhirInterpreter.N_functor_arg -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_function_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_fun_seq_expr -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_fun_params -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_fun_param_as_list -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_fun_expr -> default_expr ()
    | MenhirInterpreter.N MenhirInterpreter.N_fun_body -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_formal_class_parameters -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_floating_attribute -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_rebind_epsilon_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension_constructor_rebind_BAR_ -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_extension -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_ext -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_expr_colon_package_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_direction_flag -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_delimited_type_supporting_local_open -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_delimited_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_core_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constructor_declarations -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constructor_arguments -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constrain_field -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constr_longident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constr_ident -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_constr_extra_nonprefix_ident -> raise Not_found
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
    | MenhirInterpreter.N MenhirInterpreter.N_attr_payload -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_attr_id -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_atomic_type -> raise Not_found
    | MenhirInterpreter.N MenhirInterpreter.N_any_longident -> raise Not_found
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
  [|0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;3;2;2;1;2;1;2;3;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;2;1;2;3;4;5;2;3;4;5;2;3;4;5;1;1;1;1;1;1;2;3;1;4;5;1;1;1;1;1;1;2;1;2;3;1;1;2;3;4;5;6;1;1;2;1;2;3;1;1;2;4;1;2;1;1;1;2;2;1;1;1;2;2;1;2;3;2;3;5;6;1;1;1;1;2;1;1;2;1;2;3;4;5;6;7;8;1;2;3;4;1;1;1;2;1;1;2;3;4;5;6;7;8;1;2;1;2;3;1;2;3;1;1;1;2;3;4;1;1;1;2;1;2;1;1;1;1;1;2;3;1;1;1;2;3;4;1;1;2;1;2;2;1;1;1;1;2;3;4;2;3;1;2;3;1;2;2;1;2;1;1;2;1;2;2;3;4;5;4;1;2;1;1;2;1;1;1;1;1;1;2;3;2;1;2;1;2;3;2;3;2;3;2;3;4;5;3;1;1;2;3;4;3;3;3;2;3;4;5;6;7;8;2;2;3;2;3;4;3;1;1;3;4;2;3;1;2;1;3;4;2;3;5;1;2;1;2;1;2;3;2;3;4;5;3;4;3;4;4;5;6;2;1;5;6;7;8;9;10;11;12;13;9;1;2;2;1;2;2;1;1;2;3;4;1;5;6;6;1;2;1;2;3;1;2;1;4;2;1;2;1;1;2;3;3;1;1;3;1;2;4;5;4;5;6;2;3;4;5;1;1;2;3;4;5;2;1;2;3;3;1;1;1;2;3;2;3;1;1;4;5;2;3;4;2;3;4;1;3;2;3;5;3;4;5;7;8;1;1;1;2;1;2;3;1;1;2;2;1;1;2;3;1;1;2;1;1;1;1;1;1;4;1;1;2;3;1;1;1;2;3;4;1;2;3;4;5;6;7;8;9;5;4;5;1;1;1;1;2;3;1;1;2;3;4;1;1;1;2;2;1;1;2;3;4;5;6;7;8;4;3;4;3;3;2;3;1;2;3;1;2;4;5;6;2;3;2;3;2;3;4;5;6;7;8;4;3;4;3;3;3;4;5;2;3;2;3;2;4;4;4;4;5;4;5;3;4;2;3;1;2;3;3;2;3;4;5;1;6;5;2;2;3;8;9;8;8;2;3;4;5;6;7;8;9;5;4;5;4;4;2;3;3;4;5;4;1;1;2;1;3;4;5;1;1;1;2;3;1;4;1;1;1;1;1;2;3;1;1;1;1;2;1;1;2;3;4;5;6;7;8;9;10;11;12;13;9;8;9;8;1;8;2;3;2;1;1;1;2;3;4;5;6;7;8;4;3;4;3;3;2;3;4;5;6;7;8;9;5;4;5;4;4;1;2;3;4;5;6;7;8;9;5;4;5;4;4;1;1;2;1;2;1;2;3;3;1;3;4;2;1;2;3;1;1;2;3;1;1;2;3;4;1;2;3;1;1;1;1;1;1;1;2;3;1;2;3;1;1;1;2;1;1;2;3;4;1;1;4;5;6;1;2;3;4;1;5;2;3;2;3;3;4;5;2;2;1;1;7;8;9;10;1;1;1;1;2;3;4;1;2;2;3;2;3;1;2;3;1;2;3;1;1;2;1;2;3;1;1;2;1;2;3;3;4;5;1;2;1;2;3;4;2;3;4;5;6;7;1;2;3;4;5;6;7;8;2;1;1;1;2;4;1;2;5;6;1;2;3;4;5;6;7;8;1;2;3;4;9;10;7;6;7;2;3;2;3;1;2;3;4;5;1;2;3;4;1;1;1;1;1;2;3;4;5;1;1;1;2;3;4;5;2;1;2;1;2;1;2;2;3;1;2;3;4;5;6;1;2;3;4;5;6;7;4;3;4;3;4;5;6;2;3;1;2;1;2;3;1;1;2;3;4;5;6;3;2;3;4;5;6;3;2;1;2;1;2;3;4;5;2;2;3;4;5;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;3;2;3;4;5;6;7;4;3;4;3;4;5;6;3;2;3;4;5;6;3;1;2;1;1;2;2;3;4;5;6;7;8;3;4;5;6;7;2;3;4;2;1;1;2;3;1;4;1;1;2;3;4;5;1;2;3;2;3;4;5;6;7;8;4;3;4;3;3;2;3;2;3;1;3;1;2;4;2;3;1;2;3;4;5;6;7;8;9;10;7;3;4;5;6;7;8;9;10;7;3;4;5;6;7;8;9;10;7;2;3;4;5;6;7;8;9;10;7;3;4;5;6;7;8;9;10;7;3;4;5;6;7;8;9;10;7;3;4;5;6;7;8;9;10;11;12;9;5;6;7;8;9;10;11;12;9;5;6;7;8;9;10;11;12;9;3;4;5;6;7;8;5;1;2;2;1;2;6;1;1;7;8;9;10;11;4;5;3;4;5;3;4;5;3;4;5;6;7;5;6;7;5;6;7;3;6;7;8;9;6;7;3;4;5;2;3;3;2;4;4;5;6;7;8;9;10;11;12;13;14;11;6;7;8;9;10;11;8;4;5;3;4;5;3;4;5;3;4;5;6;7;8;5;4;5;4;5;6;7;4;5;1;2;3;2;3;4;2;3;1;1;4;5;3;4;4;5;3;4;4;5;3;4;5;3;1;2;3;1;2;3;4;5;1;4;5;1;2;3;3;4;4;4;5;2;3;2;3;4;5;2;2;3;4;2;2;3;2;3;1;2;3;4;5;6;5;6;7;8;1;2;3;2;3;4;5;4;5;5;6;2;3;4;5;1;2;3;4;5;1;2;6;7;2;3;4;5;1;2;1;2;3;4;6;7;1;2;3;4;5;6;1;2;8;4;5;6;1;2;1;2;3;4;1;2;1;2;3;4;5;1;2;3;4;5;6;7;1;2;8;9;1;2;3;1;1;2;3;1;4;1;1;1;2;3;1;2;3;4;5;6;7;1;2;3;1;2;1;1;2;3;2;1;5;1;1;2;3;6;7;8;1;2;3;4;5;6;4;2;3;4;2;5;6;7;1;1;1;1;2;3;4;5;6;2;3;4;5;1;2;3;4;5;6;7;8;2;3;4;5;6;7;4;5;6;7;8;1;2;3;4;5;6;7;9;4;5;6;7;1;2;5;6;1;2;1;2;3;4;5;1;2;3;4;1;2;3;4;1;5;1;2;3;6;7;8;1;2;1;2;3;3;1;2;1;2;1;2;3;4;5;6;7;1;2;1;2;1;2;3;4;5;6;7;1;2;1;2;3;4;5;6;1;2;3;4;2;3;1;1;1;7;2;3;4;5;6;3;4;1;2;1;2;3;3;4;4;5;1;2;1;1;2;9;10;1;2;3;4;5;6;7;8;9;11;2;3;4;5;6;7;1;2;3;4;1;1;1;2;1;2;3;1;1;4;1;3;5;8;9;1;2;3;4;5;6;7;8;9;10;1;1;1;1;1;1;1;1;2;1;2;1;1;2;3;4;5;6;7;8;2;1;1;2;3;4;5;1;1;2;3;1;2;1;1;2;3;4;1;1;2;6;7;8;9;1;1;1;2;3;4;5;6;4;4;1;2;3;3;4;5;3;3;1;2;1;1;2;2;1;2;1;2;3;4;5;6;1;1;1;2;3;1;1;2;1;3;4;5;6;7;8;9;10;11;6;7;8;5;2;3;1;1;2;1;2;2;3;4;5;2;3;4;5;6;7;8;9;10;5;6;7;4;1;2;3;4;1;2;3;1;1;2;3;4;5;6;7;2;3;4;5;6;1;2;3;4;1;2;1;2;1;2;1;1;2;1;3;2;2;3;2;3;7;3;4;5;6;2;3;4;5;2;3;3;4;5;4;1;2;5;6;2;3;4;5;1;2;3;4;4;5;1;2;1;1;2;2;1;2;3;4;1;2;7;8;1;2;3;4;5;6;7;8;9;1;1;1;2;3;4;5;6;1;1;1;1;1;1;2;1;1;2;1;2;1;1;1;1;2;3;1;1;1;3;4;3;4;4;5;6;2;4;5;2;2;3;4;5;6;3;2;2;3;2;2;3;4;5;6;6;7;8;2;3;3;4;4;5;6;2;3;2;6;5;6;7;2;3;1;1;2;1;2;2;3;4;5;2;3;4;5;4;5;6;5;6;2;3;4;2;3;4;2;3;5;6;1;2;3;4;5;6;1;7;1;2;3;2;2;3;2;4;5;6;7;8;9;10;11;8;7;8;7;8;9;10;7;2;3;4;5;6;7;8;5;4;5;4;5;6;7;4;4;5;6;3;4;9;6;7;8;1;2;3;4;5;9;10;2;2;1;1;1;1;1;2;3;4;4;5;6;7;8;5;6;7;8;9;3;4;5;9;10;11;12;4;5;6;7;8;9;3;4;5;3;4;5;6;7;2;3;4;5;6;7;2;3;4;2;2;2;2;7;8;9;10;6;7;8;9;10;2;1;1;4;5;6;7;8;9;5;6;7;8;9;3;4;7;8;9;10;6;7;3;4;3;4;5;6;7;1;2;1;0;1;2;1;0;1;2;3;1;1;1;2;3;4;5;3;3;1;1;1;1;2;0;1;1;2;0;1;1;2;0;1;2;1;0;1;1;2;0;1;1;2;0;1;1;2;0;1;1;2;0;1;1;2;0;1;2;1;0;1;2;1;1;2;0;1;2;3;3;3;3;3;3;1;1;1;2;1;2;1;2;3;1;2;0;1;1;1;2;2;2;3;4;2;1;1;2;3;4;1;2;|]

let can_pop (type a) : a terminal -> bool = function
  | T_WITH -> true
  | T_WHILE -> true
  | T_WHEN -> true
  | T_VIRTUAL -> true
  | T_VAL -> true
  | T_UNDERSCORE -> true
  | T_TYPE -> true
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
  | T_METAOCAML_ESCAPE -> true
  | T_METAOCAML_BRACKET_OPEN -> true
  | T_METAOCAML_BRACKET_CLOSE -> true
  | T_MATCH -> true
  | T_LPAREN -> true
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
  | T_GREATER -> true
  | T_FUNCTOR -> true
  | T_FUNCTION -> true
  | T_FUN -> true
  | T_FOR -> true
  | T_FALSE -> true
  | T_EXTERNAL -> true
  | T_EXCEPTION -> true
  | T_EQUAL -> true
  | T_EOL -> true
  | T_END -> true
  | T_ELSE -> true
  | T_EFFECT -> true
  | T_DOWNTO -> true
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
  let r0 = [R 237] in
  let r1 = S (N N_fun_expr) :: r0 in
  let r2 = [R 725] in
  let r3 = Sub (r1) :: r2 in
  let r4 = [R 155] in
  let r5 = S (T T_DONE) :: r4 in
  let r6 = Sub (r3) :: r5 in
  let r7 = S (T T_DO) :: r6 in
  let r8 = Sub (r3) :: r7 in
  let r9 = R 362 :: r8 in
  let r10 = [R 832] in
  let r11 = S (T T_AND) :: r10 in
  let r12 = [R 32] in
  let r13 = Sub (r11) :: r12 in
  let r14 = [R 133] in
  let r15 = [R 33] in
  let r16 = [R 599] in
  let r17 = S (N N_structure) :: r16 in
  let r18 = [R 34] in
  let r19 = Sub (r17) :: r18 in
  let r20 = [R 35] in
  let r21 = S (T T_RBRACKET) :: r20 in
  let r22 = Sub (r19) :: r21 in
  let r23 = [R 932] in
  let r24 = S (T T_LIDENT) :: r23 in
  let r25 = [R 31] in
  let r26 = S (T T_UNDERSCORE) :: r25 in
  let r27 = [R 905] in
  let r28 = Sub (r26) :: r27 in
  let r29 = [R 241] in
  let r30 = Sub (r28) :: r29 in
  let r31 = [R 17] in
  let r32 = Sub (r30) :: r31 in
  let r33 = [R 108] in
  let r34 = Sub (r32) :: r33 in
  let r35 = [R 604] in
  let r36 = Sub (r34) :: r35 in
  let r37 = [R 940] in
  let r38 = R 368 :: r37 in
  let r39 = Sub (r36) :: r38 in
  let r40 = S (T T_COLON) :: r39 in
  let r41 = Sub (r24) :: r40 in
  let r42 = R 362 :: r41 in
  let r43 = [R 537] in
  let r44 = S (T T_AMPERAMPER) :: r43 in
  let r45 = [R 931] in
  let r46 = S (T T_RPAREN) :: r45 in
  let r47 = Sub (r44) :: r46 in
  let r48 = [R 511] in
  let r49 = S (T T_RPAREN) :: r48 in
  let r50 = R 270 :: r49 in
  let r51 = [R 271] in
  let r52 = [R 513] in
  let r53 = S (T T_RBRACKET) :: r52 in
  let r54 = [R 515] in
  let r55 = S (T T_RBRACE) :: r54 in
  let r56 = [R 426] in
  let r57 = [R 135] in
  let r58 = [R 268] in
  let r59 = S (T T_LIDENT) :: r58 in
  let r60 = [R 685] in
  let r61 = [R 30] in
  let r62 = Sub (r59) :: r61 in
  let r63 = [R 563] in
  let r64 = S (T T_COLON) :: r63 in
  let r65 = [R 114] in
  let r66 = S (T T_RPAREN) :: r65 in
  let r67 = S (N N_module_type) :: r66 in
  let r68 = R 362 :: r67 in
  let r69 = R 132 :: r68 in
  let r70 = S (T T_MODULE) :: r69 in
  let r71 = [R 251] in
  let r72 = Sub (r30) :: r71 in
  let r73 = S (T T_MINUSGREATER) :: r72 in
  let r74 = S (T T_RPAREN) :: r73 in
  let r75 = S (N N_module_type) :: r74 in
  let r76 = S (T T_COLON) :: r75 in
  let r77 = S (T T_UIDENT) :: r76 in
  let r78 = R 362 :: r77 in
  let r79 = R 132 :: r78 in
  let r80 = [R 728] in
  let r81 = R 370 :: r80 in
  let r82 = [R 462] in
  let r83 = S (T T_END) :: r82 in
  let r84 = Sub (r81) :: r83 in
  let r85 = [R 265] in
  let r86 = R 368 :: r85 in
  let r87 = R 675 :: r86 in
  let r88 = R 910 :: r87 in
  let r89 = S (T T_LIDENT) :: r88 in
  let r90 = R 914 :: r89 in
  let r91 = R 362 :: r90 in
  let r92 = R 132 :: r91 in
  let r93 = [R 424] in
  let r94 = S (T T_LIDENT) :: r93 in
  let r95 = [R 912] in
  let r96 = Sub (r94) :: r95 in
  let r97 = [R 93] in
  let r98 = S (T T_FALSE) :: r97 in
  let r99 = [R 97] in
  let r100 = Sub (r98) :: r99 in
  let r101 = [R 262] in
  let r102 = R 362 :: r101 in
  let r103 = R 255 :: r102 in
  let r104 = Sub (r100) :: r103 in
  let r105 = [R 630] in
  let r106 = Sub (r104) :: r105 in
  let r107 = [R 735] in
  let r108 = R 368 :: r107 in
  let r109 = Sub (r106) :: r108 in
  let r110 = R 610 :: r109 in
  let r111 = S (T T_PLUSEQ) :: r110 in
  let r112 = Sub (r96) :: r111 in
  let r113 = R 914 :: r112 in
  let r114 = R 362 :: r113 in
  let r115 = [R 266] in
  let r116 = R 368 :: r115 in
  let r117 = R 675 :: r116 in
  let r118 = R 910 :: r117 in
  let r119 = S (T T_LIDENT) :: r118 in
  let r120 = R 914 :: r119 in
  let r121 = [R 736] in
  let r122 = R 368 :: r121 in
  let r123 = Sub (r106) :: r122 in
  let r124 = R 610 :: r123 in
  let r125 = S (T T_PLUSEQ) :: r124 in
  let r126 = Sub (r96) :: r125 in
  let r127 = [R 918] in
  let r128 = S (T T_UNDERSCORE) :: r127 in
  let r129 = [R 913] in
  let r130 = Sub (r128) :: r129 in
  let r131 = R 919 :: r130 in
  let r132 = [R 698] in
  let r133 = Sub (r131) :: r132 in
  let r134 = [R 916] in
  let r135 = S (T T_RPAREN) :: r134 in
  let r136 = [R 917] in
  let r137 = [R 699] in
  let r138 = [R 493] in
  let r139 = S (T T_DOTDOT) :: r138 in
  let r140 = [R 911] in
  let r141 = [R 494] in
  let r142 = [R 96] in
  let r143 = S (T T_RPAREN) :: r142 in
  let r144 = [R 92] in
  let r145 = [R 702] in
  let r146 = Sub (r26) :: r145 in
  let r147 = [R 249] in
  let r148 = Sub (r146) :: r147 in
  let r149 = S (T T_STAR) :: r148 in
  let r150 = Sub (r26) :: r149 in
  let r151 = [R 250] in
  let r152 = Sub (r30) :: r151 in
  let r153 = S (T T_MINUSGREATER) :: r152 in
  let r154 = S (T T_RPAREN) :: r153 in
  let r155 = S (N N_module_type) :: r154 in
  let r156 = [R 465] in
  let r157 = S (N N_module_expr) :: r156 in
  let r158 = R 362 :: r157 in
  let r159 = S (T T_OF) :: r158 in
  let r160 = [R 438] in
  let r161 = [R 450] in
  let r162 = S (T T_END) :: r161 in
  let r163 = S (N N_structure) :: r162 in
  let r164 = [R 790] in
  let r165 = [R 624] in
  let r166 = Sub (r104) :: r165 in
  let r167 = [R 397] in
  let r168 = R 368 :: r167 in
  let r169 = Sub (r166) :: r168 in
  let r170 = R 610 :: r169 in
  let r171 = S (T T_PLUSEQ) :: r170 in
  let r172 = Sub (r96) :: r171 in
  let r173 = R 914 :: r172 in
  let r174 = R 362 :: r173 in
  let r175 = [R 398] in
  let r176 = R 368 :: r175 in
  let r177 = Sub (r166) :: r176 in
  let r178 = R 610 :: r177 in
  let r179 = S (T T_PLUSEQ) :: r178 in
  let r180 = Sub (r96) :: r179 in
  let r181 = [R 608] in
  let r182 = S (T T_RBRACKET) :: r181 in
  let r183 = Sub (r19) :: r182 in
  let r184 = [R 407] in
  let r185 = Sub (r3) :: r184 in
  let r186 = S (T T_MINUSGREATER) :: r185 in
  let r187 = S (N N_pattern) :: r186 in
  let r188 = [R 687] in
  let r189 = Sub (r187) :: r188 in
  let r190 = [R 148] in
  let r191 = Sub (r189) :: r190 in
  let r192 = S (T T_WITH) :: r191 in
  let r193 = Sub (r3) :: r192 in
  let r194 = R 362 :: r193 in
  let r195 = [R 653] in
  let r196 = S (N N_fun_expr) :: r195 in
  let r197 = S (T T_COMMA) :: r196 in
  let r198 = [R 907] in
  let r199 = Sub (r34) :: r198 in
  let r200 = S (T T_COLON) :: r199 in
  let r201 = [R 658] in
  let r202 = S (N N_fun_expr) :: r201 in
  let r203 = S (T T_COMMA) :: r202 in
  let r204 = S (T T_RPAREN) :: r203 in
  let r205 = Sub (r200) :: r204 in
  let r206 = [R 909] in
  let r207 = [R 503] in
  let r208 = [R 252] in
  let r209 = [R 466] in
  let r210 = S (T T_RPAREN) :: r209 in
  let r211 = [R 460] in
  let r212 = [R 134] in
  let r213 = S (T T_RBRACKET) :: r212 in
  let r214 = Sub (r17) :: r213 in
  let r215 = [R 374] in
  let r216 = [R 274] in
  let r217 = S (T T_UNDERSCORE) :: r164 in
  let r218 = [R 780] in
  let r219 = [R 775] in
  let r220 = S (T T_END) :: r219 in
  let r221 = R 379 :: r220 in
  let r222 = R 60 :: r221 in
  let r223 = R 362 :: r222 in
  let r224 = [R 58] in
  let r225 = S (T T_RPAREN) :: r224 in
  let r226 = [R 818] in
  let r227 = [R 667] in
  let r228 = S (T T_DOTDOT) :: r227 in
  let r229 = S (T T_COMMA) :: r228 in
  let r230 = [R 668] in
  let r231 = S (T T_DOTDOT) :: r230 in
  let r232 = S (T T_COMMA) :: r231 in
  let r233 = S (T T_RPAREN) :: r232 in
  let r234 = Sub (r34) :: r233 in
  let r235 = S (T T_COLON) :: r234 in
  let r236 = [R 709] in
  let r237 = Sub (r34) :: r236 in
  let r238 = [R 694] in
  let r239 = Sub (r237) :: r238 in
  let r240 = [R 120] in
  let r241 = S (T T_RBRACKET) :: r240 in
  let r242 = Sub (r239) :: r241 in
  let r243 = [R 119] in
  let r244 = S (T T_RBRACKET) :: r243 in
  let r245 = [R 118] in
  let r246 = S (T T_RBRACKET) :: r245 in
  let r247 = [R 482] in
  let r248 = Sub (r59) :: r247 in
  let r249 = S (T T_BACKQUOTE) :: r248 in
  let r250 = [R 893] in
  let r251 = R 362 :: r250 in
  let r252 = Sub (r249) :: r251 in
  let r253 = [R 115] in
  let r254 = S (T T_RBRACKET) :: r253 in
  let r255 = [R 86] in
  let r256 = Sub (r94) :: r255 in
  let r257 = [R 26] in
  let r258 = [R 425] in
  let r259 = S (T T_LIDENT) :: r258 in
  let r260 = S (T T_DOT) :: r259 in
  let r261 = S (T T_UIDENT) :: r56 in
  let r262 = [R 442] in
  let r263 = Sub (r261) :: r262 in
  let r264 = [R 443] in
  let r265 = S (T T_RPAREN) :: r264 in
  let r266 = [R 427] in
  let r267 = S (T T_UIDENT) :: r266 in
  let r268 = [R 116] in
  let r269 = S (T T_RBRACKET) :: r268 in
  let r270 = [R 247] in
  let r271 = [R 243] in
  let r272 = Sub (r30) :: r271 in
  let r273 = S (T T_MINUSGREATER) :: r272 in
  let r274 = [R 25] in
  let r275 = Sub (r96) :: r274 in
  let r276 = [R 28] in
  let r277 = [R 706] in
  let r278 = S (T T_DOT) :: r267 in
  let r279 = S (T T_LBRACKETGREATER) :: r244 in
  let r280 = [R 29] in
  let r281 = Sub (r279) :: r280 in
  let r282 = [R 113] in
  let r283 = [R 906] in
  let r284 = [R 703] in
  let r285 = Sub (r26) :: r284 in
  let r286 = [R 27] in
  let r287 = [R 704] in
  let r288 = [R 705] in
  let r289 = [R 18] in
  let r290 = Sub (r59) :: r289 in
  let r291 = [R 242] in
  let r292 = Sub (r30) :: r291 in
  let r293 = S (T T_MINUSGREATER) :: r292 in
  let r294 = S (T T_RPAREN) :: r293 in
  let r295 = Sub (r34) :: r294 in
  let r296 = [R 686] in
  let r297 = [R 707] in
  let r298 = [R 695] in
  let r299 = [R 690] in
  let r300 = Sub (r32) :: r299 in
  let r301 = [R 892] in
  let r302 = R 362 :: r301 in
  let r303 = Sub (r300) :: r302 in
  let r304 = [R 691] in
  let r305 = [R 363] in
  let r306 = [R 117] in
  let r307 = S (T T_RBRACKET) :: r306 in
  let r308 = Sub (r239) :: r307 in
  let r309 = [R 683] in
  let r310 = Sub (r249) :: r309 in
  let r311 = [R 121] in
  let r312 = S (T T_RBRACKET) :: r311 in
  let r313 = [R 307] in
  let r314 = [R 308] in
  let r315 = S (T T_RPAREN) :: r314 in
  let r316 = Sub (r34) :: r315 in
  let r317 = S (T T_COLON) :: r316 in
  let r318 = [R 751] in
  let r319 = [R 749] in
  let r320 = [R 814] in
  let r321 = S (T T_RPAREN) :: r320 in
  let r322 = S (N N_pattern) :: r321 in
  let r323 = S (T T_UNDERSCORE) :: r211 in
  let r324 = [R 816] in
  let r325 = S (T T_RPAREN) :: r324 in
  let r326 = Sub (r323) :: r325 in
  let r327 = R 362 :: r326 in
  let r328 = [R 817] in
  let r329 = S (T T_RPAREN) :: r328 in
  let r330 = [R 463] in
  let r331 = S (N N_module_type) :: r330 in
  let r332 = S (T T_MINUSGREATER) :: r331 in
  let r333 = S (N N_functor_args) :: r332 in
  let r334 = [R 253] in
  let r335 = S (T T_RPAREN) :: r334 in
  let r336 = S (N N_module_type) :: r335 in
  let r337 = [R 434] in
  let r338 = Sub (r59) :: r337 in
  let r339 = [R 474] in
  let r340 = Sub (r338) :: r339 in
  let r341 = [R 953] in
  let r342 = S (N N_module_type) :: r341 in
  let r343 = S (T T_EQUAL) :: r342 in
  let r344 = Sub (r340) :: r343 in
  let r345 = S (T T_TYPE) :: r344 in
  let r346 = S (T T_MODULE) :: r345 in
  let r347 = [R 692] in
  let r348 = Sub (r346) :: r347 in
  let r349 = [R 470] in
  let r350 = [R 436] in
  let r351 = S (T T_LIDENT) :: r350 in
  let r352 = [R 282] in
  let r353 = Sub (r351) :: r352 in
  let r354 = [R 950] in
  let r355 = Sub (r32) :: r354 in
  let r356 = S (T T_COLONEQUAL) :: r355 in
  let r357 = Sub (r353) :: r356 in
  let r358 = [R 437] in
  let r359 = S (T T_LIDENT) :: r358 in
  let r360 = [R 439] in
  let r361 = [R 444] in
  let r362 = [R 949] in
  let r363 = R 675 :: r362 in
  let r364 = [R 676] in
  let r365 = Sub (r34) :: r364 in
  let r366 = S (T T_EQUAL) :: r365 in
  let r367 = [R 435] in
  let r368 = Sub (r59) :: r367 in
  let r369 = [R 464] in
  let r370 = S (N N_module_type) :: r369 in
  let r371 = [R 469] in
  let r372 = [R 954] in
  let r373 = [R 951] in
  let r374 = Sub (r263) :: r373 in
  let r375 = S (T T_UIDENT) :: r360 in
  let r376 = [R 952] in
  let r377 = [R 693] in
  let r378 = [R 756] in
  let r379 = [R 91] in
  let r380 = [R 719] in
  let r381 = S (N N_pattern) :: r380 in
  let r382 = [R 754] in
  let r383 = S (T T_RBRACKET) :: r382 in
  let r384 = [R 388] in
  let r385 = R 556 :: r384 in
  let r386 = R 549 :: r385 in
  let r387 = Sub (r353) :: r386 in
  let r388 = [R 753] in
  let r389 = S (T T_RBRACE) :: r388 in
  let r390 = [R 550] in
  let r391 = [R 557] in
  let r392 = S (T T_UNDERSCORE) :: r226 in
  let r393 = [R 813] in
  let r394 = Sub (r392) :: r393 in
  let r395 = [R 590] in
  let r396 = Sub (r394) :: r395 in
  let r397 = R 362 :: r396 in
  let r398 = [R 87] in
  let r399 = [R 823] in
  let r400 = S (T T_INT) :: r398 in
  let r401 = [R 748] in
  let r402 = Sub (r400) :: r401 in
  let r403 = [R 820] in
  let r404 = [R 825] in
  let r405 = S (T T_RBRACKET) :: r404 in
  let r406 = S (T T_LBRACKET) :: r405 in
  let r407 = [R 826] in
  let r408 = [R 666] in
  let r409 = S (T T_DOTDOT) :: r408 in
  let r410 = S (T T_COMMA) :: r409 in
  let r411 = [R 299] in
  let r412 = [R 300] in
  let r413 = S (T T_RPAREN) :: r412 in
  let r414 = Sub (r34) :: r413 in
  let r415 = S (T T_COLON) :: r414 in
  let r416 = [R 298] in
  let r417 = [R 101] in
  let r418 = [R 584] in
  let r419 = S (N N_pattern) :: r418 in
  let r420 = R 362 :: r419 in
  let r421 = [R 586] in
  let r422 = Sub (r394) :: r421 in
  let r423 = [R 585] in
  let r424 = Sub (r394) :: r423 in
  let r425 = S (T T_COMMA) :: r424 in
  let r426 = [R 589] in
  let r427 = [R 664] in
  let r428 = [R 291] in
  let r429 = [R 292] in
  let r430 = S (T T_RPAREN) :: r429 in
  let r431 = Sub (r34) :: r430 in
  let r432 = S (T T_COLON) :: r431 in
  let r433 = [R 290] in
  let r434 = [R 578] in
  let r435 = [R 587] in
  let r436 = [R 483] in
  let r437 = S (T T_LIDENT) :: r436 in
  let r438 = [R 588] in
  let r439 = Sub (r394) :: r438 in
  let r440 = S (T T_RPAREN) :: r439 in
  let r441 = [R 100] in
  let r442 = S (T T_RPAREN) :: r441 in
  let r443 = [R 665] in
  let r444 = [R 295] in
  let r445 = [R 296] in
  let r446 = S (T T_RPAREN) :: r445 in
  let r447 = Sub (r34) :: r446 in
  let r448 = S (T T_COLON) :: r447 in
  let r449 = [R 294] in
  let r450 = [R 828] in
  let r451 = S (T T_RPAREN) :: r450 in
  let r452 = Sub (r34) :: r451 in
  let r453 = [R 583] in
  let r454 = [R 581] in
  let r455 = [R 99] in
  let r456 = S (T T_RPAREN) :: r455 in
  let r457 = [R 827] in
  let r458 = [R 390] in
  let r459 = [R 755] in
  let r460 = [R 306] in
  let r461 = [R 303] in
  let r462 = [R 304] in
  let r463 = S (T T_RPAREN) :: r462 in
  let r464 = Sub (r34) :: r463 in
  let r465 = S (T T_COLON) :: r464 in
  let r466 = [R 302] in
  let r467 = [R 59] in
  let r468 = S (T T_RPAREN) :: r467 in
  let r469 = [R 936] in
  let r470 = Sub (r3) :: r469 in
  let r471 = S (T T_EQUAL) :: r470 in
  let r472 = S (T T_LIDENT) :: r471 in
  let r473 = R 475 :: r472 in
  let r474 = R 362 :: r473 in
  let r475 = [R 46] in
  let r476 = R 368 :: r475 in
  let r477 = [R 937] in
  let r478 = Sub (r3) :: r477 in
  let r479 = S (T T_EQUAL) :: r478 in
  let r480 = S (T T_LIDENT) :: r479 in
  let r481 = R 475 :: r480 in
  let r482 = [R 57] in
  let r483 = Sub (r351) :: r482 in
  let r484 = [R 772] in
  let r485 = Sub (r483) :: r484 in
  let r486 = R 362 :: r485 in
  let r487 = [R 768] in
  let r488 = [R 769] in
  let r489 = S (T T_METAOCAML_BRACKET_CLOSE) :: r488 in
  let r490 = [R 147] in
  let r491 = Sub (r189) :: r490 in
  let r492 = S (T T_WITH) :: r491 in
  let r493 = Sub (r3) :: r492 in
  let r494 = R 362 :: r493 in
  let r495 = [R 757] in
  let r496 = S (T T_RPAREN) :: r495 in
  let r497 = [R 795] in
  let r498 = [R 211] in
  let r499 = [R 347] in
  let r500 = Sub (r24) :: r499 in
  let r501 = [R 350] in
  let r502 = Sub (r500) :: r501 in
  let r503 = [R 208] in
  let r504 = Sub (r3) :: r503 in
  let r505 = S (T T_IN) :: r504 in
  let r506 = [R 673] in
  let r507 = S (T T_DOTDOT) :: r506 in
  let r508 = S (T T_COMMA) :: r507 in
  let r509 = [R 674] in
  let r510 = S (T T_DOTDOT) :: r509 in
  let r511 = S (T T_COMMA) :: r510 in
  let r512 = S (T T_RPAREN) :: r511 in
  let r513 = Sub (r34) :: r512 in
  let r514 = S (T T_COLON) :: r513 in
  let r515 = [R 327] in
  let r516 = [R 328] in
  let r517 = S (T T_RPAREN) :: r516 in
  let r518 = Sub (r34) :: r517 in
  let r519 = S (T T_COLON) :: r518 in
  let r520 = [R 326] in
  let r521 = [R 591] in
  let r522 = [R 670] in
  let r523 = [R 311] in
  let r524 = [R 312] in
  let r525 = S (T T_RPAREN) :: r524 in
  let r526 = Sub (r34) :: r525 in
  let r527 = S (T T_COLON) :: r526 in
  let r528 = [R 310] in
  let r529 = [R 323] in
  let r530 = [R 324] in
  let r531 = S (T T_RPAREN) :: r530 in
  let r532 = Sub (r34) :: r531 in
  let r533 = S (T T_COLON) :: r532 in
  let r534 = [R 322] in
  let r535 = [R 672] in
  let r536 = S (T T_DOTDOT) :: r535 in
  let r537 = S (T T_COMMA) :: r536 in
  let r538 = [R 319] in
  let r539 = [R 320] in
  let r540 = S (T T_RPAREN) :: r539 in
  let r541 = Sub (r34) :: r540 in
  let r542 = S (T T_COLON) :: r541 in
  let r543 = [R 318] in
  let r544 = [R 807] in
  let r545 = [R 280] in
  let r546 = S (T T_LIDENT) :: r545 in
  let r547 = [R 806] in
  let r548 = S (T T_RPAREN) :: r547 in
  let r549 = [R 281] in
  let r550 = [R 605] in
  let r551 = Sub (r34) :: r550 in
  let r552 = [R 803] in
  let r553 = [R 802] in
  let r554 = S (T T_RPAREN) :: r553 in
  let r555 = R 558 :: r554 in
  let r556 = [R 559] in
  let r557 = [R 332] in
  let r558 = Sub (r24) :: r557 in
  let r559 = [R 339] in
  let r560 = R 368 :: r559 in
  let r561 = Sub (r558) :: r560 in
  let r562 = R 617 :: r561 in
  let r563 = R 362 :: r562 in
  let r564 = R 132 :: r563 in
  let r565 = S (T T_QUOTED_STRING_ITEM) :: r216 in
  let r566 = [R 392] in
  let r567 = R 368 :: r566 in
  let r568 = Sub (r565) :: r567 in
  let r569 = [R 145] in
  let r570 = Sub (r3) :: r569 in
  let r571 = S (T T_IN) :: r570 in
  let r572 = Sub (r568) :: r571 in
  let r573 = R 362 :: r572 in
  let r574 = [R 504] in
  let r575 = R 368 :: r574 in
  let r576 = S (N N_module_expr) :: r575 in
  let r577 = R 362 :: r576 in
  let r578 = [R 505] in
  let r579 = R 368 :: r578 in
  let r580 = S (N N_module_expr) :: r579 in
  let r581 = R 362 :: r580 in
  let r582 = [R 565] in
  let r583 = S (T T_RPAREN) :: r582 in
  let r584 = [R 124] in
  let r585 = S (N N_fun_expr) :: r584 in
  let r586 = [R 566] in
  let r587 = S (T T_RPAREN) :: r586 in
  let r588 = Sub (r585) :: r587 in
  let r589 = [R 710] in
  let r590 = S (N N_fun_expr) :: r589 in
  let r591 = [R 798] in
  let r592 = S (T T_RBRACKET) :: r591 in
  let r593 = [R 783] in
  let r594 = [R 716] in
  let r595 = R 551 :: r594 in
  let r596 = [R 552] in
  let r597 = [R 722] in
  let r598 = R 551 :: r597 in
  let r599 = R 560 :: r598 in
  let r600 = Sub (r353) :: r599 in
  let r601 = [R 619] in
  let r602 = Sub (r600) :: r601 in
  let r603 = [R 792] in
  let r604 = S (T T_RBRACE) :: r603 in
  let r605 = [R 771] in
  let r606 = S (T T_END) :: r605 in
  let r607 = R 362 :: r606 in
  let r608 = [R 158] in
  let r609 = Sub (r217) :: r608 in
  let r610 = R 362 :: r609 in
  let r611 = [R 781] in
  let r612 = [R 791] in
  let r613 = S (T T_RPAREN) :: r612 in
  let r614 = S (T T_LPAREN) :: r613 in
  let r615 = S (T T_DOT) :: r614 in
  let r616 = [R 801] in
  let r617 = S (T T_RPAREN) :: r616 in
  let r618 = S (N N_module_type) :: r617 in
  let r619 = S (T T_COLON) :: r618 in
  let r620 = S (N N_module_expr) :: r619 in
  let r621 = R 362 :: r620 in
  let r622 = [R 451] in
  let r623 = S (N N_module_expr) :: r622 in
  let r624 = S (T T_MINUSGREATER) :: r623 in
  let r625 = S (N N_functor_args) :: r624 in
  let r626 = [R 456] in
  let r627 = [R 564] in
  let r628 = S (T T_RPAREN) :: r627 in
  let r629 = [R 348] in
  let r630 = Sub (r3) :: r629 in
  let r631 = S (T T_EQUAL) :: r630 in
  let r632 = [R 648] in
  let r633 = S (N N_fun_expr) :: r632 in
  let r634 = S (T T_COMMA) :: r633 in
  let r635 = [R 788] in
  let r636 = [R 762] in
  let r637 = S (T T_RPAREN) :: r636 in
  let r638 = Sub (r590) :: r637 in
  let r639 = S (T T_LPAREN) :: r638 in
  let r640 = [R 153] in
  let r641 = S (N N_fun_expr) :: r640 in
  let r642 = S (T T_THEN) :: r641 in
  let r643 = Sub (r3) :: r642 in
  let r644 = R 362 :: r643 in
  let r645 = [R 726] in
  let r646 = Sub (r189) :: r645 in
  let r647 = R 362 :: r646 in
  let r648 = [R 688] in
  let r649 = [R 408] in
  let r650 = Sub (r3) :: r649 in
  let r651 = S (T T_MINUSGREATER) :: r650 in
  let r652 = [R 809] in
  let r653 = Sub (r394) :: r652 in
  let r654 = [R 235] in
  let r655 = Sub (r653) :: r654 in
  let r656 = [R 677] in
  let r657 = Sub (r655) :: r656 in
  let r658 = [R 236] in
  let r659 = Sub (r657) :: r658 in
  let r660 = [R 143] in
  let r661 = Sub (r1) :: r660 in
  let r662 = [R 146] in
  let r663 = Sub (r661) :: r662 in
  let r664 = S (T T_MINUSGREATER) :: r663 in
  let r665 = R 547 :: r664 in
  let r666 = Sub (r659) :: r665 in
  let r667 = R 362 :: r666 in
  let r668 = [R 598] in
  let r669 = S (T T_UNDERSCORE) :: r668 in
  let r670 = [R 805] in
  let r671 = [R 804] in
  let r672 = S (T T_RPAREN) :: r671 in
  let r673 = R 558 :: r672 in
  let r674 = [R 345] in
  let r675 = [R 234] in
  let r676 = S (T T_RPAREN) :: r675 in
  let r677 = [R 811] in
  let r678 = S (T T_RPAREN) :: r677 in
  let r679 = Sub (r34) :: r678 in
  let r680 = [R 808] in
  let r681 = [R 810] in
  let r682 = S (T T_RPAREN) :: r681 in
  let r683 = Sub (r34) :: r682 in
  let r684 = [R 548] in
  let r685 = [R 142] in
  let r686 = Sub (r189) :: r685 in
  let r687 = R 362 :: r686 in
  let r688 = [R 643] in
  let r689 = [R 646] in
  let r690 = [R 647] in
  let r691 = S (T T_RPAREN) :: r690 in
  let r692 = Sub (r200) :: r691 in
  let r693 = [R 908] in
  let r694 = [R 645] in
  let r695 = [R 787] in
  let r696 = [R 759] in
  let r697 = S (T T_RPAREN) :: r696 in
  let r698 = Sub (r3) :: r697 in
  let r699 = S (T T_LPAREN) :: r698 in
  let r700 = [R 123] in
  let r701 = S (T T_DOWNTO) :: r700 in
  let r702 = [R 156] in
  let r703 = S (T T_DONE) :: r702 in
  let r704 = Sub (r3) :: r703 in
  let r705 = S (T T_DO) :: r704 in
  let r706 = Sub (r3) :: r705 in
  let r707 = Sub (r701) :: r706 in
  let r708 = Sub (r3) :: r707 in
  let r709 = S (T T_EQUAL) :: r708 in
  let r710 = S (N N_pattern) :: r709 in
  let r711 = R 362 :: r710 in
  let r712 = [R 157] in
  let r713 = Sub (r217) :: r712 in
  let r714 = R 362 :: r713 in
  let r715 = [R 203] in
  let r716 = [R 204] in
  let r717 = Sub (r189) :: r716 in
  let r718 = R 362 :: r717 in
  let r719 = [R 285] in
  let r720 = [R 286] in
  let r721 = S (T T_RPAREN) :: r720 in
  let r722 = Sub (r200) :: r721 in
  let r723 = [R 287] in
  let r724 = [R 288] in
  let r725 = [R 284] in
  let r726 = [R 712] in
  let r727 = Sub (r189) :: r726 in
  let r728 = R 362 :: r727 in
  let r729 = [R 633] in
  let r730 = [R 636] in
  let r731 = [R 637] in
  let r732 = S (T T_RPAREN) :: r731 in
  let r733 = Sub (r200) :: r732 in
  let r734 = [R 635] in
  let r735 = [R 634] in
  let r736 = Sub (r189) :: r735 in
  let r737 = R 362 :: r736 in
  let r738 = [R 689] in
  let r739 = [R 207] in
  let r740 = Sub (r3) :: r739 in
  let r741 = [R 183] in
  let r742 = [R 184] in
  let r743 = Sub (r189) :: r742 in
  let r744 = R 362 :: r743 in
  let r745 = [R 171] in
  let r746 = [R 172] in
  let r747 = Sub (r189) :: r746 in
  let r748 = R 362 :: r747 in
  let r749 = [R 205] in
  let r750 = [R 206] in
  let r751 = Sub (r189) :: r750 in
  let r752 = R 362 :: r751 in
  let r753 = [R 240] in
  let r754 = Sub (r3) :: r753 in
  let r755 = [R 177] in
  let r756 = [R 178] in
  let r757 = Sub (r189) :: r756 in
  let r758 = R 362 :: r757 in
  let r759 = [R 185] in
  let r760 = [R 186] in
  let r761 = Sub (r189) :: r760 in
  let r762 = R 362 :: r761 in
  let r763 = [R 169] in
  let r764 = [R 170] in
  let r765 = Sub (r189) :: r764 in
  let r766 = R 362 :: r765 in
  let r767 = [R 175] in
  let r768 = [R 176] in
  let r769 = Sub (r189) :: r768 in
  let r770 = R 362 :: r769 in
  let r771 = [R 173] in
  let r772 = [R 174] in
  let r773 = Sub (r189) :: r772 in
  let r774 = R 362 :: r773 in
  let r775 = [R 193] in
  let r776 = [R 194] in
  let r777 = Sub (r189) :: r776 in
  let r778 = R 362 :: r777 in
  let r779 = [R 181] in
  let r780 = [R 182] in
  let r781 = Sub (r189) :: r780 in
  let r782 = R 362 :: r781 in
  let r783 = [R 179] in
  let r784 = [R 180] in
  let r785 = Sub (r189) :: r784 in
  let r786 = R 362 :: r785 in
  let r787 = [R 189] in
  let r788 = [R 190] in
  let r789 = Sub (r189) :: r788 in
  let r790 = R 362 :: r789 in
  let r791 = [R 167] in
  let r792 = [R 168] in
  let r793 = Sub (r189) :: r792 in
  let r794 = R 362 :: r793 in
  let r795 = [R 165] in
  let r796 = [R 166] in
  let r797 = Sub (r189) :: r796 in
  let r798 = R 362 :: r797 in
  let r799 = [R 209] in
  let r800 = [R 210] in
  let r801 = Sub (r189) :: r800 in
  let r802 = R 362 :: r801 in
  let r803 = [R 163] in
  let r804 = [R 164] in
  let r805 = Sub (r189) :: r804 in
  let r806 = R 362 :: r805 in
  let r807 = [R 191] in
  let r808 = [R 192] in
  let r809 = Sub (r189) :: r808 in
  let r810 = R 362 :: r809 in
  let r811 = [R 187] in
  let r812 = [R 188] in
  let r813 = Sub (r189) :: r812 in
  let r814 = R 362 :: r813 in
  let r815 = [R 195] in
  let r816 = [R 196] in
  let r817 = Sub (r189) :: r816 in
  let r818 = R 362 :: r817 in
  let r819 = [R 197] in
  let r820 = [R 198] in
  let r821 = Sub (r189) :: r820 in
  let r822 = R 362 :: r821 in
  let r823 = [R 199] in
  let r824 = [R 200] in
  let r825 = Sub (r189) :: r824 in
  let r826 = R 362 :: r825 in
  let r827 = [R 638] in
  let r828 = [R 641] in
  let r829 = [R 642] in
  let r830 = S (T T_RPAREN) :: r829 in
  let r831 = Sub (r200) :: r830 in
  let r832 = [R 640] in
  let r833 = [R 639] in
  let r834 = Sub (r189) :: r833 in
  let r835 = R 362 :: r834 in
  let r836 = [R 201] in
  let r837 = [R 202] in
  let r838 = Sub (r189) :: r837 in
  let r839 = R 362 :: r838 in
  let r840 = [R 19] in
  let r841 = R 368 :: r840 in
  let r842 = Sub (r558) :: r841 in
  let r843 = [R 883] in
  let r844 = Sub (r3) :: r843 in
  let r845 = [R 336] in
  let r846 = Sub (r3) :: r845 in
  let r847 = S (T T_EQUAL) :: r846 in
  let r848 = Sub (r34) :: r847 in
  let r849 = S (T T_DOT) :: r848 in
  let r850 = [R 335] in
  let r851 = Sub (r3) :: r850 in
  let r852 = S (T T_EQUAL) :: r851 in
  let r853 = Sub (r34) :: r852 in
  let r854 = [R 334] in
  let r855 = Sub (r3) :: r854 in
  let r856 = [R 884] in
  let r857 = Sub (r661) :: r856 in
  let r858 = S (T T_EQUAL) :: r857 in
  let r859 = [R 338] in
  let r860 = Sub (r3) :: r859 in
  let r861 = S (T T_EQUAL) :: r860 in
  let r862 = [R 337] in
  let r863 = Sub (r3) :: r862 in
  let r864 = [R 671] in
  let r865 = [R 315] in
  let r866 = [R 316] in
  let r867 = S (T T_RPAREN) :: r866 in
  let r868 = Sub (r34) :: r867 in
  let r869 = S (T T_COLON) :: r868 in
  let r870 = [R 314] in
  let r871 = [R 596] in
  let r872 = [R 594] in
  let r873 = [R 369] in
  let r874 = [R 221] in
  let r875 = [R 222] in
  let r876 = Sub (r189) :: r875 in
  let r877 = R 362 :: r876 in
  let r878 = [R 766] in
  let r879 = S (T T_RBRACKET) :: r878 in
  let r880 = Sub (r590) :: r879 in
  let r881 = [R 229] in
  let r882 = [R 230] in
  let r883 = Sub (r189) :: r882 in
  let r884 = R 362 :: r883 in
  let r885 = [R 764] in
  let r886 = S (T T_RBRACE) :: r885 in
  let r887 = Sub (r590) :: r886 in
  let r888 = [R 225] in
  let r889 = [R 226] in
  let r890 = Sub (r189) :: r889 in
  let r891 = R 362 :: r890 in
  let r892 = [R 215] in
  let r893 = [R 216] in
  let r894 = Sub (r189) :: r893 in
  let r895 = R 362 :: r894 in
  let r896 = [R 761] in
  let r897 = S (T T_RBRACKET) :: r896 in
  let r898 = Sub (r3) :: r897 in
  let r899 = [R 219] in
  let r900 = [R 220] in
  let r901 = Sub (r189) :: r900 in
  let r902 = R 362 :: r901 in
  let r903 = [R 760] in
  let r904 = S (T T_RBRACE) :: r903 in
  let r905 = Sub (r3) :: r904 in
  let r906 = [R 217] in
  let r907 = [R 218] in
  let r908 = Sub (r189) :: r907 in
  let r909 = R 362 :: r908 in
  let r910 = [R 763] in
  let r911 = S (T T_RPAREN) :: r910 in
  let r912 = Sub (r590) :: r911 in
  let r913 = S (T T_LPAREN) :: r912 in
  let r914 = [R 223] in
  let r915 = [R 224] in
  let r916 = Sub (r189) :: r915 in
  let r917 = R 362 :: r916 in
  let r918 = [R 767] in
  let r919 = S (T T_RBRACKET) :: r918 in
  let r920 = Sub (r590) :: r919 in
  let r921 = [R 231] in
  let r922 = [R 232] in
  let r923 = Sub (r189) :: r922 in
  let r924 = R 362 :: r923 in
  let r925 = [R 765] in
  let r926 = S (T T_RBRACE) :: r925 in
  let r927 = Sub (r590) :: r926 in
  let r928 = [R 227] in
  let r929 = [R 228] in
  let r930 = Sub (r189) :: r929 in
  let r931 = R 362 :: r930 in
  let r932 = [R 213] in
  let r933 = [R 214] in
  let r934 = Sub (r189) :: r933 in
  let r935 = R 362 :: r934 in
  let r936 = [R 644] in
  let r937 = Sub (r189) :: r936 in
  let r938 = R 362 :: r937 in
  let r939 = [R 154] in
  let r940 = Sub (r189) :: r939 in
  let r941 = R 362 :: r940 in
  let r942 = [R 151] in
  let r943 = [R 152] in
  let r944 = Sub (r189) :: r943 in
  let r945 = R 362 :: r944 in
  let r946 = [R 149] in
  let r947 = [R 150] in
  let r948 = Sub (r189) :: r947 in
  let r949 = R 362 :: r948 in
  let r950 = [R 651] in
  let r951 = [R 652] in
  let r952 = S (T T_RPAREN) :: r951 in
  let r953 = Sub (r200) :: r952 in
  let r954 = [R 650] in
  let r955 = [R 649] in
  let r956 = Sub (r189) :: r955 in
  let r957 = R 362 :: r956 in
  let r958 = [R 349] in
  let r959 = Sub (r3) :: r958 in
  let r960 = [R 351] in
  let r961 = [R 785] in
  let r962 = [R 797] in
  let r963 = [R 796] in
  let r964 = [R 800] in
  let r965 = [R 799] in
  let r966 = S (T T_LIDENT) :: r595 in
  let r967 = [R 786] in
  let r968 = S (T T_GREATERRBRACE) :: r967 in
  let r969 = [R 793] in
  let r970 = S (T T_RBRACE) :: r969 in
  let r971 = [R 620] in
  let r972 = Sub (r600) :: r971 in
  let r973 = [R 770] in
  let r974 = [R 553] in
  let r975 = Sub (r189) :: r974 in
  let r976 = R 362 :: r975 in
  let r977 = [R 782] in
  let r978 = [R 125] in
  let r979 = Sub (r189) :: r978 in
  let r980 = R 362 :: r979 in
  let r981 = [R 131] in
  let r982 = [R 127] in
  let r983 = [R 129] in
  let r984 = [R 130] in
  let r985 = [R 126] in
  let r986 = [R 128] in
  let r987 = [R 445] in
  let r988 = S (N N_module_expr) :: r987 in
  let r989 = S (T T_EQUAL) :: r988 in
  let r990 = [R 405] in
  let r991 = R 368 :: r990 in
  let r992 = Sub (r989) :: r991 in
  let r993 = Sub (r323) :: r992 in
  let r994 = R 362 :: r993 in
  let r995 = [R 472] in
  let r996 = R 368 :: r995 in
  let r997 = R 554 :: r996 in
  let r998 = Sub (r59) :: r997 in
  let r999 = R 362 :: r998 in
  let r1000 = R 132 :: r999 in
  let r1001 = [R 555] in
  let r1002 = [R 400] in
  let r1003 = R 358 :: r1002 in
  let r1004 = R 368 :: r1003 in
  let r1005 = Sub (r989) :: r1004 in
  let r1006 = [R 446] in
  let r1007 = S (N N_module_expr) :: r1006 in
  let r1008 = S (T T_EQUAL) :: r1007 in
  let r1009 = [R 359] in
  let r1010 = R 358 :: r1009 in
  let r1011 = R 368 :: r1010 in
  let r1012 = Sub (r989) :: r1011 in
  let r1013 = Sub (r323) :: r1012 in
  let r1014 = [R 447] in
  let r1015 = [R 273] in
  let r1016 = S (T T_RBRACKET) :: r1015 in
  let r1017 = Sub (r17) :: r1016 in
  let r1018 = [R 602] in
  let r1019 = [R 603] in
  let r1020 = [R 139] in
  let r1021 = S (T T_RBRACKET) :: r1020 in
  let r1022 = Sub (r19) :: r1021 in
  let r1023 = [R 888] in
  let r1024 = R 368 :: r1023 in
  let r1025 = S (N N_module_expr) :: r1024 in
  let r1026 = R 362 :: r1025 in
  let r1027 = [R 485] in
  let r1028 = S (T T_STRING) :: r1027 in
  let r1029 = [R 609] in
  let r1030 = R 368 :: r1029 in
  let r1031 = Sub (r1028) :: r1030 in
  let r1032 = S (T T_EQUAL) :: r1031 in
  let r1033 = Sub (r36) :: r1032 in
  let r1034 = S (T T_COLON) :: r1033 in
  let r1035 = Sub (r24) :: r1034 in
  let r1036 = R 362 :: r1035 in
  let r1037 = [R 727] in
  let r1038 = R 368 :: r1037 in
  let r1039 = R 362 :: r1038 in
  let r1040 = R 255 :: r1039 in
  let r1041 = Sub (r100) :: r1040 in
  let r1042 = R 362 :: r1041 in
  let r1043 = R 132 :: r1042 in
  let r1044 = [R 103] in
  let r1045 = Sub (r26) :: r1044 in
  let r1046 = [R 256] in
  let r1047 = [R 606] in
  let r1048 = Sub (r32) :: r1047 in
  let r1049 = [R 275] in
  let r1050 = R 362 :: r1049 in
  let r1051 = Sub (r1048) :: r1050 in
  let r1052 = S (T T_COLON) :: r1051 in
  let r1053 = S (T T_LIDENT) :: r1052 in
  let r1054 = R 475 :: r1053 in
  let r1055 = [R 277] in
  let r1056 = Sub (r1054) :: r1055 in
  let r1057 = [R 105] in
  let r1058 = S (T T_RBRACE) :: r1057 in
  let r1059 = [R 276] in
  let r1060 = R 362 :: r1059 in
  let r1061 = S (T T_SEMI) :: r1060 in
  let r1062 = R 362 :: r1061 in
  let r1063 = Sub (r1048) :: r1062 in
  let r1064 = S (T T_COLON) :: r1063 in
  let r1065 = [R 607] in
  let r1066 = Sub (r32) :: r1065 in
  let r1067 = [R 104] in
  let r1068 = Sub (r26) :: r1067 in
  let r1069 = Sub (r98) :: r417 in
  let r1070 = [R 882] in
  let r1071 = R 368 :: r1070 in
  let r1072 = R 362 :: r1071 in
  let r1073 = S (T T_COLONCOLON) :: r456 in
  let r1074 = [R 259] in
  let r1075 = [R 260] in
  let r1076 = Sub (r26) :: r1075 in
  let r1077 = [R 258] in
  let r1078 = Sub (r26) :: r1077 in
  let r1079 = [R 257] in
  let r1080 = Sub (r26) :: r1079 in
  let r1081 = [R 600] in
  let r1082 = [R 371] in
  let r1083 = [R 506] in
  let r1084 = R 368 :: r1083 in
  let r1085 = Sub (r263) :: r1084 in
  let r1086 = R 362 :: r1085 in
  let r1087 = [R 507] in
  let r1088 = R 368 :: r1087 in
  let r1089 = Sub (r263) :: r1088 in
  let r1090 = R 362 :: r1089 in
  let r1091 = [R 448] in
  let r1092 = S (N N_module_type) :: r1091 in
  let r1093 = S (T T_COLON) :: r1092 in
  let r1094 = [R 738] in
  let r1095 = R 368 :: r1094 in
  let r1096 = Sub (r1093) :: r1095 in
  let r1097 = Sub (r323) :: r1096 in
  let r1098 = R 362 :: r1097 in
  let r1099 = [R 473] in
  let r1100 = R 368 :: r1099 in
  let r1101 = S (N N_module_type) :: r1100 in
  let r1102 = S (T T_COLONEQUAL) :: r1101 in
  let r1103 = Sub (r59) :: r1102 in
  let r1104 = R 362 :: r1103 in
  let r1105 = [R 461] in
  let r1106 = R 368 :: r1105 in
  let r1107 = [R 741] in
  let r1108 = R 360 :: r1107 in
  let r1109 = R 368 :: r1108 in
  let r1110 = S (N N_module_type) :: r1109 in
  let r1111 = S (T T_COLON) :: r1110 in
  let r1112 = [R 361] in
  let r1113 = R 360 :: r1112 in
  let r1114 = R 368 :: r1113 in
  let r1115 = S (N N_module_type) :: r1114 in
  let r1116 = S (T T_COLON) :: r1115 in
  let r1117 = Sub (r323) :: r1116 in
  let r1118 = S (T T_UIDENT) :: r160 in
  let r1119 = Sub (r1118) :: r361 in
  let r1120 = [R 739] in
  let r1121 = R 368 :: r1120 in
  let r1122 = [R 449] in
  let r1123 = [R 745] in
  let r1124 = R 368 :: r1123 in
  let r1125 = S (N N_module_type) :: r1124 in
  let r1126 = R 362 :: r1125 in
  let r1127 = S (T T_QUOTED_STRING_EXPR) :: r57 in
  let r1128 = [R 71] in
  let r1129 = Sub (r1127) :: r1128 in
  let r1130 = [R 81] in
  let r1131 = Sub (r1129) :: r1130 in
  let r1132 = [R 746] in
  let r1133 = R 354 :: r1132 in
  let r1134 = R 368 :: r1133 in
  let r1135 = Sub (r1131) :: r1134 in
  let r1136 = S (T T_COLON) :: r1135 in
  let r1137 = S (T T_LIDENT) :: r1136 in
  let r1138 = R 140 :: r1137 in
  let r1139 = R 941 :: r1138 in
  let r1140 = R 362 :: r1139 in
  let r1141 = [R 85] in
  let r1142 = R 356 :: r1141 in
  let r1143 = R 368 :: r1142 in
  let r1144 = Sub (r1129) :: r1143 in
  let r1145 = S (T T_EQUAL) :: r1144 in
  let r1146 = S (T T_LIDENT) :: r1145 in
  let r1147 = R 140 :: r1146 in
  let r1148 = R 941 :: r1147 in
  let r1149 = R 362 :: r1148 in
  let r1150 = [R 141] in
  let r1151 = S (T T_RBRACKET) :: r1150 in
  let r1152 = [R 72] in
  let r1153 = S (T T_END) :: r1152 in
  let r1154 = R 377 :: r1153 in
  let r1155 = R 62 :: r1154 in
  let r1156 = [R 61] in
  let r1157 = S (T T_RPAREN) :: r1156 in
  let r1158 = [R 64] in
  let r1159 = R 368 :: r1158 in
  let r1160 = Sub (r34) :: r1159 in
  let r1161 = S (T T_COLON) :: r1160 in
  let r1162 = S (T T_LIDENT) :: r1161 in
  let r1163 = R 477 :: r1162 in
  let r1164 = [R 65] in
  let r1165 = R 368 :: r1164 in
  let r1166 = Sub (r36) :: r1165 in
  let r1167 = S (T T_COLON) :: r1166 in
  let r1168 = S (T T_LIDENT) :: r1167 in
  let r1169 = R 612 :: r1168 in
  let r1170 = [R 63] in
  let r1171 = R 368 :: r1170 in
  let r1172 = Sub (r1129) :: r1171 in
  let r1173 = [R 74] in
  let r1174 = Sub (r1129) :: r1173 in
  let r1175 = S (T T_IN) :: r1174 in
  let r1176 = Sub (r1119) :: r1175 in
  let r1177 = R 362 :: r1176 in
  let r1178 = [R 75] in
  let r1179 = Sub (r1129) :: r1178 in
  let r1180 = S (T T_IN) :: r1179 in
  let r1181 = Sub (r1119) :: r1180 in
  let r1182 = [R 696] in
  let r1183 = Sub (r34) :: r1182 in
  let r1184 = [R 70] in
  let r1185 = Sub (r256) :: r1184 in
  let r1186 = S (T T_RBRACKET) :: r1185 in
  let r1187 = Sub (r1183) :: r1186 in
  let r1188 = [R 697] in
  let r1189 = [R 102] in
  let r1190 = Sub (r34) :: r1189 in
  let r1191 = S (T T_EQUAL) :: r1190 in
  let r1192 = Sub (r34) :: r1191 in
  let r1193 = [R 66] in
  let r1194 = R 368 :: r1193 in
  let r1195 = Sub (r1192) :: r1194 in
  let r1196 = [R 67] in
  let r1197 = [R 378] in
  let r1198 = [R 357] in
  let r1199 = R 356 :: r1198 in
  let r1200 = R 368 :: r1199 in
  let r1201 = Sub (r1129) :: r1200 in
  let r1202 = S (T T_EQUAL) :: r1201 in
  let r1203 = S (T T_LIDENT) :: r1202 in
  let r1204 = R 140 :: r1203 in
  let r1205 = R 941 :: r1204 in
  let r1206 = [R 83] in
  let r1207 = Sub (r1131) :: r1206 in
  let r1208 = S (T T_MINUSGREATER) :: r1207 in
  let r1209 = Sub (r28) :: r1208 in
  let r1210 = [R 84] in
  let r1211 = Sub (r1131) :: r1210 in
  let r1212 = [R 82] in
  let r1213 = Sub (r1131) :: r1212 in
  let r1214 = S (T T_MINUSGREATER) :: r1213 in
  let r1215 = [R 355] in
  let r1216 = R 354 :: r1215 in
  let r1217 = R 368 :: r1216 in
  let r1218 = Sub (r1131) :: r1217 in
  let r1219 = S (T T_COLON) :: r1218 in
  let r1220 = S (T T_LIDENT) :: r1219 in
  let r1221 = R 140 :: r1220 in
  let r1222 = R 941 :: r1221 in
  let r1223 = [R 372] in
  let r1224 = [R 729] in
  let r1225 = [R 733] in
  let r1226 = [R 365] in
  let r1227 = R 364 :: r1226 in
  let r1228 = R 368 :: r1227 in
  let r1229 = R 675 :: r1228 in
  let r1230 = R 910 :: r1229 in
  let r1231 = S (T T_LIDENT) :: r1230 in
  let r1232 = R 914 :: r1231 in
  let r1233 = [R 734] in
  let r1234 = [R 367] in
  let r1235 = R 366 :: r1234 in
  let r1236 = R 368 :: r1235 in
  let r1237 = R 675 :: r1236 in
  let r1238 = Sub (r139) :: r1237 in
  let r1239 = S (T T_COLONEQUAL) :: r1238 in
  let r1240 = S (T T_LIDENT) :: r1239 in
  let r1241 = R 914 :: r1240 in
  let r1242 = [R 497] in
  let r1243 = S (T T_RBRACE) :: r1242 in
  let r1244 = [R 501] in
  let r1245 = [R 261] in
  let r1246 = R 362 :: r1245 in
  let r1247 = R 255 :: r1246 in
  let r1248 = Sub (r100) :: r1247 in
  let r1249 = [R 495] in
  let r1250 = [R 496] in
  let r1251 = [R 500] in
  let r1252 = S (T T_RBRACE) :: r1251 in
  let r1253 = [R 499] in
  let r1254 = S (T T_RBRACE) :: r1253 in
  let r1255 = [R 43] in
  let r1256 = Sub (r1127) :: r1255 in
  let r1257 = [R 52] in
  let r1258 = Sub (r1256) :: r1257 in
  let r1259 = S (T T_EQUAL) :: r1258 in
  let r1260 = [R 402] in
  let r1261 = R 352 :: r1260 in
  let r1262 = R 368 :: r1261 in
  let r1263 = Sub (r1259) :: r1262 in
  let r1264 = S (T T_LIDENT) :: r1263 in
  let r1265 = R 140 :: r1264 in
  let r1266 = R 941 :: r1265 in
  let r1267 = R 362 :: r1266 in
  let r1268 = [R 80] in
  let r1269 = S (T T_END) :: r1268 in
  let r1270 = R 379 :: r1269 in
  let r1271 = R 60 :: r1270 in
  let r1272 = S (T T_EQUAL) :: r844 in
  let r1273 = [R 418] in
  let r1274 = Sub (r1272) :: r1273 in
  let r1275 = S (T T_LIDENT) :: r1274 in
  let r1276 = R 610 :: r1275 in
  let r1277 = R 362 :: r1276 in
  let r1278 = [R 47] in
  let r1279 = R 368 :: r1278 in
  let r1280 = [R 419] in
  let r1281 = Sub (r1272) :: r1280 in
  let r1282 = S (T T_LIDENT) :: r1281 in
  let r1283 = R 610 :: r1282 in
  let r1284 = [R 421] in
  let r1285 = Sub (r3) :: r1284 in
  let r1286 = S (T T_EQUAL) :: r1285 in
  let r1287 = [R 423] in
  let r1288 = Sub (r3) :: r1287 in
  let r1289 = S (T T_EQUAL) :: r1288 in
  let r1290 = Sub (r34) :: r1289 in
  let r1291 = S (T T_DOT) :: r1290 in
  let r1292 = [R 417] in
  let r1293 = Sub (r36) :: r1292 in
  let r1294 = S (T T_COLON) :: r1293 in
  let r1295 = [R 420] in
  let r1296 = Sub (r3) :: r1295 in
  let r1297 = S (T T_EQUAL) :: r1296 in
  let r1298 = [R 422] in
  let r1299 = Sub (r3) :: r1298 in
  let r1300 = S (T T_EQUAL) :: r1299 in
  let r1301 = Sub (r34) :: r1300 in
  let r1302 = S (T T_DOT) :: r1301 in
  let r1303 = [R 49] in
  let r1304 = R 368 :: r1303 in
  let r1305 = Sub (r3) :: r1304 in
  let r1306 = [R 44] in
  let r1307 = R 368 :: r1306 in
  let r1308 = R 545 :: r1307 in
  let r1309 = Sub (r1256) :: r1308 in
  let r1310 = [R 45] in
  let r1311 = R 368 :: r1310 in
  let r1312 = R 545 :: r1311 in
  let r1313 = Sub (r1256) :: r1312 in
  let r1314 = [R 76] in
  let r1315 = S (T T_RPAREN) :: r1314 in
  let r1316 = [R 39] in
  let r1317 = Sub (r1256) :: r1316 in
  let r1318 = S (T T_IN) :: r1317 in
  let r1319 = Sub (r1119) :: r1318 in
  let r1320 = R 362 :: r1319 in
  let r1321 = [R 342] in
  let r1322 = R 368 :: r1321 in
  let r1323 = Sub (r558) :: r1322 in
  let r1324 = R 617 :: r1323 in
  let r1325 = R 362 :: r1324 in
  let r1326 = [R 40] in
  let r1327 = Sub (r1256) :: r1326 in
  let r1328 = S (T T_IN) :: r1327 in
  let r1329 = Sub (r1119) :: r1328 in
  let r1330 = [R 78] in
  let r1331 = Sub (r483) :: r1330 in
  let r1332 = S (T T_RBRACKET) :: r1331 in
  let r1333 = [R 55] in
  let r1334 = Sub (r1256) :: r1333 in
  let r1335 = S (T T_MINUSGREATER) :: r1334 in
  let r1336 = Sub (r653) :: r1335 in
  let r1337 = [R 37] in
  let r1338 = Sub (r1336) :: r1337 in
  let r1339 = [R 38] in
  let r1340 = Sub (r1256) :: r1339 in
  let r1341 = [R 341] in
  let r1342 = R 368 :: r1341 in
  let r1343 = Sub (r558) :: r1342 in
  let r1344 = [R 79] in
  let r1345 = S (T T_RPAREN) :: r1344 in
  let r1346 = [R 546] in
  let r1347 = [R 48] in
  let r1348 = R 368 :: r1347 in
  let r1349 = Sub (r1192) :: r1348 in
  let r1350 = [R 50] in
  let r1351 = [R 380] in
  let r1352 = [R 53] in
  let r1353 = Sub (r1256) :: r1352 in
  let r1354 = S (T T_EQUAL) :: r1353 in
  let r1355 = [R 54] in
  let r1356 = [R 353] in
  let r1357 = R 352 :: r1356 in
  let r1358 = R 368 :: r1357 in
  let r1359 = Sub (r1259) :: r1358 in
  let r1360 = S (T T_LIDENT) :: r1359 in
  let r1361 = R 140 :: r1360 in
  let r1362 = R 941 :: r1361 in
  let r1363 = [R 376] in
  let r1364 = [R 396] in
  let r1365 = [R 886] in
  let r1366 = R 373 :: r1365 in
  let r1367 = [R 212] in
  let r1368 = Sub (r189) :: r1367 in
  let r1369 = R 362 :: r1368 in
  let r1370 = [R 794] in
  let r1371 = [R 773] in
  let r1372 = S (T T_RPAREN) :: r1371 in
  let r1373 = S (N N_module_expr) :: r1372 in
  let r1374 = R 362 :: r1373 in
  let r1375 = [R 774] in
  let r1376 = S (T T_RPAREN) :: r1375 in
  let r1377 = [R 758] in
  let r1378 = [R 939] in
  let r1379 = Sub (r3) :: r1378 in
  let r1380 = [R 935] in
  let r1381 = Sub (r34) :: r1380 in
  let r1382 = S (T T_COLON) :: r1381 in
  let r1383 = [R 938] in
  let r1384 = Sub (r3) :: r1383 in
  let r1385 = [R 375] in
  let r1386 = R 373 :: r1385 in
  let r1387 = [R 414] in
  let r1388 = R 362 :: r1387 in
  let r1389 = Sub (r1048) :: r1388 in
  let r1390 = [R 412] in
  let r1391 = [R 502] in
  let r1392 = [R 661] in
  let r1393 = [R 662] in
  let r1394 = S (T T_RPAREN) :: r1393 in
  let r1395 = Sub (r200) :: r1394 in
  let r1396 = [R 660] in
  let r1397 = [R 659] in
  let r1398 = Sub (r189) :: r1397 in
  let r1399 = R 362 :: r1398 in
  let r1400 = [R 656] in
  let r1401 = [R 657] in
  let r1402 = S (T T_RPAREN) :: r1401 in
  let r1403 = Sub (r200) :: r1402 in
  let r1404 = [R 655] in
  let r1405 = [R 654] in
  let r1406 = Sub (r189) :: r1405 in
  let r1407 = R 362 :: r1406 in
  let r1408 = [R 136] in
  let r1409 = R 362 :: r1408 in
  let r1410 = [R 137] in
  let r1411 = R 362 :: r1410 in
  let r1412 = [R 244] in
  let r1413 = Sub (r30) :: r1412 in
  let r1414 = S (T T_MINUSGREATER) :: r1413 in
  let r1415 = S (T T_RPAREN) :: r1414 in
  let r1416 = Sub (r34) :: r1415 in
  let r1417 = [R 245] in
  let r1418 = Sub (r30) :: r1417 in
  let r1419 = [R 248] in
  let r1420 = [R 246] in
  let r1421 = Sub (r30) :: r1420 in
  let r1422 = S (T T_MINUSGREATER) :: r1421 in
  let r1423 = S (T T_RPAREN) :: r1422 in
  let r1424 = Sub (r34) :: r1423 in
  let r1425 = [R 498] in
  let r1426 = S (T T_RBRACE) :: r1425 in
  let r1427 = [R 264] in
  let r1428 = R 368 :: r1427 in
  let r1429 = R 675 :: r1428 in
  let r1430 = [R 263] in
  let r1431 = R 368 :: r1430 in
  let r1432 = R 675 :: r1431 in
  let r1433 = [R 269] in
  let r1434 = [R 272] in
  let r1435 = [R 429] in
  let r1436 = [R 432] in
  let r1437 = S (T T_RPAREN) :: r1436 in
  let r1438 = S (T T_COLONCOLON) :: r1437 in
  let r1439 = S (T T_LPAREN) :: r1438 in
  let r1440 = [R 567] in
  let r1441 = [R 568] in
  let r1442 = [R 569] in
  let r1443 = [R 570] in
  let r1444 = [R 571] in
  let r1445 = [R 572] in
  let r1446 = [R 573] in
  let r1447 = [R 574] in
  let r1448 = [R 575] in
  let r1449 = [R 576] in
  let r1450 = [R 577] in
  let r1451 = [R 894] in
  let r1452 = [R 903] in
  let r1453 = [R 382] in
  let r1454 = [R 901] in
  let r1455 = S (T T_SEMISEMI) :: r1454 in
  let r1456 = [R 902] in
  let r1457 = [R 384] in
  let r1458 = [R 387] in
  let r1459 = [R 386] in
  let r1460 = [R 385] in
  let r1461 = R 383 :: r1460 in
  let r1462 = [R 930] in
  let r1463 = S (T T_EOF) :: r1462 in
  let r1464 = R 383 :: r1463 in
  let r1465 = [R 929] in
  function
  | 0 | 2155 | 2159 | 2177 | 2181 | 2185 | 2189 | 2193 | 2197 | 2201 | 2205 | 2209 | 2213 | 2219 | 2239 -> Nothing
  | 2154 -> One ([R 0])
  | 2158 -> One ([R 1])
  | 2164 -> One ([R 2])
  | 2178 -> One ([R 3])
  | 2182 -> One ([R 4])
  | 2188 -> One ([R 5])
  | 2190 -> One ([R 6])
  | 2194 -> One ([R 7])
  | 2198 -> One ([R 8])
  | 2202 -> One ([R 9])
  | 2206 -> One ([R 10])
  | 2212 -> One ([R 11])
  | 2216 -> One ([R 12])
  | 2229 -> One ([R 13])
  | 2249 -> One ([R 14])
  | 601 -> One ([R 15])
  | 600 -> One ([R 16])
  | 2172 -> One ([R 20])
  | 2174 -> One ([R 21])
  | 266 -> One ([R 22])
  | 246 -> One ([R 23])
  | 277 -> One ([R 24])
  | 1835 -> One ([R 36])
  | 1839 -> One ([R 41])
  | 1836 -> One ([R 42])
  | 1875 -> One ([R 51])
  | 1842 -> One ([R 56])
  | 1631 -> One ([R 68])
  | 1611 -> One ([R 69])
  | 1613 -> One ([R 73])
  | 1837 -> One ([R 77])
  | 454 -> One ([R 88])
  | 210 -> One ([R 89])
  | 452 -> One ([R 90])
  | 159 -> One ([R 94])
  | 158 | 1445 -> One ([R 95])
  | 1476 -> One ([R 98])
  | 1715 -> One ([R 106])
  | 1719 -> One ([R 107])
  | 269 -> One ([R 109])
  | 258 -> One ([R 110])
  | 263 -> One ([R 111])
  | 265 -> One ([R 112])
  | 1219 -> One ([R 122])
  | 1 -> One (R 132 :: r9)
  | 62 -> One (R 132 :: r42)
  | 192 -> One (R 132 :: r194)
  | 214 -> One (R 132 :: r223)
  | 358 -> One (R 132 :: r327)
  | 446 -> One (R 132 :: r397)
  | 484 -> One (R 132 :: r420)
  | 602 -> One (R 132 :: r486)
  | 611 -> One (R 132 :: r494)
  | 705 -> One (R 132 :: r577)
  | 706 -> One (R 132 :: r581)
  | 718 -> One (R 132 :: r607)
  | 721 -> One (R 132 :: r610)
  | 735 -> One (R 132 :: r621)
  | 772 -> One (R 132 :: r644)
  | 775 -> One (R 132 :: r647)
  | 781 -> One (R 132 :: r667)
  | 823 -> One (R 132 :: r687)
  | 844 -> One (R 132 :: r711)
  | 849 -> One (R 132 :: r714)
  | 858 -> One (R 132 :: r718)
  | 878 -> One (R 132 :: r728)
  | 894 -> One (R 132 :: r737)
  | 908 -> One (R 132 :: r744)
  | 914 -> One (R 132 :: r748)
  | 923 -> One (R 132 :: r752)
  | 934 -> One (R 132 :: r758)
  | 940 -> One (R 132 :: r762)
  | 946 -> One (R 132 :: r766)
  | 952 -> One (R 132 :: r770)
  | 958 -> One (R 132 :: r774)
  | 964 -> One (R 132 :: r778)
  | 970 -> One (R 132 :: r782)
  | 976 -> One (R 132 :: r786)
  | 982 -> One (R 132 :: r790)
  | 988 -> One (R 132 :: r794)
  | 994 -> One (R 132 :: r798)
  | 1000 -> One (R 132 :: r802)
  | 1006 -> One (R 132 :: r806)
  | 1012 -> One (R 132 :: r810)
  | 1018 -> One (R 132 :: r814)
  | 1024 -> One (R 132 :: r818)
  | 1030 -> One (R 132 :: r822)
  | 1036 -> One (R 132 :: r826)
  | 1050 -> One (R 132 :: r835)
  | 1056 -> One (R 132 :: r839)
  | 1126 -> One (R 132 :: r877)
  | 1135 -> One (R 132 :: r884)
  | 1144 -> One (R 132 :: r891)
  | 1154 -> One (R 132 :: r895)
  | 1163 -> One (R 132 :: r902)
  | 1172 -> One (R 132 :: r909)
  | 1183 -> One (R 132 :: r917)
  | 1192 -> One (R 132 :: r924)
  | 1201 -> One (R 132 :: r931)
  | 1208 -> One (R 132 :: r935)
  | 1246 -> One (R 132 :: r938)
  | 1262 -> One (R 132 :: r941)
  | 1267 -> One (R 132 :: r945)
  | 1274 -> One (R 132 :: r949)
  | 1296 -> One (R 132 :: r957)
  | 1347 -> One (R 132 :: r976)
  | 1360 -> One (R 132 :: r980)
  | 1385 -> One (R 132 :: r994)
  | 1426 -> One (R 132 :: r1026)
  | 1431 -> One (R 132 :: r1036)
  | 1499 -> One (R 132 :: r1086)
  | 1500 -> One (R 132 :: r1090)
  | 1509 -> One (R 132 :: r1098)
  | 1546 -> One (R 132 :: r1126)
  | 1555 -> One (R 132 :: r1140)
  | 1556 -> One (R 132 :: r1149)
  | 1752 -> One (R 132 :: r1267)
  | 1937 -> One (R 132 :: r1369)
  | 1946 -> One (R 132 :: r1374)
  | 2024 -> One (R 132 :: r1399)
  | 2039 -> One (R 132 :: r1407)
  | 264 -> One ([R 138])
  | 863 -> One ([R 144])
  | 1214 -> One ([R 159])
  | 884 -> One ([R 160])
  | 921 -> One ([R 161])
  | 901 -> One ([R 162])
  | 919 -> One ([R 233])
  | 928 -> One ([R 238])
  | 932 -> One ([R 239])
  | 370 -> One ([R 254])
  | 115 -> One ([R 267])
  | 92 -> One (R 270 :: r53)
  | 96 -> One (R 270 :: r55)
  | 1466 -> One ([R 278])
  | 1467 -> One ([R 279])
  | 1213 -> One ([R 283])
  | 510 -> One ([R 289])
  | 536 -> One ([R 293])
  | 547 -> One ([R 297])
  | 586 -> One ([R 301])
  | 573 -> One ([R 305])
  | 656 -> One ([R 309])
  | 1108 -> One ([R 313])
  | 683 -> One ([R 317])
  | 669 -> One ([R 321])
  | 638 -> One ([R 325])
  | 493 -> One ([R 329])
  | 637 -> One ([R 330])
  | 1113 -> One ([R 331])
  | 1081 -> One ([R 333])
  | 1118 -> One ([R 340])
  | 1840 -> One ([R 343])
  | 787 -> One ([R 344])
  | 1936 -> One ([R 346])
  | 129 -> One (R 362 :: r84)
  | 179 -> One (R 362 :: r163)
  | 320 -> One (R 362 :: r305)
  | 365 -> One (R 362 :: r333)
  | 594 -> One (R 362 :: r481)
  | 710 -> One (R 362 :: r588)
  | 738 -> One (R 362 :: r625)
  | 1061 -> One (R 362 :: r842)
  | 1406 -> One (R 362 :: r1013)
  | 1528 -> One (R 362 :: r1117)
  | 1567 -> One (R 362 :: r1155)
  | 1573 -> One (R 362 :: r1163)
  | 1584 -> One (R 362 :: r1169)
  | 1595 -> One (R 362 :: r1172)
  | 1599 -> One (R 362 :: r1181)
  | 1620 -> One (R 362 :: r1195)
  | 1636 -> One (R 362 :: r1205)
  | 1671 -> One (R 362 :: r1222)
  | 1693 -> One (R 362 :: r1232)
  | 1703 -> One (R 362 :: r1241)
  | 1760 -> One (R 362 :: r1271)
  | 1764 -> One (R 362 :: r1283)
  | 1804 -> One (R 362 :: r1305)
  | 1808 -> One (R 362 :: r1309)
  | 1809 -> One (R 362 :: r1313)
  | 1820 -> One (R 362 :: r1329)
  | 1828 -> One (R 362 :: r1338)
  | 1867 -> One (R 362 :: r1349)
  | 1887 -> One (R 362 :: r1362)
  | 2002 -> One (R 362 :: r1390)
  | 1692 -> One (R 364 :: r1225)
  | 1914 -> One (R 364 :: r1364)
  | 1702 -> One (R 366 :: r1233)
  | 1115 -> One (R 368 :: r873)
  | 1629 -> One (R 368 :: r1196)
  | 1690 -> One (R 368 :: r1224)
  | 1873 -> One (R 368 :: r1350)
  | 1919 -> One (R 368 :: r1366)
  | 1988 -> One (R 368 :: r1386)
  | 2234 -> One (R 368 :: r1455)
  | 2245 -> One (R 368 :: r1461)
  | 2250 -> One (R 368 :: r1464)
  | 1498 -> One (R 370 :: r1082)
  | 1682 -> One (R 370 :: r1223)
  | 211 -> One (R 373 :: r215)
  | 1897 -> One (R 373 :: r1363)
  | 1632 -> One (R 377 :: r1197)
  | 1876 -> One (R 379 :: r1351)
  | 2232 -> One (R 381 :: r1453)
  | 2240 -> One (R 383 :: r1457)
  | 2241 -> One (R 383 :: r1458)
  | 2242 -> One (R 383 :: r1459)
  | 562 -> One ([R 389])
  | 566 -> One ([R 391])
  | 1916 -> One ([R 393])
  | 1906 -> One ([R 394])
  | 1896 -> One ([R 395])
  | 1904 -> One ([R 399])
  | 1908 -> One ([R 401])
  | 1917 -> One ([R 403])
  | 1905 -> One ([R 404])
  | 1907 -> One ([R 406])
  | 1256 -> One ([R 409])
  | 2005 -> One ([R 410])
  | 2008 -> One ([R 411])
  | 2007 -> One ([R 413])
  | 2006 -> One ([R 415])
  | 2004 -> One ([R 416])
  | 2173 -> One ([R 428])
  | 2163 -> One ([R 430])
  | 2171 -> One ([R 431])
  | 2170 -> One ([R 433])
  | 726 -> One ([R 440])
  | 1340 -> One ([R 441])
  | 742 -> One ([R 452])
  | 752 -> One ([R 453])
  | 753 -> One ([R 454])
  | 751 -> One ([R 455])
  | 754 -> One ([R 457])
  | 177 -> One ([R 458])
  | 206 | 361 | 1519 -> One ([R 459])
  | 402 -> One ([R 467])
  | 372 -> One ([R 468])
  | 415 -> One ([R 471])
  | 596 | 1973 -> One ([R 476])
  | 1577 -> One ([R 478])
  | 1575 -> One ([R 479])
  | 1578 -> One ([R 480])
  | 1576 -> One ([R 481])
  | 517 -> One ([R 484])
  | 1439 -> One ([R 486])
  | 1728 -> One ([R 487])
  | 2111 -> One ([R 488])
  | 1744 -> One ([R 489])
  | 2112 -> One ([R 490])
  | 1743 -> One ([R 491])
  | 1735 -> One ([R 492])
  | 67 | 615 -> One ([R 508])
  | 75 | 761 -> One ([R 509])
  | 103 -> One ([R 510])
  | 91 -> One ([R 512])
  | 95 -> One ([R 514])
  | 99 -> One ([R 516])
  | 82 -> One ([R 517])
  | 102 | 1311 -> One ([R 518])
  | 81 -> One ([R 519])
  | 80 -> One ([R 520])
  | 79 -> One ([R 521])
  | 78 -> One ([R 522])
  | 77 -> One ([R 523])
  | 70 | 357 | 734 -> One ([R 524])
  | 69 | 733 -> One ([R 525])
  | 68 -> One ([R 526])
  | 74 | 434 | 760 -> One ([R 527])
  | 73 | 759 -> One ([R 528])
  | 66 -> One ([R 529])
  | 71 -> One ([R 530])
  | 84 -> One ([R 531])
  | 76 -> One ([R 532])
  | 83 -> One ([R 533])
  | 72 -> One ([R 534])
  | 101 -> One ([R 535])
  | 104 -> One ([R 536])
  | 100 -> One ([R 538])
  | 314 -> One ([R 539])
  | 313 -> One (R 540 :: r303)
  | 223 -> One (R 541 :: r242)
  | 224 -> One ([R 542])
  | 563 -> One (R 543 :: r458)
  | 564 -> One ([R 544])
  | 1082 -> One (R 560 :: r858)
  | 1083 -> One ([R 561])
  | 121 -> One ([R 562])
  | 496 -> One ([R 579])
  | 494 -> One ([R 580])
  | 497 -> One ([R 582])
  | 641 -> One ([R 592])
  | 642 -> One ([R 593])
  | 643 -> One ([R 595])
  | 793 -> One ([R 597])
  | 1751 -> One ([R 601])
  | 1766 | 1785 -> One ([R 611])
  | 1588 -> One ([R 613])
  | 1586 -> One ([R 614])
  | 1589 -> One ([R 615])
  | 1587 -> One ([R 616])
  | 1849 -> One (R 617 :: r1343)
  | 704 -> One ([R 618])
  | 1726 -> One ([R 621])
  | 1727 -> One ([R 622])
  | 1721 -> One ([R 623])
  | 2064 -> One ([R 625])
  | 2063 -> One ([R 626])
  | 2065 -> One ([R 627])
  | 2060 -> One ([R 628])
  | 2061 -> One ([R 629])
  | 2125 -> One ([R 631])
  | 2123 -> One ([R 632])
  | 498 -> One ([R 663])
  | 644 -> One ([R 669])
  | 817 -> One ([R 678])
  | 414 -> One ([R 679])
  | 371 -> One ([R 680])
  | 1216 -> One ([R 681])
  | 1215 -> One ([R 682])
  | 338 -> One ([R 684])
  | 306 -> One ([R 708])
  | 1121 -> One ([R 711])
  | 882 -> One ([R 713])
  | 1122 -> One ([R 714])
  | 883 -> One ([R 715])
  | 1353 -> One ([R 717])
  | 1354 -> One ([R 718])
  | 557 -> One ([R 720])
  | 558 -> One ([R 721])
  | 1332 -> One ([R 723])
  | 1333 -> One ([R 724])
  | 1746 -> One ([R 730])
  | 1681 -> One ([R 731])
  | 1684 -> One ([R 732])
  | 1683 -> One ([R 737])
  | 1688 -> One ([R 740])
  | 1687 -> One ([R 742])
  | 1686 -> One ([R 743])
  | 1685 -> One ([R 744])
  | 1747 -> One ([R 747])
  | 355 -> One ([R 750])
  | 352 -> One ([R 752])
  | 725 -> One ([R 776])
  | 856 -> One ([R 777])
  | 855 | 920 -> One ([R 778])
  | 728 | 900 -> One ([R 779])
  | 1206 | 1245 -> One ([R 784])
  | 854 -> One ([R 789])
  | 455 -> One ([R 812])
  | 459 -> One ([R 815])
  | 460 -> One ([R 819])
  | 482 -> One ([R 821])
  | 464 -> One ([R 822])
  | 559 -> One ([R 824])
  | 481 -> One ([R 829])
  | 28 -> One ([R 830])
  | 8 -> One ([R 831])
  | 53 -> One ([R 833])
  | 52 -> One ([R 834])
  | 51 -> One ([R 835])
  | 50 -> One ([R 836])
  | 49 -> One ([R 837])
  | 48 -> One ([R 838])
  | 47 -> One ([R 839])
  | 46 -> One ([R 840])
  | 45 -> One ([R 841])
  | 44 -> One ([R 842])
  | 43 -> One ([R 843])
  | 42 -> One ([R 844])
  | 41 -> One ([R 845])
  | 40 -> One ([R 846])
  | 39 -> One ([R 847])
  | 38 -> One ([R 848])
  | 37 -> One ([R 849])
  | 36 -> One ([R 850])
  | 35 -> One ([R 851])
  | 34 -> One ([R 852])
  | 33 -> One ([R 853])
  | 32 -> One ([R 854])
  | 31 -> One ([R 855])
  | 30 -> One ([R 856])
  | 29 -> One ([R 857])
  | 27 -> One ([R 858])
  | 26 -> One ([R 859])
  | 25 -> One ([R 860])
  | 24 -> One ([R 861])
  | 23 -> One ([R 862])
  | 22 -> One ([R 863])
  | 21 -> One ([R 864])
  | 20 -> One ([R 865])
  | 19 -> One ([R 866])
  | 18 -> One ([R 867])
  | 17 -> One ([R 868])
  | 16 -> One ([R 869])
  | 15 -> One ([R 870])
  | 14 -> One ([R 871])
  | 13 -> One ([R 872])
  | 12 -> One ([R 873])
  | 11 -> One ([R 874])
  | 10 -> One ([R 875])
  | 9 -> One ([R 876])
  | 7 -> One ([R 877])
  | 6 -> One ([R 878])
  | 5 -> One ([R 879])
  | 4 -> One ([R 880])
  | 3 -> One ([R 881])
  | 1923 -> One ([R 885])
  | 1911 | 1924 -> One ([R 887])
  | 1909 -> One ([R 889])
  | 608 -> One ([R 890])
  | 607 -> One ([R 891])
  | 2223 -> One ([R 895])
  | 2224 -> One ([R 896])
  | 2226 -> One ([R 897])
  | 2227 -> One ([R 898])
  | 2225 -> One ([R 899])
  | 2222 -> One ([R 900])
  | 2228 -> One ([R 904])
  | 375 -> One (R 914 :: r357)
  | 396 -> One ([R 915])
  | 135 -> One ([R 920])
  | 138 -> One ([R 921])
  | 142 -> One ([R 922])
  | 136 -> One ([R 923])
  | 143 -> One ([R 924])
  | 139 -> One ([R 925])
  | 144 -> One ([R 926])
  | 141 -> One ([R 927])
  | 134 -> One ([R 928])
  | 456 -> One ([R 933])
  | 853 -> One ([R 934])
  | 1559 -> One ([R 942])
  | 1971 -> One ([R 943])
  | 1974 -> One ([R 944])
  | 1972 -> One ([R 945])
  | 1783 -> One ([R 946])
  | 1786 -> One ([R 947])
  | 1784 -> One ([R 948])
  | 385 -> One ([R 955])
  | 386 -> One ([R 956])
  | 1326 -> One (S (T T_WITH) :: r972)
  | 173 -> One (S (T T_TYPE) :: r159)
  | 1712 -> One (S (T T_STRING) :: r1244)
  | 1469 -> One (S (T T_STAR) :: r1068)
  | 2230 -> One (S (T T_SEMISEMI) :: r1452)
  | 2237 -> One (S (T T_SEMISEMI) :: r1456)
  | 2160 -> One (S (T T_RPAREN) :: r144)
  | 367 -> One (S (T T_RPAREN) :: r208)
  | 251 -> One (S (T T_RPAREN) :: r275)
  | 267 | 299 -> One (S (T T_RPAREN) :: r282)
  | 467 -> One (S (T T_RPAREN) :: r407)
  | 550 -> One (S (T T_RPAREN) :: r457)
  | 744 -> One (S (T T_RPAREN) :: r626)
  | 1312 -> One (S (T T_RPAREN) :: r961)
  | 1956 -> One (S (T T_RPAREN) :: r1377)
  | 2161 -> One (S (T T_RPAREN) :: r1435)
  | 1449 | 1708 -> One (S (T T_RBRACKET) :: r379)
  | 1318 -> One (S (T T_RBRACKET) :: r964)
  | 1320 -> One (S (T T_RBRACKET) :: r965)
  | 286 -> One (S (T T_QUOTE) :: r290)
  | 1597 -> One (S (T T_OPEN) :: r1177)
  | 1812 -> One (S (T T_OPEN) :: r1320)
  | 409 -> One (S (T T_MINUSGREATER) :: r370)
  | 1485 -> One (S (T T_MINUSGREATER) :: r1078)
  | 1489 -> One (S (T T_MINUSGREATER) :: r1080)
  | 1658 -> One (S (T T_MINUSGREATER) :: r1211)
  | 2093 -> One (S (T T_MINUSGREATER) :: r1418)
  | 85 -> One (S (T T_LPAREN) :: r50)
  | 118 -> One (S (T T_LIDENT) :: r64)
  | 195 -> One (S (T T_LIDENT) :: r197)
  | 196 -> One (S (T T_LIDENT) :: r205)
  | 219 -> One (S (T T_LIDENT) :: r229)
  | 220 -> One (S (T T_LIDENT) :: r235)
  | 343 -> One (S (T T_LIDENT) :: r313)
  | 344 -> One (S (T T_LIDENT) :: r317)
  | 472 -> One (S (T T_LIDENT) :: r411)
  | 473 -> One (S (T T_LIDENT) :: r415)
  | 500 -> One (S (T T_LIDENT) :: r428)
  | 501 -> One (S (T T_LIDENT) :: r432)
  | 526 -> One (S (T T_LIDENT) :: r444)
  | 527 -> One (S (T T_LIDENT) :: r448)
  | 576 -> One (S (T T_LIDENT) :: r461)
  | 577 -> One (S (T T_LIDENT) :: r465)
  | 620 -> One (S (T T_LIDENT) :: r508)
  | 621 -> One (S (T T_LIDENT) :: r514)
  | 627 -> One (S (T T_LIDENT) :: r515)
  | 628 -> One (S (T T_LIDENT) :: r519)
  | 646 -> One (S (T T_LIDENT) :: r523)
  | 647 -> One (S (T T_LIDENT) :: r527)
  | 659 -> One (S (T T_LIDENT) :: r529)
  | 660 -> One (S (T T_LIDENT) :: r533)
  | 673 -> One (S (T T_LIDENT) :: r538)
  | 674 -> One (S (T T_LIDENT) :: r542)
  | 685 -> One (S (T T_LIDENT) :: r544)
  | 697 -> One (S (T T_LIDENT) :: r552)
  | 828 -> One (S (T T_LIDENT) :: r689)
  | 829 -> One (S (T T_LIDENT) :: r692)
  | 840 -> One (S (T T_LIDENT) :: r695)
  | 864 -> One (S (T T_LIDENT) :: r719)
  | 865 -> One (S (T T_LIDENT) :: r722)
  | 870 -> One (S (T T_LIDENT) :: r723)
  | 886 -> One (S (T T_LIDENT) :: r730)
  | 887 -> One (S (T T_LIDENT) :: r733)
  | 1042 -> One (S (T T_LIDENT) :: r828)
  | 1043 -> One (S (T T_LIDENT) :: r831)
  | 1098 -> One (S (T T_LIDENT) :: r865)
  | 1099 -> One (S (T T_LIDENT) :: r869)
  | 1288 -> One (S (T T_LIDENT) :: r950)
  | 1289 -> One (S (T T_LIDENT) :: r953)
  | 1453 -> One (S (T T_LIDENT) :: r1064)
  | 1787 -> One (S (T T_LIDENT) :: r1294)
  | 1859 -> One (S (T T_LIDENT) :: r1346)
  | 1975 -> One (S (T T_LIDENT) :: r1382)
  | 2016 -> One (S (T T_LIDENT) :: r1392)
  | 2017 -> One (S (T T_LIDENT) :: r1395)
  | 2031 -> One (S (T T_LIDENT) :: r1400)
  | 2032 -> One (S (T T_LIDENT) :: r1403)
  | 350 -> One (S (T T_INT) :: r318)
  | 353 -> One (S (T T_INT) :: r319)
  | 902 -> One (S (T T_IN) :: r740)
  | 1832 -> One (S (T T_IN) :: r1340)
  | 713 -> One (S (T T_GREATERRBRACE) :: r593)
  | 1356 -> One (S (T T_GREATERRBRACE) :: r977)
  | 199 -> One (S (T T_GREATER) :: r207)
  | 2010 -> One (S (T T_GREATER) :: r1391)
  | 418 -> One (S (T T_EQUAL) :: r374)
  | 1078 -> One (S (T T_EQUAL) :: r855)
  | 1094 -> One (S (T T_EQUAL) :: r863)
  | 1302 -> One (S (T T_EQUAL) :: r959)
  | 1965 -> One (S (T T_EQUAL) :: r1379)
  | 1983 -> One (S (T T_EQUAL) :: r1384)
  | 2152 -> One (S (T T_EOF) :: r1433)
  | 2156 -> One (S (T T_EOF) :: r1434)
  | 2175 -> One (S (T T_EOF) :: r1440)
  | 2179 -> One (S (T T_EOF) :: r1441)
  | 2183 -> One (S (T T_EOF) :: r1442)
  | 2186 -> One (S (T T_EOF) :: r1443)
  | 2191 -> One (S (T T_EOF) :: r1444)
  | 2195 -> One (S (T T_EOF) :: r1445)
  | 2199 -> One (S (T T_EOF) :: r1446)
  | 2203 -> One (S (T T_EOF) :: r1447)
  | 2207 -> One (S (T T_EOF) :: r1448)
  | 2210 -> One (S (T T_EOF) :: r1449)
  | 2214 -> One (S (T T_EOF) :: r1450)
  | 2254 -> One (S (T T_EOF) :: r1465)
  | 1343 -> One (S (T T_END) :: r973)
  | 87 -> One (S (T T_DOTDOT) :: r51)
  | 162 -> One (S (T T_DOTDOT) :: r141)
  | 499 -> One (S (T T_DOTDOT) :: r427)
  | 525 -> One (S (T T_DOTDOT) :: r443)
  | 645 -> One (S (T T_DOTDOT) :: r522)
  | 1097 -> One (S (T T_DOTDOT) :: r864)
  | 1729 -> One (S (T T_DOTDOT) :: r1249)
  | 1730 -> One (S (T T_DOTDOT) :: r1250)
  | 291 -> One (S (T T_DOT) :: r295)
  | 378 | 1177 | 1234 -> One (S (T T_DOT) :: r359)
  | 2217 -> One (S (T T_DOT) :: r375)
  | 689 -> One (S (T T_DOT) :: r551)
  | 801 -> One (S (T T_DOT) :: r679)
  | 809 -> One (S (T T_DOT) :: r683)
  | 1073 -> One (S (T T_DOT) :: r853)
  | 1456 -> One (S (T T_DOT) :: r1066)
  | 1483 -> One (S (T T_DOT) :: r1076)
  | 2087 -> One (S (T T_DOT) :: r1416)
  | 2101 -> One (S (T T_DOT) :: r1424)
  | 2165 -> One (S (T T_DOT) :: r1439)
  | 163 | 1446 -> One (S (T T_COLONCOLON) :: r143)
  | 171 -> One (S (T T_COLON) :: r155)
  | 272 -> One (S (T T_COLON) :: r285)
  | 368 -> One (S (T T_COLON) :: r336)
  | 1652 -> One (S (T T_COLON) :: r1209)
  | 1998 -> One (S (T T_COLON) :: r1389)
  | 435 -> One (S (T T_BARRBRACKET) :: r378)
  | 568 -> One (S (T T_BARRBRACKET) :: r459)
  | 616 -> One (S (T T_BARRBRACKET) :: r497)
  | 1314 -> One (S (T T_BARRBRACKET) :: r962)
  | 1316 -> One (S (T T_BARRBRACKET) :: r963)
  | 1943 -> One (S (T T_BARRBRACKET) :: r1370)
  | 327 -> One (S (T T_BAR) :: r308)
  | 217 -> One (S (N N_pattern) :: r225)
  | 445 -> One (S (N N_pattern) :: r391)
  | 511 -> One (S (N N_pattern) :: r434)
  | 540 -> One (S (N N_pattern) :: r453)
  | 639 -> One (S (N N_pattern) :: r521)
  | 1109 -> One (S (N N_pattern) :: r871)
  | 1420 -> One (S (N N_pattern) :: r1018)
  | 364 -> One (S (N N_module_type) :: r329)
  | 412 -> One (S (N N_module_type) :: r371)
  | 416 -> One (S (N N_module_type) :: r372)
  | 748 -> One (S (N N_module_type) :: r628)
  | 1364 -> One (S (N N_module_type) :: r981)
  | 1366 -> One (S (N N_module_type) :: r982)
  | 1368 -> One (S (N N_module_type) :: r983)
  | 1371 -> One (S (N N_module_type) :: r984)
  | 1373 -> One (S (N N_module_type) :: r985)
  | 1375 -> One (S (N N_module_type) :: r986)
  | 1390 -> One (S (N N_module_type) :: r1001)
  | 1400 -> One (S (N N_module_type) :: r1008)
  | 1951 -> One (S (N N_module_type) :: r1376)
  | 709 -> One (S (N N_module_expr) :: r583)
  | 786 -> One (S (N N_let_pattern) :: r673)
  | 618 -> One (S (N N_fun_expr) :: r498)
  | 715 -> One (S (N N_fun_expr) :: r596)
  | 827 -> One (S (N N_fun_expr) :: r688)
  | 857 -> One (S (N N_fun_expr) :: r715)
  | 885 -> One (S (N N_fun_expr) :: r729)
  | 907 -> One (S (N N_fun_expr) :: r741)
  | 913 -> One (S (N N_fun_expr) :: r745)
  | 922 -> One (S (N N_fun_expr) :: r749)
  | 933 -> One (S (N N_fun_expr) :: r755)
  | 939 -> One (S (N N_fun_expr) :: r759)
  | 945 -> One (S (N N_fun_expr) :: r763)
  | 951 -> One (S (N N_fun_expr) :: r767)
  | 957 -> One (S (N N_fun_expr) :: r771)
  | 963 -> One (S (N N_fun_expr) :: r775)
  | 969 -> One (S (N N_fun_expr) :: r779)
  | 975 -> One (S (N N_fun_expr) :: r783)
  | 981 -> One (S (N N_fun_expr) :: r787)
  | 987 -> One (S (N N_fun_expr) :: r791)
  | 993 -> One (S (N N_fun_expr) :: r795)
  | 999 -> One (S (N N_fun_expr) :: r799)
  | 1005 -> One (S (N N_fun_expr) :: r803)
  | 1011 -> One (S (N N_fun_expr) :: r807)
  | 1017 -> One (S (N N_fun_expr) :: r811)
  | 1023 -> One (S (N N_fun_expr) :: r815)
  | 1029 -> One (S (N N_fun_expr) :: r819)
  | 1035 -> One (S (N N_fun_expr) :: r823)
  | 1041 -> One (S (N N_fun_expr) :: r827)
  | 1055 -> One (S (N N_fun_expr) :: r836)
  | 1125 -> One (S (N N_fun_expr) :: r874)
  | 1134 -> One (S (N N_fun_expr) :: r881)
  | 1143 -> One (S (N N_fun_expr) :: r888)
  | 1153 -> One (S (N N_fun_expr) :: r892)
  | 1162 -> One (S (N N_fun_expr) :: r899)
  | 1171 -> One (S (N N_fun_expr) :: r906)
  | 1182 -> One (S (N N_fun_expr) :: r914)
  | 1191 -> One (S (N N_fun_expr) :: r921)
  | 1200 -> One (S (N N_fun_expr) :: r928)
  | 1207 -> One (S (N N_fun_expr) :: r932)
  | 1266 -> One (S (N N_fun_expr) :: r942)
  | 1273 -> One (S (N N_fun_expr) :: r946)
  | 610 -> One (Sub (r3) :: r489)
  | 700 -> One (Sub (r3) :: r556)
  | 780 -> One (Sub (r3) :: r651)
  | 1422 -> One (Sub (r3) :: r1019)
  | 2 -> One (Sub (r13) :: r14)
  | 56 -> One (Sub (r13) :: r15)
  | 60 -> One (Sub (r13) :: r22)
  | 190 -> One (Sub (r13) :: r183)
  | 208 -> One (Sub (r13) :: r214)
  | 929 -> One (Sub (r13) :: r754)
  | 1418 -> One (Sub (r13) :: r1017)
  | 1424 -> One (Sub (r13) :: r1022)
  | 1813 -> One (Sub (r13) :: r1325)
  | 542 -> One (Sub (r24) :: r454)
  | 1111 -> One (Sub (r24) :: r872)
  | 279 -> One (Sub (r26) :: r287)
  | 281 -> One (Sub (r26) :: r288)
  | 819 -> One (Sub (r26) :: r684)
  | 1482 -> One (Sub (r26) :: r1074)
  | 249 -> One (Sub (r28) :: r273)
  | 1660 -> One (Sub (r28) :: r1214)
  | 248 -> One (Sub (r30) :: r270)
  | 2099 -> One (Sub (r30) :: r1419)
  | 317 -> One (Sub (r32) :: r304)
  | 389 -> One (Sub (r32) :: r363)
  | 198 -> One (Sub (r34) :: r206)
  | 257 -> One (Sub (r34) :: r277)
  | 300 -> One (Sub (r34) :: r297)
  | 392 -> One (Sub (r34) :: r366)
  | 442 -> One (Sub (r34) :: r390)
  | 589 -> One (Sub (r34) :: r468)
  | 763 -> One (Sub (r34) :: r631)
  | 833 -> One (Sub (r34) :: r693)
  | 1090 -> One (Sub (r34) :: r861)
  | 1569 -> One (Sub (r34) :: r1157)
  | 1607 -> One (Sub (r34) :: r1188)
  | 688 -> One (Sub (r36) :: r549)
  | 788 -> One (Sub (r36) :: r674)
  | 1769 -> One (Sub (r36) :: r1286)
  | 1793 -> One (Sub (r36) :: r1297)
  | 147 -> One (Sub (r59) :: r136)
  | 292 -> One (Sub (r59) :: r296)
  | 2220 -> One (Sub (r59) :: r1451)
  | 1497 -> One (Sub (r81) :: r1081)
  | 450 -> One (Sub (r96) :: r399)
  | 153 -> One (Sub (r131) :: r137)
  | 140 -> One (Sub (r133) :: r135)
  | 1561 -> One (Sub (r133) :: r1151)
  | 157 -> One (Sub (r139) :: r140)
  | 2114 -> One (Sub (r139) :: r1429)
  | 2128 -> One (Sub (r139) :: r1432)
  | 271 -> One (Sub (r146) :: r283)
  | 778 -> One (Sub (r187) :: r648)
  | 898 -> One (Sub (r187) :: r738)
  | 213 -> One (Sub (r217) :: r218)
  | 609 -> One (Sub (r217) :: r487)
  | 724 -> One (Sub (r217) :: r611)
  | 766 -> One (Sub (r217) :: r634)
  | 768 -> One (Sub (r217) :: r635)
  | 838 -> One (Sub (r217) :: r694)
  | 872 -> One (Sub (r217) :: r724)
  | 874 -> One (Sub (r217) :: r725)
  | 892 -> One (Sub (r217) :: r734)
  | 1048 -> One (Sub (r217) :: r832)
  | 1294 -> One (Sub (r217) :: r954)
  | 2022 -> One (Sub (r217) :: r1396)
  | 2037 -> One (Sub (r217) :: r1404)
  | 310 -> One (Sub (r237) :: r298)
  | 228 -> One (Sub (r239) :: r246)
  | 243 -> One (Sub (r239) :: r269)
  | 229 -> One (Sub (r252) :: r254)
  | 230 -> One (Sub (r256) :: r257)
  | 253 -> One (Sub (r256) :: r276)
  | 275 -> One (Sub (r256) :: r286)
  | 233 -> One (Sub (r263) :: r265)
  | 422 -> One (Sub (r263) :: r376)
  | 1520 -> One (Sub (r263) :: r1106)
  | 335 -> One (Sub (r310) :: r312)
  | 1396 -> One (Sub (r323) :: r1005)
  | 1523 -> One (Sub (r323) :: r1111)
  | 426 -> One (Sub (r346) :: r377)
  | 374 -> One (Sub (r348) :: r349)
  | 438 -> One (Sub (r387) :: r389)
  | 469 -> One (Sub (r394) :: r410)
  | 479 -> One (Sub (r394) :: r416)
  | 507 -> One (Sub (r394) :: r433)
  | 533 -> One (Sub (r394) :: r449)
  | 570 -> One (Sub (r394) :: r460)
  | 583 -> One (Sub (r394) :: r466)
  | 634 -> One (Sub (r394) :: r520)
  | 653 -> One (Sub (r394) :: r528)
  | 666 -> One (Sub (r394) :: r534)
  | 670 -> One (Sub (r394) :: r537)
  | 680 -> One (Sub (r394) :: r543)
  | 805 -> One (Sub (r394) :: r680)
  | 1105 -> One (Sub (r394) :: r870)
  | 461 -> One (Sub (r402) :: r403)
  | 487 -> One (Sub (r422) :: r425)
  | 515 -> One (Sub (r437) :: r440)
  | 796 -> One (Sub (r437) :: r676)
  | 1067 -> One (Sub (r437) :: r849)
  | 1770 -> One (Sub (r437) :: r1291)
  | 1794 -> One (Sub (r437) :: r1302)
  | 593 -> One (Sub (r474) :: r476)
  | 1308 -> One (Sub (r500) :: r960)
  | 619 -> One (Sub (r502) :: r505)
  | 686 -> One (Sub (r546) :: r548)
  | 698 -> One (Sub (r546) :: r555)
  | 716 -> One (Sub (r602) :: r604)
  | 1325 -> One (Sub (r602) :: r970)
  | 784 -> One (Sub (r669) :: r670)
  | 1322 -> One (Sub (r966) :: r968)
  | 1412 -> One (Sub (r989) :: r1014)
  | 1451 -> One (Sub (r1045) :: r1046)
  | 1452 -> One (Sub (r1056) :: r1058)
  | 1709 -> One (Sub (r1056) :: r1243)
  | 1731 -> One (Sub (r1056) :: r1252)
  | 1739 -> One (Sub (r1056) :: r1254)
  | 2107 -> One (Sub (r1056) :: r1426)
  | 1474 -> One (Sub (r1069) :: r1072)
  | 2055 -> One (Sub (r1069) :: r1409)
  | 2067 -> One (Sub (r1069) :: r1411)
  | 1544 -> One (Sub (r1093) :: r1122)
  | 1537 -> One (Sub (r1119) :: r1121)
  | 1855 -> One (Sub (r1131) :: r1345)
  | 1879 -> One (Sub (r1131) :: r1354)
  | 1824 -> One (Sub (r1183) :: r1332)
  | 1811 -> One (Sub (r1256) :: r1315)
  | 1883 -> One (Sub (r1259) :: r1355)
  | 1763 -> One (Sub (r1277) :: r1279)
  | 906 -> One (r0)
  | 905 -> One (r2)
  | 2151 -> One (r4)
  | 2150 -> One (r5)
  | 2149 -> One (r6)
  | 2148 -> One (r7)
  | 2147 -> One (r8)
  | 59 -> One (r9)
  | 54 -> One (r10)
  | 55 -> One (r12)
  | 58 -> One (r14)
  | 57 -> One (r15)
  | 1918 -> One (r16)
  | 1922 -> One (r18)
  | 2146 -> One (r20)
  | 2145 -> One (r21)
  | 61 -> One (r22)
  | 108 | 617 | 717 | 1339 -> One (r23)
  | 111 -> One (r25)
  | 270 -> One (r27)
  | 247 -> One (r29)
  | 262 -> One (r31)
  | 285 -> One (r33)
  | 693 -> One (r35)
  | 2144 -> One (r37)
  | 2143 -> One (r38)
  | 110 -> One (r39)
  | 109 -> One (r40)
  | 64 -> One (r41)
  | 63 -> One (r42)
  | 105 -> One (r43)
  | 107 -> One (r45)
  | 106 -> One (r46)
  | 65 -> One (r47)
  | 90 -> One (r48)
  | 89 -> One (r49)
  | 86 -> One (r50)
  | 88 -> One (r51)
  | 94 -> One (r52)
  | 93 -> One (r53)
  | 98 -> One (r54)
  | 97 -> One (r55)
  | 112 | 128 -> One (r56)
  | 113 -> One (r57)
  | 116 -> One (r58)
  | 161 -> One (r61)
  | 160 -> One (r62)
  | 120 -> One (r63)
  | 119 -> One (r64)
  | 1997 -> One (r65)
  | 1996 -> One (r66)
  | 170 | 203 -> One (r67)
  | 169 | 202 -> One (r68)
  | 168 | 201 -> One (r69)
  | 167 | 200 | 250 | 261 -> One (r70)
  | 2142 -> One (r71)
  | 2141 -> One (r72)
  | 2140 -> One (r73)
  | 2139 -> One (r74)
  | 127 -> One (r75)
  | 126 -> One (r76)
  | 1750 -> One (r80)
  | 2138 -> One (r82)
  | 2137 -> One (r83)
  | 130 -> One (r84)
  | 2074 -> One (r85)
  | 2073 -> One (r86)
  | 2072 -> One (r87)
  | 231 | 280 -> One (r93)
  | 256 -> One (r95)
  | 453 -> One (r97)
  | 1496 -> One (r99)
  | 1738 -> One (r101)
  | 1737 -> One (r102)
  | 1736 | 2066 -> One (r103)
  | 2124 -> One (r105)
  | 2136 -> One (r107)
  | 2135 -> One (r108)
  | 2134 -> One (r109)
  | 2133 -> One (r110)
  | 2132 -> One (r111)
  | 2049 -> One (r115)
  | 189 -> One (r116)
  | 188 -> One (r117)
  | 2122 -> One (r121)
  | 2121 -> One (r122)
  | 2120 -> One (r123)
  | 2119 -> One (r124)
  | 2118 -> One (r125)
  | 146 -> One (r127)
  | 149 -> One (r129)
  | 145 -> One (r130)
  | 150 -> One (r132)
  | 152 -> One (r134)
  | 151 -> One (r135)
  | 148 -> One (r136)
  | 154 -> One (r137)
  | 1714 -> One (r138)
  | 2113 -> One (r140)
  | 2110 -> One (r141)
  | 1448 -> One (r142)
  | 1447 -> One (r143)
  | 164 -> One (r144)
  | 284 -> One (r145)
  | 2098 -> One (r147)
  | 2097 -> One (r148)
  | 2096 -> One (r149)
  | 166 -> One (r150)
  | 2086 -> One (r151)
  | 2085 -> One (r152)
  | 2084 -> One (r153)
  | 2083 -> One (r154)
  | 172 -> One (r155)
  | 2082 -> One (r156)
  | 176 -> One (r157)
  | 175 -> One (r158)
  | 174 -> One (r159)
  | 178 -> One (r160)
  | 2081 -> One (r161)
  | 2080 -> One (r162)
  | 180 -> One (r163)
  | 181 -> One (r164)
  | 2062 -> One (r165)
  | 2079 -> One (r167)
  | 2078 -> One (r168)
  | 2077 -> One (r169)
  | 2076 -> One (r170)
  | 2075 -> One (r171)
  | 2059 -> One (r175)
  | 2058 -> One (r176)
  | 2052 -> One (r177)
  | 2051 -> One (r178)
  | 2050 -> One (r179)
  | 2048 -> One (r181)
  | 2047 -> One (r182)
  | 191 -> One (r183)
  | 1257 -> One (r184)
  | 1255 -> One (r185)
  | 779 -> One (r186)
  | 862 -> One (r188)
  | 2046 -> One (r190)
  | 2045 -> One (r191)
  | 2044 -> One (r192)
  | 194 -> One (r193)
  | 193 -> One (r194)
  | 2043 -> One (r195)
  | 2030 -> One (r196)
  | 2029 -> One (r197)
  | 832 -> One (r198)
  | 831 | 1066 -> One (r199)
  | 2028 -> One (r201)
  | 2015 -> One (r202)
  | 2014 -> One (r203)
  | 2013 -> One (r204)
  | 197 -> One (r205)
  | 2012 -> One (r206)
  | 2009 -> One (r207)
  | 207 -> One (r208)
  | 1995 -> One (r209)
  | 1994 -> One (r210)
  | 205 -> One (r211)
  | 1993 -> One (r212)
  | 1992 -> One (r213)
  | 209 -> One (r214)
  | 1991 -> One (r215)
  | 212 -> One (r216)
  | 1945 -> One (r218)
  | 1987 -> One (r219)
  | 1986 -> One (r220)
  | 592 -> One (r221)
  | 216 -> One (r222)
  | 215 -> One (r223)
  | 588 -> One (r224)
  | 587 -> One (r225)
  | 218 -> One (r226)
  | 585 -> One (r227)
  | 575 -> One (r228)
  | 574 -> One (r229)
  | 572 -> One (r230)
  | 342 -> One (r231)
  | 341 -> One (r232)
  | 340 -> One (r233)
  | 222 -> One (r234)
  | 221 -> One (r235)
  | 324 -> One (r236)
  | 307 -> One (r238)
  | 334 -> One (r240)
  | 333 -> One (r241)
  | 225 -> One (r242)
  | 227 -> One (r243)
  | 226 -> One (r244)
  | 332 -> One (r245)
  | 331 -> One (r246)
  | 245 -> One (r247)
  | 244 -> One (r248)
  | 323 -> One (r250)
  | 312 -> One (r251)
  | 326 -> One (r253)
  | 325 -> One (r254)
  | 241 | 1663 -> One (r255)
  | 242 -> One (r257)
  | 240 -> One (r258)
  | 239 -> One (r259)
  | 232 -> One (r260)
  | 238 -> One (r262)
  | 235 -> One (r264)
  | 234 -> One (r265)
  | 237 -> One (r266)
  | 236 -> One (r267)
  | 309 -> One (r268)
  | 308 -> One (r269)
  | 305 -> One (r270)
  | 304 -> One (r271)
  | 303 -> One (r272)
  | 302 -> One (r273)
  | 255 -> One (r274)
  | 252 -> One (r275)
  | 254 -> One (r276)
  | 290 -> One (r277)
  | 289 -> One (r280)
  | 268 -> One (r282)
  | 278 -> One (r283)
  | 274 -> One (r284)
  | 273 -> One (r285)
  | 276 -> One (r286)
  | 283 -> One (r287)
  | 282 -> One (r288)
  | 288 -> One (r289)
  | 287 -> One (r290)
  | 298 -> One (r291)
  | 297 -> One (r292)
  | 296 -> One (r293)
  | 295 -> One (r294)
  | 294 -> One (r295)
  | 293 -> One (r296)
  | 301 -> One (r297)
  | 311 -> One (r298)
  | 322 -> One (r299)
  | 319 -> One (r301)
  | 316 -> One (r302)
  | 315 -> One (r303)
  | 318 -> One (r304)
  | 321 -> One (r305)
  | 330 -> One (r306)
  | 329 -> One (r307)
  | 328 -> One (r308)
  | 339 -> One (r309)
  | 337 -> One (r311)
  | 336 -> One (r312)
  | 349 -> One (r313)
  | 348 -> One (r314)
  | 347 -> One (r315)
  | 346 -> One (r316)
  | 345 -> One (r317)
  | 351 -> One (r318)
  | 354 -> One (r319)
  | 524 -> One (r320)
  | 523 | 799 | 807 -> One (r321)
  | 514 | 795 | 806 | 1758 -> One (r322)
  | 363 -> One (r324)
  | 362 -> One (r325)
  | 360 -> One (r326)
  | 359 -> One (r327)
  | 433 -> One (r328)
  | 432 -> One (r329)
  | 431 -> One (r330)
  | 430 -> One (r331)
  | 429 -> One (r332)
  | 366 -> One (r333)
  | 428 -> One (r334)
  | 373 -> One (r335)
  | 369 -> One (r336)
  | 408 -> One (r337)
  | 407 -> One (r339)
  | 401 -> One (r341)
  | 400 -> One (r342)
  | 399 -> One (r343)
  | 398 -> One (r344)
  | 397 -> One (r345)
  | 424 -> One (r347)
  | 425 -> One (r349)
  | 377 -> One (r350)
  | 383 -> One (r352)
  | 388 -> One (r354)
  | 387 -> One (r355)
  | 384 -> One (r356)
  | 376 -> One (r357)
  | 381 -> One (r358)
  | 379 -> One (r359)
  | 380 -> One (r360)
  | 382 -> One (r361)
  | 391 -> One (r362)
  | 390 -> One (r363)
  | 395 -> One (r364)
  | 394 -> One (r365)
  | 393 -> One (r366)
  | 406 -> One (r367)
  | 411 -> One (r369)
  | 410 -> One (r370)
  | 413 -> One (r371)
  | 417 -> One (r372)
  | 420 -> One (r373)
  | 419 -> One (r374)
  | 421 | 2218 -> One (r375)
  | 423 -> One (r376)
  | 427 -> One (r377)
  | 567 -> One (r378)
  | 437 -> One (r379)
  | 556 -> One (r380)
  | 555 -> One (r382)
  | 554 -> One (r383)
  | 561 -> One (r384)
  | 444 -> One (r385)
  | 441 -> One (r386)
  | 440 -> One (r388)
  | 439 -> One (r389)
  | 443 -> One (r390)
  | 560 -> One (r391)
  | 457 | 1089 -> One (r393)
  | 458 -> One (r395)
  | 448 -> One (r396)
  | 447 -> One (r397)
  | 449 -> One (r398)
  | 451 -> One (r399)
  | 463 -> One (r401)
  | 462 -> One (r403)
  | 553 -> One (r404)
  | 552 -> One (r405)
  | 466 -> One (r406)
  | 468 -> One (r407)
  | 546 -> One (r408)
  | 471 -> One (r409)
  | 470 -> One (r410)
  | 478 -> One (r411)
  | 477 -> One (r412)
  | 476 -> One (r413)
  | 475 -> One (r414)
  | 474 -> One (r415)
  | 480 -> One (r416)
  | 483 -> One (r417)
  | 545 -> One (r418)
  | 486 -> One (r419)
  | 485 -> One (r420)
  | 488 | 762 -> One (r421)
  | 491 -> One (r423)
  | 490 -> One (r424)
  | 489 -> One (r425)
  | 495 -> One (r426)
  | 509 -> One (r427)
  | 506 -> One (r428)
  | 505 -> One (r429)
  | 504 -> One (r430)
  | 503 -> One (r431)
  | 502 -> One (r432)
  | 508 -> One (r433)
  | 512 -> One (r434)
  | 544 -> One (r435)
  | 516 -> One (r436)
  | 520 -> One (r438)
  | 519 -> One (r439)
  | 518 -> One (r440)
  | 522 -> One (r441)
  | 521 -> One (r442)
  | 535 -> One (r443)
  | 532 -> One (r444)
  | 531 -> One (r445)
  | 530 -> One (r446)
  | 529 -> One (r447)
  | 528 -> One (r448)
  | 534 -> One (r449)
  | 539 -> One (r450)
  | 538 -> One (r451)
  | 537 | 800 | 808 -> One (r452)
  | 541 -> One (r453)
  | 543 -> One (r454)
  | 549 -> One (r455)
  | 548 -> One (r456)
  | 551 -> One (r457)
  | 565 -> One (r458)
  | 569 -> One (r459)
  | 571 -> One (r460)
  | 582 -> One (r461)
  | 581 -> One (r462)
  | 580 -> One (r463)
  | 579 -> One (r464)
  | 578 -> One (r465)
  | 584 -> One (r466)
  | 591 -> One (r467)
  | 590 -> One (r468)
  | 1982 -> One (r469)
  | 1981 -> One (r470)
  | 1980 -> One (r471)
  | 1979 -> One (r472)
  | 1970 -> One (r473)
  | 1969 -> One (r475)
  | 1968 -> One (r476)
  | 1964 -> One (r477)
  | 599 -> One (r478)
  | 598 -> One (r479)
  | 597 -> One (r480)
  | 595 -> One (r481)
  | 605 -> One (r482)
  | 606 -> One (r484)
  | 604 -> One (r485)
  | 603 -> One (r486)
  | 1963 -> One (r487)
  | 1962 -> One (r488)
  | 1961 -> One (r489)
  | 1960 -> One (r490)
  | 1959 -> One (r491)
  | 1958 -> One (r492)
  | 613 -> One (r493)
  | 612 -> One (r494)
  | 1955 -> One (r495)
  | 1954 -> One (r496)
  | 1942 -> One (r497)
  | 1941 -> One (r498)
  | 684 -> One (r499)
  | 1310 -> One (r501)
  | 1307 -> One (r503)
  | 1306 -> One (r504)
  | 1305 -> One (r505)
  | 668 -> One (r506)
  | 658 -> One (r507)
  | 657 -> One (r508)
  | 636 -> One (r509)
  | 626 -> One (r510)
  | 625 -> One (r511)
  | 624 -> One (r512)
  | 623 -> One (r513)
  | 622 -> One (r514)
  | 633 -> One (r515)
  | 632 -> One (r516)
  | 631 -> One (r517)
  | 630 -> One (r518)
  | 629 -> One (r519)
  | 635 -> One (r520)
  | 640 -> One (r521)
  | 655 -> One (r522)
  | 652 -> One (r523)
  | 651 -> One (r524)
  | 650 -> One (r525)
  | 649 -> One (r526)
  | 648 -> One (r527)
  | 654 -> One (r528)
  | 665 -> One (r529)
  | 664 -> One (r530)
  | 663 -> One (r531)
  | 662 -> One (r532)
  | 661 -> One (r533)
  | 667 -> One (r534)
  | 682 -> One (r535)
  | 672 -> One (r536)
  | 671 -> One (r537)
  | 679 -> One (r538)
  | 678 -> One (r539)
  | 677 -> One (r540)
  | 676 -> One (r541)
  | 675 -> One (r542)
  | 681 -> One (r543)
  | 696 -> One (r544)
  | 687 -> One (r545)
  | 695 -> One (r547)
  | 694 -> One (r548)
  | 692 -> One (r549)
  | 691 -> One (r550)
  | 690 -> One (r551)
  | 1935 -> One (r552)
  | 1934 -> One (r553)
  | 1933 -> One (r554)
  | 699 -> One (r555)
  | 1932 -> One (r556)
  | 1063 -> One (r557)
  | 1903 -> One (r559)
  | 1902 -> One (r560)
  | 1901 -> One (r561)
  | 1900 -> One (r562)
  | 1899 -> One (r563)
  | 1898 -> One (r564)
  | 1913 -> One (r566)
  | 1912 -> One (r567)
  | 1931 -> One (r569)
  | 1930 -> One (r570)
  | 1929 -> One (r571)
  | 1384 -> One (r574)
  | 1383 -> One (r575)
  | 1382 -> One (r576)
  | 1381 -> One (r577)
  | 1380 -> One (r578)
  | 1379 -> One (r579)
  | 708 -> One (r580)
  | 707 -> One (r581)
  | 747 -> One (r582)
  | 746 -> One (r583)
  | 1370 -> One (r584)
  | 1378 -> One (r586)
  | 1377 -> One (r587)
  | 711 -> One (r588)
  | 1120 -> One (r589)
  | 1359 -> One (r591)
  | 1358 -> One (r592)
  | 1355 -> One (r593)
  | 1352 -> One (r594)
  | 714 -> One (r595)
  | 1351 -> One (r596)
  | 1331 -> One (r597)
  | 1330 -> One (r598)
  | 1329 -> One (r599)
  | 1334 -> One (r601)
  | 1346 -> One (r603)
  | 1345 -> One (r604)
  | 1342 -> One (r605)
  | 720 -> One (r606)
  | 719 -> One (r607)
  | 1341 -> One (r608)
  | 723 -> One (r609)
  | 722 -> One (r610)
  | 727 -> One (r611)
  | 732 -> One (r612)
  | 731 -> One (r613)
  | 730 | 1338 -> One (r614)
  | 1337 -> One (r615)
  | 758 -> One (r616)
  | 757 -> One (r617)
  | 756 -> One (r618)
  | 755 -> One (r619)
  | 737 -> One (r620)
  | 736 -> One (r621)
  | 743 -> One (r622)
  | 741 -> One (r623)
  | 740 -> One (r624)
  | 739 -> One (r625)
  | 745 -> One (r626)
  | 750 -> One (r627)
  | 749 -> One (r628)
  | 1301 -> One (r629)
  | 765 -> One (r630)
  | 764 -> One (r631)
  | 1300 -> One (r632)
  | 1287 -> One (r633)
  | 767 -> One (r634)
  | 769 -> One (r635)
  | 1124 | 1280 -> One (r636)
  | 1123 | 1279 -> One (r637)
  | 771 | 877 -> One (r638)
  | 770 | 876 -> One (r639)
  | 1272 -> One (r640)
  | 1261 -> One (r641)
  | 1260 -> One (r642)
  | 774 -> One (r643)
  | 773 -> One (r644)
  | 1259 -> One (r645)
  | 777 -> One (r646)
  | 776 -> One (r647)
  | 1258 -> One (r648)
  | 1254 -> One (r649)
  | 1253 -> One (r650)
  | 1252 -> One (r651)
  | 814 -> One (r652)
  | 815 -> One (r654)
  | 1088 -> One (r656)
  | 816 -> One (r658)
  | 1086 -> One (r660)
  | 1251 -> One (r662)
  | 822 -> One (r663)
  | 821 -> One (r664)
  | 818 -> One (r665)
  | 783 -> One (r666)
  | 782 -> One (r667)
  | 785 -> One (r668)
  | 794 -> One (r670)
  | 792 -> One (r671)
  | 791 -> One (r672)
  | 790 -> One (r673)
  | 789 -> One (r674)
  | 798 -> One (r675)
  | 797 -> One (r676)
  | 804 -> One (r677)
  | 803 -> One (r678)
  | 802 -> One (r679)
  | 813 -> One (r680)
  | 812 -> One (r681)
  | 811 -> One (r682)
  | 810 -> One (r683)
  | 820 -> One (r684)
  | 826 -> One (r685)
  | 825 -> One (r686)
  | 824 -> One (r687)
  | 1250 -> One (r688)
  | 837 -> One (r689)
  | 836 -> One (r690)
  | 835 -> One (r691)
  | 830 -> One (r692)
  | 834 -> One (r693)
  | 839 -> One (r694)
  | 841 -> One (r695)
  | 1152 | 1227 -> One (r696)
  | 1151 | 1226 -> One (r697)
  | 843 | 1150 -> One (r698)
  | 842 | 1149 -> One (r699)
  | 1220 -> One (r700)
  | 1225 -> One (r702)
  | 1224 -> One (r703)
  | 1223 -> One (r704)
  | 1222 -> One (r705)
  | 1221 -> One (r706)
  | 1218 -> One (r707)
  | 848 -> One (r708)
  | 847 -> One (r709)
  | 846 -> One (r710)
  | 845 -> One (r711)
  | 852 -> One (r712)
  | 851 -> One (r713)
  | 850 -> One (r714)
  | 1217 -> One (r715)
  | 861 -> One (r716)
  | 860 -> One (r717)
  | 859 -> One (r718)
  | 869 -> One (r719)
  | 868 -> One (r720)
  | 867 -> One (r721)
  | 866 -> One (r722)
  | 871 -> One (r723)
  | 873 -> One (r724)
  | 875 -> One (r725)
  | 881 -> One (r726)
  | 880 -> One (r727)
  | 879 -> One (r728)
  | 1119 -> One (r729)
  | 891 -> One (r730)
  | 890 -> One (r731)
  | 889 -> One (r732)
  | 888 -> One (r733)
  | 893 -> One (r734)
  | 897 -> One (r735)
  | 896 -> One (r736)
  | 895 -> One (r737)
  | 899 -> One (r738)
  | 904 -> One (r739)
  | 903 -> One (r740)
  | 912 -> One (r741)
  | 911 -> One (r742)
  | 910 -> One (r743)
  | 909 -> One (r744)
  | 918 -> One (r745)
  | 917 -> One (r746)
  | 916 -> One (r747)
  | 915 -> One (r748)
  | 927 -> One (r749)
  | 926 -> One (r750)
  | 925 -> One (r751)
  | 924 -> One (r752)
  | 931 -> One (r753)
  | 930 -> One (r754)
  | 938 -> One (r755)
  | 937 -> One (r756)
  | 936 -> One (r757)
  | 935 -> One (r758)
  | 944 -> One (r759)
  | 943 -> One (r760)
  | 942 -> One (r761)
  | 941 -> One (r762)
  | 950 -> One (r763)
  | 949 -> One (r764)
  | 948 -> One (r765)
  | 947 -> One (r766)
  | 956 -> One (r767)
  | 955 -> One (r768)
  | 954 -> One (r769)
  | 953 -> One (r770)
  | 962 -> One (r771)
  | 961 -> One (r772)
  | 960 -> One (r773)
  | 959 -> One (r774)
  | 968 -> One (r775)
  | 967 -> One (r776)
  | 966 -> One (r777)
  | 965 -> One (r778)
  | 974 -> One (r779)
  | 973 -> One (r780)
  | 972 -> One (r781)
  | 971 -> One (r782)
  | 980 -> One (r783)
  | 979 -> One (r784)
  | 978 -> One (r785)
  | 977 -> One (r786)
  | 986 -> One (r787)
  | 985 -> One (r788)
  | 984 -> One (r789)
  | 983 -> One (r790)
  | 992 -> One (r791)
  | 991 -> One (r792)
  | 990 -> One (r793)
  | 989 -> One (r794)
  | 998 -> One (r795)
  | 997 -> One (r796)
  | 996 -> One (r797)
  | 995 -> One (r798)
  | 1004 -> One (r799)
  | 1003 -> One (r800)
  | 1002 -> One (r801)
  | 1001 -> One (r802)
  | 1010 -> One (r803)
  | 1009 -> One (r804)
  | 1008 -> One (r805)
  | 1007 -> One (r806)
  | 1016 -> One (r807)
  | 1015 -> One (r808)
  | 1014 -> One (r809)
  | 1013 -> One (r810)
  | 1022 -> One (r811)
  | 1021 -> One (r812)
  | 1020 -> One (r813)
  | 1019 -> One (r814)
  | 1028 -> One (r815)
  | 1027 -> One (r816)
  | 1026 -> One (r817)
  | 1025 -> One (r818)
  | 1034 -> One (r819)
  | 1033 -> One (r820)
  | 1032 -> One (r821)
  | 1031 -> One (r822)
  | 1040 -> One (r823)
  | 1039 -> One (r824)
  | 1038 -> One (r825)
  | 1037 -> One (r826)
  | 1054 -> One (r827)
  | 1047 -> One (r828)
  | 1046 -> One (r829)
  | 1045 -> One (r830)
  | 1044 -> One (r831)
  | 1049 -> One (r832)
  | 1053 -> One (r833)
  | 1052 -> One (r834)
  | 1051 -> One (r835)
  | 1060 -> One (r836)
  | 1059 -> One (r837)
  | 1058 -> One (r838)
  | 1057 -> One (r839)
  | 1117 -> One (r840)
  | 1114 -> One (r841)
  | 1062 -> One (r842)
  | 1065 -> One (r843)
  | 1064 -> One (r844)
  | 1072 -> One (r845)
  | 1071 -> One (r846)
  | 1070 -> One (r847)
  | 1069 -> One (r848)
  | 1068 -> One (r849)
  | 1077 -> One (r850)
  | 1076 -> One (r851)
  | 1075 -> One (r852)
  | 1074 -> One (r853)
  | 1080 -> One (r854)
  | 1079 -> One (r855)
  | 1087 -> One (r856)
  | 1085 -> One (r857)
  | 1084 -> One (r858)
  | 1093 -> One (r859)
  | 1092 -> One (r860)
  | 1091 -> One (r861)
  | 1096 -> One (r862)
  | 1095 -> One (r863)
  | 1107 -> One (r864)
  | 1104 -> One (r865)
  | 1103 -> One (r866)
  | 1102 -> One (r867)
  | 1101 -> One (r868)
  | 1100 -> One (r869)
  | 1106 -> One (r870)
  | 1110 -> One (r871)
  | 1112 -> One (r872)
  | 1116 -> One (r873)
  | 1130 -> One (r874)
  | 1129 -> One (r875)
  | 1128 -> One (r876)
  | 1127 -> One (r877)
  | 1133 | 1283 -> One (r878)
  | 1132 | 1282 -> One (r879)
  | 1131 | 1281 -> One (r880)
  | 1139 -> One (r881)
  | 1138 -> One (r882)
  | 1137 -> One (r883)
  | 1136 -> One (r884)
  | 1142 | 1286 -> One (r885)
  | 1141 | 1285 -> One (r886)
  | 1140 | 1284 -> One (r887)
  | 1148 -> One (r888)
  | 1147 -> One (r889)
  | 1146 -> One (r890)
  | 1145 -> One (r891)
  | 1158 -> One (r892)
  | 1157 -> One (r893)
  | 1156 -> One (r894)
  | 1155 -> One (r895)
  | 1161 | 1230 -> One (r896)
  | 1160 | 1229 -> One (r897)
  | 1159 | 1228 -> One (r898)
  | 1167 -> One (r899)
  | 1166 -> One (r900)
  | 1165 -> One (r901)
  | 1164 -> One (r902)
  | 1170 | 1233 -> One (r903)
  | 1169 | 1232 -> One (r904)
  | 1168 | 1231 -> One (r905)
  | 1176 -> One (r906)
  | 1175 -> One (r907)
  | 1174 -> One (r908)
  | 1173 -> One (r909)
  | 1181 | 1238 -> One (r910)
  | 1180 | 1237 -> One (r911)
  | 1179 | 1236 -> One (r912)
  | 1178 | 1235 -> One (r913)
  | 1187 -> One (r914)
  | 1186 -> One (r915)
  | 1185 -> One (r916)
  | 1184 -> One (r917)
  | 1190 | 1241 -> One (r918)
  | 1189 | 1240 -> One (r919)
  | 1188 | 1239 -> One (r920)
  | 1196 -> One (r921)
  | 1195 -> One (r922)
  | 1194 -> One (r923)
  | 1193 -> One (r924)
  | 1199 | 1244 -> One (r925)
  | 1198 | 1243 -> One (r926)
  | 1197 | 1242 -> One (r927)
  | 1205 -> One (r928)
  | 1204 -> One (r929)
  | 1203 -> One (r930)
  | 1202 -> One (r931)
  | 1212 -> One (r932)
  | 1211 -> One (r933)
  | 1210 -> One (r934)
  | 1209 -> One (r935)
  | 1249 -> One (r936)
  | 1248 -> One (r937)
  | 1247 -> One (r938)
  | 1265 -> One (r939)
  | 1264 -> One (r940)
  | 1263 -> One (r941)
  | 1271 -> One (r942)
  | 1270 -> One (r943)
  | 1269 -> One (r944)
  | 1268 -> One (r945)
  | 1278 -> One (r946)
  | 1277 -> One (r947)
  | 1276 -> One (r948)
  | 1275 -> One (r949)
  | 1293 -> One (r950)
  | 1292 -> One (r951)
  | 1291 -> One (r952)
  | 1290 -> One (r953)
  | 1295 -> One (r954)
  | 1299 -> One (r955)
  | 1298 -> One (r956)
  | 1297 -> One (r957)
  | 1304 -> One (r958)
  | 1303 -> One (r959)
  | 1309 -> One (r960)
  | 1313 -> One (r961)
  | 1315 -> One (r962)
  | 1317 -> One (r963)
  | 1319 -> One (r964)
  | 1321 -> One (r965)
  | 1324 -> One (r967)
  | 1323 -> One (r968)
  | 1336 -> One (r969)
  | 1335 -> One (r970)
  | 1328 -> One (r971)
  | 1327 -> One (r972)
  | 1344 -> One (r973)
  | 1350 -> One (r974)
  | 1349 -> One (r975)
  | 1348 -> One (r976)
  | 1357 -> One (r977)
  | 1363 -> One (r978)
  | 1362 -> One (r979)
  | 1361 -> One (r980)
  | 1365 -> One (r981)
  | 1367 -> One (r982)
  | 1369 -> One (r983)
  | 1372 -> One (r984)
  | 1374 -> One (r985)
  | 1376 -> One (r986)
  | 1399 -> One (r987)
  | 1398 -> One (r988)
  | 1417 -> One (r990)
  | 1416 -> One (r991)
  | 1415 -> One (r992)
  | 1395 -> One (r993)
  | 1394 -> One (r994)
  | 1393 -> One (r995)
  | 1392 -> One (r996)
  | 1389 -> One (r997)
  | 1388 -> One (r998)
  | 1387 -> One (r999)
  | 1386 -> One (r1000)
  | 1391 -> One (r1001)
  | 1414 -> One (r1002)
  | 1405 -> One (r1003)
  | 1404 -> One (r1004)
  | 1397 -> One (r1005)
  | 1403 -> One (r1006)
  | 1402 -> One (r1007)
  | 1401 -> One (r1008)
  | 1411 -> One (r1009)
  | 1410 -> One (r1010)
  | 1409 -> One (r1011)
  | 1408 -> One (r1012)
  | 1407 -> One (r1013)
  | 1413 -> One (r1014)
  | 1928 -> One (r1015)
  | 1927 -> One (r1016)
  | 1419 -> One (r1017)
  | 1421 -> One (r1018)
  | 1423 -> One (r1019)
  | 1926 -> One (r1020)
  | 1925 -> One (r1021)
  | 1425 -> One (r1022)
  | 1430 -> One (r1023)
  | 1429 -> One (r1024)
  | 1428 -> One (r1025)
  | 1427 -> One (r1026)
  | 1438 -> One (r1027)
  | 1441 -> One (r1029)
  | 1440 -> One (r1030)
  | 1437 -> One (r1031)
  | 1436 -> One (r1032)
  | 1435 -> One (r1033)
  | 1434 -> One (r1034)
  | 1433 -> One (r1035)
  | 1432 -> One (r1036)
  | 1495 -> One (r1037)
  | 1494 -> One (r1038)
  | 1493 -> One (r1039)
  | 1450 | 1554 -> One (r1040)
  | 1444 | 1553 -> One (r1041)
  | 1443 | 1552 -> One (r1042)
  | 1442 | 1551 -> One (r1043)
  | 1473 -> One (r1044)
  | 1472 -> One (r1046)
  | 1463 -> One (r1047)
  | 1468 -> One (r1055)
  | 1465 -> One (r1057)
  | 1464 -> One (r1058)
  | 1462 -> One (r1059)
  | 1461 -> One (r1060)
  | 1460 -> One (r1061)
  | 1459 -> One (r1062)
  | 1455 -> One (r1063)
  | 1454 -> One (r1064)
  | 1458 -> One (r1065)
  | 1457 -> One (r1066)
  | 1471 -> One (r1067)
  | 1470 -> One (r1068)
  | 1481 -> One (r1070)
  | 1480 -> One (r1071)
  | 1479 -> One (r1072)
  | 1478 -> One (r1073)
  | 1492 -> One (r1074)
  | 1488 -> One (r1075)
  | 1484 -> One (r1076)
  | 1487 -> One (r1077)
  | 1486 -> One (r1078)
  | 1491 -> One (r1079)
  | 1490 -> One (r1080)
  | 1749 -> One (r1081)
  | 1748 -> One (r1082)
  | 1508 -> One (r1083)
  | 1507 -> One (r1084)
  | 1506 -> One (r1085)
  | 1505 -> One (r1086)
  | 1504 -> One (r1087)
  | 1503 -> One (r1088)
  | 1502 -> One (r1089)
  | 1501 -> One (r1090)
  | 1541 -> One (r1091)
  | 1540 -> One (r1092)
  | 1543 -> One (r1094)
  | 1542 -> One (r1095)
  | 1536 -> One (r1096)
  | 1518 -> One (r1097)
  | 1517 -> One (r1098)
  | 1516 -> One (r1099)
  | 1515 -> One (r1100)
  | 1514 -> One (r1101)
  | 1522 -> One (r1105)
  | 1521 -> One (r1106)
  | 1535 -> One (r1107)
  | 1527 -> One (r1108)
  | 1526 -> One (r1109)
  | 1525 -> One (r1110)
  | 1524 -> One (r1111)
  | 1534 -> One (r1112)
  | 1533 -> One (r1113)
  | 1532 -> One (r1114)
  | 1531 -> One (r1115)
  | 1530 -> One (r1116)
  | 1529 -> One (r1117)
  | 1539 -> One (r1120)
  | 1538 -> One (r1121)
  | 1545 -> One (r1122)
  | 1550 -> One (r1123)
  | 1549 -> One (r1124)
  | 1548 -> One (r1125)
  | 1547 -> One (r1126)
  | 1610 | 1664 -> One (r1128)
  | 1666 -> One (r1130)
  | 1680 -> One (r1132)
  | 1670 -> One (r1133)
  | 1669 -> One (r1134)
  | 1651 -> One (r1135)
  | 1650 -> One (r1136)
  | 1649 -> One (r1137)
  | 1648 -> One (r1138)
  | 1647 -> One (r1139)
  | 1646 -> One (r1140)
  | 1645 -> One (r1141)
  | 1635 -> One (r1142)
  | 1634 -> One (r1143)
  | 1566 -> One (r1144)
  | 1565 -> One (r1145)
  | 1564 -> One (r1146)
  | 1560 -> One (r1147)
  | 1558 -> One (r1148)
  | 1557 -> One (r1149)
  | 1563 -> One (r1150)
  | 1562 -> One (r1151)
  | 1628 -> One (r1152)
  | 1627 -> One (r1153)
  | 1572 -> One (r1154)
  | 1568 -> One (r1155)
  | 1571 -> One (r1156)
  | 1570 -> One (r1157)
  | 1583 -> One (r1158)
  | 1582 -> One (r1159)
  | 1581 -> One (r1160)
  | 1580 -> One (r1161)
  | 1579 -> One (r1162)
  | 1574 -> One (r1163)
  | 1594 -> One (r1164)
  | 1593 -> One (r1165)
  | 1592 -> One (r1166)
  | 1591 -> One (r1167)
  | 1590 -> One (r1168)
  | 1585 -> One (r1169)
  | 1619 -> One (r1170)
  | 1618 -> One (r1171)
  | 1596 -> One (r1172)
  | 1617 -> One (r1173)
  | 1616 -> One (r1174)
  | 1615 -> One (r1175)
  | 1614 -> One (r1176)
  | 1598 -> One (r1177)
  | 1612 -> One (r1178)
  | 1602 -> One (r1179)
  | 1601 -> One (r1180)
  | 1600 -> One (r1181)
  | 1609 | 1657 -> One (r1182)
  | 1606 -> One (r1184)
  | 1605 -> One (r1185)
  | 1604 -> One (r1186)
  | 1603 | 1656 -> One (r1187)
  | 1608 -> One (r1188)
  | 1624 -> One (r1189)
  | 1623 -> One (r1190)
  | 1622 -> One (r1191)
  | 1626 -> One (r1193)
  | 1625 -> One (r1194)
  | 1621 -> One (r1195)
  | 1630 -> One (r1196)
  | 1633 -> One (r1197)
  | 1644 -> One (r1198)
  | 1643 -> One (r1199)
  | 1642 -> One (r1200)
  | 1641 -> One (r1201)
  | 1640 -> One (r1202)
  | 1639 -> One (r1203)
  | 1638 -> One (r1204)
  | 1637 -> One (r1205)
  | 1668 -> One (r1206)
  | 1655 -> One (r1207)
  | 1654 -> One (r1208)
  | 1653 -> One (r1209)
  | 1667 -> One (r1210)
  | 1659 -> One (r1211)
  | 1665 -> One (r1212)
  | 1662 -> One (r1213)
  | 1661 -> One (r1214)
  | 1679 -> One (r1215)
  | 1678 -> One (r1216)
  | 1677 -> One (r1217)
  | 1676 -> One (r1218)
  | 1675 -> One (r1219)
  | 1674 -> One (r1220)
  | 1673 -> One (r1221)
  | 1672 -> One (r1222)
  | 1689 -> One (r1223)
  | 1691 -> One (r1224)
  | 1701 -> One (r1225)
  | 1700 -> One (r1226)
  | 1699 -> One (r1227)
  | 1698 -> One (r1228)
  | 1697 -> One (r1229)
  | 1696 -> One (r1230)
  | 1695 -> One (r1231)
  | 1694 -> One (r1232)
  | 1745 -> One (r1233)
  | 1725 -> One (r1234)
  | 1724 -> One (r1235)
  | 1723 -> One (r1236)
  | 1722 -> One (r1237)
  | 1707 -> One (r1238)
  | 1706 -> One (r1239)
  | 1705 -> One (r1240)
  | 1704 -> One (r1241)
  | 1711 -> One (r1242)
  | 1710 -> One (r1243)
  | 1713 -> One (r1244)
  | 1718 -> One (r1245)
  | 1717 -> One (r1246)
  | 1716 | 2054 -> One (r1247)
  | 1720 | 2053 -> One (r1248)
  | 1742 -> One (r1249)
  | 1734 -> One (r1250)
  | 1733 -> One (r1251)
  | 1732 -> One (r1252)
  | 1741 -> One (r1253)
  | 1740 -> One (r1254)
  | 1834 -> One (r1255)
  | 1878 -> One (r1257)
  | 1759 -> One (r1258)
  | 1895 -> One (r1260)
  | 1886 -> One (r1261)
  | 1885 -> One (r1262)
  | 1757 -> One (r1263)
  | 1756 -> One (r1264)
  | 1755 -> One (r1265)
  | 1754 -> One (r1266)
  | 1753 -> One (r1267)
  | 1872 -> One (r1268)
  | 1871 -> One (r1269)
  | 1762 -> One (r1270)
  | 1761 -> One (r1271)
  | 1803 -> One (r1273)
  | 1792 -> One (r1274)
  | 1791 -> One (r1275)
  | 1782 -> One (r1276)
  | 1781 -> One (r1278)
  | 1780 -> One (r1279)
  | 1779 -> One (r1280)
  | 1768 -> One (r1281)
  | 1767 -> One (r1282)
  | 1765 -> One (r1283)
  | 1778 -> One (r1284)
  | 1777 -> One (r1285)
  | 1776 -> One (r1286)
  | 1775 -> One (r1287)
  | 1774 -> One (r1288)
  | 1773 -> One (r1289)
  | 1772 -> One (r1290)
  | 1771 -> One (r1291)
  | 1790 -> One (r1292)
  | 1789 -> One (r1293)
  | 1788 -> One (r1294)
  | 1802 -> One (r1295)
  | 1801 -> One (r1296)
  | 1800 -> One (r1297)
  | 1799 -> One (r1298)
  | 1798 -> One (r1299)
  | 1797 -> One (r1300)
  | 1796 -> One (r1301)
  | 1795 -> One (r1302)
  | 1807 -> One (r1303)
  | 1806 -> One (r1304)
  | 1805 -> One (r1305)
  | 1866 -> One (r1306)
  | 1865 -> One (r1307)
  | 1864 -> One (r1308)
  | 1863 -> One (r1309)
  | 1862 -> One (r1310)
  | 1861 -> One (r1311)
  | 1858 -> One (r1312)
  | 1810 -> One (r1313)
  | 1854 -> One (r1314)
  | 1853 -> One (r1315)
  | 1848 -> One (r1316)
  | 1847 -> One (r1317)
  | 1846 -> One (r1318)
  | 1845 -> One (r1319)
  | 1819 -> One (r1320)
  | 1818 -> One (r1321)
  | 1817 -> One (r1322)
  | 1816 -> One (r1323)
  | 1815 -> One (r1324)
  | 1814 -> One (r1325)
  | 1844 -> One (r1326)
  | 1823 -> One (r1327)
  | 1822 -> One (r1328)
  | 1821 -> One (r1329)
  | 1827 -> One (r1330)
  | 1826 -> One (r1331)
  | 1825 -> One (r1332)
  | 1841 -> One (r1333)
  | 1831 -> One (r1334)
  | 1830 -> One (r1335)
  | 1843 -> One (r1337)
  | 1829 -> One (r1338)
  | 1838 -> One (r1339)
  | 1833 -> One (r1340)
  | 1852 -> One (r1341)
  | 1851 -> One (r1342)
  | 1850 -> One (r1343)
  | 1857 -> One (r1344)
  | 1856 -> One (r1345)
  | 1860 -> One (r1346)
  | 1870 -> One (r1347)
  | 1869 -> One (r1348)
  | 1868 -> One (r1349)
  | 1874 -> One (r1350)
  | 1877 -> One (r1351)
  | 1882 -> One (r1352)
  | 1881 -> One (r1353)
  | 1880 -> One (r1354)
  | 1884 -> One (r1355)
  | 1894 -> One (r1356)
  | 1893 -> One (r1357)
  | 1892 -> One (r1358)
  | 1891 -> One (r1359)
  | 1890 -> One (r1360)
  | 1889 -> One (r1361)
  | 1888 -> One (r1362)
  | 1910 -> One (r1363)
  | 1915 -> One (r1364)
  | 1921 -> One (r1365)
  | 1920 -> One (r1366)
  | 1940 -> One (r1367)
  | 1939 -> One (r1368)
  | 1938 -> One (r1369)
  | 1944 -> One (r1370)
  | 1950 -> One (r1371)
  | 1949 -> One (r1372)
  | 1948 -> One (r1373)
  | 1947 -> One (r1374)
  | 1953 -> One (r1375)
  | 1952 -> One (r1376)
  | 1957 -> One (r1377)
  | 1967 -> One (r1378)
  | 1966 -> One (r1379)
  | 1978 -> One (r1380)
  | 1977 -> One (r1381)
  | 1976 -> One (r1382)
  | 1985 -> One (r1383)
  | 1984 -> One (r1384)
  | 1990 -> One (r1385)
  | 1989 -> One (r1386)
  | 2001 -> One (r1387)
  | 2000 -> One (r1388)
  | 1999 -> One (r1389)
  | 2003 -> One (r1390)
  | 2011 -> One (r1391)
  | 2021 -> One (r1392)
  | 2020 -> One (r1393)
  | 2019 -> One (r1394)
  | 2018 -> One (r1395)
  | 2023 -> One (r1396)
  | 2027 -> One (r1397)
  | 2026 -> One (r1398)
  | 2025 -> One (r1399)
  | 2036 -> One (r1400)
  | 2035 -> One (r1401)
  | 2034 -> One (r1402)
  | 2033 -> One (r1403)
  | 2038 -> One (r1404)
  | 2042 -> One (r1405)
  | 2041 -> One (r1406)
  | 2040 -> One (r1407)
  | 2057 -> One (r1408)
  | 2056 -> One (r1409)
  | 2069 -> One (r1410)
  | 2068 -> One (r1411)
  | 2092 -> One (r1412)
  | 2091 -> One (r1413)
  | 2090 -> One (r1414)
  | 2089 -> One (r1415)
  | 2088 -> One (r1416)
  | 2095 -> One (r1417)
  | 2094 -> One (r1418)
  | 2100 -> One (r1419)
  | 2106 -> One (r1420)
  | 2105 -> One (r1421)
  | 2104 -> One (r1422)
  | 2103 -> One (r1423)
  | 2102 -> One (r1424)
  | 2109 -> One (r1425)
  | 2108 -> One (r1426)
  | 2117 -> One (r1427)
  | 2116 -> One (r1428)
  | 2115 -> One (r1429)
  | 2131 -> One (r1430)
  | 2130 -> One (r1431)
  | 2129 -> One (r1432)
  | 2153 -> One (r1433)
  | 2157 -> One (r1434)
  | 2162 -> One (r1435)
  | 2169 -> One (r1436)
  | 2168 -> One (r1437)
  | 2167 -> One (r1438)
  | 2166 -> One (r1439)
  | 2176 -> One (r1440)
  | 2180 -> One (r1441)
  | 2184 -> One (r1442)
  | 2187 -> One (r1443)
  | 2192 -> One (r1444)
  | 2196 -> One (r1445)
  | 2200 -> One (r1446)
  | 2204 -> One (r1447)
  | 2208 -> One (r1448)
  | 2211 -> One (r1449)
  | 2215 -> One (r1450)
  | 2221 -> One (r1451)
  | 2231 -> One (r1452)
  | 2233 -> One (r1453)
  | 2236 -> One (r1454)
  | 2235 -> One (r1455)
  | 2238 -> One (r1456)
  | 2248 -> One (r1457)
  | 2244 -> One (r1458)
  | 2243 -> One (r1459)
  | 2247 -> One (r1460)
  | 2246 -> One (r1461)
  | 2253 -> One (r1462)
  | 2252 -> One (r1463)
  | 2251 -> One (r1464)
  | 2255 -> One (r1465)
  | 465 -> Select (function
    | -1 -> [R 98]
    | _ -> S (T T_DOT) :: r406)
  | 729 -> Select (function
    | -1 -> [R 98]
    | _ -> r615)
  | 131 -> Select (function
    | -1 -> r92
    | _ -> R 132 :: r114)
  | 182 -> Select (function
    | -1 -> r92
    | _ -> R 132 :: r174)
  | 701 -> Select (function
    | -1 | 61 | 180 | 191 | 209 | 211 | 1419 | 1425 | 2240 -> r564
    | _ -> R 132 :: r573)
  | 1510 -> Select (function
    | -1 -> r1000
    | _ -> R 132 :: r1104)
  | 405 -> Select (function
    | -1 -> r266
    | _ -> [R 267])
  | 513 -> Select (function
    | -1 -> [R 821]
    | _ -> S (N N_pattern) :: r435)
  | 492 -> Select (function
    | -1 -> [R 822]
    | _ -> S (N N_pattern) :: r426)
  | 137 -> Select (function
    | -1 -> r120
    | _ -> R 914 :: r126)
  | 185 -> Select (function
    | -1 -> r120
    | _ -> R 914 :: r180)
  | 1475 -> Select (function
    | -1 -> S (T T_RPAREN) :: r144
    | _ -> S (T T_COLONCOLON) :: r442)
  | 614 -> Select (function
    | -1 -> S (T T_RPAREN) :: r144
    | _ -> Sub (r3) :: r496)
  | 356 -> Select (function
    | 619 | 761 | 1062 | 1308 | 1816 | 1850 | 1901 -> r47
    | -1 -> S (T T_RPAREN) :: r144
    | _ -> r322)
  | 204 -> Select (function
    | -1 -> S (T T_RPAREN) :: r208
    | _ -> S (N N_module_type) :: r210)
  | 436 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r379
    | _ -> Sub (r381) :: r383)
  | 712 -> Select (function
    | -1 -> S (T T_RBRACKET) :: r379
    | _ -> Sub (r590) :: r592)
  | 122 -> Select (function
    | -1 -> r70
    | _ -> S (T T_MODULE) :: r79)
  | 1477 -> Select (function
    | -1 -> r375
    | _ -> S (T T_LPAREN) :: r1073)
  | 259 -> Select (function
    | 1651 | 1655 | 1659 | 1662 | 1676 | 1855 | 1879 -> r260
    | -1 -> r278
    | _ -> S (T T_DOT) :: r281)
  | 403 -> Select (function
    | -1 -> r278
    | _ -> S (T T_DOT) :: r368)
  | 165 -> Select (function
    | -1 -> r93
    | _ -> S (T T_COLON) :: r150)
  | 114 -> Select (function
    | 122 | 163 | 167 | 250 | 800 | 808 | 1066 | 1482 -> r62
    | _ -> Sub (r59) :: r60)
  | 117 -> Select (function
    | 122 | 163 | 167 | 250 | 800 | 808 | 1066 | 1482 -> r61
    | _ -> r60)
  | 125 -> Select (function
    | -1 -> r67
    | _ -> r77)
  | 124 -> Select (function
    | -1 -> r68
    | _ -> r78)
  | 123 -> Select (function
    | -1 -> r69
    | _ -> r79)
  | 2071 -> Select (function
    | -1 -> r88
    | _ -> r93)
  | 2127 -> Select (function
    | -1 -> r88
    | _ -> r93)
  | 2126 -> Select (function
    | -1 -> r89
    | _ -> r112)
  | 2070 -> Select (function
    | -1 -> r89
    | _ -> r172)
  | 133 -> Select (function
    | -1 -> r90
    | _ -> r113)
  | 184 -> Select (function
    | -1 -> r90
    | _ -> r173)
  | 132 -> Select (function
    | -1 -> r91
    | _ -> r114)
  | 183 -> Select (function
    | -1 -> r91
    | _ -> r174)
  | 187 -> Select (function
    | -1 -> r118
    | _ -> r93)
  | 156 -> Select (function
    | -1 -> r118
    | _ -> r93)
  | 155 -> Select (function
    | -1 -> r119
    | _ -> r126)
  | 186 -> Select (function
    | -1 -> r119
    | _ -> r180)
  | 260 -> Select (function
    | 1651 | 1655 | 1659 | 1662 | 1676 | 1855 | 1879 -> r259
    | -1 -> r267
    | _ -> r281)
  | 404 -> Select (function
    | -1 -> r267
    | _ -> r368)
  | 703 -> Select (function
    | -1 | 61 | 180 | 191 | 209 | 211 | 1419 | 1425 | 2240 -> r562
    | _ -> r572)
  | 702 -> Select (function
    | -1 | 61 | 180 | 191 | 209 | 211 | 1419 | 1425 | 2240 -> r563
    | _ -> r573)
  | 1513 -> Select (function
    | -1 -> r997
    | _ -> r1102)
  | 1512 -> Select (function
    | -1 -> r998
    | _ -> r1103)
  | 1511 -> Select (function
    | -1 -> r999
    | _ -> r1104)
  | _ -> raise Not_found
