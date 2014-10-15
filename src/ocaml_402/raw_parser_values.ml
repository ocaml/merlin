(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2014  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

open Std

(** Helpers around Menhir generated definitions *)

type token = Raw_parser.token

type annotation =
  [ `Shift of int
  | `Shift_token of int * token
  | `Cost of int
  | `Indent of int
  | `Unclosed of string
  | `Close
  | `Item of string
  ]

type 'a token_class = 'a Raw_parser.token_class
type 'a nonterminal_class = 'a Raw_parser.nonterminal_class

type symbol_class = Raw_parser.symbol_class =
  | CT_ : 'a token_class * annotation list -> symbol_class
  | CN_ : 'a nonterminal_class  * annotation list -> symbol_class

type symbol = Raw_parser.symbol =
  | T_ : 'a token_class * 'a -> symbol
  | N_ : 'a nonterminal_class * 'a -> symbol
  | Bottom

let class_of_symbol = function
  | T_ (k,_) -> CT_ (k, [])
  | N_ (k,_) -> CN_ (k, [])
  | Bottom -> assert false

open Raw_parser
let string_of_token : type a. a token_class -> string = function
  | T_WITH                 -> "WITH"
  | T_WHILE_LWT            -> "WHILE_LWT"
  | T_WHILE                -> "WHILE"
  | T_WHEN                 -> "WHEN"
  | T_VIRTUAL              -> "VIRTUAL"
  | T_VAL                  -> "VAL"
  | T_UNDERSCORE           -> "UNDERSCORE"
  | T_UIDENT               -> "UIDENT"
  | T_TYPE                 -> "TYPE"
  | T_TRY_LWT              -> "TRY_LWT"
  | T_TRY                  -> "TRY"
  | T_TRUE                 -> "TRUE"
  | T_TO                   -> "TO"
  | T_TILDE                -> "TILDE"
  | T_THEN                 -> "THEN"
  | T_STRUCT               -> "STRUCT"
  | T_STRING               -> "STRING"
  | T_STAR                 -> "STAR"
  | T_SIG                  -> "SIG"
  | T_SHARP                -> "SHARP"
  | T_SEMISEMI             -> "SEMISEMI"
  | T_SEMI                 -> "SEMI"
  | T_RPAREN               -> "RPAREN"
  | T_REC                  -> "REC"
  | T_RBRACKET             -> "RBRACKET"
  | T_RBRACE               -> "RBRACE"
  | T_QUOTE                -> "QUOTE"
  | T_QUESTION             -> "QUESTION"
  | T_PRIVATE              -> "PRIVATE"
  | T_PREFIXOP             -> "PREFIXOP"
  | T_PLUSEQ               -> "PLUSEQ"
  | T_PLUSDOT              -> "PLUSDOT"
  | T_PLUS                 -> "PLUS"
  | T_PERCENT              -> "PERCENT"
  | T_P4_QUOTATION         -> "P4_QUOTATION"
  | T_OUNIT_TEST_UNIT      -> "OUNIT_TEST_UNIT"
  | T_OUNIT_TEST_MODULE    -> "OUNIT_TEST_MODULE"
  | T_OUNIT_TEST           -> "OUNIT_TEST"
  | T_OUNIT_BENCH_MODULE   -> "OUNIT_BENCH_MODULE"
  | T_OUNIT_BENCH_INDEXED  -> "OUNIT_BENCH_INDEXED"
  | T_OUNIT_BENCH_FUN      -> "OUNIT_BENCH_FUN"
  | T_OUNIT_BENCH          -> "OUNIT_BENCH"
  | T_OR                   -> "OR"
  | T_OPTLABEL             -> "OPTLABEL"
  | T_OPEN                 -> "OPEN"
  | T_OF                   -> "OF"
  | T_OBJECT               -> "OBJECT"
  | T_NONREC               -> "NONREC"
  | T_NEW                  -> "NEW"
  | T_NATIVEINT            -> "NATIVEINT"
  | T_MUTABLE              -> "MUTABLE"
  | T_MODULE               -> "MODULE"
  | T_MINUSGREATER         -> "MINUSGREATER"
  | T_MINUSDOT             -> "MINUSDOT"
  | T_MINUS                -> "MINUS"
  | T_METHOD               -> "METHOD"
  | T_MATCH_LWT            -> "MATCH_LWT"
  | T_MATCH                -> "MATCH"
  | T_LPAREN               -> "LPAREN"
  | T_LIDENT               -> "LIDENT"
  | T_LET_LWT              -> "LET_LWT"
  | T_LET                  -> "LET"
  | T_LESSMINUS            -> "LESSMINUS"
  | T_LESS                 -> "LESS"
  | T_LBRACKETPERCENTPERCENT -> "LBRACKETPERCENTPERCENT"
  | T_LBRACKETPERCENT      -> "LBRACKETPERCENT"
  | T_LBRACKETLESS         -> "LBRACKETLESS"
  | T_LBRACKETGREATER      -> "LBRACKETGREATER"
  | T_LBRACKETBAR          -> "LBRACKETBAR"
  | T_LBRACKETATATAT       -> "LBRACKETATATAT"
  | T_LBRACKETATAT         -> "LBRACKETATAT"
  | T_LBRACKETAT           -> "LBRACKETAT"
  | T_LBRACKET             -> "LBRACKET"
  | T_LBRACELESS           -> "LBRACELESS"
  | T_LBRACE               -> "LBRACE"
  | T_LAZY                 -> "LAZY"
  | T_LABEL                -> "LABEL"
  | T_JSNEW                -> "JSNEW"
  | T_INT64                -> "INT64"
  | T_INT32                -> "INT32"
  | T_INT                  -> "INT"
  | T_INITIALIZER          -> "INITIALIZER"
  | T_INHERIT              -> "INHERIT"
  | T_INFIXOP4             -> "INFIXOP4"
  | T_INFIXOP3             -> "INFIXOP3"
  | T_INFIXOP2             -> "INFIXOP2"
  | T_INFIXOP1             -> "INFIXOP1"
  | T_INFIXOP0             -> "INFIXOP0"
  | T_INCLUDE              -> "INCLUDE"
  | T_IN                   -> "IN"
  | T_IF                   -> "IF"
  | T_GREATERRBRACKET      -> "GREATERRBRACKET"
  | T_GREATERRBRACE        -> "GREATERRBRACE"
  | T_GREATER              -> "GREATER"
  | T_FUNCTOR              -> "FUNCTOR"
  | T_FUNCTION             -> "FUNCTION"
  | T_FUN                  -> "FUN"
  | T_FOR_LWT              -> "FOR_LWT"
  | T_FOR                  -> "FOR"
  | T_FLOAT                -> "FLOAT"
  | T_FINALLY_LWT          -> "FINALLY_LWT"
  | T_FALSE                -> "FALSE"
  | T_EXTERNAL             -> "EXTERNAL"
  | T_EXCEPTION            -> "EXCEPTION"
  | T_EQUAL                -> "EQUAL"
  | T_EOL                  -> "EOL"
  | T_EOF                  -> "EOF"
  | T_EXITPOINT            -> "EXITPOINT"
  | T_ENTRYPOINT           -> "ENTRYPOINT"
  | T_END                  -> "END"
  | T_ELSE                 -> "ELSE"
  | T_DOWNTO               -> "DOWNTO"
  | T_DOTDOT               -> "DOTDOT"
  | T_DOT                  -> "DOT"
  | T_DONE                 -> "DONE"
  | T_DO                   -> "DO"
  | T_CONSTRAINT           -> "CONSTRAINT"
  | T_COMMENT              -> "COMMENT"
  | T_COMMA                -> "COMMA"
  | T_COLONGREATER         -> "COLONGREATER"
  | T_COLONEQUAL           -> "COLONEQUAL"
  | T_COLONCOLON           -> "COLONCOLON"
  | T_COLON                -> "COLON"
  | T_CLASS                -> "CLASS"
  | T_CHAR                 -> "CHAR"
  | T_BEGIN                -> "BEGIN"
  | T_BARRBRACKET          -> "BARRBRACKET"
  | T_BARBAR               -> "BARBAR"
  | T_BAR                  -> "BAR"
  | T_BANG                 -> "BANG"
  | T_CUSTOM_BANG          -> "CUSTOM_BANG"
  | T_BACKQUOTE            -> "BACKQUOTE"
  | T_ASSERT               -> "ASSERT"
  | T_AS                   -> "AS"
  | T_AND                  -> "AND"
  | T_AMPERSAND            -> "AMPERSAND"
  | T_AMPERAMPER           -> "AMPERAMPER"

let string_of_nonterminal : type a. a nonterminal_class -> string = function
  | N_with_type_binder                  -> "with_type_binder"
  | N_with_extensions                   -> "with_extensions"
  | N_with_constraints                  -> "with_constraints"
  | N_with_constraint                   -> "with_constraint"
  | N_virtual_flag                      -> "virtual_flag"
  | N_value_type                        -> "value_type"
  | N_value                             -> "value"
  | N_val_longident                     -> "val_longident"
  | N_val_ident                         -> "val_ident"
  | N_typevar_list                      -> "typevar_list"
  | N_type_variance                     -> "type_variance"
  | N_type_variable                     -> "type_variable"
  | N_type_parameters                   -> "type_parameters"
  | N_type_parameter_list               -> "type_parameter_list"
  | N_type_parameter                    -> "type_parameter"
  | N_type_longident                    -> "type_longident"
  | N_type_kind                         -> "type_kind"
  | N_type_declarations                 -> "type_declarations"
  | N_type_declaration                  -> "type_declaration"
  | N_type_constraint                   -> "type_constraint"
  | N_toplevel_directives               -> "toplevel_directives"
  | N_tag_field                         -> "tag_field"
  | N_subtractive                       -> "subtractive"
  | N_structure_head                    -> "structure_head"
  | N_structure_tail                    -> "structure_tail"
  | N_structure_item                    -> "structure_item"
  | N_structure                         -> "structure"
  | N_strict_binding                    -> "strict_binding"
  | N_str_type_extension                -> "str_type_extension"
  | N_str_extension_constructors        -> "str_extension_constructors"
  | N_str_exception_declaration         -> "str_exception_declaration"
  | N_single_attr_id                    -> "single_attr_id"
  | N_simple_pattern_not_ident          -> "simple_pattern_not_ident"
  | N_simple_pattern                    -> "simple_pattern"
  | N_simple_labeled_expr_list          -> "simple_labeled_expr_list"
  | N_simple_expr                       -> "simple_expr"
  | N_simple_core_type_or_tuple_no_attr -> "simple_core_type_or_tuple_no_attr"
  | N_simple_core_type_or_tuple         -> "simple_core_type_or_tuple"
  | N_simple_core_type_no_attr          -> "simple_core_type_no_attr"
  | N_simple_core_type2                 -> "simple_core_type2"
  | N_simple_core_type                  -> "simple_core_type"
  | N_signed_constant                   -> "signed_constant"
  | N_signature_item                    -> "signature_item"
  | N_signature                         -> "signature"
  | N_sig_type_extension                -> "sig_type_extension"
  | N_sig_extension_constructors        -> "sig_extension_constructors"
  | N_sig_exception_declaration         -> "sig_exception_declaration"
  | N_seq_expr                          -> "seq_expr"
  | N_row_field_list                    -> "row_field_list"
  | N_row_field                         -> "row_field"
  | N_record_expr                       -> "record_expr"
  | N_rec_flag                          -> "rec_flag"
  | N_private_virtual_flags             -> "private_virtual_flags"
  | N_private_flag                      -> "private_flag"
  | N_primitive_declaration             -> "primitive_declaration"
  | N_post_item_attributes              -> "post_item_attributes"
  | N_post_item_attribute               -> "post_item_attribute"
  | N_poly_type                         -> "poly_type"
  | N_payload                           -> "payload"
  | N_pattern_var                       -> "pattern_var"
  | N_pattern_semi_list                 -> "pattern_semi_list"
  | N_pattern_comma_list                -> "pattern_comma_list"
  | N_pattern                           -> "pattern"
  | N_parse_expression                  -> "parse_expression"
  | N_parent_binder                     -> "parent_binder"
  | N_package_type_cstrs                -> "package_type_cstrs"
  | N_package_type_cstr                 -> "package_type_cstr"
  | N_package_type                      -> "package_type"
  | N_override_flag                     -> "override_flag"
  | N_optional_type_variable            -> "optional_type_variable"
  | N_optional_type_parameters          -> "optional_type_parameters"
  | N_optional_type_parameter_list      -> "optional_type_parameter_list"
  | N_optional_type_parameter           -> "optional_type_parameter"
  | N_option_STRING_                    -> "option_STRING_"
  | N_opt_semi                          -> "opt_semi"
  | N_opt_default                       -> "opt_default"
  | N_opt_bar                           -> "opt_bar"
  | N_opt_ampersand                     -> "opt_ampersand"
  | N_operator                          -> "operator"
  | N_open_statement                    -> "open_statement"
  | N_newtype                           -> "newtype"
  | N_name_tag_list                     -> "name_tag_list"
  | N_name_tag                          -> "name_tag"
  | N_mutable_flag                      -> "mutable_flag"
  | N_mty_longident                     -> "mty_longident"
  | N_module_type                       -> "module_type"
  | N_module_rec_declarations           -> "module_rec_declarations"
  | N_module_rec_declaration            -> "module_rec_declaration"
  | N_module_expr                       -> "module_expr"
  | N_module_declaration                -> "module_declaration"
  | N_module_bindings                   -> "module_bindings"
  | N_module_binding_body               -> "module_binding_body"
  | N_module_binding                    -> "module_binding"
  | N_mod_longident                     -> "mod_longident"
  | N_mod_ext_longident                 -> "mod_ext_longident"
  | N_method_                           -> "method_"
  | N_meth_list                         -> "meth_list"
  | N_match_cases                       -> "match_cases"
  | N_match_case                        -> "match_case"
  | N_lident_list                       -> "lident_list"
  | N_let_pattern                       -> "let_pattern"
  | N_let_bindings                      -> "let_bindings"
  | N_let_binding_                      -> "let_binding_"
  | N_let_binding                       -> "let_binding"
  | N_let_bindings_no_attrs             -> "let_bindings_no_attrs"
  | N_lbl_pattern_list                  -> "lbl_pattern_list"
  | N_lbl_pattern                       -> "lbl_pattern"
  | N_lbl_expr_list                     -> "lbl_expr_list"
  | N_lbl_expr                          -> "lbl_expr"
  | N_labeled_simple_pattern            -> "labeled_simple_pattern"
  | N_labeled_simple_expr               -> "labeled_simple_expr"
  | N_label_var                         -> "label_var"
  | N_label_longident                   -> "label_longident"
  | N_label_let_pattern                 -> "label_let_pattern"
  | N_label_ident                       -> "label_ident"
  | N_label_expr                        -> "label_expr"
  | N_label_declarations                -> "label_declarations"
  | N_label_declaration                 -> "label_declaration"
  | N_label                             -> "label"
  | N_item_extension                    -> "item_extension"
  | N_interface                         -> "interface"
  | N_implementation                    -> "implementation"
  | N_ident                             -> "ident"
  | N_generalized_constructor_arguments -> "generalized_constructor_arguments"
  | N_functor_args                      -> "functor_args"
  | N_functor_arg_name                  -> "functor_arg_name"
  | N_functor_arg                       -> "functor_arg"
  | N_fun_def                           -> "fun_def"
  | N_fun_binding                       -> "fun_binding"
  | N_floating_attribute                -> "floating_attribute"
  | N_field_expr_list                   -> "field_expr_list"
  | N_field                             -> "field"
  | N_extension_constructor_rebind      -> "extension_constructor_rebind"
  | N_extension_constructor_declaration -> "extension_constructor_declaration"
  | N_extension                         -> "extension"
  | N_ext_attributes                    -> "ext_attributes"
  | N_expr_semi_list                    -> "expr_semi_list"
  | N_expr_open                         -> "expr_open"
  | N_expr_comma_list                   -> "expr_comma_list"
  | N_expr_comma_opt_list               -> "expr_comma_opt_list"
  | N_expr                              -> "expr"
  | N_dummy                             -> "dummy"
  | N_direction_flag                    -> "direction_flag"
  | N_core_type_list_no_attr            -> "core_type_list_no_attr"
  | N_core_type_list                    -> "core_type_list"
  | N_core_type_comma_list              -> "core_type_comma_list"
  | N_core_type2                        -> "core_type2"
  | N_core_type                         -> "core_type"
  | N_constructor_declarations          -> "constructor_declarations"
  | N_constructor_declaration           -> "constructor_declaration"
  | N_constraints                       -> "constraints"
  | N_constrain_field                   -> "constrain_field"
  | N_constrain                         -> "constrain"
  | N_constr_longident                  -> "constr_longident"
  | N_constr_ident                      -> "constr_ident"
  | N_constant                          -> "constant"
  | N_clty_longident                    -> "clty_longident"
  | N_class_type_parameters             -> "class_type_parameters"
  | N_class_type_declarations           -> "class_type_declarations"
  | N_class_type_declaration            -> "class_type_declaration"
  | N_class_type                        -> "class_type"
  | N_class_structure                   -> "class_structure"
  | N_class_simple_expr                 -> "class_simple_expr"
  | N_class_signature                   -> "class_signature"
  | N_class_sig_fields                  -> "class_sig_fields"
  | N_class_sig_field                   -> "class_sig_field"
  | N_class_sig_body                    -> "class_sig_body"
  | N_class_self_type                   -> "class_self_type"
  | N_class_self_pattern                -> "class_self_pattern"
  | N_class_longident                   -> "class_longident"
  | N_class_fun_def                     -> "class_fun_def"
  | N_class_fun_binding                 -> "class_fun_binding"
  | N_class_fields                      -> "class_fields"
  | N_class_field                       -> "class_field"
  | N_class_expr                        -> "class_expr"
  | N_class_descriptions                -> "class_descriptions"
  | N_class_description                 -> "class_description"
  | N_class_declarations                -> "class_declarations"
  | N_class_declaration                 -> "class_declaration"
  | N_attributes                        -> "attributes"
  | N_attribute                         -> "attribute"
  | N_attr_id                           -> "attr_id"
  | N_amper_type_list                   -> "amper_type_list"
  | N_additive                          -> "additive"

let string_of_class = function
  | CT_ (t,_) -> string_of_token t
  | CN_ (n,_) -> string_of_nonterminal n

let symbol_of_token = function
  | WITH                         -> T_ (T_WITH, ())
  | WHILE_LWT                    -> T_ (T_WHILE_LWT, ())
  | WHILE                        -> T_ (T_WHILE, ())
  | WHEN                         -> T_ (T_WHEN, ())
  | VIRTUAL                      -> T_ (T_VIRTUAL, ())
  | VAL                          -> T_ (T_VAL, ())
  | UNDERSCORE                   -> T_ (T_UNDERSCORE, ())
  | UIDENT v                     -> T_ (T_UIDENT, v)
  | TYPE                         -> T_ (T_TYPE, ())
  | TRY_LWT                      -> T_ (T_TRY_LWT, ())
  | TRY                          -> T_ (T_TRY, ())
  | TRUE                         -> T_ (T_TRUE, ())
  | TO                           -> T_ (T_TO, ())
  | TILDE                        -> T_ (T_TILDE, ())
  | THEN                         -> T_ (T_THEN, ())
  | STRUCT                       -> T_ (T_STRUCT, ())
  | STRING v                     -> T_ (T_STRING, v)
  | STAR                         -> T_ (T_STAR, ())
  | SIG                          -> T_ (T_SIG, ())
  | SHARP                        -> T_ (T_SHARP, ())
  | SEMISEMI                     -> T_ (T_SEMISEMI, ())
  | SEMI                         -> T_ (T_SEMI, ())
  | RPAREN                       -> T_ (T_RPAREN, ())
  | REC                          -> T_ (T_REC, ())
  | RBRACKET                     -> T_ (T_RBRACKET, ())
  | RBRACE                       -> T_ (T_RBRACE, ())
  | QUOTE                        -> T_ (T_QUOTE, ())
  | QUESTION                     -> T_ (T_QUESTION, ())
  | PRIVATE                      -> T_ (T_PRIVATE, ())
  | PREFIXOP v                   -> T_ (T_PREFIXOP, v)
  | PLUSEQ                       -> T_ (T_PLUSEQ, ())
  | PLUSDOT                      -> T_ (T_PLUSDOT, ())
  | PLUS                         -> T_ (T_PLUS, ())
  | PERCENT                      -> T_ (T_PERCENT, ())
  | P4_QUOTATION                 -> T_ (T_P4_QUOTATION, ())
  | OUNIT_TEST_UNIT              -> T_ (T_OUNIT_TEST_UNIT, ())
  | OUNIT_TEST_MODULE            -> T_ (T_OUNIT_TEST_MODULE, ())
  | OUNIT_TEST                   -> T_ (T_OUNIT_TEST, ())
  | OUNIT_BENCH_MODULE           -> T_ (T_OUNIT_BENCH_MODULE, ())
  | OUNIT_BENCH_INDEXED          -> T_ (T_OUNIT_BENCH_INDEXED, ())
  | OUNIT_BENCH_FUN              -> T_ (T_OUNIT_BENCH_FUN, ())
  | OUNIT_BENCH                  -> T_ (T_OUNIT_BENCH, ())
  | OR                           -> T_ (T_OR, ())
  | OPTLABEL v                   -> T_ (T_OPTLABEL, v)
  | OPEN                         -> T_ (T_OPEN, ())
  | OF                           -> T_ (T_OF, ())
  | OBJECT                       -> T_ (T_OBJECT, ())
  | NONREC                       -> T_ (T_NONREC, ())
  | NEW                          -> T_ (T_NEW, ())
  | NATIVEINT v                  -> T_ (T_NATIVEINT, v)
  | MUTABLE                      -> T_ (T_MUTABLE, ())
  | MODULE                       -> T_ (T_MODULE, ())
  | MINUSGREATER                 -> T_ (T_MINUSGREATER, ())
  | MINUSDOT                     -> T_ (T_MINUSDOT, ())
  | MINUS                        -> T_ (T_MINUS, ())
  | METHOD                       -> T_ (T_METHOD, ())
  | MATCH_LWT                    -> T_ (T_MATCH_LWT, ())
  | MATCH                        -> T_ (T_MATCH, ())
  | LPAREN                       -> T_ (T_LPAREN, ())
  | LIDENT v                     -> T_ (T_LIDENT, v)
  | LET_LWT                      -> T_ (T_LET_LWT, ())
  | LET                          -> T_ (T_LET, ())
  | LESSMINUS                    -> T_ (T_LESSMINUS, ())
  | LESS                         -> T_ (T_LESS, ())
  | LBRACKETPERCENTPERCENT       -> T_ (T_LBRACKETPERCENTPERCENT, ())
  | LBRACKETPERCENT              -> T_ (T_LBRACKETPERCENT, ())
  | LBRACKETLESS                 -> T_ (T_LBRACKETLESS, ())
  | LBRACKETGREATER              -> T_ (T_LBRACKETGREATER, ())
  | LBRACKETBAR                  -> T_ (T_LBRACKETBAR, ())
  | LBRACKETATATAT               -> T_ (T_LBRACKETATATAT, ())
  | LBRACKETATAT                 -> T_ (T_LBRACKETATAT, ())
  | LBRACKETAT                   -> T_ (T_LBRACKETAT, ())
  | LBRACKET                     -> T_ (T_LBRACKET, ())
  | LBRACELESS                   -> T_ (T_LBRACELESS, ())
  | LBRACE                       -> T_ (T_LBRACE, ())
  | LAZY                         -> T_ (T_LAZY, ())
  | LABEL v                      -> T_ (T_LABEL, v)
  | JSNEW                        -> T_ (T_JSNEW, ())
  | INT64 v                      -> T_ (T_INT64, v)
  | INT32 v                      -> T_ (T_INT32, v)
  | INT v                        -> T_ (T_INT, v)
  | INITIALIZER                  -> T_ (T_INITIALIZER, ())
  | INHERIT                      -> T_ (T_INHERIT, ())
  | INFIXOP4 v                   -> T_ (T_INFIXOP4, v)
  | INFIXOP3 v                   -> T_ (T_INFIXOP3, v)
  | INFIXOP2 v                   -> T_ (T_INFIXOP2, v)
  | INFIXOP1 v                   -> T_ (T_INFIXOP1, v)
  | INFIXOP0 v                   -> T_ (T_INFIXOP0, v)
  | INCLUDE                      -> T_ (T_INCLUDE, ())
  | IN                           -> T_ (T_IN, ())
  | IF                           -> T_ (T_IF, ())
  | GREATERRBRACKET              -> T_ (T_GREATERRBRACKET, ())
  | GREATERRBRACE                -> T_ (T_GREATERRBRACE, ())
  | GREATER                      -> T_ (T_GREATER, ())
  | FUNCTOR                      -> T_ (T_FUNCTOR, ())
  | FUNCTION                     -> T_ (T_FUNCTION, ())
  | FUN                          -> T_ (T_FUN, ())
  | FOR_LWT                      -> T_ (T_FOR_LWT, ())
  | FOR                          -> T_ (T_FOR, ())
  | FLOAT v                      -> T_ (T_FLOAT, v)
  | FINALLY_LWT                  -> T_ (T_FINALLY_LWT, ())
  | FALSE                        -> T_ (T_FALSE, ())
  | EXTERNAL                     -> T_ (T_EXTERNAL, ())
  | EXCEPTION                    -> T_ (T_EXCEPTION, ())
  | EQUAL                        -> T_ (T_EQUAL, ())
  | EOL                          -> T_ (T_EOL, ())
  | EOF                          -> T_ (T_EOF, ())
  | EXITPOINT                    -> T_ (T_EXITPOINT, ())
  | ENTRYPOINT                   -> T_ (T_ENTRYPOINT, ())
  | END                          -> T_ (T_END, ())
  | ELSE                         -> T_ (T_ELSE, ())
  | DOWNTO                       -> T_ (T_DOWNTO, ())
  | DOTDOT                       -> T_ (T_DOTDOT, ())
  | DOT                          -> T_ (T_DOT, ())
  | DONE                         -> T_ (T_DONE, ())
  | DO                           -> T_ (T_DO, ())
  | CONSTRAINT                   -> T_ (T_CONSTRAINT, ())
  | COMMENT v                    -> T_ (T_COMMENT, v)
  | COMMA                        -> T_ (T_COMMA, ())
  | COLONGREATER                 -> T_ (T_COLONGREATER, ())
  | COLONEQUAL                   -> T_ (T_COLONEQUAL, ())
  | COLONCOLON                   -> T_ (T_COLONCOLON, ())
  | COLON                        -> T_ (T_COLON, ())
  | CLASS                        -> T_ (T_CLASS, ())
  | CHAR v                       -> T_ (T_CHAR, v)
  | BEGIN                        -> T_ (T_BEGIN, ())
  | BARRBRACKET                  -> T_ (T_BARRBRACKET, ())
  | BARBAR                       -> T_ (T_BARBAR, ())
  | BAR                          -> T_ (T_BAR, ())
  | BANG                         -> T_ (T_BANG, ())
  | CUSTOM_BANG                  -> T_ (T_CUSTOM_BANG, ())
  | BACKQUOTE                    -> T_ (T_BACKQUOTE, ())
  | ASSERT                       -> T_ (T_ASSERT, ())
  | AS                           -> T_ (T_AS, ())
  | AND                          -> T_ (T_AND, ())
  | AMPERSAND                    -> T_ (T_AMPERSAND, ())
  | AMPERAMPER                   -> T_ (T_AMPERAMPER, ())

let token_of_symbol (type a) (t : a token_class) (v : a) =
  match t with
  | T_WITH                   -> WITH
  | T_WHILE_LWT              -> WHILE_LWT
  | T_WHILE                  -> WHILE
  | T_WHEN                   -> WHEN
  | T_VIRTUAL                -> VIRTUAL
  | T_VAL                    -> VAL
  | T_UNDERSCORE             -> UNDERSCORE
  | T_UIDENT                 -> UIDENT v
  | T_TYPE                   -> TYPE
  | T_TRY_LWT                -> TRY_LWT
  | T_TRY                    -> TRY
  | T_TRUE                   -> TRUE
  | T_TO                     -> TO
  | T_TILDE                  -> TILDE
  | T_THEN                   -> THEN
  | T_STRUCT                 -> STRUCT
  | T_STRING                 -> STRING v
  | T_STAR                   -> STAR
  | T_SIG                    -> SIG
  | T_SHARP                  -> SHARP
  | T_SEMISEMI               -> SEMISEMI
  | T_SEMI                   -> SEMI
  | T_RPAREN                 -> RPAREN
  | T_REC                    -> REC
  | T_RBRACKET               -> RBRACKET
  | T_RBRACE                 -> RBRACE
  | T_QUOTE                  -> QUOTE
  | T_QUESTION               -> QUESTION
  | T_PRIVATE                -> PRIVATE
  | T_PREFIXOP               -> PREFIXOP v
  | T_PLUSEQ                 -> PLUSEQ
  | T_PLUSDOT                -> PLUSDOT
  | T_PLUS                   -> PLUS
  | T_PERCENT                -> PERCENT
  | T_P4_QUOTATION           -> P4_QUOTATION
  | T_OUNIT_TEST_UNIT        -> OUNIT_TEST_UNIT
  | T_OUNIT_TEST_MODULE      -> OUNIT_TEST_MODULE
  | T_OUNIT_TEST             -> OUNIT_TEST
  | T_OUNIT_BENCH_MODULE     -> OUNIT_BENCH_MODULE
  | T_OUNIT_BENCH_INDEXED    -> OUNIT_BENCH_INDEXED
  | T_OUNIT_BENCH_FUN        -> OUNIT_BENCH_FUN
  | T_OUNIT_BENCH            -> OUNIT_BENCH
  | T_OR                     -> OR
  | T_OPTLABEL               -> OPTLABEL v
  | T_OPEN                   -> OPEN
  | T_OF                     -> OF
  | T_OBJECT                 -> OBJECT
  | T_NONREC                 -> NONREC
  | T_NEW                    -> NEW
  | T_NATIVEINT              -> NATIVEINT v
  | T_MUTABLE                -> MUTABLE
  | T_MODULE                 -> MODULE
  | T_MINUSGREATER           -> MINUSGREATER
  | T_MINUSDOT               -> MINUSDOT
  | T_MINUS                  -> MINUS
  | T_METHOD                 -> METHOD
  | T_MATCH_LWT              -> MATCH_LWT
  | T_MATCH                  -> MATCH
  | T_LPAREN                 -> LPAREN
  | T_LIDENT                 -> LIDENT v
  | T_LET_LWT                -> LET_LWT
  | T_LET                    -> LET
  | T_LESSMINUS              -> LESSMINUS
  | T_LESS                   -> LESS
  | T_LBRACKETPERCENTPERCENT -> LBRACKETPERCENTPERCENT
  | T_LBRACKETPERCENT        -> LBRACKETPERCENT
  | T_LBRACKETLESS           -> LBRACKETLESS
  | T_LBRACKETGREATER        -> LBRACKETGREATER
  | T_LBRACKETBAR            -> LBRACKETBAR
  | T_LBRACKETATATAT         -> LBRACKETATATAT
  | T_LBRACKETATAT           -> LBRACKETATAT
  | T_LBRACKETAT             -> LBRACKETAT
  | T_LBRACKET               -> LBRACKET
  | T_LBRACELESS             -> LBRACELESS
  | T_LBRACE                 -> LBRACE
  | T_LAZY                   -> LAZY
  | T_LABEL                  -> LABEL v
  | T_JSNEW                  -> JSNEW
  | T_INT64                  -> INT64 v
  | T_INT32                  -> INT32 v
  | T_INT                    -> INT v
  | T_INITIALIZER            -> INITIALIZER
  | T_INHERIT                -> INHERIT
  | T_INFIXOP4               -> INFIXOP4 v
  | T_INFIXOP3               -> INFIXOP3 v
  | T_INFIXOP2               -> INFIXOP2 v
  | T_INFIXOP1               -> INFIXOP1 v
  | T_INFIXOP0               -> INFIXOP0 v
  | T_INCLUDE                -> INCLUDE
  | T_IN                     -> IN
  | T_IF                     -> IF
  | T_GREATERRBRACKET        -> GREATERRBRACKET
  | T_GREATERRBRACE          -> GREATERRBRACE
  | T_GREATER                -> GREATER
  | T_FUNCTOR                -> FUNCTOR
  | T_FUNCTION               -> FUNCTION
  | T_FUN                    -> FUN
  | T_FOR_LWT                -> FOR_LWT
  | T_FOR                    -> FOR
  | T_FLOAT                  -> FLOAT v
  | T_FINALLY_LWT            -> FINALLY_LWT
  | T_FALSE                  -> FALSE
  | T_EXTERNAL               -> EXTERNAL
  | T_EXCEPTION              -> EXCEPTION
  | T_EQUAL                  -> EQUAL
  | T_EOL                    -> EOL
  | T_EOF                    -> EOF
  | T_EXITPOINT              -> EXITPOINT
  | T_ENTRYPOINT             -> ENTRYPOINT
  | T_END                    -> END
  | T_ELSE                   -> ELSE
  | T_DOWNTO                 -> DOWNTO
  | T_DOTDOT                 -> DOTDOT
  | T_DOT                    -> DOT
  | T_DONE                   -> DONE
  | T_DO                     -> DO
  | T_CONSTRAINT             -> CONSTRAINT
  | T_COMMENT                -> COMMENT v
  | T_COMMA                  -> COMMA
  | T_COLONGREATER           -> COLONGREATER
  | T_COLONEQUAL             -> COLONEQUAL
  | T_COLONCOLON             -> COLONCOLON
  | T_COLON                  -> COLON
  | T_CLASS                  -> CLASS
  | T_CHAR                   -> CHAR v
  | T_BEGIN                  -> BEGIN
  | T_BARRBRACKET            -> BARRBRACKET
  | T_BARBAR                 -> BARBAR
  | T_BAR                    -> BAR
  | T_BANG                   -> BANG
  | T_CUSTOM_BANG            -> CUSTOM_BANG
  | T_BACKQUOTE              -> BACKQUOTE
  | T_ASSERT                 -> ASSERT
  | T_AS                     -> AS
  | T_AND                    -> AND
  | T_AMPERSAND              -> AMPERSAND
  | T_AMPERAMPER             -> AMPERAMPER

let default_token (type a) (t : a token_class) : int * a =
  match t with
  | T_WITH                    -> 0, ()
  | T_WHILE_LWT               -> 0, ()
  | T_WHILE                   -> 0, ()
  | T_WHEN                    -> 0, ()
  | T_VIRTUAL                 -> 0, ()
  | T_VAL                     -> 0, ()
  | T_UNDERSCORE              -> 0, ()
  | T_TYPE                    -> 0, ()
  | T_TRY_LWT                 -> 0, ()
  | T_TRY                     -> 0, ()
  | T_TRUE                    -> 0, ()
  | T_TO                      -> 0, ()
  | T_TILDE                   -> 0, ()
  | T_THEN                    -> 0, ()
  | T_STRUCT                  -> 0, ()
  | T_STAR                    -> 0, ()
  | T_SIG                     -> 0, ()
  | T_SHARP                   -> 0, ()
  | T_SEMISEMI                -> 0, ()
  | T_SEMI                    -> 0, ()
  | T_RPAREN                  -> 0, ()
  | T_REC                     -> 0, ()
  | T_RBRACKET                -> 0, ()
  | T_RBRACE                  -> 0, ()
  | T_QUOTE                   -> 0, ()
  | T_QUESTION                -> 0, ()
  | T_PRIVATE                 -> 0, ()
  | T_PLUSEQ                  -> 0, ()
  | T_PLUSDOT                 -> 0, ()
  | T_PLUS                    -> 0, ()
  | T_PERCENT                 -> 0, ()
  | T_P4_QUOTATION            -> 0, ()
  | T_OR                      -> 0, ()
  | T_OPEN                    -> 0, ()
  | T_OF                      -> 0, ()
  | T_OBJECT                  -> 0, ()
  | T_NONREC                  -> 0, ()
  | T_NEW                     -> 0, ()
  | T_MUTABLE                 -> 0, ()
  | T_MODULE                  -> 0, ()
  | T_MINUSGREATER            -> 0, ()
  | T_MINUSDOT                -> 0, ()
  | T_MINUS                   -> 0, ()
  | T_METHOD                  -> 0, ()
  | T_MATCH_LWT               -> 0, ()
  | T_MATCH                   -> 0, ()
  | T_LPAREN                  -> 0, ()
  | T_LET_LWT                 -> 0, ()
  | T_LET                     -> 0, ()
  | T_LESSMINUS               -> 0, ()
  | T_LESS                    -> 0, ()
  | T_LBRACKETPERCENT         -> 0, ()
  | T_LBRACKETLESS            -> 0, ()
  | T_LBRACKETGREATER         -> 0, ()
  | T_LBRACKETBAR             -> 0, ()
  | T_LBRACKETATATAT          -> 0, ()
  | T_LBRACKETATAT            -> 0, ()
  | T_LBRACKETAT              -> 0, ()
  | T_LBRACKET                -> 0, ()
  | T_LBRACELESS              -> 0, ()
  | T_LBRACE                  -> 0, ()
  | T_LAZY                    -> 0, ()
  | T_JSNEW                   -> 0, ()
  | T_INITIALIZER             -> 0, ()
  | T_INHERIT                 -> 0, ()
  | T_INCLUDE                 -> 0, ()
  | T_IN                      -> 0, ()
  | T_IF                      -> 0, ()
  | T_GREATERRBRACKET         -> 0, ()
  | T_GREATERRBRACE           -> 0, ()
  | T_GREATER                 -> 0, ()
  | T_FUNCTOR                 -> 0, ()
  | T_FUNCTION                -> 0, ()
  | T_FUN                     -> 0, ()
  | T_FOR_LWT                 -> 0, ()
  | T_FOR                     -> 0, ()
  | T_FINALLY_LWT             -> 0, ()
  | T_FALSE                   -> 0, ()
  | T_EXTERNAL                -> 0, ()
  | T_EXCEPTION               -> 0, ()
  | T_EQUAL                   -> 0, ()
  | T_EOL                     -> 0, ()
  | T_EOF                     -> 0, ()
  | T_EXITPOINT               -> 10, ()
  | T_ENTRYPOINT              -> 0, ()
  | T_END                     -> 0, ()
  | T_ELSE                    -> 0, ()
  | T_DOWNTO                  -> 0, ()
  | T_DOTDOT                  -> 0, ()
  | T_DOT                     -> 0, ()
  | T_DONE                    -> 0, ()
  | T_DO                      -> 0, ()
  | T_CONSTRAINT              -> 0, ()
  | T_COMMA                   -> 0, ()
  | T_COLONGREATER            -> 0, ()
  | T_COLONEQUAL              -> 0, ()
  | T_COLONCOLON              -> 0, ()
  | T_COLON                   -> 0, ()
  | T_CLASS                   -> 0, ()
  | T_BEGIN                   -> 0, ()
  | T_BARRBRACKET             -> 0, ()
  | T_BARBAR                  -> 0, ()
  | T_BAR                     -> 0, ()
  | T_BANG                    -> 0, ()
  | T_CUSTOM_BANG             -> 0, ()
  | T_BACKQUOTE               -> 0, ()
  | T_ASSERT                  -> 0, ()
  | T_AS                      -> 0, ()
  | T_AND                     -> 0, ()
  | T_AMPERSAND               -> 0, ()
  | T_AMPERAMPER              -> 0, ()
  | T_OUNIT_TEST_UNIT         -> 0, ()
  | T_OUNIT_TEST_MODULE       -> 0, ()
  | T_OUNIT_TEST              -> 0, ()
  | T_OUNIT_BENCH_MODULE      -> 0, ()
  | T_OUNIT_BENCH_INDEXED     -> 0, ()
  | T_OUNIT_BENCH_FUN         -> 0, ()
  | T_OUNIT_BENCH             -> 0, ()
  | T_LBRACKETPERCENTPERCENT  -> 0, ()

  | T_UIDENT    -> 2, "_"
  | T_STRING    -> 2, ("", None)
  | T_PREFIXOP  -> 2, "!"
  | T_OPTLABEL  -> 2, "_"
  | T_NATIVEINT -> 1, 0n
  | T_LIDENT    -> 2, "_"
  | T_LABEL     -> 2, "_"
  | T_INT64     -> 1, 0L
  | T_INT32     -> 1, 0l
  | T_INT       -> 1, 0
  | T_INFIXOP4  -> 2, "_"
  | T_INFIXOP3  -> 2, "_"
  | T_INFIXOP2  -> 2, "_"
  | T_INFIXOP1  -> 2, "_"
  | T_INFIXOP0  -> 2, "_"
  | T_FLOAT     -> 2, "0."
  | T_COMMENT   -> 2, ("", Location.none)
  | T_CHAR      -> 2, '_'

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

let default_nonterminal (type a) (n : a nonterminal_class) : int * a =
  match n with
  | N_with_type_binder                  -> 1, Asttypes.Public
  | N_with_extensions                   -> 1, []
  | N_with_constraints                  -> 0, []
  | N_with_constraint                   -> 0, []
  | N_virtual_flag                      -> 1, Asttypes.Concrete
  | N_value_type                        ->
    raise Not_found (*(string * Asttypes.mutable_flag * Asttypes.virtual_flag * Parsetree.core_type) nonterminal_class*)
  | N_value                             ->
    raise Not_found (*(string Asttypes.loc * Asttypes.mutable_flag * Parsetree.class_field_kind) nonterminal_class*)
  | N_val_longident                     -> 1, default_longident
  | N_val_ident                         -> 1, "_"
  | N_typevar_list                      -> 0, []
  | N_type_variance                     -> 1, Asttypes.Invariant
  | N_type_variable                     -> 1, default_type
  | N_type_parameters                   -> 0, []
  | N_type_parameter_list               -> 0, []
  | N_type_parameter                    -> 1, (default_type, Asttypes.Invariant)
  | N_type_longident                    ->
    raise Not_found (*(Longident.t) nonterminal_class*)
  | N_type_kind                         ->
    raise Not_found (*(Parsetree.type_kind * Asttypes.private_flag * Parsetree.core_type option) nonterminal_class*)
  | N_type_declarations                 -> 0, []
  | N_type_declaration                  ->
    raise Not_found (*(Parsetree.type_declaration) nonterminal_class*)
  | N_type_constraint                   -> 1, (None, None)
  | N_toplevel_directives               -> raise Not_found
  | N_tag_field                         ->
    raise Not_found (*(Parsetree.row_field) nonterminal_class*)
  | N_subtractive                       -> 1, "-"
  | N_structure_head                    -> 0, []
  | N_structure_tail                    -> 0, []
  | N_structure_item                    -> 0, []
  | N_structure                         -> 0, []
  | N_strict_binding                    -> 1, default_expr
  | N_str_type_extension                ->
    raise Not_found (*(Parsetree.type_extension) nonterminal_class*)
  | N_str_extension_constructors        -> 0, []
  | N_str_exception_declaration         ->
    raise Not_found (*(Parsetree.extension_constructor) nonterminal_class*)
  | N_single_attr_id                    -> 1, ""
  | N_simple_pattern_not_ident          -> 0, default_pattern
  | N_simple_pattern                    -> 0, default_pattern
  | N_simple_labeled_expr_list          -> 0, []
  | N_simple_expr                       -> 1, default_expr
  | N_simple_core_type_or_tuple_no_attr -> 1, default_type
  | N_simple_core_type_or_tuple         -> 1, default_type
  | N_simple_core_type_no_attr          -> 1, default_type
  | N_simple_core_type2                 -> 1, default_type
  | N_simple_core_type                  -> 1, default_type
  | N_signed_constant                   -> 1, Asttypes.Const_int 0
  | N_signature_item                    -> 0, []
  | N_signature                         -> 0, []
  | N_sig_type_extension                ->
    raise Not_found (*(Parsetree.type_extension) nonterminal_class*)
  | N_sig_extension_constructors        -> 0, []
  | N_sig_exception_declaration         ->
    raise Not_found (*(Parsetree.extension_constructor) nonterminal_class*)
  | N_seq_expr                          -> 1, default_expr
  | N_row_field_list                    -> 0, []
  | N_row_field                         ->
    raise Not_found (*(Parsetree.row_field) nonterminal_class*)
  | N_record_expr                       -> 0, (None, [])
  | N_rec_flag                          -> 1, Asttypes.Nonrecursive
  | N_private_virtual_flags             ->
    raise Not_found (*(Asttypes.private_flag * Asttypes.virtual_flag) nonterminal_class*)
  | N_private_flag                      -> 1, Asttypes.Public
  | N_primitive_declaration             -> 0, []
  | N_post_item_attributes              -> 0, []
  | N_post_item_attribute               -> 1, (Location.mknoloc "", default_payload)
  | N_poly_type                         -> 1, default_type
  | N_payload                           -> 1, default_payload
  | N_pattern_var                       -> 0, default_pattern
  | N_pattern_semi_list                 -> 0, []
  | N_pattern_comma_list                -> 0, []
  | N_pattern                           -> 0, default_pattern
  | N_parse_expression                  -> 1, default_expr
  | N_parent_binder                     -> 1, None
  | N_package_type_cstrs                -> 0, []
  | N_package_type_cstr                 -> 1, (default_longident_loc, default_type)
  | N_package_type                      -> 1, (default_longident_loc, [])
  | N_override_flag                     -> 1, Asttypes.Fresh
  | N_optional_type_variable            -> 1, default_type
  | N_optional_type_parameters          -> 0, []
  | N_optional_type_parameter_list      -> 0, []
  | N_optional_type_parameter           -> 1, (default_type, Asttypes.Invariant)
  | N_option_STRING_                    -> 0, None
  | N_opt_semi                          -> 0, ()
  | N_opt_default                       -> 1, None
  | N_opt_bar                           -> 0, ()
  | N_opt_ampersand                     -> 1, false
  | N_operator                          -> 1, "_"
  | N_open_statement                    ->
    raise Not_found (*(Parsetree.open_description) nonterminal_class*)
  | N_newtype                           -> 1, "_"
  | N_name_tag_list                     -> 0, []
  | N_name_tag                          -> 1, ""
  | N_mutable_flag                      -> 1, Asttypes.Immutable
  | N_mty_longident                     -> 2, default_longident
  | N_module_type                       -> 1, default_module_type
  | N_module_rec_declarations           -> 0, []
  | N_module_rec_declaration            -> 1, default_module_decl
  | N_module_expr                       -> 1, default_module_expr
  | N_module_declaration                -> 1, default_module_type
  | N_module_bindings                   -> 0, []
  | N_module_binding_body               -> 1, default_module_expr
  | N_module_binding                    -> 1, default_module_bind
  | N_mod_longident                     -> 2, default_longident
  | N_mod_ext_longident                 -> 2, default_longident
  | N_method_                           ->
    raise Not_found (*(string Asttypes.loc * Asttypes.private_flag * Parsetree.class_field_kind) nonterminal_class*)
  | N_meth_list                         ->
    raise Not_found (*((string * Parsetree.attributes * Parsetree.core_type) list * Asttypes.closed_flag) nonterminal_class*)
  | N_match_cases                       -> 0, []
  | N_match_case                        ->
    1, Ast_helper.Exp.case default_pattern default_expr
  | N_lident_list                       -> 0, []
  | N_let_pattern                       -> 0, default_pattern
  | N_let_bindings                      -> 0, []
  | N_let_bindings_no_attrs             -> 0, []
  | N_let_binding_                      -> 1, (default_pattern, default_expr)
  | N_let_binding                       ->
    1, Ast_helper.Vb.mk default_pattern default_expr
  | N_lbl_pattern_list                  -> 1, ([], Asttypes.Closed)
  | N_lbl_pattern                       -> 2, (default_longident_loc, default_pattern)
  | N_lbl_expr_list                     -> 0, []
  | N_lbl_expr                          -> 2, (default_longident_loc, default_expr)
  | N_labeled_simple_pattern            -> 1, ("", None, default_pattern)
  | N_labeled_simple_expr               -> 1, ("", default_expr)
  | N_label_var                         -> 1, ("", default_pattern)
  | N_label_longident                   -> 2, default_longident
  | N_label_let_pattern                 -> 1, ("", default_pattern)
  | N_label_ident                       -> 2, ("", default_expr)
  | N_label_expr                        -> 1, ("", default_expr)
  | N_label_declarations                -> 0, []
  | N_label_declaration                 ->
    raise Not_found (*(Parsetree.label_declaration) nonterminal_class*)
  | N_label                             -> 1, ""
  | N_item_extension                    ->
    raise Not_found (*(Parsetree.extension) nonterminal_class*)
  | N_interface                         -> 0, []
  | N_implementation                    -> 0, []
  | N_ident                             -> 2, ""
  | N_generalized_constructor_arguments -> 1, ([], None)
  | N_functor_args                      -> 0, []
  | N_functor_arg_name                  -> 1, ""
  | N_functor_arg                       -> 1, (Location.mknoloc "", None)
  | N_fun_def                           -> 1, default_expr
  | N_fun_binding                       -> 1, default_expr
  | N_floating_attribute                -> 1, default_attribute
  | N_field_expr_list                   -> 0, []
  | N_field                             ->
    raise Not_found (*(string * Parsetree.attributes * Parsetree.core_type) nonterminal_class*)
  | N_extension_constructor_rebind      ->
    raise Not_found (*(Parsetree.extension_constructor) nonterminal_class*)
  | N_extension_constructor_declaration ->
    raise Not_found (*(Parsetree.extension_constructor) nonterminal_class*)
  | N_extension                         ->
    raise Not_found (*(Parsetree.extension) nonterminal_class*)
  | N_ext_attributes                    -> 1, (None, [])
  | N_expr_semi_list                    -> 0, []
  | N_expr_open                         ->
    raise Not_found (*(Asttypes.override_flag * Longident.t Asttypes.loc * (string Asttypes.loc option * Parsetree.attributes)) nonterminal_class*)
  | N_expr_comma_list                   -> 0, []
  | N_expr_comma_opt_list               -> 0, []
  | N_expr                              -> 1, default_expr
  | N_dummy                             -> 0, ()
  | N_direction_flag                    -> 0, Asttypes.Upto
  | N_core_type_list_no_attr            -> 0, []
  | N_core_type_list                    -> 0, []
  | N_core_type_comma_list              -> 0, []
  | N_core_type2                        -> 1, default_type
  | N_core_type                         -> 1, default_type
  | N_constructor_declarations          -> 0, []
  | N_constructor_declaration           ->
    raise Not_found (*(Parsetree.constructor_declaration) nonterminal_class*)
  | N_constraints                       -> 0, []
  | N_constrain_field                   -> 1, (default_type, default_type)
  | N_constrain                         -> 1, (default_type, default_type, Location.none)
  | N_constr_longident                  -> 2, default_longident
  | N_constr_ident                      -> 1, ""
  | N_constant                          -> 1, Asttypes.Const_int 0
  | N_clty_longident                    -> 2, default_longident
  | N_class_type_parameters             -> 0, []
  | N_class_type_declarations           -> 0, []
  | N_class_type_declaration            -> 0, []
  | N_class_type                        ->
    raise Not_found (*(Parsetree.class_type) nonterminal_class*)
  | N_class_structure                   ->
    raise Not_found (*(Parsetree.class_structure) nonterminal_class*)
  | N_class_simple_expr                 ->
    raise Not_found (*(Parsetree.class_expr) nonterminal_class*)
  | N_class_signature                   ->
    raise Not_found (*(Parsetree.class_type) nonterminal_class*)
  | N_class_sig_fields                  -> 0, []
  | N_class_sig_field                   ->
    raise Not_found (*(Parsetree.class_type_field) nonterminal_class*)
  | N_class_sig_body                    ->
    raise Not_found (*(Parsetree.class_signature) nonterminal_class*)
  | N_class_self_type                   ->
    raise Not_found (*(Parsetree.core_type) nonterminal_class*)
  | N_class_self_pattern                ->
    raise Not_found (*(Parsetree.pattern) nonterminal_class*)
  | N_class_longident                   ->
    raise Not_found (*(Longident.t) nonterminal_class*)
  | N_class_fun_def                     ->
    raise Not_found (*(Parsetree.class_expr) nonterminal_class*)
  | N_class_fun_binding                 ->
    raise Not_found (*(Parsetree.class_expr) nonterminal_class*)
  | N_class_fields                      -> 0, []
  | N_class_field                       -> 0, []
  | N_class_expr                        ->
    raise Not_found (*(Parsetree.class_expr) nonterminal_class*)
  | N_class_descriptions                -> 0, []
  | N_class_description                 -> 0, []
  | N_class_declarations                -> 0, []
  | N_class_declaration                 -> 0, []
  | N_attributes                        -> 0, []
  | N_attribute                         -> 1, default_attribute
  | N_attr_id                           ->
    raise Not_found (*(string Asttypes.loc) nonterminal_class*)
  | N_amper_type_list                   -> 0, []
  | N_additive                          -> 1, "+"

let default_symbol = function
  | CT_ (t,_) ->
    let q, v = default_token t in
    q, T_ (t, v)
  | CN_ (n,_) ->
    try
      let q, v = default_nonterminal n in
      q, N_ (n, v)
    with Not_found ->
      min_int, Bottom

let selection_priority = function
  | CN_ (N_structure_item, _) -> 1
  | _ -> 0

let is_lparen =
  let open Raw_parser in function
    | LPAREN -> Some "("
    | _ -> None

let is_operator =
  let open Raw_parser in function
    | PREFIXOP s
    | INFIXOP0 s | INFIXOP1 s | INFIXOP2 s | INFIXOP3 s | INFIXOP4 s -> Some s
    | BANG -> Some "!"        | CUSTOM_BANG -> Some "!"
    | PERCENT -> Some "%"
    | PLUS -> Some "+"        | PLUSDOT -> Some "+."
    | MINUS -> Some "-"       | MINUSDOT -> Some "-."
    | STAR -> Some "*"        | EQUAL -> Some "="
    | LESS -> Some "<"        | GREATER -> Some ">"
    | OR -> Some "or"         | BARBAR -> Some "||"
    | AMPERSAND -> Some "&"   | AMPERAMPER -> Some "&&"
    | COLONEQUAL -> Some ":=" | PLUSEQ -> Some "+="
    | _ -> None

let is_ident =
  let open Raw_parser in function
    | UIDENT s | LIDENT s -> Some s
    | _ -> None

let is_uident =
  let open Raw_parser in function
    | UIDENT s -> Some s
    | _ -> None

let friendly_name_of_token : type a. a token_class -> string option = function
  | T_WITH                 -> Some "with"
  | T_WHILE_LWT            -> Some "while_lwt"
  | T_WHILE                -> Some "while"
  | T_WHEN                 -> Some "when"
  | T_VIRTUAL              -> Some "virtual"
  | T_VAL                  -> Some "val"
  | T_UNDERSCORE           -> Some "_"
  | T_UIDENT               -> Some "identifier"
  | T_TYPE                 -> Some "type"
  | T_TRY_LWT              -> Some "try_lwt"
  | T_TRY                  -> Some "try"
  | T_TRUE                 -> None
  | T_TO                   -> Some "to"
  | T_TILDE                -> Some "~"
  | T_THEN                 -> Some "then"
  | T_STRUCT               -> Some "struct"
  | T_STRING               -> Some "string"
  | T_STAR                 -> Some "*"
  | T_SIG                  -> Some "sig"
  | T_SHARP                -> Some "#"
  | T_SEMISEMI             -> Some ";;"
  | T_SEMI                 -> Some ";"
  | T_RPAREN               -> Some ")"
  | T_REC                  -> Some "rec"
  | T_RBRACKET             -> Some "]"
  | T_RBRACE               -> Some "}"
  | T_QUOTE                -> None
  | T_QUESTION             -> Some "?"
  | T_PRIVATE              -> Some "private"
  | T_PREFIXOP             -> None
  | T_PLUSEQ               -> Some "+="
  | T_PLUSDOT              -> Some "+."
  | T_PLUS                 -> Some "+"
  | T_PERCENT              -> Some "%"
  | T_P4_QUOTATION         -> None
  | T_OUNIT_TEST_UNIT      -> None
  | T_OUNIT_TEST_MODULE    -> None
  | T_OUNIT_TEST           -> None
  | T_OUNIT_BENCH_MODULE   -> None
  | T_OUNIT_BENCH_INDEXED  -> None
  | T_OUNIT_BENCH_FUN      -> None
  | T_OUNIT_BENCH          -> None
  | T_OR                   -> None
  | T_OPTLABEL             -> None
  | T_OPEN                 -> Some "open"
  | T_OF                   -> Some "of"
  | T_OBJECT               -> Some "object"
  | T_NONREC               -> None
  | T_NEW                  -> Some "new"
  | T_NATIVEINT            -> None
  | T_MUTABLE              -> Some "mutable"
  | T_MODULE               -> Some "module"
  | T_MINUSGREATER         -> Some "->"
  | T_MINUSDOT             -> Some "-."
  | T_MINUS                -> Some "-"
  | T_METHOD               -> Some "method"
  | T_MATCH_LWT            -> None
  | T_MATCH                -> None
  | T_LPAREN               -> Some "("
  | T_LIDENT               -> Some "identifier"
  | T_LET_LWT              -> Some "let_lwt"
  | T_LET                  -> Some "let"
  | T_LESSMINUS            -> Some "<-"
  | T_LESS                 -> Some "<"
  | T_LBRACKETPERCENTPERCENT -> None
  | T_LBRACKETPERCENT      -> None
  | T_LBRACKETLESS         -> None
  | T_LBRACKETGREATER      -> None
  | T_LBRACKETBAR          -> None
  | T_LBRACKETATATAT       -> None
  | T_LBRACKETATAT         -> None
  | T_LBRACKETAT           -> None
  | T_LBRACKET             -> Some "["
  | T_LBRACELESS           -> None
  | T_LBRACE               -> Some "{"
  | T_LAZY                 -> Some "lazy"
  | T_LABEL                -> Some "label"
  | T_JSNEW                -> Some "jsnew"
  | T_INT64                -> Some "int64"
  | T_INT32                -> Some "int32"
  | T_INT                  -> None
  | T_INITIALIZER          -> Some "initializer"
  | T_INHERIT              -> Some "inherit"
  | T_INFIXOP4             -> None
  | T_INFIXOP3             -> None
  | T_INFIXOP2             -> None
  | T_INFIXOP1             -> None
  | T_INFIXOP0             -> None
  | T_INCLUDE              -> Some "include"
  | T_IN                   -> Some "in"
  | T_IF                   -> Some "if"
  | T_GREATERRBRACKET      -> Some ">]"
  | T_GREATERRBRACE        -> Some ">}"
  | T_GREATER              -> Some ">"
  | T_FUNCTOR              -> None
  | T_FUNCTION             -> None
  | T_FUN                  -> None
  | T_FOR_LWT              -> Some "for_lwt"
  | T_FOR                  -> Some "for"
  | T_FLOAT                -> None
  | T_FINALLY_LWT          -> Some "finally_lwt"
  | T_FALSE                -> None
  | T_EXTERNAL             -> Some "external"
  | T_EXCEPTION            -> Some "exception"
  | T_EQUAL                -> Some "="
  | T_EOL                  -> None
  | T_EOF                  -> None
  | T_EXITPOINT            -> None
  | T_ENTRYPOINT           -> None
  | T_END                  -> Some "end"
  | T_ELSE                 -> Some "else"
  | T_DOWNTO               -> Some "downto"
  | T_DOTDOT               -> Some ".."
  | T_DOT                  -> Some "."
  | T_DONE                 -> Some "done"
  | T_DO                   -> Some "do"
  | T_CONSTRAINT           -> Some "constraint"
  | T_COMMENT              -> None
  | T_COMMA                -> Some ","
  | T_COLONGREATER         -> Some ":>"
  | T_COLONEQUAL           -> Some ":="
  | T_COLONCOLON           -> Some "::"
  | T_COLON                -> Some ":"
  | T_CLASS                -> Some "class"
  | T_CHAR                 -> None
  | T_BEGIN                -> Some "begin"
  | T_BARRBRACKET          -> Some "|]"
  | T_BARBAR               -> Some "||"
  | T_BAR                  -> Some "|"
  | T_BANG                 -> Some "!"
  | T_CUSTOM_BANG          -> Some "!"
  | T_BACKQUOTE            -> Some "`"
  | T_ASSERT               -> None
  | T_AS                   -> Some "as"
  | T_AND                  -> Some "and"
  | T_AMPERSAND            -> Some "&"
  | T_AMPERAMPER           -> Some "&&"

let friendly_name_of_nonterminal : type a. a nonterminal_class -> string option = function
  | N_with_type_binder                  -> None
  | N_with_extensions                   -> None
  | N_with_constraints                  -> None
  | N_with_constraint                   -> Some "constraint"
  | N_virtual_flag                      -> None
  | N_value_type                        -> Some "value definition"
  | N_value                             -> Some "object value"
  | N_val_longident                     -> None
  | N_val_ident                         -> None
  | N_typevar_list                      -> None
  | N_type_variance                     -> Some "type variance"
  | N_type_variable                     -> Some "type variable"
  | N_type_parameters                   -> None
  | N_type_parameter_list               -> None
  | N_type_parameter                    -> Some "type parameter"
  | N_type_longident                    -> None
  | N_type_kind                         -> None
  | N_type_declarations                 -> None
  | N_type_declaration                  -> Some "type declaration"
  | N_type_constraint                   -> Some "type constraint"
  | N_toplevel_directives               -> None
  | N_tag_field                         -> None
  | N_subtractive                       -> None
  | N_structure_head                    -> None
  | N_structure_tail                    -> None
  | N_structure_item                    -> Some "any definition"
  | N_structure                         -> Some "structure"
  | N_strict_binding                    -> Some "'= expression'"
  | N_str_type_extension                -> None
  | N_str_extension_constructors        -> None
  | N_str_exception_declaration         -> None
  | N_single_attr_id                    -> None
  | N_simple_pattern_not_ident          -> Some "pattern"
  | N_simple_pattern                    -> Some "pattern"
  | N_simple_labeled_expr_list          -> Some "expression"
  | N_simple_expr                       -> Some "expression"
  | N_simple_core_type_or_tuple_no_attr -> None
  | N_simple_core_type_or_tuple         -> None
  | N_simple_core_type_no_attr          -> None
  | N_simple_core_type2                 -> Some "type expression"
  | N_simple_core_type                  -> Some "type expression"
  | N_signed_constant                   -> None
  | N_signature_item                    -> Some "any declaration"
  | N_signature                         -> Some "signature"
  | N_sig_type_extension                -> None
  | N_sig_extension_constructors        -> None
  | N_sig_exception_declaration         -> None
  | N_seq_expr                          -> Some "expression"
  | N_row_field_list                    -> None
  | N_row_field                         -> Some "row field"
  | N_record_expr                       -> None
  | N_rec_flag                          -> None
  | N_private_virtual_flags             -> None
  | N_private_flag                      -> None
  | N_primitive_declaration             -> Some "primitive declaration"
  | N_post_item_attributes              -> None
  | N_post_item_attribute               -> None
  | N_poly_type                         -> None
  | N_payload                           -> None
  | N_pattern_var                       -> None
  | N_pattern_semi_list                 -> None
  | N_pattern_comma_list                -> None
  | N_pattern                           -> Some "pattern"
  | N_parse_expression                  -> None
  | N_parent_binder                     -> None
  | N_package_type_cstrs                -> None
  | N_package_type_cstr                 -> None
  | N_package_type                      -> Some "package type"
  | N_override_flag                     -> None
  | N_optional_type_variable            -> None
  | N_optional_type_parameters          -> None
  | N_optional_type_parameter_list      -> None
  | N_optional_type_parameter           -> None
  | N_option_STRING_                    -> None
  | N_opt_semi                          -> None
  | N_opt_default                       -> None
  | N_opt_bar                           -> None
  | N_opt_ampersand                     -> None
  | N_operator                          -> Some "operator"
  | N_open_statement                    -> Some "open statement"
  | N_newtype                           -> Some "newtype"
  | N_name_tag_list                     -> None
  | N_name_tag                          -> None
  | N_mutable_flag                      -> None
  | N_mty_longident                     -> None
  | N_module_type                       -> Some "module type"
  | N_module_rec_declarations           -> None
  | N_module_rec_declaration            -> Some "module declaration"
  | N_module_expr                       -> Some "module expression"
  | N_module_declaration                -> Some "module declaration"
  | N_module_bindings                   -> None
  | N_module_binding_body               -> None
  | N_module_binding                    -> Some "module binding"
  | N_mod_longident                     -> None
  | N_mod_ext_longident                 -> None
  | N_method_                           -> None
  | N_meth_list                         -> None
  | N_match_cases                       -> None
  | N_match_case                        -> Some "match case"
  | N_lident_list                       -> None
  | N_let_pattern                       -> None
  | N_let_bindings                      -> None
  | N_let_binding_                      -> None
  | N_let_binding                       -> Some "binding"
  | N_let_bindings_no_attrs             -> None
  | N_lbl_pattern_list                  -> None
  | N_lbl_pattern                       -> Some "labeled pattern"
  | N_lbl_expr_list                     -> Some "field list"
  | N_lbl_expr                          -> Some "field expression"
  | N_labeled_simple_pattern            -> None
  | N_labeled_simple_expr               -> None
  | N_label_var                         -> None
  | N_label_longident                   -> None
  | N_label_let_pattern                 -> Some "labelled pattern"
  | N_label_ident                       -> None
  | N_label_expr                        -> Some "labelled expression"
  | N_label_declarations                -> None
  | N_label_declaration                 -> Some "label declaration"
  | N_label                             -> Some "label"
  | N_item_extension                    -> None
  | N_interface                         -> None
  | N_implementation                    -> None
  | N_ident                             -> Some "ident"
  | N_generalized_constructor_arguments -> Some "constructor arguments"
  | N_functor_args                      -> None
  | N_functor_arg_name                  -> None
  | N_functor_arg                       -> Some "functor argument"
  | N_fun_def                           -> None
  | N_fun_binding                       -> None
  | N_floating_attribute                -> None
  | N_field_expr_list                   -> Some "field expression"
  | N_field                             -> Some "field"
  | N_extension_constructor_rebind      -> None
  | N_extension_constructor_declaration -> None
  | N_extension                         -> Some "extension"
  | N_ext_attributes                    -> None
  | N_expr_semi_list                    -> None
  | N_expr_open                         -> None
  | N_expr_comma_list                   -> None
  | N_expr_comma_opt_list               -> None
  | N_expr                              -> Some "expression"
  | N_dummy                             -> None
  | N_direction_flag                    -> None
  | N_core_type_list_no_attr            -> None
  | N_core_type_list                    -> None
  | N_core_type_comma_list              -> None
  | N_core_type2                        -> Some "type expression"
  | N_core_type                         -> Some "type expression"
  | N_constructor_declarations          -> None
  | N_constructor_declaration           -> Some "constructor declaration"
  | N_constraints                       -> None
  | N_constrain_field                   -> None
  | N_constrain                         -> Some "constraint"
  | N_constr_longident                  -> None
  | N_constr_ident                      -> None
  | N_constant                          -> None
  | N_clty_longident                    -> None
  | N_class_type_parameters             -> None
  | N_class_type_declarations           -> None
  | N_class_type_declaration            -> Some "class type declaration"
  | N_class_type                        -> Some "class type"
  | N_class_structure                   -> Some "class field"
  | N_class_simple_expr                 -> Some "class expression"
  | N_class_signature                   -> None
  | N_class_sig_fields                  -> None
  | N_class_sig_field                   -> Some "class field"
  | N_class_sig_body                    -> Some "class field"
  | N_class_self_type                   -> None
  | N_class_self_pattern                -> None
  | N_class_longident                   -> None
  | N_class_fun_def                     -> None
  | N_class_fun_binding                 -> None
  | N_class_fields                      -> None
  | N_class_field                       -> Some "class field"
  | N_class_expr                        -> Some "class expression"
  | N_class_descriptions                -> None
  | N_class_description                 -> Some "class description"
  | N_class_declarations                -> None
  | N_class_declaration                 -> Some "class declaration"
  | N_attributes                        -> None
  | N_attribute                         -> None
  | N_attr_id                           -> None
  | N_amper_type_list                   -> None
  | N_additive                          -> None


let friendly_name = function
  | CT_ (t,_) -> friendly_name_of_token t
  | CN_ (n,_) -> friendly_name_of_nonterminal n
