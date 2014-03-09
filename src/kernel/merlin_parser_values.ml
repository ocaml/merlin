open Std
open Raw_parser

type terminal = Query.terminal

module type S = sig
  type t
  type index = private int

  val to_index: t -> index
  val of_index: index -> t

  val to_terminal: t -> terminal
  val of_terminal: terminal -> t option

  (*val index_of_terminal: terminal -> index option*)
(*  val terminal_of_index: index -> terminal*)

  val to_string: t -> string

  val all: t array

  val iter: (index -> unit) -> unit
end

let index_table = ref [||]

module Token : S with type t = token =
struct
  type t = token

  let all = [|
    WITH; WHILE_LWT; WHILE; WHEN; VIRTUAL; VAL; UNDERSCORE; TYPE; TRY_LWT; TRY;
    TRUE; TO; TILDE; THEN; STRUCT; STAR; SIG; SHARP; SEMISEMI; SEMI; RPAREN; REC;
    RBRACKET; RBRACE; QUOTE; QUESTION; PRIVATE; PLUSDOT; PLUS; OR; OPEN; OF;
    OBJECT; NONREC; NEW; MUTABLE; MODULE; MINUSGREATER; MINUSDOT; MINUS; METHOD;
    MATCH_LWT; MATCH; LPAREN; LET_LWT; LET; LESSMINUS; LESS; LBRACKETLESS;
    LBRACKETGREATER; LBRACKETBAR; LBRACKET; LBRACELESS; LBRACE; LAZY; JSNEW;
    INITIALIZER; INHERIT; INCLUDE; IN; IF; GREATERRBRACKET; GREATERRBRACE;
    GREATER; FUNCTOR; FUNCTION; FUN; FOR_LWT; FOR; FINALLY_LWT; FALSE; EXTERNAL;
    EXCEPTION; EQUAL; EOF; END; ELSE; DOWNTO; DOTDOT; DOT; DONE; DO; CONSTRAINT;
    COMMA; COLONGREATER; COLONEQUAL; COLONCOLON; COLON; CLASS; BEGIN;
    BARRBRACKET; BARBAR; BAR; BANG; BACKQUOTE; ASSERT; AS; AND; AMPERSAND;
    AMPERAMPER; P4_QUOTATION; OUNIT_TEST_UNIT; OUNIT_TEST_MODULE; OUNIT_TEST;
    OUNIT_BENCH_MODULE; OUNIT_BENCH_INDEXED; OUNIT_BENCH_FUN; OUNIT_BENCH;
    STRING ""; CHAR ' '; INT32 0l; INT 0; INT64 0L; UIDENT ""; OPTLABEL "";
    NATIVEINT 0n; LIDENT ""; LABEL ""; PREFIXOP ""; FLOAT "";
    INFIXOP4 ""; INFIXOP3 ""; INFIXOP2 ""; INFIXOP1 ""; INFIXOP0 "";
  |]


  let to_string =
    let p a s = "(" ^ s ^ ")" in
    function
    | WITH            -> "WITH"            | WHILE_LWT       -> "WHILE_LWT"
    | WHILE           -> "WHILE"           | WHEN            -> "WHEN"
    | VIRTUAL         -> "VIRTUAL"         | VAL             -> "VAL"
    | UNDERSCORE      -> "UNDERSCORE"      | UIDENT _        -> "UIDENT"
    | TYPE            -> "TYPE"            | TRY_LWT         -> "TRY_LWT"
    | TRY             -> "TRY"             | TRUE            -> "TRUE"
    | TO              -> "TO"              | TILDE           -> "TILDE"
    | THEN            -> "THEN"            | STRUCT          -> "STRUCT"
    | STRING _        -> "STRING"          | STAR            -> "STAR"
    | SIG             -> "SIG"             | SHARP           -> "SHARP"
    | SEMISEMI        -> "SEMISEMI"        | SEMI            -> "SEMI"
    | RPAREN          -> "RPAREN"          | REC             -> "REC"
    | RBRACKET        -> "RBRACKET"        | RBRACE          -> "RBRACE"
    | QUOTE           -> "QUOTE"           | QUESTION        -> "QUESTION"
    | PRIVATE         -> "PRIVATE"         | PREFIXOP s      -> p"PREFIXOP" s
    | PLUSDOT         -> "PLUSDOT"         | PLUS            -> "PLUS"
    | OR              -> "OR"              | OPTLABEL _      -> "OPTLABEL"
    | OPEN            -> "OPEN"            | OF              -> "OF"
    | OBJECT          -> "OBJECT"          | NONREC          -> "NONREC"
    | NEW             -> "NEW"             | NATIVEINT _     -> "NATIVEINT"
    | MUTABLE         -> "MUTABLE"         | MODULE          -> "MODULE"
    | MINUSGREATER    -> "MINUSGREATER"    | MINUSDOT        -> "MINUSDOT"
    | MINUS           -> "MINUS"           | METHOD          -> "METHOD"
    | MATCH_LWT       -> "MATCH_LWT"       | MATCH           -> "MATCH"
    | LPAREN          -> "LPAREN"          | LIDENT _        -> "LIDENT"
    | LET_LWT         -> "LET_LWT"         | LET             -> "LET"
    | LESSMINUS       -> "LESSMINUS"       | LESS            -> "LESS"
    | LBRACKETLESS    -> "LBRACKETLESS"    | LBRACKETGREATER -> "LBRACKETGREATER"
    | LBRACKETBAR     -> "LBRACKETBAR"     | LBRACKET        -> "LBRACKET"
    | LBRACELESS      -> "LBRACELESS"      | LBRACE          -> "LBRACE"
    | LAZY            -> "LAZY"            | LABEL _         -> "LABEL"
    | JSNEW           -> "JSNEW"           | INT64 _         -> "INT64"
    | INT32 _         -> "INT32"           | INT _           -> "INT"
    | INITIALIZER     -> "INITIALIZER"     | INHERIT         -> "INHERIT"
    | INFIXOP4 s      -> p"INFIXOP4" s     | INFIXOP3 s      -> p"INFIXOP3" s
    | INFIXOP2 s      -> p"INFIXOP2" s     | INFIXOP1 s      -> p"INFIXOP1" s
    | INFIXOP0 s      -> p"INFIXOP0" s     | INCLUDE         -> "INCLUDE"
    | IN              -> "IN"              | IF              -> "IF"
    | GREATERRBRACKET -> "GREATERRBRACKET" | GREATERRBRACE   -> "GREATERRBRACE"
    | GREATER         -> "GREATER"         | FUNCTOR         -> "FUNCTOR"
    | FUNCTION        -> "FUNCTION"        | FUN             -> "FUN"
    | FOR_LWT         -> "FOR_LWT"         | FOR             -> "FOR"
    | FLOAT _         -> "FLOAT"           | FINALLY_LWT     -> "FINALLY_LWT"
    | FALSE           -> "FALSE"           | EXTERNAL        -> "EXTERNAL"
    | EXCEPTION       -> "EXCEPTION"       | EQUAL           -> "EQUAL"
    | EOF             -> "EOF"             | END             -> "END"
    | ELSE            -> "ELSE"            | DOWNTO          -> "DOWNTO"
    | DOTDOT          -> "DOTDOT"          | DOT             -> "DOT"
    | DONE            -> "DONE"            | DO              -> "DO"
    | CONSTRAINT      -> "CONSTRAINT"      | COMMENT _       -> "COMMENT"
    | COMMA           -> "COMMA"           | COLONGREATER    -> "COLONGREATER"
    | COLONEQUAL      -> "COLONEQUAL"      | COLONCOLON      -> "COLONCOLON"
    | COLON           -> "COLON"           | CLASS           -> "CLASS"
    | CHAR _          -> "CHAR"            | BEGIN           -> "BEGIN"
    | BARRBRACKET     -> "BARRBRACKET"     | BARBAR          -> "BARBAR"
    | BAR             -> "BAR"             | BANG            -> "BANG"
    | BACKQUOTE       -> "BACKQUOTE"       | ASSERT          -> "ASSERT"
    | AS              -> "AS"              | AND             -> "AND"
    | AMPERSAND       -> "AMPERSAND"       | AMPERAMPER      -> "AMPERAMPER"
    | P4_QUOTATION        -> "P4_QUOTATION"
    | OUNIT_TEST_UNIT     -> "OUNIT_TEST_UNIT"
    | OUNIT_TEST_MODULE   -> "OUNIT_TEST_MODULE"
    | OUNIT_TEST          -> "OUNIT_TEST"
    | OUNIT_BENCH_MODULE  -> "OUNIT_BENCH_MODULE"
    | OUNIT_BENCH_INDEXED -> "OUNIT_BENCH_INDEXED"
    | OUNIT_BENCH_FUN     -> "OUNIT_BENCH_FUN"
    | OUNIT_BENCH         -> "OUNIT_BENCH"
    | ENTRYPOINT -> "ENTRYPOINT"
    | RECOVER -> "RECOVER"
    | RECONSTRUCT _ -> "RECONSTRUCT"

  let to_terminal t = Query.index t
  let of_terminal (t : terminal) =
    match snd (!index_table).((t :> int)) with
    | Terminal t -> Some t
    | _ -> None

  let all' = Array.map to_terminal all

  type index = int
  let to_index t = fst (!index_table).((to_terminal t :> int))
  let of_index t = all.(t)

  let index_of_terminal (t : terminal) =
    match (!index_table).((t :> int)) with
    | idx, Terminal _ -> Some idx
    | _ -> None

  let terminal_of_index t = all'.(t)

  let iter f =
    for i = 0 to Array.length all - 1 do
      f i
    done
end

module Nonterminal : S with type t = nonterminal =
struct
  open Asttypes
  open Parsetree

  type t = nonterminal

  let all =
    let mktyp d = { ptyp_desc = d; ptyp_loc = Location.none } in
    let mkpat d = { ppat_desc = d; ppat_loc = Location.none } in
    let mkmty d = { pmty_desc = d; pmty_loc = Location.none } in
    let mkmod d = { pmod_desc = d; pmod_loc = Location.none } in
    let any_pat = mkpat Ppat_any in
    let any_typ = mktyp Ptyp_any in
    let any_lident = Longident.Lident "" in
    let any_mty = mkmty (Pmty_signature []) in
    let any_mod = mkmod (Pmod_structure []) in
    let any_cls_sig = {pcsig_self = any_typ; pcsig_fields = []; pcsig_loc = Location.none} in
    let any_cls_typ = {pcty_desc = Pcty_signature any_cls_sig; pcty_loc = Location.none} in
    let any_cls_str = {pcstr_pat = any_pat; pcstr_fields = []} in
    let any_cls_exp = {pcl_desc = Pcl_structure any_cls_str; pcl_loc = Location.none} in
    [|
      NT'additive "+";
      NT'amper_type_list [];
      NT'class_declaration [];
      NT'class_declarations [];
      NT'class_description [];
      NT'class_descriptions [];
      NT'class_expr any_cls_exp;
      NT'class_field [];
      NT'class_fields [];
      NT'class_fun_binding any_cls_exp;
      NT'class_fun_def any_cls_exp;
      NT'class_longident any_lident;
      NT'class_self_pattern any_pat;
      NT'class_self_type any_typ;
      NT'class_sig_body any_cls_sig;
      NT'class_sig_field [];
      NT'class_sig_fields [];
      NT'class_signature any_cls_typ;
      NT'class_simple_expr any_cls_exp;
      NT'class_structure any_cls_str;
      NT'class_type any_cls_typ;
      NT'class_type_declaration [];
      NT'class_type_declarations [];
      NT'class_type_parameters ([],Location.none);
      NT'clty_longident any_lident;
      NT'concrete_method None;
      NT'constant (Const_int 0);
      NT'constrain [];
      NT'constrain_field None;
      NT'constraints [];
      NT'constrained_seq_expr Fake.any_val';
      NT'constr_ident "";
      NT'constr_longident any_lident;
      NT'constructor_arguments [];
      NT'constructor_declaration [];
      NT'constructor_declarations [];
      NT'core_type2 any_typ;
      NT'core_type any_typ;
      NT'core_type_comma_list [];
      NT'core_type_list [];
      NT'direction_flag Upto;
      NT'expr_comma_list [];
      NT'expr_comma_opt_list [];
      NT'expr Fake.any_val';
      NT'expr_semi_list [];
      NT'field [];
      NT'field_expr_list [];
      NT'fun_binding Fake.any_val';
      NT'fun_def Fake.any_val';
      NT'generalized_constructor_arguments ([], None);
      NT'ident "";
      NT'implementation [];
      NT'interface [];
      NT'label "";
      NT'label_declaration [];
      NT'label_declarations [];
      NT'label_declaration_with ();
      NT'labeled_simple_expr ("", Fake.any_val');
      NT'labeled_simple_pattern ("", None, any_pat);
      NT'label_expr ("",Fake.any_val');
      NT'label_ident ("",Fake.any_val');
      NT'label_let_pattern ("", any_pat);
      NT'label_longident any_lident;
      NT'label_var ("", any_pat);
      NT'lbl_expr [];
      NT'lbl_expr_list [];
      NT'lbl_pattern [];
      NT'lbl_pattern_list ([], Open);
      NT'let_binding [];
      NT'let_bindings [];
      NT'let_pattern any_pat;
      NT'lident_list [];
      NT'match_action Fake.any_val';
      NT'match_cases [];
      NT'meth_list [];
      NT'method_type None;
      NT'mod_ext_longident any_lident;
      NT'mod_longident any_lident;
      NT'mod_open (Fresh,(Location.mkloc any_lident Location.none));
      NT'module_binding any_mod;
      NT'module_declaration any_mty;
      NT'module_expr any_mod;
      NT'module_functor_arg (Location.mkloc "" Location.none, any_mty);
      NT'module_rec_binding [];
      NT'module_rec_bindings [];
      NT'module_rec_declaration [];
      NT'module_rec_declarations [];
      NT'module_type any_mty;
      NT'mty_longident any_lident;
      NT'mutable_flag Immutable;
      NT'name_tag "";
      NT'name_tag_list [];
      NT'new_type "";
      NT'operator "+";
      NT'opt_ampersand false;
      NT'opt_default None;
      NT'optional_type_parameter_list [];
      NT'optional_type_parameter (None, (false, false));
      NT'optional_type_parameters [];
      NT'option_BAR_ None;
      NT'option_SEMI_ None;
      NT'option_SEMISEMI_ None;
      NT'option_STRING_ None;
      NT'opt_present [];
      NT'override_flag Fresh;
      NT'package_type_cstr [];
      NT'package_type_cstrs [];
      NT'package_type (Location.mknoloc any_lident, []);
      NT'parent_binder None;
      NT'pattern any_pat;
      NT'pattern_comma_list [];
      NT'pattern_semi_list [];
      NT'pattern_var any_pat;
      NT'poly_type any_typ;
      NT'primitive_declaration [];
      NT'private_flag Public;
      NT'rec_flag Default;
      NT'record_expr (None, []);
      NT'row_field [];
      NT'row_field_list [];
      NT'seq_expr Fake.any_val';
      NT'signature [];
      NT'signature_item [];
      NT'signed_constant (Const_int 0);
      NT'simple_core_type2 any_typ;
      NT'simple_core_type any_typ;
      NT'simple_core_type_or_tuple any_typ;
      NT'simple_expr Fake.any_val';
      NT'simple_labeled_expr_list [];
      NT'simple_pattern any_pat;
      NT'strict_binding Fake.any_val';
      NT'structure [];
      NT'structure_item [];
      NT'structure_tail [];
      NT'subtractive "-";
      NT'tag_field [];
      NT'top_expr Fake.any_val';
      NT'type_constraint (None, None);
      NT'type_declaration [];
      NT'type_declarations [];
      NT'type_kind (Ptype_abstract, Public, None);
      NT'type_longident any_lident;
      NT'type_parameter [];
      NT'type_parameter_list [];
      NT'type_parameters [];
      NT'type_variance (false, false);
      NT'typevar_list [];
      NT'val_ident "";
      NT'val_longident any_lident;
      NT'value None;
      NT'value_type None;
      NT'virtual_flag Virtual;
      NT'virtual_method None;
      NT'virtual_method_type None;
      NT'virtual_value None;
      NT'with_constraint [];
      NT'with_constraints [];
      NT'with_extensions [];
      NT'with_type_binder Public;
    |]

  let to_string = function
    | NT'with_type_binder                  _ -> "with_type_binder"
    | NT'with_extensions                   _ -> "with_extensions"
    | NT'with_constraints                  _ -> "with_constraints"
    | NT'with_constraint                   _ -> "with_constraint"
    | NT'virtual_value                     _ -> "virtual_value"
    | NT'virtual_method_type               _ -> "virtual_method_type"
    | NT'virtual_method                    _ -> "virtual_method"
    | NT'virtual_flag                      _ -> "virtual_flag"
    | NT'value_type                        _ -> "value_type"
    | NT'value                             _ -> "value"
    | NT'val_longident                     _ -> "val_longident"
    | NT'val_ident                         _ -> "val_ident"
    | NT'typevar_list                      _ -> "typevar_list"
    | NT'type_variance                     _ -> "type_variance"
    | NT'type_parameters                   _ -> "type_parameters"
    | NT'type_parameter_list               _ -> "type_parameter_list"
    | NT'type_parameter                    _ -> "type_parameter"
    | NT'type_longident                    _ -> "type_longident"
    | NT'type_kind                         _ -> "type_kind"
    | NT'type_declarations                 _ -> "type_declarations"
    | NT'type_declaration                  _ -> "type_declaration"
    | NT'type_constraint                   _ -> "type_constraint"
    | NT'tag_field                         _ -> "tag_field"
    | NT'top_expr                          _ -> "top_expr"
    | NT'subtractive                       _ -> "subtractive"
    | NT'structure_tail                    _ -> "structure_tail"
    | NT'structure_item                    _ -> "structure_item"
    | NT'structure                         _ -> "structure"
    | NT'strict_binding                    _ -> "strict_binding"
    | NT'simple_pattern                    _ -> "simple_pattern"
    | NT'simple_labeled_expr_list          _ -> "simple_labeled_expr_list"
    | NT'simple_expr                       _ -> "simple_expr"
    | NT'simple_core_type_or_tuple         _ -> "simple_core_type_or_tuple"
    | NT'simple_core_type2                 _ -> "simple_core_type2"
    | NT'simple_core_type                  _ -> "simple_core_type"
    | NT'signed_constant                   _ -> "signed_constant"
    | NT'signature_item                    _ -> "signature_item"
    | NT'signature                         _ -> "signature"
    | NT'seq_expr                          _ -> "seq_expr"
    | NT'row_field_list                    _ -> "row_field_list"
    | NT'row_field                         _ -> "row_field"
    | NT'record_expr                       _ -> "record_expr"
    | NT'rec_flag                          _ -> "rec_flag"
    | NT'private_flag                      _ -> "private_flag"
    | NT'primitive_declaration             _ -> "primitive_declaration"
    | NT'poly_type                         _ -> "poly_type"
    | NT'pattern_var                       _ -> "pattern_var"
    | NT'pattern_semi_list                 _ -> "pattern_semi_list"
    | NT'pattern_comma_list                _ -> "pattern_comma_list"
    | NT'pattern                           _ -> "pattern"
    | NT'parent_binder                     _ -> "parent_binder"
    | NT'package_type_cstrs                _ -> "package_type_cstrs"
    | NT'package_type_cstr                 _ -> "package_type_cstr"
    | NT'package_type                      _ -> "package_type"
    | NT'override_flag                     _ -> "override_flag"
    | NT'optional_type_parameters          _ -> "optional_type_parameters"
    | NT'optional_type_parameter_list      _ -> "optional_type_parameter_list"
    | NT'optional_type_parameter           _ -> "optional_type_parameter"
    | NT'option_STRING_                    _ -> "option_STRING_"
    | NT'option_SEMISEMI_                  _ -> "option_SEMISEMI_"
    | NT'option_SEMI_                      _ -> "option_SEMI_"
    | NT'option_BAR_                       _ -> "option_BAR_"
    | NT'opt_present                       _ -> "opt_present"
    | NT'opt_default                       _ -> "opt_default"
    | NT'opt_ampersand                     _ -> "opt_ampersand"
    | NT'operator                          _ -> "operator"
    | NT'new_type                          _ -> "new_type"
    | NT'name_tag_list                     _ -> "name_tag_list"
    | NT'name_tag                          _ -> "name_tag"
    | NT'mutable_flag                      _ -> "mutable_flag"
    | NT'mty_longident                     _ -> "mty_longident"
    | NT'module_type                       _ -> "module_type"
    | NT'module_rec_declarations           _ -> "module_rec_declarations"
    | NT'module_rec_declaration            _ -> "module_rec_declaration"
    | NT'module_rec_bindings               _ -> "module_rec_bindings"
    | NT'module_rec_binding                _ -> "module_rec_binding"
    | NT'module_functor_arg                _ -> "module_functor_arg"
    | NT'module_expr                       _ -> "module_expr"
    | NT'module_declaration                _ -> "module_declaration"
    | NT'module_binding                    _ -> "module_binding"
    | NT'mod_open                          _ -> "mod_open"
    | NT'mod_longident                     _ -> "mod_longident"
    | NT'mod_ext_longident                 _ -> "mod_ext_longident"
    | NT'method_type                       _ -> "method_type"
    | NT'meth_list                         _ -> "meth_list"
    | NT'match_cases                       _ -> "match_cases"
    | NT'match_action                      _ -> "match_action"
    | NT'lident_list                       _ -> "lident_list"
    | NT'let_pattern                       _ -> "let_pattern"
    | NT'let_bindings                      _ -> "let_bindings"
    | NT'let_binding                       _ -> "let_binding"
    | NT'lbl_pattern_list                  _ -> "lbl_pattern_list"
    | NT'lbl_pattern                       _ -> "lbl_pattern"
    | NT'lbl_expr_list                     _ -> "lbl_expr_list"
    | NT'lbl_expr                          _ -> "lbl_expr"
    | NT'labeled_simple_pattern            _ -> "labeled_simple_pattern"
    | NT'labeled_simple_expr               _ -> "labeled_simple_expr"
    | NT'label_var                         _ -> "label_var"
    | NT'label_longident                   _ -> "label_longident"
    | NT'label_let_pattern                 _ -> "label_let_pattern"
    | NT'label_ident                       _ -> "label_ident"
    | NT'label_expr                        _ -> "label_expr"
    | NT'label_declarations                _ -> "label_declarations"
    | NT'label_declaration_with            _ -> "label_declaration_with"
    | NT'label_declaration                 _ -> "label_declaration"
    | NT'label                             _ -> "label"
    | NT'interface                         _ -> "interface"
    | NT'implementation                    _ -> "implementation"
    | NT'ident                             _ -> "ident"
    | NT'generalized_constructor_arguments _ -> "generalized_constructor_arguments"
    | NT'fun_def                           _ -> "fun_def"
    | NT'fun_binding                       _ -> "fun_binding"
    | NT'field_expr_list                   _ -> "field_expr_list"
    | NT'field                             _ -> "field"
    | NT'expr_semi_list                    _ -> "expr_semi_list"
    | NT'expr_comma_opt_list               _ -> "expr_comma_opt_list"
    | NT'expr_comma_list                   _ -> "expr_comma_list"
    | NT'expr                              _ -> "expr"
    | NT'direction_flag                    _ -> "direction_flag"
    | NT'core_type_list                    _ -> "core_type_list"
    | NT'core_type_comma_list              _ -> "core_type_comma_list"
    | NT'core_type2                        _ -> "core_type2"
    | NT'core_type                         _ -> "core_type"
    | NT'constructor_declarations          _ -> "constructor_declarations"
    | NT'constructor_declaration           _ -> "constructor_declaration"
    | NT'constructor_arguments             _ -> "constructor_arguments"
    | NT'constraints                       _ -> "constraints"
    | NT'constrained_seq_expr              _ -> "constrained_seq_expr"
    | NT'constrain_field                   _ -> "constrain_field"
    | NT'constrain                         _ -> "constrain"
    | NT'constr_longident                  _ -> "constr_longident"
    | NT'constr_ident                      _ -> "constr_ident"
    | NT'constant                          _ -> "constant"
    | NT'concrete_method                   _ -> "concrete_method"
    | NT'clty_longident                    _ -> "clty_longident"
    | NT'class_type_parameters             _ -> "class_type_parameters"
    | NT'class_type_declarations           _ -> "class_type_declarations"
    | NT'class_type_declaration            _ -> "class_type_declaration"
    | NT'class_type                        _ -> "class_type"
    | NT'class_structure                   _ -> "class_structure"
    | NT'class_simple_expr                 _ -> "class_simple_expr"
    | NT'class_signature                   _ -> "class_signature"
    | NT'class_sig_fields                  _ -> "class_sig_fields"
    | NT'class_sig_field                   _ -> "class_sig_field"
    | NT'class_sig_body                    _ -> "class_sig_body"
    | NT'class_self_type                   _ -> "class_self_type"
    | NT'class_self_pattern                _ -> "class_self_pattern"
    | NT'class_longident                   _ -> "class_longident"
    | NT'class_fun_def                     _ -> "class_fun_def"
    | NT'class_fun_binding                 _ -> "class_fun_binding"
    | NT'class_fields                      _ -> "class_fields"
    | NT'class_field                       _ -> "class_field"
    | NT'class_expr                        _ -> "class_expr"
    | NT'class_descriptions                _ -> "class_descriptions"
    | NT'class_description                 _ -> "class_description"
    | NT'class_declarations                _ -> "class_declarations"
    | NT'class_declaration                 _ -> "class_declaration"
    | NT'amper_type_list                   _ -> "amper_type_list"
    | NT'additive                          _ -> "additive"

  let to_terminal t =
    failwith "Not supported with current settings (enable --feed-nonterminal)"
      (*Query.index (Nonterminal t)*)
  let of_terminal (t : terminal) =
    match snd (!index_table).((t :> int)) with
    | Nonterminal t -> Some t
    | _ -> None

  (*let all' = Array.map to_terminal all*)

  type index = int
  let to_index t = fst (!index_table).((to_terminal t :> int))
  let of_index t = all.(t)

  let index_of_terminal (t : terminal) =
    match (!index_table).((t :> int)) with
    | idx, Nonterminal _ -> Some idx
    | _ -> None

  (*let terminal_of_index t = all'.(t)*)

  let iter f =
    for i = 0 to Array.length all - 1 do
      f i
    done
end

module Value : sig
  type t = Raw_parser.semantic_value =
    | Bottom
    | Terminal of Token.t
    | Nonterminal of Nonterminal.t

  val to_terminal: t -> terminal
  val of_terminal: terminal -> t
  val to_string: t -> string
  val all: t array
end = struct

  type t = Raw_parser.semantic_value =
    | Bottom
    | Terminal of Token.t
    | Nonterminal of Nonterminal.t

  let all =
    Array.concat [
      Array.map (fun x -> Terminal x) Token.all;
      Array.map (fun x -> Nonterminal x) Nonterminal.all;
    ]

  let to_string = function
    | Bottom -> "Bottom"
    | Terminal t -> Token.to_string t
    | Nonterminal nt -> Nonterminal.to_string nt

  let to_terminal = function
    | Terminal t -> Query.index t
    | _ -> failwith "Not supported with current settings (enable --feed-nonterminal)"
    (* Query.index *)

  let of_terminal (t : terminal) = snd (!index_table).((t :> int))
end

(*let max_terminal = Array.fold_left
    (fun acc v -> max acc (Value.to_terminal v))
    (Value.to_terminal Bottom)
    Value.all

let () =
  let table = Array.create ((max_terminal :> int) + 1) (-1, Bottom) in
  Array.iteri
    (fun i v -> table.((Token.to_terminal v :> int)) <- (i, Terminal v))
    Token.all;
  Array.iteri
    (fun i v -> table.((Nonterminal.to_terminal v :> int)) <- (i, Nonterminal v))
    Nonterminal.all;
  index_table := table
*)
