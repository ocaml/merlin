open Parser_raw
let named_item_at = function
  | 2 -> "while_lwt"
  | 58 -> "attribute"
  | 153 -> "extension"
  | 361 -> "while"
  | 363 -> "try_lwt"
  | 365 -> "try"
  | 380 -> "object"
  | 402 -> "attribute"
  | 420 -> "match_lwt"
  | 422 -> "match"
  | 434 -> "lwt"
  | 453 -> "let"
  | 454 -> "let open"
  | 475 -> "if"
  | 477 -> "function"
  | 480 -> "pattern"
  | 481 -> "when guard"
  | 482 -> "fun"
  | 504 -> "for_lwt"
  | 508 -> "for"
  | 529 -> "val"
  | 534 -> "type"
  | 536 -> "type nonrec"
  | 653 -> "open"
  | 657 -> "module"
  | 732 -> "module type"
  | 738 -> "recursive module"
  | 747 -> "extension"
  | 751 -> "attribute"
  | 755 -> "include"
  | 758 -> "external"
  | 767 -> "exception"
  | 773 -> "class"
  | 775 -> "class type"
  | 783 -> "object"
  | 942 -> "meta-let"
  | 1109 -> "for body"
  | 1113 -> "for body"
  | 1120 -> "for body"
  | 1135 -> "match action"
  | 1137 -> "match action"
  | 1144 -> "then clause"
  | 1146 -> "else clause"
  | 1170 -> "let module"
  | 1200 -> "type constraint"
  | 1202 -> "type constraint"
  | 1213 -> "type constraint"
  | 1307 -> "object"
  | 1366 -> "while body"
  | 1375 -> "while_lwt body"
  | 1378 -> "type"
  | 1379 -> "type nonrec"
  | 1447 -> "module"
  | 1451 -> "module type"
  | 1457 -> "recursive module"
  | 1463 -> "lwt"
  | 1467 -> "let"
  | 1471 -> "include"
  | 1474 -> "external"
  | 1481 -> "exception"
  | 1489 -> "class"
  | 1490 -> "class type"
  | 1511 -> "lwt"
  | 1515 -> "let"
  | _ -> raise Not_found

let nullable (type a) : a MenhirInterpreter.nonterminal -> bool =
  let open MenhirInterpreter in function
  | N_virtual_flag -> true
  | N_type_variance -> true
  | N_type_parameters -> true
  | N_type_kind -> true
  | N_toplevel_directives -> true
  | N_structure_tail -> true
  | N_structure_head -> true
  | N_structure -> true
  | N_signature -> true
  | N_rec_flag -> true
  | N_private_virtual_flags -> true
  | N_private_flag -> true
  | N_post_item_attributes -> true
  | N_payload -> true
  | N_parent_binder -> true
  | N_override_flag -> true
  | N_optional_type_parameters -> true
  | N_option_STRING_ -> true
  | N_opt_semi -> true
  | N_opt_default -> true
  | N_opt_bar -> true
  | N_opt_ampersand -> true
  | N_mutable_flag -> true
  | N_generalized_constructor_arguments -> true
  | N_ext_attributes -> true
  | N_constraints -> true
  | N_class_type_parameters -> true
  | N_class_structure -> true
  | N_class_sig_fields -> true
  | N_class_sig_body -> true
  | N_class_self_type -> true
  | N_class_self_pattern -> true
  | N_class_fields -> true
  | N_attributes -> true
  | _ -> false
