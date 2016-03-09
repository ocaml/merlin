open Parser_raw
let named_item_at = function
  | 2 -> "while_lwt"
  | 58 -> "attribute"
  | 153 -> "extension"
  | 361 -> "while"
  | 363 -> "try_lwt"
  | 365 -> "try"
  | 379 -> "object"
  | 401 -> "attribute"
  | 419 -> "match_lwt"
  | 421 -> "match"
  | 433 -> "lwt"
  | 452 -> "let"
  | 453 -> "let open"
  | 474 -> "if"
  | 476 -> "function"
  | 479 -> "pattern"
  | 480 -> "when guard"
  | 481 -> "fun"
  | 503 -> "for_lwt"
  | 507 -> "for"
  | 528 -> "val"
  | 533 -> "type"
  | 535 -> "type nonrec"
  | 652 -> "open"
  | 656 -> "module"
  | 731 -> "module type"
  | 737 -> "recursive module"
  | 746 -> "extension"
  | 750 -> "attribute"
  | 754 -> "include"
  | 757 -> "external"
  | 766 -> "exception"
  | 772 -> "class"
  | 774 -> "class type"
  | 782 -> "object"
  | 941 -> "meta-let"
  | 1108 -> "for body"
  | 1112 -> "for body"
  | 1119 -> "for body"
  | 1134 -> "match action"
  | 1136 -> "match action"
  | 1143 -> "then clause"
  | 1145 -> "else clause"
  | 1169 -> "let module"
  | 1199 -> "type constraint"
  | 1201 -> "type constraint"
  | 1212 -> "type constraint"
  | 1306 -> "object"
  | 1365 -> "while body"
  | 1374 -> "while_lwt body"
  | 1377 -> "type"
  | 1378 -> "type nonrec"
  | 1446 -> "module"
  | 1450 -> "module type"
  | 1456 -> "recursive module"
  | 1462 -> "lwt"
  | 1466 -> "let"
  | 1470 -> "include"
  | 1473 -> "external"
  | 1480 -> "exception"
  | 1488 -> "class"
  | 1489 -> "class type"
  | 1510 -> "lwt"
  | 1514 -> "let"
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
