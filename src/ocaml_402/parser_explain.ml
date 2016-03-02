open Parser_raw
let named_item_at = function
  | 24 -> "while_lwt"
  | 80 -> "attribute"
  | 175 -> "extension"
  | 384 -> "while"
  | 386 -> "try_lwt"
  | 388 -> "try"
  | 402 -> "object"
  | 424 -> "attribute"
  | 442 -> "match_lwt"
  | 444 -> "match"
  | 456 -> "lwt"
  | 475 -> "let"
  | 476 -> "let open"
  | 497 -> "if"
  | 499 -> "function"
  | 502 -> "pattern"
  | 503 -> "when guard"
  | 504 -> "fun"
  | 526 -> "for_lwt"
  | 530 -> "for"
  | 551 -> "val"
  | 556 -> "type"
  | 558 -> "type nonrec"
  | 675 -> "open"
  | 679 -> "module"
  | 754 -> "module type"
  | 760 -> "recursive module"
  | 769 -> "extension"
  | 773 -> "attribute"
  | 777 -> "include"
  | 780 -> "external"
  | 789 -> "exception"
  | 795 -> "class"
  | 797 -> "class type"
  | 805 -> "object"
  | 964 -> "meta-let"
  | 1131 -> "for body"
  | 1135 -> "for body"
  | 1142 -> "for body"
  | 1157 -> "match action"
  | 1159 -> "match action"
  | 1166 -> "then clause"
  | 1168 -> "else clause"
  | 1192 -> "let module"
  | 1222 -> "type constraint"
  | 1224 -> "type constraint"
  | 1235 -> "type constraint"
  | 1329 -> "object"
  | 1388 -> "while body"
  | 1397 -> "while_lwt body"
  | 1400 -> "type"
  | 1401 -> "type nonrec"
  | 1469 -> "module"
  | 1473 -> "module type"
  | 1479 -> "recursive module"
  | 1485 -> "lwt"
  | 1489 -> "let"
  | 1493 -> "include"
  | 1496 -> "external"
  | 1503 -> "exception"
  | 1511 -> "class"
  | 1512 -> "class type"
  | 1533 -> "lwt"
  | 1537 -> "let"
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
