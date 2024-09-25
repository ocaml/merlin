open Std

let log_section = "type-enclosing"
let { Logger.log } = Logger.for_section log_section

type type_info =
  | Modtype of Env.t * Types.module_type
  | Type of Env.t * Types.type_expr
  | Type_decl of Env.t * Ident.t * Types.type_declaration
  | String of string

type typed_enclosings =
  (Location.t * type_info * Query_protocol.is_tail_position) list

let from_nodes ~path =
  let aux (env, node, tail) =
    let open Browse_raw in
    let ret x = Some (Mbrowse.node_loc node, x, tail) in
    match[@ocaml.warning "-9"] node with
    | Expression { exp_type = t }
    | Pattern { pat_type = t }
    | Core_type { ctyp_type = t }
    | Value_description { val_desc = { ctyp_type = t } } -> ret (Type (env, t))
    | Type_declaration { typ_id = id; typ_type = t } ->
      ret (Type_decl (env, id, t))
    | Module_expr { mod_type = Types.Mty_for_hole } -> None
    | Module_expr { mod_type = m }
    | Module_type { mty_type = m }
    | Module_binding { mb_expr = { mod_type = m } }
    | Module_declaration { md_type = { mty_type = m } }
    | Module_type_declaration { mtd_type = Some { mty_type = m } }
    | Module_binding_name { mb_expr = { mod_type = m } }
    | Module_declaration_name { md_type = { mty_type = m } }
    | Module_type_declaration_name { mtd_type = Some { mty_type = m } } ->
      ret (Modtype (env, m))
    | Class_field
        { cf_desc = Tcf_method (_, _, Tcfk_concrete (_, { exp_type })) } ->
    begin
      match Types.get_desc exp_type with
      | Tarrow (_, _, t, _) -> ret (Type (env, t))
      | _ -> None
    end
    | Class_field
        { cf_desc = Tcf_val (_, _, _, Tcfk_concrete (_, { exp_type = t }), _) }
      -> ret (Type (env, t))
    | Class_field
        { cf_desc = Tcf_method (_, _, Tcfk_virtual { ctyp_type = t }) } ->
      ret (Type (env, t))
    | Class_field
        { cf_desc = Tcf_val (_, _, _, Tcfk_virtual { ctyp_type = t }, _) } ->
      ret (Type (env, t))
    | Binding_op { bop_op_type; _ } -> ret (Type (env, bop_op_type))
    | _ -> None
  in
  List.filter_map ~f:aux path

let from_reconstructed ~nodes ~cursor ~verbosity exprs =
  let open Browse_raw in
  let env, node = Mbrowse.leaf_node nodes in
  log ~title:"from_reconstructed" "node = %s\nexprs = [%s]"
    (Browse_raw.string_of_node node)
    (String.concat ~sep:";" (List.map exprs ~f:(fun l -> l.Location.txt)));
  let include_lident =
    match node with
    | Pattern _ -> false
    | _ -> true
  in
  let include_uident =
    match node with
    | Module_binding _
    | Module_binding_name _
    | Module_declaration _
    | Module_declaration_name _
    | Module_type_declaration _
    | Module_type_declaration_name _ -> false
    | _ -> true
  in

  let get_context lident =
    Context.inspect_browse_tree ~cursor (Longident.parse lident) [ nodes ]
  in

  let f { Location.txt = source; loc } =
    let context = get_context source in
    Option.iter context ~f:(fun ctx ->
        log ~title:"from_reconstructed" "source = %s; context = %s" source
          (Context.to_string ctx));
    match context with
    (* Retrieve the type from the AST when it is possible *)
    | Some (Context.Constructor (cd, loc)) ->
      log ~title:"from_reconstructed" "ctx: constructor %s" cd.cstr_name;
      let ppf, to_string = Format.to_string () in
      Type_utils.print_constr ~verbosity env ppf cd;
      Some (loc, String (to_string ()), `No)
    | Some (Context.Label { lbl_name; lbl_arg; _ }) ->
      log ~title:"from_reconstructed" "ctx: label %s" lbl_name;
      let ppf, to_string = Format.to_string () in
      Type_utils.print_type_with_decl ~verbosity env ppf lbl_arg;
      Some (loc, String (to_string ()), `No)
    | Some Context.Constant -> None
    | _ -> (
      let context = Option.value ~default:Context.Expr context in
      (* Else use the reconstructed identifier *)
      match source with
      | "" ->
        log ~title:"from_reconstructed" "no reconstructed identifier";
        None
      | source when (not include_lident) && Char.is_lowercase source.[0] ->
        log ~title:"from_reconstructed" "skipping lident";
        None
      | source when (not include_uident) && Char.is_uppercase source.[0] ->
        log ~title:"from_reconstructed" "skipping uident";
        None
      | source -> (
        try
          let ppf, to_string = Format.to_string () in
          if Type_utils.type_in_env ~verbosity ~context env ppf source then (
            log ~title:"from_reconstructed" "typed %s" source;
            Some (loc, String (to_string ()), `No))
          else (
            log ~title:"from_reconstructed" "FAILED to type %s" source;
            None)
        with _ -> None))
  in
  List.filter_map exprs ~f
