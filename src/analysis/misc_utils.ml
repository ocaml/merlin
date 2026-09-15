open Std

module Path : sig
  val is_opened : Env.t -> Path.t -> bool

  val to_shortest_lid :
    env:Env.t ->
    ?name:string ->
    env_check:(Longident.t -> Env.t -> 'a) ->
    Path.t ->
    Longident.t
end = struct
  let opens env =
    let rec aux acc = function
      | Env.Env_open (s, path) -> aux (path :: acc) s
      | s ->
        Option.map ~f:(aux acc) (Browse_misc.summary_prev s)
        |> Option.value ~default:acc
    in
    aux [] env

  let is_opened env path = List.mem path ~set:(opens (Env.summary env))

  let rec to_shortest_lid ~(opens : Path.t list) = function
    | Path.Pdot (path, name) when List.exists ~f:(Path.same path) opens ->
      Longident.Lident name
    | Path.Pdot (path, name) ->
      let lid = Location.mknoloc (to_shortest_lid ~opens path) in
      Ldot (lid, Location.mknoloc name)
    | Pident ident -> Lident (Ident.name ident)
    | _ -> assert false

  let maybe_replace_name ?name lid =
    let open Longident in
    Option.value_map name ~default:lid ~f:(fun name ->
        match lid with
        | Lident _ -> Lident name
        | Ldot (lid, _) -> Ldot (lid, Location.mknoloc name)
        | _ -> assert false)

  let to_shortest_lid ~env ?name ~env_check path =
    let opens = opens (Env.summary env) in
    let lid = to_shortest_lid ~opens path |> maybe_replace_name ?name in
    try
      env_check lid env |> ignore;
      lid
    with Not_found -> maybe_replace_name ?name (Untypeast.lident_of_path path)
end

let parenthesize_name name =
  (* Qualified operators need parentheses *)
  if name = "" || not (Oprint.parenthesized_ident name) then name
  else if name.[0] = '*' || name.[String.length name - 1] = '*' then
    "( " ^ name ^ " )"
  else "(" ^ name ^ ")"

let parse_identifier (config, source) pos =
  let path = Mreader.reconstruct_identifier config source pos in
  let path = Mreader_lexer.identifier_suffix path in
  Logger.log ~section:Type_enclosing.log_section ~title:"reconstruct-identifier"
    "paths: [%s]"
    (String.concat ~sep:";" (List.map path ~f:(fun l -> l.Location.txt)));
  path

let reconstruct_identifier pipeline pos = function
  | None ->
    let config = Mpipeline.input_config pipeline in
    let source = Mpipeline.raw_source pipeline in
    let path = parse_identifier (config, source) pos in
    let reify dot =
      if
        dot = ""
        || (dot.[0] >= 'a' && dot.[0] <= 'z')
        || (dot.[0] >= 'A' && dot.[0] <= 'Z')
      then dot
      else "( " ^ dot ^ ")"
    in
    begin match path with
    | [] -> []
    | base :: tail ->
      let f { Location.txt = base; loc = bl } { Location.txt = dot; loc = dl } =
        let loc = Location_aux.union bl dl in
        let txt = base ^ "." ^ reify dot in
        Location.mkloc txt loc
      in
      [ List.fold_left tail ~init:base ~f ]
    end
  | Some (expr, offset) ->
    let loc_start =
      let l, c = Lexing.split_pos pos in
      Lexing.make_pos (l, c - offset)
    in
    let shift loc int =
      let l, c = Lexing.split_pos loc in
      Lexing.make_pos (l, c + int)
    in
    let add_loc source =
      let loc =
        { Location.loc_start;
          loc_end = shift loc_start (String.length source);
          loc_ghost = false
        }
      in
      Location.mkloc source loc
    in
    let len = String.length expr in
    let rec aux acc i =
      if i >= len then List.rev_map ~f:add_loc (expr :: acc)
      else if expr.[i] = '.' then
        aux (String.sub expr ~pos:0 ~len:i :: acc) (succ i)
      else aux acc (succ i)
    in
    aux [] offset

let split_lid_up_to_cursor cursor_pos lid =
  let rec aux acc (lid : Longident.t Location.loc) =
    match lid with
    | { txt = Lident _; _ } -> lid :: acc
    | { txt = Ldot (_lid, { loc; _ }); _ }
    | { txt = Lapply (_lid, { loc; _ }); _ }
      when Lexing.compare_pos loc.loc_start cursor_pos <= 0 -> lid :: acc
    | { txt = Ldot (lid', _); _ } -> aux (lid :: acc) lid'
    | { txt = Lapply (lid', _); _ } -> aux (lid :: acc) lid'
  in
  aux [] lid

let find_record_field fields loc =
  Array.find_map
    (fun (_lbl_desc, lbl_def) ->
      match lbl_def with
      | Typedtree.Overridden (lid, { exp_desc = Texp_ident _; exp_loc; _ })
        when lid.Location.loc.loc_ghost
             && Location_aux.compare lid.Location.loc exp_loc = 0
             && Location_aux.compare loc exp_loc = 0 -> Some lid
      | Overridden _ | Kept _ -> None)
    fields

let find_pat_record_field fields loc =
  List.find_some fields ~f:(fun (lid, _lbl_desc, (pat : Typedtree.pattern)) ->
      match pat.pat_desc with
      | Tpat_var _ ->
        lid.Location.loc.loc_ghost
        && Location_aux.compare lid.Location.loc pat.pat_loc = 0
        && Location_aux.compare loc pat.pat_loc = 0
      | _ -> false)
  |> Option.map ~f:(fun (lid, _lbl_desc, _pat) -> lid)

let with_none v = function
  | None -> Some v
  | Some v -> Some v

let get_identifier_from_nodes nodes pos =
  let lid =
    (* The first two cases handle record punning *)
    match nodes with
    | ( _,
        Browse_raw.Expression { exp_desc = Texp_ident (_, lid, _); exp_loc; _ }
      )
      :: (_, Browse_raw.Expression { exp_desc = Texp_record { fields; _ }; _ })
      :: _ -> find_record_field fields exp_loc |> with_none lid
    | (_, Browse_raw.Pattern { pat_desc = Tpat_var (_, name, _); pat_loc; _ })
      :: (_, Browse_raw.Pattern { pat_desc = Tpat_record (fields, _); _ })
      :: _ ->
      find_pat_record_field fields pat_loc
      |> with_none (Location.mkloc (Longident.Lident name.txt) name.loc)
    | (_, Browse_raw.Expression { exp_desc = Texp_ident (_path, lid, _); _ })
      :: _ -> Some lid
    | (_, Browse_raw.Module_expr { mod_desc = Tmod_ident (_path, lid); _ }) :: _
      -> Some lid
    | _ -> None
  in
  let is_type_error (lid : Longident.t Location.loc) =
    match lid.txt with
    | Longident.Lident "*type-error*" -> true
    | _ -> false
  in
  match lid with
  | None -> []
  | Some lid when is_type_error lid -> []
  | Some lid -> split_lid_up_to_cursor pos lid

let get_or_reconstruct_identifier pipeline pos idento =
  match idento with
  | Some _ -> `Strings (reconstruct_identifier pipeline pos idento)
  | None ->
    let nodes =
      Mtyper.node_at ~disambiguate:Mbrowse.Tie_breaker.prefer_expression
        (Mpipeline.typer_result pipeline)
        pos
    in
    let from_node = get_identifier_from_nodes nodes pos in
    if not (List.is_empty from_node) then `Longidents from_node
    else `Strings (reconstruct_identifier pipeline pos None)
