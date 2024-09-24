open Std

let { Logger.log } = Logger.for_section "inlay-hints"

module Iterator = Ocaml_typing.Tast_iterator

let is_ghost_location avoid_ghost loc = loc.Location.loc_ghost && avoid_ghost

let pattern_has_constraint (type a) (pattern : a Typedtree.general_pattern) =
  List.exists
    ~f:(fun (extra, _, _) ->
      match extra with
      | Typedtree.Tpat_constraint _ -> true
      | Typedtree.Tpat_type (_, _)
      | Typedtree.Tpat_open (_, _, _)
      | Typedtree.Tpat_unpack -> false)
    pattern.pat_extra

let structure_iterator hint_let_binding hint_pattern_binding
    avoid_ghost_location typedtree range callback =
  let case_iterator hint_lhs (iterator : Iterator.iterator) case =
    let () = log ~title:"case" "on case" in
    let () = if hint_lhs then iterator.pat iterator case.Typedtree.c_lhs in
    let () = Option.iter ~f:(iterator.expr iterator) case.c_guard in
    iterator.expr iterator case.c_rhs
  in

  let value_binding_iterator hint_lhs (iterator : Iterator.iterator) vb =
    let () =
      log ~title:"value_binding" "%a" Logger.fmt (fun fmt ->
          Format.fprintf fmt "On value binding %a" (Printtyped.pattern 0)
            vb.Typedtree.vb_pat)
    in
    if Location_aux.overlap_with_range range vb.Typedtree.vb_loc then
      if hint_lhs then
        let () = log ~title:"value_binding" "overlap" in
        match vb.vb_expr.exp_desc with
        | Texp_function _ -> iterator.expr iterator vb.vb_expr
        | _ -> Iterator.default_iterator.value_binding iterator vb
      else iterator.expr iterator vb.vb_expr
  in

  let expr_iterator (iterator : Iterator.iterator) expr =
    let () =
      log ~title:"expression" "%a" Logger.fmt (fun fmt ->
          Format.fprintf fmt "On expression %a" Printtyped.expression expr)
    in
    if Location_aux.overlap_with_range range expr.Typedtree.exp_loc then
      let () = log ~title:"expression" "overlap" in
      match expr.exp_desc with
      | Texp_let (_, bindings, body) ->
        let () = log ~title:"expression" "on let" in
        let () =
          List.iter
            ~f:(value_binding_iterator hint_let_binding iterator)
            bindings
        in
        iterator.expr iterator body
      | Texp_letop { body; _ } ->
        let () = log ~title:"expression" "on let-op" in
        case_iterator hint_let_binding iterator body
      | Texp_match (expr, cases, _) ->
        let () = log ~title:"expression" "on match" in
        let () = iterator.expr iterator expr in
        List.iter ~f:(case_iterator hint_pattern_binding iterator) cases
      | Texp_function
          ( _,
            Tfunction_cases
              { cases =
                  [ { c_rhs =
                        { exp_desc = Texp_let (_, [ { vb_pat; _ } ], body); _ };
                      _
                    }
                  ];
                _
              } ) ->
        let () = log ~title:"expression" "on function" in
        let () = iterator.pat iterator vb_pat in
        iterator.expr iterator body
      | _ when is_ghost_location avoid_ghost_location expr.exp_loc ->
        (* Stop iterating when we see a ghost location to avoid
           annotating generated code *)
        log ~title:"ghost" "ghost-location found"
      | _ -> Iterator.default_iterator.expr iterator expr
  in

  let structure_item_iterator (iterator : Iterator.iterator) item =
    if Location_aux.overlap_with_range range item.Typedtree.str_loc then
      let () = log ~title:"structure_item" "overlap" in
      match item.str_desc with
      | Tstr_value (_, bindings) ->
        List.iter
          ~f:(fun binding -> expr_iterator iterator binding.Typedtree.vb_expr)
          bindings
      | _ when is_ghost_location avoid_ghost_location item.str_loc ->
        (* Stop iterating when we see a ghost location to avoid
           annotating generated code *)
        log ~title:"ghost" "ghost-location found"
      | _ -> Iterator.default_iterator.structure_item iterator item
  in

  let pattern_iterator (type a) iterator (pattern : a Typedtree.general_pattern)
      =
    let () =
      log ~title:"pattern" "%a" Logger.fmt (fun fmt ->
          Format.fprintf fmt "On pattern %a" (Printtyped.pattern 0) pattern)
    in
    if
      Location_aux.overlap_with_range range pattern.pat_loc
      && not (pattern_has_constraint pattern)
    then
      let () = log ~title:"pattern" "overlap" in
      let () = Iterator.default_iterator.pat iterator pattern in
      match pattern.pat_desc with
      | Tpat_var _ when not pattern.pat_loc.loc_ghost ->
        let () = log ~title:"pattern" "found" in
        callback pattern.pat_env pattern.pat_type pattern.pat_loc
      | _ -> log ~title:"pattern" "not a var"
  in

  let iterator =
    { Ocaml_typing.Tast_iterator.default_iterator with
      expr = expr_iterator;
      structure_item = structure_item_iterator;
      pat = pattern_iterator;
      value_binding = value_binding_iterator true
    }
  in
  iterator.structure iterator typedtree

type hint = Lexing.position * string

let create_hint env typ loc =
  let label =
    Printtyp.wrap_printing_env env (fun () ->
        Format.asprintf "%a" Printtyp.type_scheme typ)
  in
  let position = loc.Location.loc_end in
  (position, label)

let of_structure ~hint_let_binding ~hint_pattern_binding ~avoid_ghost_location
    ~start ~stop structure =
  let () =
    log ~title:"start" "%a" Logger.fmt (fun fmt ->
        Format.fprintf fmt
          "Start on %s to %s with : let: %b, pat: %b, ghost: %b"
          (Lexing.print_position () start)
          (Lexing.print_position () stop)
          hint_let_binding hint_pattern_binding avoid_ghost_location)
  in
  let range = (start, stop) in
  let hints = ref [] in
  let () =
    structure_iterator hint_let_binding hint_pattern_binding
      avoid_ghost_location structure range (fun env typ loc ->
        let () =
          log ~title:"hint" "Find hint %a" Logger.fmt (fun fmt ->
              Format.fprintf fmt "%s - %a"
                (Location_aux.print () loc)
                Printtyp.type_expr typ)
        in
        let hint = create_hint env typ loc in
        hints := hint :: !hints)
  in
  !hints
