(* TODO SP make it (or check it is made) explicit that all rules are used *)

(*

We call U the set of all paths used directly in a file:

- 1. Any path occurring in the file is in U. For example, List.map occurring in
     the file will add both List.map and List to U.
- 2. All paths for definitions in the current file are in U. So if module M = …
     occurs in the file then M is in U.
- 3. All paths for things “defined” using include or open in the current file
     are in U. It is possible that all of these would end up in D anyway via
     other rules, but it's not entirely obvious so I've included this rule here
     just to make sure they do.
- Note that constructors or fields only used via type-based disambiguation are
  not in U.

We call D the domain of discourse:

- 1. The paths of all the predefined types that are intended for direct use by
     users, like int, are in D.
- 2. If a path is in U then it is also in D.
- 3. If a module path is in U then all the paths of its subcomponents are in D.
- 4. If a value path is in U and its value description was written by a user -
     as opposed to being inferred - then the paths used in that description are
     in D.
- 5. If a module path is in U and its module description was written by a user -
     as opposed to being inferred - then the paths used in that description are
     in D, excluding those paths that only appear inside of a sig…end.
- 6. If a type path is in U then any paths used in its equation or
     representation are in D.
- 7. If a constructor or record field is in U then any paths used in its type
     are in D.
- 8. If a module type path is in U then any paths used in its definition are in
     D, excluding those paths that only appear inside of a sig…end.
- 9. If a class path is in U and its class description was written by a user -
     as opposed to being inferred - then of any paths used in that description
     are in D.
- 10. If a class type path is in U then any paths used in its definition are in
      D.
- 11. If a path is in D and it includes another module path within it, then that
      module path is also in D.
- 12. If a module path m in D - note D not U - is a module alias with target n
      and another path p in D includes n within it, then the path obtained by
      substituting the m for n in p is also in D.
*)

(* This logging section logs a "recap" of the discourse creation: The value of
   [U] before [D.of_U] is called, and the value of [D] at the end.

   It will also display the "intermediate" U that are built during the creation
   of D (see "special rule") *)
let recap_log_section = "discourse-recap"
let { Logger.log = log_recap } = Logger.for_section recap_log_section

let trie_of_paths (d : Discourse_types.t) =
  let open Discourse_types in
  Paths.fold
    (fun (kind, path) acc -> Path_trie.add path kind acc)
    (Paths.union d.local d.extern)
    Path_trie.empty

let pp_d fmt d =
  let open Discourse_types in
  let open Format in
  let pp_sep fmt () = fprintf fmt ";@ " in
  let pp_path_set fmt set =
    fprintf fmt "@[<1>[%a]@]"
      (pp_print_list ~pp_sep (Format_doc.compat Path.print))
      (Path.Set.elements set)
  in
  let pp_substs_binding fmt (path, paths) =
    fprintf fmt "@[<2>%a ->@ %a@]"
      (Format_doc.compat Path.print)
      path pp_path_set paths
  in
  let pp_substs fmt map =
    if Path.Map.is_empty map then fprintf fmt "[]"
    else
      fprintf fmt "@[<v>[%a]@]"
        (pp_print_list ~pp_sep pp_substs_binding)
        (Path.Map.bindings map)
  in
  fprintf fmt
    "@[<v 2>Discourse {@;size = %i;@;paths =@ %a;@;substs =@ %a@;<-2>}@]"
    (Path_trie.size d.paths) Path_trie.pp_seq d.paths pp_substs d.substs

(* A more verbose section which logs every addition to U or D, along with the
   reason the element is added (U1, U2, U3, D2, …, D12) *)
let log_section = "discourse-verbose"
let { Logger.log } = Logger.for_section log_section

open Shape.Sig_component_kind
open Discourse_types

module Item_map = Map.Make(Item)

module U = struct
  (* We build U lazily: during typing we only log [event]s, and the actual work
     (walking signatures, looking up the environment, building the tries) only
     happens in [force], which is called by [D.of_U] the first time something
     needs to be printed. *)
  type event =
    | Used of
        { kind : Shape.Sig_component_kind.t;
          lid : Longident.t Location.loc;
          path : Path.t;
          env : Env.t
        }  (** Rule U1: a path occurring in the file *)
    | Used_constructor of
        { env : Env.t;
          lid : Longident.t Location.loc;
          discourse : Discourse_types.t
        }  (** Rule U1 for constructors (and rule D7 for their discourse) *)
    | Used_label of
        { env : Env.t;
          lid : Longident.t Location.loc;
          discourse : Discourse_types.t
        }  (** Rule U1 for labels (and rule D7 for their discourse) *)
    | Initial_discourse of Discourse_types.t
        (** Rule D1: the predefined types *)
    | Defined of { kind : Shape.Sig_component_kind.t; id : Ident.t }
        (** Rule U2: a type or module type defined in the file *)
    | Defined_module of { decl : Types.module_declaration; id : Ident.t }
        (** Rule U2: a module defined in the file *)
    | Defined_signature of Types.signature
        (** Rule U3: the components brought by an [include], or by opening a
            module expression ([open struct ... end]) *)
    | Opened of { env : Env.t; path : Path.t }
        (** Rule U3: the components brought by opening a module path *)

  type u =
    { u_paths : Env.t Item_map.t;
      substs : Discourse_types.substs;
      discourse : Path_trie.t;
      pending : event list
    }

  let pp_u fmt u =
    let open Format in
    let pp_sep fmt () = fprintf fmt ";@ " in
    let pp_u_item fmt (kind, path) =
      fprintf fmt "@[<1>{item = (%s,@ %a)}@]"
        (Shape.Sig_component_kind.to_string kind)
        (Format_doc.compat Path.print)
        path
    in
    let pp_u_paths_binding fmt (((_, path) as item), _env) =
      fprintf fmt "@[<2>%a ->@ %a@]"
        (Format_doc.compat Path.print)
        path pp_u_item item
    in
    let pp_u_paths fmt map =
      fprintf fmt "@[<v>[%a]@]"
        (pp_print_list ~pp_sep pp_u_paths_binding)
        (Item_map.bindings map)
    in
    fprintf fmt "@[<v 2>{ u_paths =@ %a;@ substs =@ %a }@]" pp_u_paths u.u_paths
      Discourse_types.pp_substs u.substs


  let add_item item env u =
    if Item_map.mem item u.u_paths then u
    else { u with u_paths = Item_map.add item env u.u_paths }

  let empty_u : u =
    { u_paths = Item_map.empty;
      substs = Path.Map.empty;
      discourse = Path_trie.empty;
      pending = []
    }
  let g = Local_store.s_ref empty_u

  (** We call U the set of all paths used directly in a file:

    1. Any path occurring in the file is in U. For example, List.map occurring in
       the file will add both List.map and List to U.
    2. All paths for definitions in the current file are in U. So if module M = …
       occurs in the file then M is in U.
    3. All paths for things “defined” using include or open in the current file
       are in U. It is possible that all of these would end up in D anyway via
       other rules, but it's not entirely obvious so I've included this rule here
       just to make sure they do.
    - Note that constructors or fields only used via type-based disambiguation are
      not in U.
*)

  let get () = !g
  let set v = g := v
  let reset () = g := empty_u

  let record_usages = Config.merlin

  (* We only log events during typing, post-processing is deferred. *)
  let record event =
    if record_usages then g := { !g with pending = event :: !g.pending }

  let merge_discourse paths u =
    { u with discourse = Path_trie.union (trie_of_paths paths) u.discourse }

  let fold_on_common_lid_and_path_segments ~init ~kind ~f (lid, path) =
    let rec aux acc kind ((lid, path) : Longident.t * Path.t) =
      let acc = f acc kind (lid, path) in
      match (lid, path) with
      | Lident _, Pident _ -> acc
      | Ldot (l, _), Pdot (p, _) -> aux acc Module (l.txt, p)
      | Lapply (l1, l2), Papply (p1, p2) ->
        let acc = aux acc Module (l2.txt, p2) in
        aux acc Module (l1.txt, p1)
      | _ -> acc
    in
    aux init kind (lid, path)

  let fold_on_path_segments ~init ~kind ~f path =
    let rec aux acc kind (path : Path.t) =
      let acc = f acc kind path in
      match path with
      | Pident _ -> acc
      | Pdot (p, _) -> aux acc Module p
      | Papply (p1, p2) ->
        let acc = aux acc Module p2 in
        aux acc Module p1
      | _ -> (* TODO Handle extra ? *) acc
    in
    aux init kind path

  let add_subst substs path by_path =
    log ~title:"subst" "subst: %a -> %a (open/alias defined in file)" Logger.fmt
      (Fun.flip (Format_doc.compat Path.print) path)
      Logger.fmt
      (Fun.flip (Format_doc.compat Path.print) by_path);
    let substs =
      Path.Map.update path
        (function
          | None -> Some (Path.Set.singleton by_path)
          | Some paths -> Some (Path.Set.add by_path paths))
        substs
    in
    substs

  let add_subst_u path path' u =
    { u with substs = add_subst u.substs path path' }

  (** {1 Rules U2 and U3: paths defined in the current file, or brought in scope
      by an include or an open} *)

  let path_of_ident ?root id =
    match root with
    | Some path -> Path.Pdot (path, Ident.name id)
    | None -> Path.Pident id

  (* [full_path] is the complete path in the environment. *)
  let define ~from kind ?root ?full_path id u =
    let path = path_of_ident ?root id in
    let title, origin =
      match from with
      | `File -> ("U2", "defined in current file")
      | `Open -> ("U3", "brought in scope by an open")
      | `Include ->
        ( "U3",
          "brought in scope by an include (or an open of a module expression)"
        )
    in
    log ~title "%s: %s %a%a %s" title
      (Shape.Sig_component_kind.to_string kind)
      Logger.fmt
      (fun fmt -> (Format_doc.compat Path.print) fmt path)
      Logger.fmt
      (fun fmt ->
        match full_path with
        | None -> ()
        | Some full_path ->
          Format.fprintf fmt " [%a]" (Format_doc.compat Path.print) full_path)
      origin;
    { u with discourse = Path_trie.add ?full_path path kind u.discourse }

  let rec define_signature ?(from = `File) ?root sg u =
    List.fold_left (fun u item -> define_component ~from ?root item u) u sg

  and define_component ?(from = `File) ?root sig_item u =
    match (sig_item : Types.signature_item) with
    | Sig_type (id, _, _, _) -> define_type ~from ?root id u
    | Sig_value (id, _, _) -> define_value ~from ?root id u
    | Sig_typext (_, _, _, _) -> u
    | Sig_module (id, _, md, _, _) -> define_module ~from ?root md id u
    | Sig_modtype (id, _, _) -> define_modtype ~from ?root id u
    | Sig_class (_, _, _, _) | Sig_class_type (_, _, _, _) -> (* TODO: do *) u

  and define_type ?(from = `File) ?root ?full_path id u =
    define ~from ?root ?full_path Type id u

  and define_value ?(from = `File) ?root ?full_path id u =
    define ~from ?root ?full_path Value id u

  and define_module ?(from = `File) ?root (decl : Types.module_declaration) id u
      =
    let u = define ~from Module ?root id u in
    let root = path_of_ident ?root id in
    match decl.md_type with
    | Mty_ident path | Mty_alias path -> add_subst_u path root u
    | Mty_signature module_type -> define_signature ~from ~root module_type u
    | _ -> u

  and define_modtype ?(from = `File) ?root ?full_path id u =
    define ~from ?root ?full_path Module_type id u

  (** {1 Rule U3}

     All paths for things “defined” using include or open in the current file
     are in U. It is possible that all of these would end up in D anyway via
     other rules, but it's not entirely obvious so I've included this rule here
     just to make sure they do.
  *)

  (* [open_path] is the path of the module being opened. [path_under_open] is
     the current path relative to the path of the module being opened.

     A component [x] is known under two paths: [path_under_open.x], the one the
     user can write now that the module is opened (and the one we want to print)
     and [open_path.x], the one valid in the current environment. We record the
     pair, so that a candidate coming out of an [open] can still be checked
     against an environment and filed under the right canonical path. *)
  let rec define_signature_for_open ~open_path ?path_under_open
      (sg : Subst.Lazy.signature) u =
    List.fold_left
      (fun u sig_item ->
        match (sig_item : Subst.Lazy.signature_item) with
        | SigL_type (id, _, _, _) ->
          log ~title:"U3" "U3: type %a brought in scope by open" Logger.fmt
            (Fun.flip Ident.print id);
          let full_path = path_of_ident ~root:open_path id in
          define_type ~from:`Open ?root:path_under_open ~full_path id u
        | SigL_value (id, _, _) ->
          log ~title:"U3" "U3: value %a brought in scope by open" Logger.fmt
            (Fun.flip Ident.print id);
          let full_path = path_of_ident ~root:open_path id in
          define_value ~from:`Open ?root:path_under_open ~full_path id u
        | SigL_typext (_, _, _, _) -> u
        | SigL_module (id, Mp_present, { mdl_type = MtyL_signature s; _ }, _, _)
          ->
          (* We recursively bring everything that is directly defined in the
             opened module, but without following aliases. *)
          log ~title:"U3" "U3: module (present) %a brought in scope by open"
            Logger.fmt (Fun.flip Ident.print id);
          let full_path = path_of_ident ~root:open_path id in
          let u =
            define ~from:`Open Module ?root:path_under_open ~full_path id u
          in
          let path_under_open = path_of_ident ?root:path_under_open id in
          let u = add_subst_u full_path path_under_open u in
          define_signature_for_open ~open_path:full_path ~path_under_open s u
        | SigL_module (id, _, { mdl_type; _ }, _, _) ->
          log ~title:"U3" "U3: module %a brought in scope by open" Logger.fmt
            (Fun.flip Ident.print id);
          let full_path = path_of_ident ~root:open_path id in
          let u =
            define ~from:`Open Module ?root:path_under_open ~full_path id u
          in
          let path_under_open = path_of_ident ?root:path_under_open id in
          let u =
            match mdl_type with
            | MtyL_alias alias_path -> add_subst_u alias_path path_under_open u
            | _ -> u
          in
          add_subst_u full_path path_under_open u
        | SigL_modtype (id, _, _) ->
          log ~title:"U3" "U3: module type %a brought in scope by open"
            Logger.fmt (Fun.flip Ident.print id);
          let full_path = path_of_ident ~root:open_path id in
          define_modtype ~from:`Open ?root:path_under_open ~full_path id u
        | SigL_class (_, _, _, _) | SigL_class_type (_, _, _, _) ->
          (* TODO: do *) u)
      u
      (Subst.Lazy.force_signature_once sg)

  let open_module env path u =
    log ~title:"U3" "U3: open module %a" Logger.fmt (fun fmt ->
        (Format_doc.compat Path.print) fmt path);
    try
      (* When opening we need to traverse the aliases to get the components *)
      let open_path = Env.normalize_module_path None env path in
      let md = Env.find_module_lazy open_path env in
      match md.mdl_type with
      | MtyL_signature sg -> define_signature_for_open ~open_path sg u
      | _ -> u
    with Not_found -> u

  (** {1 Rule U1}

      Any path occurring in the file is in U. For example, List.map occurring in
      the file will add both List.map and List to U.
  *)

  let add_used env kind lid path t =
    let loc = lid.Location.loc in
    let f acc kind path =
      log ~title:"U1" "U1: path %a used in file: %a (%a)" Logger.fmt
        (fun fmt ->
          Format.pp_print_string fmt (Shape.Sig_component_kind.to_string kind))
        Logger.fmt
        (Fun.flip (Format_doc.compat Path.print) path)
        Logger.fmt
        (fun fmt -> Location.print_loc fmt loc);
      add_item (kind, path) env acc
    in
    (* If a path is in D and it includes another module path within it, then that
       module path is also in D.*)
    fold_on_path_segments ~init:t ~kind ~f path

  let use_module env lid path = add_used env Module lid path
  let use_modtype env lid path = add_used env Module_type lid path
  let use_type env lid path = add_used env Type lid path
  let use_value env lid path = add_used env Value lid path

  (* [what] is ["constructor"] or ["label"], [discourse] is the discourse of
     the constructor or label ([cstr_discourse] or [lbl_discourse]). *)
  let use_constructor_or_label ~what env lid discourse u =
    let u =
      (* When using a constructor or a label, the modules appearing in its path
         should be added to U. TODO: we might want to do that even if the
         constructor has been disambiguated *)
      match lid.Location.txt with
      | Longident.Ldot (mod_lid, _) -> (
        (* This find should not load additional CUs, because
           [lookup_structure_components] (resp. [lookup_all_labels]) was called
           anyway by the compiler. *)
        try
          let path, _ = Env.find_module_by_name_lazy mod_lid.txt env in
          use_module env mod_lid path u
        with Not_found -> u)
      | _ -> u
    in
    (* D7: If a constructor or label is in U then any paths used in its type are
       in D. *)
    log ~title:"D7" "D7: %s %a used, merging its discourse" what Logger.fmt
      (Fun.flip Pprintast.longident lid.txt);
    merge_discourse discourse u

  (** {1 Forcing}

      Processes the recorded events *)

  let apply_event u = function
    | Used { kind; lid; path; env } -> add_used env kind lid path u
    | Used_constructor { env; lid; discourse } ->
      use_constructor_or_label ~what:"constructor" env lid discourse u
    | Used_label { env; lid; discourse } ->
      use_constructor_or_label ~what:"label" env lid discourse u
    | Initial_discourse paths -> merge_discourse paths u
    | Defined { kind; id } -> define ~from:`File kind id u
    | Defined_module { decl; id } -> define_module ~from:`File decl id u
    | Defined_signature sg -> define_signature ~from:`Include sg u
    | Opened { env; path } -> open_module env path u

  let force u =
    match u.pending with
    | [] -> u
    | pending ->
      List.fold_left apply_event { u with pending = [] } (List.rev pending)
end

module D = struct
  (**

     {2 The domain of discourse}

     - 1. The paths of all the predefined types that are intended for direct use by
       users, like int, are in D.
     - 2. If a path is in U then it is also in D.
     - 3. If a module path is in U then all the paths of its subcomponents are in D.
     - 4. If a value path is in U and its value description was written by a user -
       as opposed to being inferred - then the paths used in that description are
       in D.
     - 5. If a module path is in U and its module description was written by a user -
       as opposed to being inferred - then the paths used in that description are
       in D, excluding those paths that only appear inside of a sig…end.
     - 6. If a type path is in U then any paths used in its equation or
       representation are in D.
     - 7. If a constructor or record field is in U then any paths used in its type
       are in D.
     - 8. If a module type path is in U then any paths used in its definition are in
       D, excluding those paths that only appear inside of a sig…end.
     - 9. If a class path is in U and its class description was written by a user -
       as opposed to being inferred - then of any paths used in that description
       are in D.
     - 10. If a class type path is in U then any paths used in its definition are in
       D.
     - 11. If a path is in D and it includes another module path within it, then that
       module path is also in D.
     - 12. If a module path m in D - note D not U - is a module alias with target n
       and another path p in D includes n within it, then the path obtained by
       substituting the m for n in p is also in D.
  *)

  (* TODO: this is not really a chain, just increasingly large jumps. This
     reduces the number of combinations when unrolling substitutions but we might
     need the missing internal aliases. *)
  let follow_aliases_adding_subst log_detail env substs path path' =
    let add_to_substs substs path path' =
      log ~title:"D12" "D12: subst %a -> %a (%s)" Logger.fmt
        (Fun.flip (Format_doc.compat Path.print) path)
        Logger.fmt
        (Fun.flip (Format_doc.compat Path.print) path')
        log_detail;
      U.add_subst substs path path'
    in
    let rec loop substs path =
      match path with
      | Path.Pident id when Ident.global id -> add_to_substs substs path path'
      | _ -> (
        match Env.find_module_lazy path env with
        | { mdl_type = MtyL_alias path1; _ } ->
          let substs = add_to_substs substs path1 path' in
          loop substs path1
        | _ -> add_to_substs substs path path')
    in
    loop substs path

  let special_rule_for_aliases env { paths; substs } path alias_lid alias_path =
    try
      let alias_path, _ =
        Env.find_module_by_name_lazy (Untypeast.lident_of_path alias_path) env
      in
      let substs =
        log ~title:"D12S" "D12S: subst %a -> %a[%a] (alias at %a)" Logger.fmt
          (Fun.flip (Format_doc.compat Path.print) path)
          Logger.fmt
          (Fun.flip Pprintast.longident alias_lid.Location.txt)
          Logger.fmt
          (Fun.flip (Format_doc.compat Path.print) alias_path)
          Logger.fmt
          (Fun.flip Location.print_loc alias_lid.loc);
        U.add_subst substs path alias_path
      in
      (* TODO: This is an unwritten rule (yet): If a module in U is an alias
         then this alias is also in U. The alias path and its module prefixes
         are returned to be processed as items of U. *)
      let more =
        U.fold_on_path_segments ~init:[] ~kind:Module
          ~f:(fun acc kind path -> ((kind, path), env) :: acc)
          alias_path
      in
      ({ paths; substs }, more)
    with Not_found -> ({ paths; substs }, [])

  (* [apparent] is the path the module is reachable under, which may be shorter
     than [path] when we got here by following an alias: the components we add
     are named under it, but identified by their real path. *)
  let d3_rule env path paths substs sig_ =
    let pdot id = Path.Pdot (path, Ident.name id) in
    List.fold_left
      (fun (paths, substs) item ->
        let add kind id =
          log ~title:"D3" "D3: signature component %s %a (%a) under module %a"
            (Shape.Sig_component_kind.to_string kind)
            Logger.fmt
            (fun fmt -> Ident.print fmt id)
            Logger.fmt
            (Fun.flip (Format_doc.compat Path.print) (pdot id))
            Logger.fmt
            (Fun.flip (Format_doc.compat Path.print) path);
          Path_trie.add (pdot id) kind paths
        in
        match (item : Subst.Lazy.signature_item) with
        | Subst.Lazy.SigL_value (id, _, _) -> (add Value id, substs)
        | SigL_type (id, _, _, _) -> (add Type id, substs)
        | SigL_typext (id, _, _, _) -> (add Extension_constructor id, substs)
        | SigL_module (id, _, _, _, _) ->
          let md = Env.find_module_lazy (pdot id) env in
          let paths = add Module id in
          let substs =
            match md.mdl_type with
            | MtyL_alias path' ->
              follow_aliases_adding_subst "sub-component is a module alias" env
                substs path' (pdot id)
            | _ -> substs
          in
          (paths, substs)
        | SigL_modtype (id, _, _) -> (add Module_type id, substs)
        | SigL_class (id, _, _, _) -> (add Class id, substs)
        | SigL_class_type (id, _, _, _) -> (add Class_type id, substs))
      (paths, substs)
      (Subst.Lazy.force_signature_once sig_)

  let module_consequences { paths; substs } env path =
    let md = Env.find_module_lazy path env in
    let { paths; substs }, more =
      match md.mdl_discourse_alias with
      | None -> ({ paths; substs }, [])
      | Some (alias_lid, (_Module, alias_path)) ->
        special_rule_for_aliases env { paths; substs } path alias_lid alias_path
    in

    (* D5. If a module path is in U and its module description was written then
       the paths used in that description are in D *)
    (* TODO : If a path is in D and it includes another module path within it,
       then that module path is also in D. *)
    log ~title:"D5" "D5: merging discourse of module %a" Logger.fmt
      (Fun.flip (Format_doc.compat Path.print) path);
    let md_discourse = trie_of_paths md.mdl_discourse in
    let paths = Path_trie.union paths md_discourse in
    match md.mdl_type with
    | MtyL_alias path' ->
      (* D12. If a module path m in D - note D not U - is a module alias
           with target n and another path p in D includes n within it, then
           the path obtained by substituting the m for n in p is also in D.

           We accumulate such substitution and will apply them when shortening
           a path. *)
      let substs =
        follow_aliases_adding_subst "Mty_alias target" env substs path' path
      in
      (* We have to follow aliases to be able to add module components to
           the discourse.

           TODO now that we have md_discourse_aliases, this might be redundant
           ? *)
      let path' = Env.normalize_module_path None env path' in
      (* TODO: Check, this might not be the same as the code before the
           rebase. *)
      ({ paths; substs }, ((Module, path'), env) :: more)
    | MtyL_signature sig_ ->
      (* D3. If a module path is in U then all the paths of its subcomponents
           are in D *)
      let paths, substs = d3_rule env path paths substs sig_ in
      ({ paths; substs }, more)
    | _ -> ({ paths; substs }, more)

  (* The consequences of an item are its contribution to D, together with the
     additional items found to be in U while computing it (alias targets). *)

  let consequences d env ((kind, path) : Item.t) =
    match kind with
    | Module_type ->
      let mtd = Env.find_modtype_lazy path env in
      (* D8. If a module type path is in U then any paths used in its definition
         are in *)
      log ~title:"D8" "D8: merging discourse of module type %a" Logger.fmt
        (Fun.flip (Format_doc.compat Path.print) path);
      (* TODO : If a path is in D and it includes another module path within it,
         then that module path is also in D. *)
      let mtd_discourse = trie_of_paths mtd.mtdl_discourse in
      ({ d with paths = Path_trie.union d.paths mtd_discourse }, [])
    | Module -> module_consequences d env path
    | Value ->
      (* D4. If a value path is in U and its value description was written by a user -
         as opposed to being inferred - then the paths used in that description are
         in D. *)
      (* TODO : If a path is in D and it includes another module path within it,
         then that module path is also in D. *)
      let vd = Env.find_value path env in
      log ~title:"D4" "D4: merging discourse of value %a" Logger.fmt
        (Fun.flip (Format_doc.compat Path.print) path);
      let val_discourse = trie_of_paths vd.val_discourse in
      ({ d with paths = Path_trie.union d.paths val_discourse }, [])
    | Type ->
      (* D6. If a type path is in U then any paths used in its equation or
         representation are in D. *)
      (* TODO : If a path is in D and it includes another module path within it,
         then that module path is also in D. *)
      let td = Env.find_type path env in
      (* What does it mean when such a path is just an ident that is local to
         another module ?*)
      log ~title:"D6" "D6: merging discourse of type %a" Logger.fmt
        (Fun.flip (Format_doc.compat Path.print) path);
      let type_discourse = trie_of_paths td.type_discourse in
      ({ d with paths = Path_trie.union d.paths type_discourse }, [])
    | _ -> (d, [])

  let add_from_u_to_d :
      discourse -> Env.t -> Item.t -> discourse * (Item.t * Env.t) list =
   fun d env ((kind, path) as item) ->
    log ~title:"D2" "D2: %a in U so in D (kind: %s)" Logger.fmt
      (Fun.flip (Format_doc.compat Path.print) path)
      (Shape.Sig_component_kind.to_string kind);
    let d =
      (* D2 items in U are in D *)
      { d with paths = Path_trie.add path kind d.paths }
    in
    (* In some cases, [consequences] tries to load its own compilation unit,
       which (inconsistently to our understanding) fails.

       For instance, the two following queries on base's float raise a [Not
       found] exception:

       {[
       /path/to/this/ocamlmerlin single type-enclosing -position '892:7' -index 0  -filename /path/to/base/src/float.ml < /path/to/base/src/float.ml

       {"class":"exception","value":"Not_found
       Raised at Ocaml_typing__Ident.find_same in file \"src/ocaml/typing/ident.ml\", line 305, characters 6-21
       [...]
       Called from Ocaml_typing__Discourse.D.module_consequences in file \"src/ocaml/typing/discourse.ml\", line 574, characters 15-44
       [...]
       ","notifications":[],"timing":{"clock":326,"cpu":234,"query":20,"pp":0,"reader":7,"ppx":41,"typer":166,"error":0},"heap_mbytes":41,"cache":{"reader_phase":"miss","ppx_phase":"miss","typer":"miss","cmt":{"hit":0,"miss":0},"cms":{"hit":0,"miss":0},"cmi":{"hit":0,"miss":53},"document_overrides_phase":"miss","locate_overrides_phase":"miss"},"query_num":0}
       ]}

       and

       {[
       /path/to/this/ocamlmerlin server type-enclosing -position '201:18' -index 0  -filename /path/to/base/src/int32.ml < /path/to/base/src/int32.ml
       ]}

       TODO: find why there is such a self-module added/why it sometimes load
       sometimes raises. *)
    try consequences d env item
    with Not_found | Env.Error (Lookup_error _) -> (d, [])

  let of_U u =
    (* Apply any unprocessed event. *)
    let u = U.force u in
    log_recap ~title:"U" "U at start of D.of_U:\n%a" Logger.fmt
      (Fun.flip U.pp_u u);
    let todo = Queue.create () in
    Item_map.iter (fun item env -> Queue.add (item, env) todo) u.U.u_paths;
    let rec loop seen d =
      match Queue.take_opt todo with
      | None -> d
      | Some (item, env) ->
        (* There is no need to process twice the same path *)
        if Paths.mem item seen then loop seen d
        else begin
          (* Processing one path might bring more work *)
          let d, more = add_from_u_to_d d env item in
          List.iter (fun discovered -> Queue.add discovered todo) more;
          loop (Paths.add item seen) d
        end
    in
    let d =
      loop Paths.empty { paths = u.U.discourse; substs = u.U.substs }
    in
    log_recap ~title:"D" "Final D:\n%a" Logger.fmt (Fun.flip pp_d d);
    d

  let pp = pp_d
end

include U

(* The public recording API *)

let use_module env lid path = record (Used { kind = Module; lid; path; env })
let use_modtype env lid path =
  record (Used { kind = Module_type; lid; path; env })
let use_type env lid path = record (Used { kind = Type; lid; path; env })
let use_value env lid path = record (Used { kind = Value; lid; path; env })
let use_constructor env lid (constr : Data_types.constructor_description) =
  record (Used_constructor { env; lid; discourse = constr.cstr_discourse })
let use_label env lid (label : Data_types.label_description) =
  record (Used_label { env; lid; discourse = label.lbl_discourse })

let add_initial_discourse () = record (Initial_discourse (Predef.discourse ()))
let define_type id = record (Defined { kind = Type; id })
let define_modtype id = record (Defined { kind = Module_type; id })
let define_module decl id = record (Defined_module { decl; id })
let define_signature sg = record (Defined_signature sg)
let open_module env path = record (Opened { env; path })

(* Process all pending U events and save the result so that this is only done
   once even if both [get] and [debug_print] are called. *)
let force_g () =
  let u = force !g in
  g := u;
  u

let get () = D.of_U (force_g ())

let debug_print fmt = D.pp fmt (D.of_U (force_g ()))
