let log_section = "short-paths"
let { Logger.log } = Logger.for_section log_section
let { Logger.log = log_dbg } = Logger.for_section (log_section ^ "-dbg")

let path_print = Format_doc.compat Path.print

module Out_type = struct
  (* This part is copied from upstream's [Out_type] *)

  (* Normalize paths *)

  type param_subst = Id | Nth of int | Map of int list

  let compose l1 = function
    | Id -> Map l1
    | Map l2 -> Map (List.map (List.nth l1) l2)
    | Nth n -> Nth (List.nth l1 n)

  let rec index l x =
    match l with
    | [] -> raise Not_found
    | a :: l -> if Types.eq_type x a then 0 else 1 + index l x

  let rec uniq = function
    | [] -> true
    | a :: l -> (not (List.memq (a : int) l)) && uniq l

  let rec normalize_type_path ?(cache = false) env p =
    let open Types in
    try
      let params (* type_params *), ty (* type_manifest *), _ =
        (* Find the manifest type associated to a type when appropriate:
           - the type should be public or should have a private row,
           - the type should have an associated manifest type. *)
        Env.find_type_expansion p env
      in
      match get_desc ty with
      | Tconstr (p1, tyl, _) ->
        log ~title:"normalize_type_path" "Found type expansion %a for %a"
          Logger.fmt (Fun.flip path_print p1) Logger.fmt (Fun.flip path_print p);
        if
          List.length params = List.length tyl
          && List.for_all2 eq_type params tyl
          && p1 <> p (* TODO if this the correct way to prevent looping ? *)
        then normalize_type_path ~cache env p1
        else if
          cache
          || List.length params <= List.length tyl
          || not (uniq (List.map get_id tyl))
        then (Env.normalize_type_path None env p, Id)
        else
          let l1 = List.map (index params) tyl in
          let p2, s2 = normalize_type_path ~cache env p1 in
          (p2, compose l1 s2)
      | _ -> (Env.normalize_type_path None env p, Nth (index params ty))
    with Not_found ->
      log ~title:"normalize_type_path"
        "Calling [Env.normalize_type_path] for %a" Logger.fmt
        (Fun.flip path_print p);
      (Env.normalize_type_path None env p, Id)

  let find_double_underscore s =
    let len = String.length s in
    let rec loop i =
      if i + 1 >= len then None
      else if s.[i] = '_' && s.[i + 1] = '_' then Some i
      else loop (i + 1)
    in
    loop 0

  let penalty_size = 20

  let name_penalty s =
    if s <> "" && s.[0] = '_' then penalty_size
    else
      match find_double_underscore s with
      | None -> 2
      | Some _ -> penalty_size

  let penalty s =
    if s <> "" && s.[0] = '_' then 10
    else
      match find_double_underscore s with
      | None -> 1
      | Some _ -> 10
end

let rec longident_cost = function
  | Longident.Lident id -> Out_type.penalty id
  | Ldot (l, id) -> Out_type.penalty id.txt + longident_cost l.txt
  | Lapply (l1, l2) -> longident_cost l1.txt + longident_cost l2.txt

let compare_strings s1 s2 =
  let length_diff = String.length s1 - String.length s2 in
  if length_diff <> 0 then length_diff else String.compare s1 s2

let compare_longidents l1 l2 =
  let l1_cost = longident_cost l1 in
  let l2_cost = longident_cost l2 in
  let cost_diff = l1_cost - l2_cost in
  if cost_diff <> 0 then cost_diff
  else Discourse_types.compare_longidents ~compare_strings l1 l2

let ident_scope id =
  if Ident.is_predef id then Ident.highest_scope else Ident.scope id

(* Adapted from out_type. Same weight but we favorize predefs.

   TODO CR Ulysse there is room for adjustment here. Scope is not always a very
   good metric, proximity of the last usage might be better. *)
let path_size path =
  let name_penalty = Out_type.name_penalty in
  let rec size = function
    | Path.Pident id -> (name_penalty (Ident.name id), -ident_scope id)
    | Pdot (p, id) | Pextra_ty (p, Pcstr_ty id) ->
      let l, b = size p in
      (name_penalty id + l, b)
    | Papply (p1, p2) ->
      let l, b = size p1 in
      (l + fst (size p2), b)
    | Pextra_ty (p, Pext_ty) -> size p
  in
  size path

let compare_paths_weight p1 p2 =
  let l1, s1 = path_size p1 in
  let l2, s2 = path_size p2 in
  let c = l1 - l2 in
  if c <> 0 then c else s1 - s2

type kind = Type | Module | Module_type
let kind_of_kind = function
  | Type -> Shape.Sig_component_kind.Type
  | Module -> Module
  | Module_type -> Module_type
let string_of_kind = function
  | Type -> "Type"
  | Module -> "Module"
  | Module_type -> "Module_type"

module Path_set = struct
  module T = struct
    type t = kind * Path.t * Path.t

    let compare (k1, p1, fp1) (k2, p2, fp2) =
      let c = compare_paths_weight p1 p2 in
      if c <> 0 then c
      else
        let c = compare k1 k2 in
        if c <> 0 then c
        else
          let c = Path.compare p1 p2 in
          if c <> 0 then c else Path.compare fp1 fp2
  end
  let pp_elt fmt (kind, p, full_path) =
    Format.fprintf fmt "%a [%a] (%s)" path_print p path_print full_path
      (string_of_kind kind)

  include Set.Make (T)
end

module Path_tbl = Hashtbl.Make (struct
  type t = Path.t
  let equal = Path.same
  let hash = Hashtbl.hash
end)

module Uid = Shape.Uid
module Kinds = Discourse_types.Kinds
module Priority_queue = Path_set
module Path_trie = Discourse_types.Path_trie
module String_map = Discourse_types.String_map

(* To shorten paths we will build a table that maps "canonical paths" - i.e. a
   path which is not itself an alias - to a set of paths from D that are aliases
   to it. We will build this table by putting the elements of D into a priority
   queue ordered by their length. Then we "process" each element by looking up
   its canonical path and then adding it to the map. We do this using whatever
   environment we are currently trying to shorten paths with (ignoring any local
   constraints). If an element of D is not available in that environment then we
   skip it and leave it in the priority queue. *)

(* When shortenning:
   - We empty the Discourse into the priority queue.
   - We compute all possible paths substitutions and add them to the priority
     queue. TODO: there might be a better, lazier way.
   - We read the priority queue in orderto sort paths by their canonical form.
   - Once we found a valid path for the request we stop after finishing the
     current level (when the next path in the queue is longer).
   - The priority queue and canonical paths map are kept between queries. *)

let priority_queue : Priority_queue.t ref =
  Local_store.s_ref Priority_queue.empty
let not_in_env : Path_trie.t ref = Local_store.s_ref Path_trie.empty
let canon_table : Path_set.t Path_tbl.t ref =
  Local_store.s_table Path_tbl.create 256
(* TODO CR Ulysse having a table keyed by Uids would seem to be a better choice,
   but it does not always work. One example of that is test `sp-13` were a type
   from Deferred0 is redefined in Deferred, causing paths converging to this same
   type to have unmatching uids.*)

let pp_table fmt t =
  Path_tbl.iter
    (fun p paths ->
      let paths = Path_set.to_list paths in
      Format.fprintf fmt "@;%a -> {%a}" path_print p
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Path_set.pp_elt)
        paths)
    t

let normalize_type_path = Out_type.normalize_type_path ~cache:false

let apply_one_substitution ~target ~replacements (t : Path_trie.t) =
  match Path_trie.reach t target with
  | None -> Path_trie.empty
  | Some (Trie (kinds, children)) ->
    Path.Set.fold
      (fun path acc ->
        Path_trie.union acc @@ Path_trie.trie_of_path ~children path kinds)
      replacements Path_trie.empty

let apply_substitutions (substs : Discourse_types.substs) t =
  Path.Map.fold
    (fun target replacements acc ->
      let r = apply_one_substitution t ~target ~replacements in
      Path_trie.union acc r)
    substs Path_trie.empty

let apply_substitutions_fixpoint t substs =
  let rec aux ~fuel acc t =
    let new_paths = apply_substitutions substs t in
    if Path_trie.is_empty new_paths then acc
    else begin
      let acc_length = Path_trie.size acc in
      let acc = Path_trie.union acc new_paths in
      if fuel = 0 || acc_length = Path_trie.size acc then acc
      else aux ~fuel:(fuel - 1) acc new_paths
    end
  in
  aux ~fuel:2 t t

let fill_queue (paths : Path_trie.t) queue =
  Path_trie.to_seq paths
  |> Seq.fold_left
       (fun acc (path, full_paths) ->
         Discourse_types.Paths.fold
           (fun (kind, full_path) acc ->
             match kind with
             | Type -> Priority_queue.add (Type, path, full_path) acc
             | Module -> Priority_queue.add (Module, path, full_path) acc
             | Module_type ->
               Priority_queue.add (Module_type, path, full_path) acc
             | _ -> acc)
           full_paths acc)
       queue

(* This function should be called before any attempt to [shorten] paths in an
   environment different that the one of the previous attempt.

   This is called after any function wrapped with [Printyp.wrap_printing_env].
*)
let restore_ignored_paths id =
  match id with
  | None ->
    priority_queue := fill_queue !not_in_env !priority_queue;
    not_in_env := Path_trie.empty
  | Some ident -> (
    match Path_trie.take ident !not_in_env with
    | None, _ -> ()
    | Some t, remaining ->
      priority_queue := fill_queue t !priority_queue;
      not_in_env := remaining)

let find_by_name env kind lid =
  match kind with
  | Type -> fst (Env.find_type_by_name lid env)
  | Module -> fst (Env.find_module_by_name_lazy lid env)
  | Module_type -> fst (Env.find_modtype_by_name_lazy lid env)

let find_path_by_name env kind path =
  let lid = Untypeast.lident_of_path path in
  try Some (find_by_name env kind lid) with Not_found -> None

let normalize env kind path =
  match kind with
  | Type -> fst (normalize_type_path env path)
  | Module -> Env.normalize_module_path None env path
  | Module_type -> Env.normalize_modtype_path env path

let find_uid env kind path =
  match kind with
  | Type -> (Env.find_type path env).type_uid
  | Module -> (Env.find_module_lazy path env).mdl_uid
  | Module_type -> (Env.find_modtype_lazy path env).mtdl_uid

(* TODO CR Ulysse: should we instead add more informations to the discourse CMIs
   and stop using that workaround ?

   Two paths can canonicalize differently while denoting the same type
   declaration: [include] preserves declarations uid, but included types that
   are not a pure alias will not [normalize_type_path] through (see test sp-14).
   We workaround this by choosing  a single representative for canonical paths
   that share the same uid. Note that a shared uid does not necessarily means
   identity: two applications of the same functor share the uids of the
   declarations while producing different types (see test
   short-paths-functor-uid)

   We only do that for [Type] paths module aliases are already resolved by
   [Env.normalize_module_path]. *)
type uid_canon_entry =
  { repr : Path.t;
    mutable accepted : Path.Set.t;
    mutable rejected : Path.Set.t
  }

let canon_reprs : uid_canon_entry Uid.Tbl.t ref =
  Local_store.s_table Uid.Tbl.create 64

let equal_type_decls env (path, (decl : Types.type_declaration))
    (path', (decl' : Types.type_declaration)) =
  decl.type_arity = decl'.type_arity
  &&
  let mk_constr p =
    (* Same construction as in [Includecore.type_declarations]. *)
    Btype.newgenty (Types.Tconstr (p, decl.type_params, ref Types.Mnil))
  in
  Ctype.is_equal env false [ mk_constr path ] [ mk_constr path' ]

let canon_repr env kind path =
  match kind with
  | Module | Module_type -> path
  | Type -> (
    match Env.find_type path env with
    | exception Not_found -> path
    | decl when not (Uid.for_actual_declaration decl.type_uid) -> path
    | decl -> (
      match Uid.Tbl.find_opt !canon_reprs decl.type_uid with
      | None ->
        Uid.Tbl.add !canon_reprs decl.type_uid
          { repr = path;
            accepted = Path.Set.singleton path;
            rejected = Path.Set.empty
          };
        path
      | Some entry -> (
        if Path.Set.mem path entry.accepted then entry.repr
        else if Path.Set.mem path entry.rejected then path
        else
          match Env.find_type entry.repr env with
          | exception Not_found ->
            (* Repr is not reachable in the current environement: skip *)
            path
          | repr_decl ->
            if equal_type_decls env (path, decl) (entry.repr, repr_decl) then begin
              log ~title:"unify_canon" "%a has repr %a" Logger.fmt
                (Fun.flip path_print path) Logger.fmt
                (Fun.flip path_print entry.repr);
              entry.accepted <- Path.Set.add path entry.accepted;
              entry.repr
            end
            else begin
              log ~title:"unify_canon" "Cannot repr %a with %a: types differ"
                Logger.fmt (Fun.flip path_print path) Logger.fmt
                (Fun.flip path_print entry.repr);
              entry.rejected <- Path.Set.add path entry.rejected;
              path
            end)))

let find_path_by_name env kind apparent_path full_path =
  (* TODO it might be worth it to memoïze this function *)
  match find_path_by_name env kind apparent_path with
  (* TODO Env.find_type_by_name can raise Assertion_failure when the case Papply
     is reached in Env.lookup_type_path_full *)
  | None | (exception Assert_failure _) ->
    log ~title:"find_path_in_env" "%s name not found: %a" (string_of_kind kind)
      Logger.fmt
      (Fun.flip path_print apparent_path);
    None
  | Some path_in_env -> (
    log ~title:"find_path_in_env" "Lid: %a [%a] Path in env: %a" Logger.fmt
      (Fun.flip path_print apparent_path)
      Logger.fmt
      (Fun.flip path_print full_path)
      Logger.fmt
      (Fun.flip path_print path_in_env);
    if Path.compare full_path path_in_env == 0 then Some path_in_env
    else
      try
        let path = normalize env kind full_path in
        let path' = normalize env kind path_in_env in
        log ~title:"find_path_in_env" "%a <>? %a" Logger.fmt
          (Fun.flip path_print path) Logger.fmt
          (Fun.flip path_print path');
        if Path.compare path path' == 0 then Some path_in_env
        else begin
          (* [find_uid] might trigger more cmi loading. we only do it if we were
             not able to check validity by comparing the paths. *)
          let uid = find_uid env kind path in
          let uid' = find_uid env kind path' in
          log ~title:"find_path_in_env" "%a <>? %a" Logger.fmt
            (Fun.flip Uid.print uid) Logger.fmt (Fun.flip Uid.print uid');
          if Uid.compare uid uid' == 0 then Some path_in_env else None
        end
      with Not_found -> None)

(* [full_path] is used to disambiguate when a lookup by name is required *)
let find_path env kind ~full_path path =
  (* TODO CR Ulysse if required we might use a shortcut here with "find_"
     functions but these must be treated carefully as they do allow hidden
     module to be used. (breaks l-repro2 for example) *)
  find_path_by_name env kind path full_path

let check_path env kind ~full_path path =
  Option.is_some @@ find_path env kind ~full_path path

let find_best_path env ~canon_path table target_kind =
  match Path_tbl.find_opt table canon_path with
  | None -> None
  | Some paths ->
    Path_set.to_seq paths
    |> Seq.find_map (fun (kind, path, full_path) ->
        (* TODO we should probably have a kind*path map*)
        if kind <> target_kind then None
        else if check_path env kind ~full_path path then Some path
        else None)

let improve_path env ~canon_path table kind path =
  find_best_path env ~canon_path table kind
  |> Option.fold ~none:path ~some:Option.some

type state = { queue : Priority_queue.t; not_in_env : Path_trie.t }
let process_queue env state ~table ~canon_path target_kind best =
  let rec fill_by_level ~compare seq state best_path =
    log ~title:"fill_by_level" "Current best: %a" Logger.fmt (fun f ->
        Format.pp_print_option path_print f best_path);
    match seq () with
    | Seq.Nil ->
      log ~title:"fill_by_level" "Empty queue";
      let best_path =
        improve_path env ~canon_path table target_kind best_path
      in
      (best_path, state)
    | Seq.Cons (((_kind, next_path, _full_path) as item), next) ->
      let next_level = compare next_path < 0 in
      if next_level then
        begin match
          improve_path env ~canon_path table target_kind best_path
        with
        | Some best_path when compare_paths_weight best_path next_path < 0 ->
          log ~title:"fill_by_level"
            "Finished level and found a name shorter than the next level:\n %a"
            Logger.fmt
            (Fun.flip path_print best_path);
          (Some best_path, state)
        | best_path ->
          log ~title:"fill_by_level" "Finished a level. Current best: %a"
            Logger.fmt (fun f -> Format.pp_print_option path_print f best_path);
          add_lid_to_table state item next best_path
        end
      else add_lid_to_table state item next best_path
  and add_lid_to_table state ((kind, path, full_path) as item) next best_path =
    log ~title:"fill_by_level" "Treating %a (%s)" Logger.fmt
      (Fun.flip path_print path) (string_of_kind kind);
    let state =
      let is_valid_in_current_env =
        (* In the presence of `open` statements the Discourse contains partial
           paths that cannot be looked-up in the environement directly. We can
           find by name instead. However we must then check that we did actually
           find the type we were looking (same ident) for and not an homonym. *)
        find_path env kind ~full_path path
      in
      match is_valid_in_current_env with
      | Some path_in_env -> begin
        (* Even if it would feel natural to use Uids as keys in the table,
           Trying to `Env.find_*` them naively here would results in unwanted
           cmi loading. TODO: we could try to isolate these cases were that
           happen, but even then it's unclear which Uid we would use to identity
           them. *)
        let canonical_path =
          canon_repr env kind (normalize env kind full_path)
        in
        log ~title:"fill_by_level"
          "Updating table: %a -> { %a (%s) (in env: %a) }" Logger.fmt
          (fun fmt -> path_print fmt canonical_path)
          Logger.fmt
          (fun fmt -> path_print fmt path)
          (string_of_kind kind) Logger.fmt
          (fun fmt -> path_print fmt path_in_env);
        let () =
          let update =
            match Path_tbl.find_opt table canonical_path with
            | None -> Path_set.singleton item
            | Some set -> Path_set.add item set
          in
          Path_tbl.replace table canonical_path update
        in
        (* And remove it from the queue *)
        let queue = Priority_queue.remove item state.queue in
        { state with queue }
        end
      | _ -> begin
        log ~title:"fill_by_level" "Path %a invalid in the current env"
          Logger.fmt (Fun.flip path_print path);

        (* Elements not valid in the current environement are kept in a separate
           queue. This prevent uselessly re-testing them when [shorten] is
           called multiple times with the same environment. This notably happens
           when printing module signatures, and they can be quite large. *)
        let queue = Priority_queue.remove item state.queue in
        let not_in_env = Path_trie.add path Type state.not_in_env in
        { queue; not_in_env }
        end
    in
    fill_by_level ~compare:(compare_paths_weight path) next state best_path
  in
  match (Priority_queue.min_elt_opt state.queue, best) with
  | None, _ -> (best, state)
  | Some (_kind, shortest_path_in_queue, _), Some best'
    when compare_paths_weight shortest_path_in_queue best' > 0 ->
    (* There cannot be a better candidate in the queue *)
    log ~title:"process_queue" "No shorter paths in the queue. (> %a)"
      Logger.fmt
    @@ Fun.flip path_print shortest_path_in_queue;
    (best, state)
  | Some (_kind, shortest_path_in_queue, _), best ->
    let compare = compare_paths_weight shortest_path_in_queue in
    let seq = Priority_queue.to_seq state.queue in
    fill_by_level ~compare seq state best

(* Given a Path and a Longident that represents a suffix  of that path,
   [path_mask] returns the paths corresponding to that suffix. *)

(* This cache prevent the same ident to be given different stamps in the same query *)
(* todo: we should probably store the masking in the table instead. *)
let path_masks_cache : (Longident.t * Path.t, Path.t) Hashtbl.t ref =
  Local_store.s_table Hashtbl.create 32

(* The print function expect a path but we have longidents.

   TODO What we should really do is modify the printing function to expect a
   longident. *)
let rec path_mask (path : Path.t) (lid : Longident.t) : Path.t =
  match Hashtbl.find_opt !path_masks_cache (lid, path) with
  | Some path -> path
  | None ->
    let masked_path : Path.t =
      match (path, lid) with
      | Pident id, Lident _ -> Pident id
      | Pdot (p, s), Ldot (l, _) -> Pdot (path_mask p l.txt, s)
      | Papply (p1, p2), Lapply (l1, l2) ->
        Papply (path_mask p1 l1.txt, path_mask p2 l2.txt)
      | (Pdot _ | Papply _), Lident s ->
        let scope = Path.scope path in
        Pident (Ident.create_scoped ~scope s)
      | _ -> path
    in
    Hashtbl.add !path_masks_cache (lid, path) masked_path;
    masked_path

let rec shorten ~env ~initial ~canon_path kind =
  let canon_path = canon_repr env kind canon_path in
  let discourse = Discourse.get () in
  let queue, table = (!priority_queue, !canon_table) in
  log_dbg ~title:"shorten" "Current discourse: %a\n%!" Logger.fmt (fun fmt ->
      Discourse.debug_print fmt);
  log_dbg ~title:"shorten" "Current queue: %a" Logger.fmt (fun fmt ->
      Format.pp_print_seq ~pp_sep:Format.pp_print_space Path_set.pp_elt fmt
        (Priority_queue.to_seq queue));
  log_dbg ~title:"shorten" "Current notinenv: %a" Logger.fmt (fun fmt ->
      Discourse_types.Path_trie.pp_seq fmt !not_in_env);
  log_dbg ~title:"shorten" "Current table: %a" Logger.fmt (fun fmt ->
      pp_table fmt table);

  let queue =
    let paths =
      Path_trie.add initial (kind_of_kind kind) discourse.paths
      |> Path_trie.add canon_path (kind_of_kind kind)
    in
    let paths = apply_substitutions_fixpoint paths discourse.substs in
    log_dbg ~title:"shorten" "Discourse after substitutions: %a\n%!" Logger.fmt
      (fun fmt -> Path_trie.pp_seq fmt paths);
    fill_queue paths queue
  in

  log_dbg ~title:"shorten" "Current queue: %a" Logger.fmt (fun fmt ->
      Format.pp_print_seq ~pp_sep:Format.pp_print_space Path_set.pp_elt fmt
        (Priority_queue.to_seq queue));

  (* Do we already have a candidate ? *)
  let best = find_best_path env ~canon_path table kind in

  log ~title:"shorten" "Initial: %a; Canon: %a; Current best: %a" Logger.fmt
    (Fun.flip path_print initial) Logger.fmt (Fun.flip path_print canon_path)
    Logger.fmt (fun f -> Format.pp_print_option path_print f best);

  (* Is there a better one in the queue ? *)
  let best_path, { queue = queue'; not_in_env = not_in_env' } =
    let not_in_env = !not_in_env in
    process_queue env { queue; not_in_env } ~table ~canon_path kind best
  in

  (* Empty the discourse *)
  Discourse.set { Discourse.empty_u with substs = discourse.substs };

  (* Update the persistent queues and table *)
  priority_queue := queue';
  not_in_env := not_in_env';

  Option.value ~default:initial best_path |> shorten_application_args env
(* match best_path with
  | None ->
    log ~title:"shorten" "Falling-back on initial path %a" Logger.fmt
      (Fun.flip path_print initial);
    initial
  | Some (lid, path) ->
    log ~title:"shorten" "%a" Logger.fmt (fun fmt ->
        Format.fprintf fmt "Masking path %a with lid %a" path_print path
          Pprintast.longident lid);
    path_mask path lid *)

and shorten_application_args env path =
  match (path : Path.t) with
  | Pident _ -> path
  | Pdot (p, s) ->
    let p' = shorten_application_args env p in
    if p' == p then path else Pdot (p', s)
  | Pextra_ty (p, extra) ->
    let p' = shorten_application_args env p in
    if p' == p then path else Pextra_ty (p', extra)
  | Papply (p1, p2) ->
    let p1' = find_module env p1 in
    let p2' = find_module env p2 in
    if p1' == p1 && p2' == p2 then path else Papply (p1', p2')

and find_module env initial =
  let canon_path = Env.normalize_module_path None env initial in
  shorten ~env ~initial ~canon_path Module

type type_result = Short_paths.type_result =
  | Nth of int
  | Path of int list option * Path.t

type type_resolution = Short_paths.type_resolution =
  | Nth of int
  | Subst of int list
  | Id

let find_type env initial : type_result =
  match normalize_type_path env initial with
  | _, Nth i -> (* TODO, this looks like this is incorrect *) Nth i
  | canon_path, Id -> Path (None, shorten ~env ~initial ~canon_path Type)
  | canon_path, Map l -> Path (Some l, shorten ~env ~initial ~canon_path Type)

let find_type_resolution env path : type_resolution =
  match normalize_type_path env path with
  | _, Nth i -> Nth i
  | _, Id -> Id
  | _, Map l -> Subst l

let find_type_simple env initial =
  let canon_path, _subst = normalize_type_path env initial in
  let short = shorten ~env ~initial ~canon_path Type in
  short

let find_module_type env initial =
  let canon_path = Env.normalize_modtype_path env initial in
  shorten ~env ~initial ~canon_path Module_type
