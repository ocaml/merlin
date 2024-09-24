open Std
open Local_store

let { Logger.log } = Logger.for_section "Mtyper"

let index_changelog = Local_store.s_table Stamped_hashtable.create_changelog ()

type index_tbl =
  (Shape.Uid.t * Longident.t Location.loc, unit) Stamped_hashtable.t

(* Forward ref to be filled by analysis.Occurrences *)
let index_items :
    (index:index_tbl ->
    stamp:int ->
    Mconfig.t ->
    [ `Impl of Typedtree.structure_item list
    | `Intf of Typedtree.signature_item list ] ->
    unit)
    ref =
  ref (fun ~index:_ ~stamp:_ _config _item -> ())
let set_index_items f = index_items := f

type ('p, 't) item =
  { parsetree_item : 'p;
    typedtree_items : 't list * Types.signature_item list;
    part_snapshot : Types.snapshot;
    part_stamp : int;
    part_uid : int;
    part_env : Env.t;
    part_errors : exn list;
    part_checks : Typecore.delayed_check list;
    part_warnings : Warnings.state
  }

type typedtree =
  [ `Interface of Typedtree.signature | `Implementation of Typedtree.structure ]

type typedtree_items =
  [ `Interface of (Parsetree.signature_item, Typedtree.signature_item) item list
  | `Implementation of
    (Parsetree.structure_item, Typedtree.structure_item) item list ]

type typer_cache_stats = Miss | Hit of { reused : int; typed : int }

type 'a cache_result =
  { env : Env.t;
    snapshot : Types.snapshot;
    ident_stamp : int;
    uid_stamp : int;
    value : 'a;
    index : (Shape.Uid.t * Longident.t Location.loc, unit) Stamped_hashtable.t
  }

let cache : typedtree_items option cache_result option ref = s_ref None

let fresh_env config =
  let env0 = Typer_raw.fresh_env () in
  let env0 = Extension.register Mconfig.(config.merlin.extensions) env0 in
  let snap0 = Btype.snapshot () in
  let stamp0 = Ident.get_currentstamp () in
  let uid0 = Shape.Uid.get_current_stamp () in
  (env0, snap0, stamp0, uid0)

let get_cache config =
  match !cache with
  | Some ({ snapshot; _ } as c) when Types.is_valid snapshot -> c
  | Some _ | None ->
    let env, snapshot, ident_stamp, uid_stamp = fresh_env config in
    let index = Stamped_hashtable.create !index_changelog 256 in
    { env; snapshot; ident_stamp; uid_stamp; value = None; index }

let return_and_cache status =
  cache := Some { status with value = Some status.value };
  status

type result =
  { config : Mconfig.t;
    initial_env : Env.t;
    initial_snapshot : Types.snapshot;
    initial_stamp : int;
    stamp : int;
    initial_uid_stamp : int;
    typedtree : typedtree_items;
    index : (Shape.Uid.t * Longident.t Location.loc, unit) Stamped_hashtable.t;
    cache_stat : typer_cache_stats
  }

let initial_env res = res.initial_env

let get_cache_stat res = res.cache_stat

let compatible_prefix result_items tree_items =
  let rec aux acc = function
    | ritem :: ritems, pitem :: pitems
      when Types.is_valid ritem.part_snapshot
           && compare ritem.parsetree_item pitem = 0 ->
      aux (ritem :: acc) (ritems, pitems)
    | _, pitems ->
      let reused = List.length acc in
      let typed = List.length pitems in
      let cache_stat = Hit { reused; typed } in
      log ~title:"compatible_prefix" "reusing %d items, %d new items to type"
        reused typed;
      (acc, pitems, cache_stat)
  in
  aux [] (result_items, tree_items)

let rec type_structure caught env = function
  | parsetree_item :: rest ->
    let items, _, part_env =
      Typemod.merlin_type_structure env [ parsetree_item ]
    in
    let typedtree_items =
      (items.Typedtree.str_items, items.Typedtree.str_type)
    in
    let item =
      { parsetree_item;
        typedtree_items;
        part_env;
        part_snapshot = Btype.snapshot ();
        part_stamp = Ident.get_currentstamp ();
        part_uid = Shape.Uid.get_current_stamp ();
        part_errors = !caught;
        part_checks = !Typecore.delayed_checks;
        part_warnings = Warnings.backup ()
      }
    in
    item :: type_structure caught part_env rest
  | [] -> []

let rec type_signature caught env = function
  | parsetree_item :: rest ->
    let { Typedtree.sig_final_env = part_env; sig_items; sig_type } =
      Typemod.merlin_transl_signature env [ parsetree_item ]
    in
    let item =
      { parsetree_item;
        typedtree_items = (sig_items, sig_type);
        part_env;
        part_snapshot = Btype.snapshot ();
        part_stamp = Ident.get_currentstamp ();
        part_uid = Shape.Uid.get_current_stamp ();
        part_errors = !caught;
        part_checks = !Typecore.delayed_checks;
        part_warnings = Warnings.backup ()
      }
    in
    item :: type_signature caught part_env rest
  | [] -> []

let type_implementation config caught parsetree =
  let { env; snapshot; ident_stamp; uid_stamp; value = prefix; index; _ } =
    get_cache config
  in
  let prefix, parsetree, cache_stats =
    match prefix with
    | Some (`Implementation items) -> compatible_prefix items parsetree
    | Some (`Interface _) | None -> ([], parsetree, Miss)
  in
  let env', snap', stamp', uid_stamp', warn' =
    match prefix with
    | [] -> (env, snapshot, ident_stamp, uid_stamp, Warnings.backup ())
    | x :: _ ->
      caught := x.part_errors;
      Typecore.delayed_checks := x.part_checks;
      (x.part_env, x.part_snapshot, x.part_stamp, x.part_uid, x.part_warnings)
  in
  Btype.backtrack snap';
  Warnings.restore warn';
  Env.cleanup_functor_caches ~stamp:stamp';
  let stamp = List.length prefix - 1 in
  Stamped_hashtable.backtrack !index_changelog ~stamp;
  Env.cleanup_usage_tables ~stamp:uid_stamp';
  Shape.Uid.restore_stamp uid_stamp';
  let suffix = type_structure caught env' parsetree in
  let () =
    List.iteri
      ~f:(fun i { typedtree_items = items, _; _ } ->
        let stamp = stamp + i + 1 in
        !index_items ~index ~stamp config (`Impl items))
      suffix
  in
  let value = `Implementation (List.rev_append prefix suffix) in
  ( return_and_cache { env; snapshot; ident_stamp; uid_stamp; value; index },
    cache_stats )

let type_interface config caught parsetree =
  let { env; snapshot; ident_stamp; uid_stamp; value = prefix; index; _ } =
    get_cache config
  in
  let prefix, parsetree, cache_stats =
    match prefix with
    | Some (`Interface items) -> compatible_prefix items parsetree
    | Some (`Implementation _) | None -> ([], parsetree, Miss)
  in
  let env', snap', stamp', uid_stamp', warn' =
    match prefix with
    | [] -> (env, snapshot, ident_stamp, uid_stamp, Warnings.backup ())
    | x :: _ ->
      caught := x.part_errors;
      Typecore.delayed_checks := x.part_checks;
      (x.part_env, x.part_snapshot, x.part_stamp, x.part_uid, x.part_warnings)
  in
  Btype.backtrack snap';
  Warnings.restore warn';
  Env.cleanup_functor_caches ~stamp:stamp';
  let stamp = List.length prefix in
  Stamped_hashtable.backtrack !index_changelog ~stamp;
  Env.cleanup_usage_tables ~stamp:uid_stamp';
  Shape.Uid.restore_stamp uid_stamp';
  let suffix = type_signature caught env' parsetree in
  let () =
    List.iteri
      ~f:(fun i { typedtree_items = items, _; _ } ->
        let stamp = stamp + i + 1 in
        !index_items ~index ~stamp config (`Intf items))
      suffix
  in
  let value = `Interface (List.rev_append prefix suffix) in
  ( return_and_cache { env; snapshot; ident_stamp; uid_stamp; value; index },
    cache_stats )

let run config parsetree =
  if not (Env.check_state_consistency ()) then (
    (* Resetting the local store will clear the load_path cache.
       Save it now, reset the store and then restore the path. *)
    let { Load_path.visible; hidden } = Load_path.get_paths () in
    Mocaml.flush_caches ();
    Local_store.reset ();
    Load_path.reset ();
    Load_path.(init ~auto_include:no_auto_include ~visible ~hidden));
  let caught = ref [] in
  Msupport.catch_errors Mconfig.(config.ocaml.warnings) caught @@ fun () ->
  Typecore.reset_delayed_checks ();
  let cached_result, cache_stat =
    match parsetree with
    | `Implementation parsetree -> type_implementation config caught parsetree
    | `Interface parsetree -> type_interface config caught parsetree
  in
  let stamp = Ident.get_currentstamp () in
  Typecore.reset_delayed_checks ();
  { config;
    initial_env = cached_result.env;
    initial_snapshot = cached_result.snapshot;
    initial_stamp = cached_result.ident_stamp;
    stamp;
    initial_uid_stamp = cached_result.uid_stamp;
    typedtree = cached_result.value;
    index = cached_result.index;
    cache_stat
  }

let get_env ?pos:_ t =
  Option.value ~default:t.initial_env
    (match t.typedtree with
    | `Implementation l -> Option.map ~f:(fun x -> x.part_env) (List.last l)
    | `Interface l -> Option.map ~f:(fun x -> x.part_env) (List.last l))

let get_errors t =
  let errors, checks =
    Option.value ~default:([], [])
      (let f x = (x.part_errors, x.part_checks) in
       match t.typedtree with
       | `Implementation l -> Option.map ~f (List.last l)
       | `Interface l -> Option.map ~f (List.last l))
  in
  let caught = ref errors in
  Typecore.delayed_checks := checks;
  Msupport.catch_errors
    Mconfig.(t.config.ocaml.warnings)
    caught Typecore.force_delayed_checks;
  Typecore.reset_delayed_checks ();
  !caught

let get_typedtree t =
  let split_items l =
    let typd, typs = List.split (List.map ~f:(fun x -> x.typedtree_items) l) in
    (List.concat typd, List.concat typs)
  in
  match t.typedtree with
  | `Implementation l ->
    let str_items, str_type = split_items l in
    `Implementation { Typedtree.str_items; str_type; str_final_env = get_env t }
  | `Interface l ->
    let sig_items, sig_type = split_items l in
    `Interface { Typedtree.sig_items; sig_type; sig_final_env = get_env t }

let get_index t = t.index

let get_stamp t = t.stamp

let node_at ?(skip_recovered = false) t pos_cursor =
  let node = Mbrowse.of_typedtree (get_typedtree t) in
  log ~title:"node_at" "Node: %s" (Mbrowse.print () node);
  let rec select = function
    (* If recovery happens, the incorrect node is kept and a recovery node
       is introduced, so the node to check for recovery is the second one. *)
    | (_, _) :: ((_, node') :: _ as ancestors) when Mbrowse.is_recovered node'
      -> select ancestors
    | l -> l
  in
  match Mbrowse.deepest_before pos_cursor [ node ] with
  | [] -> [ (get_env t, Browse_raw.Dummy) ]
  | path when skip_recovered -> select path
  | path ->
    log ~title:"node_at" "Deepest before %s" (Mbrowse.print () path);
    path
