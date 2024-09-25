open Std

type t = Trie of (string * Longident.t * t list lazy_t)

module PathSet = Set.Make (Path)

type query =
  { positive : PathSet.t; pos_fun : int; negative : PathSet.t; neg_fun : int }

let remove cost set path =
  if PathSet.mem path !set then (
    decr cost;
    set := PathSet.remove path !set)

let rec normalize_path env path =
  match Env.find_type path env with
  | exception Not_found -> path
  | decl -> (
    match decl.Types.type_manifest with
    | Some body
      when decl.Types.type_private = Asttypes.Public
           ||
           match decl.Types.type_kind with
           | Types.Type_abstract _ -> false
           | _ -> true -> begin
      match Types.get_desc body with
      | Types.Tconstr (path, _, _) -> normalize_path env path
      | _ -> path
    end
    | _ -> path)

let match_query env query t =
  let cost = ref 0 in
  let rec traverse neg neg_fun pos pos_fun t =
    incr cost;
    incr cost;
    match Types.get_desc t with
    | Types.Tconstr (path, params, _) ->
      remove cost pos (normalize_path env path);
      begin
        match Env.find_type path env with
        | exception Not_found -> ()
        | { Types.type_variance; _ } ->
          List.iter2 type_variance params ~f:(fun var arg ->
              if Types.Variance.mem Types.Variance.Inj var then (
                if Types.Variance.mem Types.Variance.Pos var then
                  traverse neg neg_fun pos pos_fun arg;
                if Types.Variance.mem Types.Variance.Neg var then
                  traverse pos pos_fun neg neg_fun arg))
      end
    | Types.Tarrow (_, t1, t2, _) ->
      decr pos_fun;
      traverse neg neg_fun pos pos_fun t2;
      traverse pos pos_fun neg neg_fun t1
    | Types.Ttuple ts -> List.iter ~f:(traverse neg neg_fun pos pos_fun) ts
    | Types.Tvar _ | Types.Tunivar _ -> decr cost (* Favor polymorphic defs *)
    | _ -> ()
  in
  let neg = ref query.negative and pos = ref query.positive in
  let neg_fun = ref query.neg_fun and pos_fun = ref query.pos_fun in
  traverse neg neg_fun pos pos_fun t;
  if
    PathSet.is_empty !pos && PathSet.is_empty !neg && !neg_fun <= 0
    && !pos_fun <= 0
  then Some !cost
  else None

let build_query ~positive ~negative env =
  let prepare r l =
    if l = Longident.Lident "fun" then (
      incr r;
      None)
    else
      let set, _ = Env.find_type_by_name l env in
      Some (normalize_path env set)
  in
  let pos_fun = ref 0 and neg_fun = ref 0 in
  let positive = List.filter_map positive ~f:(prepare pos_fun) in
  let negative = List.filter_map negative ~f:(prepare neg_fun) in
  { positive = PathSet.of_list positive;
    negative = PathSet.of_list negative;
    neg_fun = !neg_fun;
    pos_fun = !pos_fun
  }

let prepare_query env query =
  let re = Str.regexp "[ |\t]+" in
  let pos, neg =
    Str.split re query |> List.partition ~f:(fun s -> s.[0] <> '-')
  in
  let prepare s =
    Longident.parse
    @@
    if s.[0] = '-' || s.[0] = '+' then
      String.sub s ~pos:1 ~len:(String.length s - 1)
    else s
  in
  build_query env ~positive:(List.map pos ~f:prepare)
    ~negative:(List.map neg ~f:prepare)

let directories ~global_modules env =
  let rec explore lident env =
    let add_module name _ md l =
      match md.Types.md_type with
      | Types.Mty_alias _ -> l
      | _ ->
        let lident = Longident.Ldot (lident, name) in
        Trie (name, lident, lazy (explore lident env)) :: l
    in
    Env.fold_modules add_module (Some lident) env []
  in
  List.fold_left
    ~f:(fun l name ->
      let lident = Longident.Lident name in
      match Env.find_module_by_name lident env with
      | exception _ -> l
      | _ -> Trie (name, lident, lazy (explore lident env)) :: l)
    ~init:[] global_modules
(*Env.fold_modules (fun name _ _ l ->
    ignore (seen name);
    let lident = Longident.Lident name in
    Trie (name, lident, lazy (explore lident env)) :: l
  ) None env []*)

let execute_query query env dirs =
  let direct dir acc =
    Env.fold_values
      (fun _ path desc acc ->
        match match_query env query desc.Types.val_type with
        | Some cost -> (cost, path, desc) :: acc
        | None -> acc)
      dir env acc
  in
  let rec recurse acc (Trie (_, dir, children)) =
    match
      ignore (Env.find_module_by_name dir env);
      Lazy.force children
    with
    | children ->
      List.fold_left ~f:recurse ~init:(direct (Some dir) acc) children
    | exception Not_found ->
      Logger.notify ~section:"polarity-search" "%S not found"
        (String.concat ~sep:"." (Longident.flatten dir));
      acc
  in
  List.fold_left dirs ~init:(direct None []) ~f:recurse

(* [execute_query_as_type_search] runs a standard polarity_search query and map
   the result for compatibility with the type-search interface. *)
let execute_query_as_type_search ?(limit = 100) ~env ~query ~modules () =
  execute_query query env modules
  |> List.map ~f:(fun (cost, path, desc) ->
         let name =
           Printtyp.wrap_printing_env env @@ fun () ->
           let path = Printtyp.rewrite_double_underscore_paths env path in
           Format.asprintf "%a" Printtyp.path path
         in
         let doc = None in
         let loc = desc.Types.val_loc in
         let typ = desc.Types.val_type in
         let constructible = Type_search.make_constructible name typ in
         Query_protocol.{ cost; name; typ; loc; doc; constructible })
  |> List.sort ~cmp:Type_search.compare_result
  |> List.take_n limit
