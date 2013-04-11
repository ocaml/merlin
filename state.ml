type t = {
  pos      : Lexing.position;
  tokens   : Outline.token list;
  outlines : Outline.t;
  chunks   : Chunk.t;
  types    : Typer.t;
}

let initial = {
  pos      = Lexing.({pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0});
  tokens   = [];
  outlines = History.empty;
  chunks   = History.empty;
  types    = History.empty;
}

(* FIXME: 
 * Pathes are global, but once support for different pathes has been added to 
 * typer, this should be made a [state] wide property.
 * *)
let source_path : string list ref = ref []
let global_modules = ref (lazy [])

let reset_global_modules () =
  let paths = !Config.load_path in
  global_modules := lazy (Misc.modules_in_path ~ext:".cmi" paths)

(** Heuristic to find suitable environment to complete / type at given position.
 *  1. Try to find environment near given cursor.
 *  2. Check if there is an invalid construct between found env and cursor :
 *    Case a.
 *      > let x = valid_expr ||
 *      The env found is the right most env from valid_expr, it's a correct
 *      answer.
 *    Case b.
 *      > let x = valid_expr
 *      > let y = invalid_construction||
 *      In this case, the env found is the same as in case a, however it is
 *      preferable to use env from enclosing module rather than an env from
 *      inside x definition.
 *)
let node_at state pos_cursor =
  let structures = Misc.list_concat_map
    (fun (str,sg) -> Browse.structure str)
    (Typer.trees state.types)
  in
  let cmp o = Misc.compare_pos pos_cursor (Outline.item_start o) in
  let outlines = History.seek_backward (fun o -> cmp o < 0) state.outlines in
  try
    let node, pos_node =
      match Browse.nearest_before pos_cursor structures with
      | Some ({ Browse.loc } as node) -> node, loc.Location.loc_end
      | None -> raise Not_found
    in
    match Outline.start outlines with
      | Some pos_next when
          Misc.(compare_pos pos_next pos_node > 0 &&
                compare_pos pos_cursor pos_next > 0) ->
           raise Not_found
      | _ -> node
  with Not_found ->
    let _, chunks = History.Sync.rewind fst outlines state.chunks in
    let _, types = History.Sync.rewind fst chunks state.types in
    Browse.({ dummy with env = Typer.env types })

(* Gather all exceptions in state (warnings, syntax, env, typer, ...) *)
let exceptions state =
  Outline.exns state.outlines @ Typer.exns state.types

(* Check if module is smaller (= has less definition, counting nested ones)
 * than a particular threshold. Return (Some n) if module has size n, or None
 * otherwise (module is bigger than threshold).
 * Used to skip printing big modules in completion. *)
let rec mod_smallerthan n m =
  if n < 0 then None
  else
  let open Types in
  match m with
  | Mty_ident _ -> Some 1
  | Mty_signature s ->
    begin match Misc.length_lessthan n s with
    | None -> None
    | Some n' ->
      List.fold_left
      begin fun acc item ->
        match acc, item with
        | None, _ -> None
        | Some n', _ when n' > n -> None
        | Some n1, Sig_modtype (_,Modtype_manifest m)
        | Some n1, Sig_module (_,m,_) ->
          (match mod_smallerthan (n - n1) m with
           | Some n2 -> Some (n1 + n2)
           | None -> None)
        | Some n', _ -> Some (succ n')
      end (Some 0) s
    end
  | Mty_functor (_,m1,m2) ->
    begin
      match mod_smallerthan n m1 with
      | None -> None
      | Some n1 ->
      match mod_smallerthan (n - n1) m2 with
      | None -> None
      | Some n2 -> Some (n1 + n2)
    end

let node_complete node prefix =
  let {Browse.env} = node in
  let fmt ~exact name path ty =
    let ident = Ident.create (Path.last path) in
    let ppf, to_string = Misc.ppf_to_string () in
    let kind =
      match ty with
      | `Value v -> Printtyp.value_description ident ppf v; "Value"
      | `Cons c  ->
          Format.pp_print_string ppf name;
          Format.pp_print_string ppf " : ";
          Browse_misc.print_constructor ppf c;
          "Constructor"
      | `Label label_descr ->
          let desc =
            Types.(Tarrow ("", label_descr.lbl_res, label_descr.lbl_arg, Cok))
          in
          Format.pp_print_string ppf name;
          Format.pp_print_string ppf " : ";
          Printtyp.type_scheme ppf (Btype.newgenty desc);
          "Label"
      | `Mod m   ->
          (if exact then
             match mod_smallerthan 200 m with
               | None -> ()
               | Some _ -> Printtyp.modtype ppf m
          ); "Module"
      | `Typ t ->
          Printtyp.type_declaration ident ppf t; "Type"
    in
    let desc, info = match kind with "Module" -> "", to_string () | _ -> to_string (), "" in
    `Assoc ["name", `String name ; "kind", `String kind ; "desc", `String desc ; "info", `String info]
  in
  let seen = Hashtbl.create 7 in
  let uniq n = if Hashtbl.mem seen n
    then false
    else (Hashtbl.add seen n (); true)
  in
  let find ?path prefix compl =
    let valid n = Misc.has_prefix prefix n && uniq n in
    (* Hack to prevent extensions namespace to leak *)
    let valid name = name <> "_" && valid name in
    let compl = [] in
    try
      let compl = Env.fold_values
        (fun name path v compl ->
          if valid name then (fmt ~exact:(name = prefix) name path (`Value v)) :: compl else compl)
        path env compl
      in
      let compl = Env.fold_constructors
        (fun name path v compl ->
          if valid name then (fmt ~exact:(name = prefix) name path (`Cons v)) :: compl else compl)
        path env compl
      in
      let compl = Env.fold_types
        (fun name path v compl ->
          if valid name then (fmt ~exact:(name = prefix)  name path (`Typ v)) :: compl else compl)
        path env compl
      in
      let compl = Env.fold_modules
        (fun name path v compl ->
          if valid name then (fmt ~exact:(name = prefix)  name path (`Mod v)) :: compl else compl)
        path env compl
      in
      compl
    with
    | exn ->
      (* Our path might be of the form [Some_path.record.Real_path.prefix] which
       * would explain why the previous cases failed.
       * We only keep [Real_path] for our path. *)
      let is_lowercase c = c = Char.lowercase c in
      let rec keep_until_lowercase li =
        let open Longident in
        match li with
        | Lident id when id <> "" && not (is_lowercase id.[0]) -> Some li
        | Ldot (path, id) when id <> "" && not (is_lowercase id.[0]) ->
          begin match keep_until_lowercase path with
          | None -> Some (Lident id)
          | Some path -> Some (Ldot (path, id))
          end
        | _ -> None
      in
      begin match path with
      | None -> raise exn (* clearly the hypothesis is wrong here *)
      | Some long_ident ->
        let path = keep_until_lowercase long_ident in
        Env.fold_labels
          (fun name path l compl ->
            if valid name then (fmt ~exact:(name = prefix) name path (`Label l)) :: compl else compl)
          path env compl
      end
  in
  try
    match Longident.parse prefix with
    | Longident.Ldot (path,prefix) -> find ~path prefix []
    | Longident.Lident prefix ->
      (* Add modules on path but not loaded *)
      let compl = find prefix [] in
      begin match Misc.length_lessthan 30 compl with
      | Some _ -> List.fold_left
        begin fun compl modname ->
        let default =
          `Assoc ["name", `String modname ; "kind", `String "module"; "desc", `String "" ; "info", `String ""]
        in match modname with
        | modname when modname = prefix && uniq modname ->
            (try let p, md = Env.lookup_module (Longident.Lident modname) env in
              fmt ~exact:true modname p (`Mod md) :: compl
            with Not_found -> default :: compl)
        | modname when Misc.has_prefix prefix modname && uniq modname ->
          default :: compl
        | _ -> compl
        end compl (Lazy.force !global_modules)
      | None -> compl
      end
    | _ -> find prefix []
  with Not_found -> []

