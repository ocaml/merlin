type state = {
  pos      : Lexing.position;
  tokens   : Outline.token list;
  outlines : Outline.t;
  chunks   : Chunk.t;
  types    : Typer.t;
}

let initial_state = {
  pos      = Lexing.({pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0});
  tokens   = [];
  outlines = History.empty;
  chunks   = History.empty;
  types    = History.empty;
}

type handler = Protocol.io -> state -> Json.json list -> state * Json.json
type t = { name : string ; handler : handler ; doc : string }
let invalid_arguments () = failwith "invalid arguments"

let commands : (string,t) Hashtbl.t = Hashtbl.create 11
let register cmd = Hashtbl.add commands cmd.name cmd

(* FIXME: cleanup: move path management in a dedicated module *)
let source_path = ref []
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
let env_at state pos_cursor =
  let structures = Misc.list_concat_map
    (fun (str,sg) -> Browse.Envs.structure str)
    (Typer.trees state.types)
  in
  let outlines' = History.move 2 (Outline.seek_before pos_cursor state.outlines) in
  try
    let pos_browsed, env = match Browse.browse_near pos_cursor structures with
      | Some ({ Location.loc_end },env,_) -> loc_end, env
      | None -> raise Not_found
    in
    let open Lexing in
    match Outline.start outlines' with
      | Some pos_next when
         Misc.(compare_pos pos_next pos_browsed > 0 && compare_pos pos_cursor pos_next > 0) ->
           raise Not_found
      | _ -> env
  with Not_found ->
    let _, chunks = History.Sync.rewind fst outlines' state.chunks in
    let _, types = History.Sync.rewind fst chunks state.types in
    Typer.env types

(* Gather all exceptions in state (warnings, syntax, env, typer, ...) *)
let exceptions_in state =
  Outline.exns state.outlines @ Typer.exns state.types

let command_tell = {
  name = "tell";

  doc = String.concat "\n" [
    "Use ['tell','struct','...ocaml impl...'] to send source code from editor to merlin.";
    "The command answers the boolean 'true' if parsing ended, 'false' if it needs more input.";
    "In this case, the only allowed command is ['tell','struct','next source content'].";
    "To interrupt the parser, run ['tell','struct',''] (meaning eof) or ['tell','struct',null].";
  ];

  handler = begin fun (i,o) state -> function
  | [`String "struct" ; `String source] ->
      Env.reset_missing_cmis ();
      let eod = ref false and eot = ref false in
      let lexbuf = Misc.lex_strings source
        begin fun () ->
          if !eot then ""
          else try
            o (Protocol.return (`Bool false));
            match Stream.next i with
              | `List [`String "tell" ; `String "struct" ; `String source] -> source
              | `List [`String "tell" ; `String "end" ; `String source] -> eod := true; source
              | `List [`String "tell" ; `String "end" ; `Null] -> eod := true; eot := true; ""
              | `List [`String "tell" ; `String "struct" ; `Null] -> eot := true; ""
                (* FIXME: parser catch this Failure. It should not *)
              | _ -> invalid_arguments ()
          with
            Stream.Failure -> invalid_arguments ()
        end
      in
      let rec loop state =
        let bufpos = ref state.pos in
        let outlines, chunks, types =
          (History.cutoff state.outlines),
          (History.cutoff state.chunks),
          (History.cutoff state.types)
        in
        let tokens, outlines =
          Outline.parse ~bufpos
            (History.of_list state.tokens) outlines lexbuf
        in
        let chunks = Chunk.sync outlines chunks in
        let types = Typer.sync chunks types in
        let tokens = History.nexts tokens in
        let pos = !bufpos in
        let state' = { tokens ; outlines ; chunks ; types ; pos } in
        if !eod || (!eot && state.tokens = state'.tokens)
        then state'
        else loop state'
      in
      let state = loop state in
      state, `Bool true
  | _ -> invalid_arguments ()
  end;
}

let command_type = {
  name = "type";

  doc = String.concat "\n" [
    "Returns the type of an expression as a string.";
    "['type','expression','... ml expression'] tries to type expression in global env.";
    "['type','at',{'line':l,'col':c}] returns the type of the expression at given position(BUGGY)";
  ];

  handler =
  let type_in_env env ppf expr =
    let lexbuf = Lexing.from_string expr in
    let print_expr expression =
      let (str, sg, _) =
        Typemod.type_toplevel_phrase env
          Parsetree.([{ pstr_desc = Pstr_eval expression ; pstr_loc = Location.curr lexbuf }])
      in
      (*let sg' = Typemod.simplify_signature sg in*)
      let open Typedtree in
      begin match str.str_items with
        | [ { str_desc = Tstr_eval exp }] ->
            Printtyp.type_scheme ppf exp.exp_type;
        | _ -> failwith "unhandled expression"
      end
    in
    begin match Chunk_parser.top_expr Lexer.token lexbuf with
      | { Parsetree.pexp_desc = Parsetree.Pexp_construct (longident,None,_) } ->
        begin
          try let _, c = Env.lookup_constructor longident.Asttypes.txt env in
            Browse.print_constructor ppf c
          with Not_found ->
          try let _, m = Env.lookup_module longident.Asttypes.txt env in
           Printtyp.modtype ppf m
          with Not_found ->
          try let p, m = Env.lookup_modtype longident.Asttypes.txt env in
           Printtyp.modtype_declaration (Ident.create (Path.last p)) ppf m
          with Not_found ->
            ()
        end
      | { Parsetree.pexp_desc = Parsetree.Pexp_ident longident } as e ->
        begin
          try print_expr e
          with exn ->
          try let p, t = Env.lookup_type longident.Asttypes.txt env in
           Printtyp.type_declaration (Ident.create (Path.last p)) ppf t
          with _ ->
            raise exn
        end
      | e -> print_expr e
    end
  in
  begin fun _ state -> function
  | [`String "expression"; `String expr] ->
      let env = Typer.env state.types in
      let ppf, to_string = Misc.ppf_to_string () in
      type_in_env env ppf expr;
      state, `String (to_string ())

  | [`String "expression"; `String expr; `String "at" ; jpos] ->
    let env = env_at state (Protocol.pos_of_json jpos) in
    let ppf, to_string = Misc.ppf_to_string () in
    type_in_env env ppf expr;
    state, `String (to_string ())

  | [`String "at" ; jpos] ->
    let pos = Protocol.pos_of_json jpos in
    let structures = Misc.list_concat_map
      (fun (str,sg) -> Browse.Envs.structure str)
      (Typer.trees state.types)
    in
    let t, loc = match Browse.browse_near pos structures with
      | Some (loc,_,t) -> t, loc
      | None -> raise Not_found
    in
    let ppf, to_string = Misc.ppf_to_string () in
    begin match t with
      | Browse.Envs.Other -> raise Not_found
      | Browse.Envs.Expr e -> Printtyp.type_expr ppf e
      | Browse.Envs.Type t -> Printtyp.type_declaration (Ident.create "_") ppf t
      | Browse.Envs.Module m -> Printtyp.modtype ppf m
      | Browse.Envs.Modtype m -> Printtyp.modtype_declaration (Ident.create "_") ppf m
      | Browse.Envs.Class (ident, cd) -> Printtyp.class_declaration ident ppf cd
      | Browse.Envs.ClassType (ident, ctd) ->
        Printtyp.cltype_declaration ident ppf ctd
    end;
    state, Protocol.with_location loc ["type", `String (to_string ())]

  | [`String "enclosing"; jpos] ->
    let pos = Protocol.pos_of_json jpos in
    let rec aux = function
      | Browse.Envs.T (loc,_,Browse.Envs.Expr e,_) :: rest ->
          let ppf, to_string = Misc.ppf_to_string () in
          Printtyp.type_expr ppf e;
          let item = Protocol.with_location loc ["type", `String (to_string ())]
          in
          item :: aux rest
      | _ :: rest -> aux rest
      | [] -> []
    in
    let structures = Misc.list_concat_map
      (fun (str,sg) -> Browse.Envs.structure str)
      (Typer.trees state.types)
    in
    let path = Browse.browse_enclosing pos structures in
    state, `List [`Int (List.length path); `List (aux path)]

  | _ -> invalid_arguments ()
  end;
}

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

let complete_in_env env prefix =
  let fmt ~exact name path ty =
    let ident = Ident.create (Path.last path) in
    let ppf, to_string = Misc.ppf_to_string () in
    let kind =
      match ty with
      | `Value v -> Printtyp.value_description ident ppf v; "Value"
      | `Cons c  ->
          Format.pp_print_string ppf name;
          Format.pp_print_string ppf " : ";
          Browse.print_constructor ppf c;
          "Constructor"
      | `Label label_descr ->
          let desc =
            Types.(Tarrow ("", label_descr.lbl_res, label_descr.lbl_arg, Cok))
          in
          Format.pp_print_string ppf name;
          Format.pp_print_string ppf " : ";
          Printtyp.type_expr ppf { Types. level = 0 ; id = 0 ; desc } ;
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
      (* Our path might be [Real_path.record_value.] which would explain why the
       * previous cases failed.
       * So we try to remove the identifier name from the path, and we only look
       * for a label name matching the given prefix. *)
      begin match path with
      | None -> raise exn
      | Some long_ident ->
        let path =
          match long_ident with
          | Longident.Lident _identifier -> None
          | Longident.Ldot (path, _identifier) -> Some path
          | _ -> Some long_ident
        in
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

let command_complete = {
  name = "complete";

  doc = String.concat "\n" [
    "['complete','prefix','...identifier path...'] or";
    "['complete','prefix','...identifier path...', 'at', {'line':l,'col':c}]";
    "returns possible completions in global environement or in environment";
    "surrounding given position for given prefix/path (path of the form: Module.ident)";
  ];

  handler =
  begin fun _ state -> function
  | [`String "prefix" ; `String prefix] ->
    begin
      let env = Typer.env state.types in
      let compl = complete_in_env env prefix in
      state, `List (List.rev compl)
    end
  | [`String "prefix" ; `String prefix ; `String "at" ; jpos ] ->
    let env = env_at state (Protocol.pos_of_json jpos) in
    let compl = complete_in_env env prefix in
    state, `List (List.rev compl)

  | _ -> invalid_arguments ()
  end;
}

let command_seek = {
  name = "seek";

  doc = "TODO";

  handler =
  begin fun _ state -> function
  | [`String "position"] ->
      state, Protocol.pos_to_json state.pos

    (* FIXME: Should seek position be renamed to "seek before" ? *)
  | [`String ("position" | "before") ; jpos] ->
      let pos = Protocol.pos_of_json jpos in
      let outlines = Outline.seek_before pos state.outlines in
      let outlines = History.seek_backward
        (function { Outline.kind = Outline_utils.Syntax_error _loc } -> true
                | _ -> false)
        outlines
      in
      let outlines, chunks = History.Sync.rewind fst outlines state.chunks in
      let chunks, types = History.Sync.rewind fst chunks state.types in
      let pos =
        match Outline.location outlines with
          | l when l = Location.none -> initial_state.pos
          | p -> p.Location.loc_end
      in
      { tokens = [] ; outlines ; chunks ; types ; pos },
      Protocol.pos_to_json pos

  | [`String "exact" ; jpos] ->
      let pos = Protocol.pos_of_json jpos in
      let outlines = Outline.seek_before pos state.outlines in
      let outlines = History.seek_forward
        (fun item -> Misc.compare_pos pos (Outline.item_start item) > 0)
        outlines
      in
      let outlines, chunks = History.Sync.rewind fst outlines state.chunks in
      let chunks, types = History.Sync.rewind fst chunks state.types in
      let pos =
        match Outline.location outlines with
          | l when l = Location.none -> initial_state.pos
          | p -> p.Location.loc_end
      in
      { tokens = [] ; outlines ; chunks ; types ; pos },
      Protocol.pos_to_json pos

  | [`String "end_of_definition"] ->
      failwith "TODO"

  | [`String "end"] ->
      let outlines = History.seek_forward (fun _ -> true) state.outlines in
      let chunks = History.Sync.right fst outlines state.chunks in
      let types  = History.Sync.right fst chunks state.types in
      let pos =
        match Outline.location outlines with
          | l when l = Location.none -> initial_state.pos
          | p -> p.Location.loc_end
      in
      { tokens = [] ; outlines ; chunks ; types ; pos },
      Protocol.pos_to_json pos

  | [`String "maximize_scope"] ->
      let rec find_end_of_module (depth,outlines) =
        if depth = 0 then (0,outlines)
        else
        match History.forward outlines with
          | None -> (depth,outlines)
          | Some ({ Outline.kind = Outline_utils.Leave_module },outlines') ->
              find_end_of_module (pred depth, outlines')
          | Some ({ Outline.kind = Outline_utils.Enter_module },outlines') ->
              find_end_of_module (succ depth, outlines')
          | Some (_,outlines') -> find_end_of_module (depth,outlines')
      in
      let rec loop outlines =
        match History.forward outlines with
          | None -> outlines
          | Some ({ Outline.kind = Outline_utils.Leave_module },_) ->
              outlines
          | Some ({ Outline.kind = Outline_utils.Enter_module },outlines') ->
              (match find_end_of_module (1,outlines') with
                | (0,outlines'') -> outlines''
                | _ -> outlines)
          | Some (_,outlines') -> loop outlines'
      in
      let outlines = loop state.outlines in
      let chunks = History.Sync.right fst outlines state.chunks in
      let types  = History.Sync.right fst chunks state.types in
      let pos =
        match Outline.location outlines with
          | l when l = Location.none -> initial_state.pos
          | p -> p.Location.loc_end
      in
      { tokens = [] ; outlines ; chunks ; types ; pos },
      Protocol.pos_to_json pos
  | _ -> invalid_arguments ()
  end;
}

let command_boundary = {
  name = "boundary";

  doc = "TODO";

  handler =
  begin fun _ state -> function
  | [] ->
      let boundaries =
        match Outline.location state.outlines with
          | l when l = Location.none -> `Null
          | { Location.loc_start ; Location.loc_end } ->
              `List [
                Protocol.pos_to_json loc_start;
                Protocol.pos_to_json loc_end;
              ]
      in
      state, boundaries
  | _ -> invalid_arguments ()
  end
}

let command_reset = {
  name = "reset";

  doc = "TODO";

  handler =
  begin fun _ state -> function
  | [] -> initial_state, Protocol.pos_to_json initial_state.pos
  | _ -> invalid_arguments ()
  end
}

let command_refresh = {
  name = "refresh";

  doc = "TODO";

  handler =
  begin fun _ state -> function
  | [] ->
      reset_global_modules ();
      Env.reset_cache ();
      let types = Typer.sync state.chunks History.empty in
      { state with types }, `Bool true
  | _ -> invalid_arguments ()
  end;
}

let command_cd = {
  name = "cd";
  doc = "TODO";

  handler =
  begin fun _ state -> function
  | [`String s] -> Sys.chdir s; state, `Bool true
  | _ -> invalid_arguments ()
  end;
}

let command_errors = {
  name = "errors";
  doc = "TODO";

  handler =
  begin fun _ state -> function
  | [] -> state, `List (Error_report.to_jsons (exceptions_in state))
  | _ -> invalid_arguments ()
  end;
}

let command_dump = {
  name = "dump";
  doc = "TODO";

  handler =
  let pr_item_desc items =
    (List.map (fun (s,i) -> `List [`String s;`Int i]) (Chunk.dump_chunk items))
  in
  begin fun _ state -> function
  | [`String "env"] ->
      let sg = Browse.Envs.signature_of_env (Typer.env state.types) in
      let aux item =
        let ppf, to_string = Misc.ppf_to_string () in
        Printtyp.signature ppf [item];
        let content = to_string () in
        let ppf, to_string = Misc.ppf_to_string () in
        match Browse.signature_loc item with
          | Some loc ->
              Location.print_loc ppf loc;
              let loc = to_string () in
              `List [`String loc ; `String content]
          | None -> `String content
      in
      state, `List (List.map aux sg)
  | [`String "env" ; `String "at" ; jpos ] ->
    let env = env_at state (Protocol.pos_of_json jpos) in
    let sg = Browse.Envs.signature_of_env env in
    let aux item =
      let ppf, to_string = Misc.ppf_to_string () in
      Printtyp.signature ppf [item];
      let content = to_string () in
      let ppf, to_string = Misc.ppf_to_string () in
      match Browse.signature_loc item with
        | Some loc ->
            Location.print_loc ppf loc;
            let loc = to_string () in
            `List [`String loc ; `String content]
        | None -> `String content
    in
    state, `List (List.map aux sg)
  | [`String "sig"] ->
      let trees = Typer.trees state.types in
      let sg = List.flatten (List.map snd trees) in
      let aux item =
        let ppf, to_string = Misc.ppf_to_string () in
        Printtyp.signature ppf [item];
        let content = to_string () in
        let ppf, to_string = Misc.ppf_to_string () in
        match Browse.signature_loc item with
          | Some loc ->
              Location.print_loc ppf loc;
              let loc = to_string () in
              `List [`String loc ; `String content]
          | None -> `String content
      in
      state, `List (List.map aux sg)
  | [`String "chunks"] ->
      state, `List (pr_item_desc state.chunks)
  | [`String "tree"] ->
      let structures = Misc.list_concat_map
        (fun (str,sg) -> Browse.Envs.structure str)
        (Typer.trees state.types)
      in
      state, Browse.dump_envs structures
  | _ -> invalid_arguments ()
  end;
}

let command_which = {
  name = "which";
  doc = "TODO";

  handler =
  begin fun _ state -> function
  | [`String "path" ; `String s] ->
      let filename =
        try Misc.find_in_path_uncap !source_path s
        with Not_found ->
          Misc.find_in_path_uncap !Config.load_path s
      in
      state, `String filename
  | [`String "with_ext" ; `String ext] ->
      let results = Misc.modules_in_path ~ext !source_path in
      state, `List (List.map (fun s -> `String s) results)
  | _ -> invalid_arguments ()
  end;
}

let command_find = {
  name = "find";
  doc = "TODO";

  handler =
  begin fun _ state -> function
  | [`String "use" ; `List packages]
  | (`String "use" :: packages) ->
      let packages = List.map
        (function `String pkg -> pkg | _ -> invalid_arguments ())
        packages
      in
      let packages = Findlib.package_deep_ancestors [] packages in
      let path = List.map Findlib.package_directory packages in
      Config.load_path := Misc.list_filter_dup (path @ !Config.load_path);
      reset_global_modules ();
      state, `Bool true
  | [`String "list"] ->
      state, `List (List.rev_map (fun s -> `String s) (Fl_package_base.list_packages ()))
  | _ -> invalid_arguments ()
  end;
}

let command_help = {
  name = "help";
  doc = "List known commands with synopsis and small description";

  handler =
  begin fun _ state -> function
  | [] ->
      let helps = Hashtbl.fold
        (fun name { doc } cmds -> (name, `String doc) :: cmds)
        commands []
      in
      state, `Assoc helps
  | _ -> invalid_arguments ()
  end;
}

let _ = List.iter register [
  command_tell; command_seek; command_reset; command_refresh;
  command_cd; command_type; command_complete; command_boundary;
  command_errors; command_dump;
  command_which; command_find;
  command_help;
]
