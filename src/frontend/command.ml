(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2014  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

open Std
open Misc

open Protocol
open Merlin_lib

type state = {
  mutable buffer : Buffer.t;
  mutable lexer : Lexer.t option;

  mutable verbosity_last : Obj.t option;
  mutable verbosity : int;
}

let new_state () =
  let buffer = Buffer.create Parser.implementation in
  {buffer; lexer = None; verbosity_last = None; verbosity = 0}

let checkout_buffer_cache = ref []
let checkout_buffer =
  let cache_size = 8 in
  fun ?dot_merlins ?path ft ->
    try
      match path with
      | None -> raise Not_found
      | Some path -> List.assoc (ft,path,dot_merlins) !checkout_buffer_cache
    with Not_found ->
      let parser = match ft with
        | `ML -> Raw_parser.implementation_state
        | `MLI ->  Raw_parser.interface_state
      in
      let buffer = Buffer.create ?dot_merlins ?path parser in
      begin match path with
        | Some path ->
          checkout_buffer_cache := ((ft,path,dot_merlins), buffer) ::
                                   List.take_n cache_size !checkout_buffer_cache
        | None -> ()
      end;
      begin match path with
        | Some path when Filename.check_suffix path "myocamlbuild.ml" ->
          let project = Buffer.project buffer in
          (* Failure is not an issue. *)
          ignore @@ Project.User.load_packages project ["ocamlbuild"]
        | _ -> ()
      end;
      buffer

let with_typer buffer f =
  let typer = Buffer.typer buffer in
  Typer.with_typer typer (fun () -> f typer)

let cursor_state state =
  let cursor, marker =
    match state.lexer with
    | None ->
      Lexer.item_end (snd (History.focused (Buffer.lexer state.buffer))),
      false
    | Some lexer ->
      Lexer.position lexer,
      Buffer.has_mark state.buffer (Lexer.get_mark lexer)
  in
  { cursor; marker }

let track_verbosity =
  let classify (type a) (request : a request) =
    let obj = Some (Obj.repr request) in
    match request with
    | Type_expr _ -> obj
    | Type_enclosing _ -> obj
    | Enclosing _ -> obj
    | Complete_prefix _ -> obj
    | Expand_prefix _ -> obj
    | _ -> None in
  fun state (type a) (request : a request) ->
    match classify request with
    | None -> 0
    | value when state.verbosity_last = value ->
      state.verbosity <- state.verbosity + 1;
      state.verbosity
    | value ->
      state.verbosity_last <- value;
      state.verbosity <- 0;
      0

let buffer_update state items =
  if Buffer.update state.buffer items = `Updated then
    state.verbosity_last <- None

let buffer_freeze state items =
  buffer_update state items;
  state.lexer <- None

module Printtyp = Type_utils.Printtyp

exception Unhandled_command

let dispatch_query ~verbosity buffer =
  fun (type a) (request : a request) ->
  (match request with
  | (Type_expr (source, pos) : a request) ->
    with_typer buffer @@ fun typer ->
    let env = match pos with
      | None -> Typer.env typer
      | Some pos ->
        let node, _ancestors = Browse.node_at typer pos in
        node.BrowseT.t_env
    in
    let ppf, to_string = Format.to_string () in
    ignore (Type_utils.type_in_env ~verbosity env ppf source : bool);
    to_string ()

  | (Type_enclosing (expro, pos) : a request) ->
    let open BrowseT in
    let open Typedtree in
    let open Override in
    with_typer buffer @@ fun typer ->
    let structures = Typer.contents typer in
    let structures = Browse.of_typer_contents structures in
    let path = Browse.enclosing pos structures in
    let path = Browse.annotate_tail_calls_from_leaf path in
    let aux (t,tail) =
      let { t_loc ; t_env ; t_node ; _ } = t in
      match t_node with
      | Expression {exp_type = t}
      | Pattern {pat_type = t}
      | Core_type {ctyp_type = t}
      | Value_description { val_desc = { ctyp_type = t } } ->
        let ppf, to_string = Format.to_string () in
        Printtyp.wrap_printing_env t_env ~verbosity
          (fun () -> Type_utils.print_type_with_decl ~verbosity t_env ppf t);
        Some (t_loc, to_string (), tail)

      | Type_declaration { typ_id = id; typ_type = t} ->
        let ppf, to_string = Format.to_string () in
        Printtyp.wrap_printing_env t_env ~verbosity
          (fun () -> Printtyp.type_declaration t_env id ppf t);
        Some (t_loc, to_string (), tail)

      | Module_expr {mod_type = m}
      | Module_type {mty_type = m}
      | Module_binding {mb_expr = {mod_type = m}}
      | Module_declaration {md_type = {mty_type = m}}
      | Module_type_declaration {mtd_type = Some {mty_type = m}}
      | Module_binding_name {mb_expr = {mod_type = m}}
      | Module_declaration_name {md_type = {mty_type = m}}
      | Module_type_declaration_name {mtd_type = Some {mty_type = m}} ->
        let ppf, to_string = Format.to_string () in
        Printtyp.wrap_printing_env t_env ~verbosity
          (fun () -> Printtyp.modtype t_env ppf m);
        Some (t_loc, to_string (), tail)

      | _ -> None
    in
    let result = List.filter_map ~f:aux path in
    (* enclosings of cursor in given expression *)
    let exprs =
      match expro with
      | None ->
        let lexer = Buffer.lexer buffer in
        let lexer =
          History.seek_backward
            (fun (_,item) -> Lexing.compare_pos pos (Lexer.item_start item) < 0)
            lexer
        in
        let path = Lexer.reconstruct_identifier lexer in
        let path = Lexer.identifier_suffix path in
        begin match path with
        | [] -> []
        | base :: tail ->
          let f {Location. txt=base; loc=bl} {Location. txt=dot; loc=dl} =
            let loc = Parsing_aux.location_union bl dl in
            let txt = base ^ "." ^ dot in
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
            { Location.
              loc_start ;
              loc_end = shift loc_start (String.length source) ;
              loc_ghost = false ;
            } in
          Location.mkloc source loc
        in
        let len = String.length expr in
        let rec aux acc i =
          if i >= len then
            List.rev_map ~f:add_loc (expr :: acc)
          else if expr.[i] = '.' then
            aux (String.sub expr ~pos:0 ~len:i :: acc) (succ i)
          else
            aux acc (succ i) in
        aux [] offset
    in
    let small_enclosings =
      let node, _ = Browse.node_at typer pos in
      let env = node.BrowseT.t_env in
      let include_lident = match node.BrowseT.t_node with
        | BrowseT.Pattern _ -> false
        | _ -> true
      in
      let include_uident = match node.BrowseT.t_node with
        | BrowseT.Module_binding _
        | BrowseT.Module_binding_name _
        | BrowseT.Module_declaration _
        | BrowseT.Module_declaration_name _
        | BrowseT.Module_type_declaration _
        | BrowseT.Module_type_declaration_name _
          -> false
        | _ -> true
      in
      List.filter_map exprs ~f:(fun {Location. txt = source; loc} ->
          match source with
          | "" -> None
          | source when not include_lident && Char.is_lowercase source.[0] ->
            None
          | source when not include_uident && Char.is_uppercase source.[0] ->
            None
          | source ->
            try
              let ppf, to_string = Format.to_string () in
              if Type_utils.type_in_env ~verbosity env ppf source then
                Some (loc, to_string (), `No)
              else
                None
            with _ ->
              None
        )
    in
    let normalize ({Location. loc_start; loc_end}, text, _tail) =
        Lexing.split_pos loc_start, Lexing.split_pos loc_end, text in
    List.merge_cons
      ~f:(fun a b ->
          (* Tail position is computed only on result, and result comes last
             As an approximation, when two items are similar, we returns the
             rightmost one *)
          if normalize a = normalize b then Some b else None)
      (small_enclosings @ result)

  | (Enclosing pos : a request) ->
    with_typer buffer @@ fun typer ->
    let structures = Typer.contents typer in
    let structures = Browse.of_typer_contents structures in
    let path = Browse.enclosing pos structures in
    List.map (fun t -> t.BrowseT.t_loc) path

  | (Complete_prefix (prefix, pos, with_doc) : a request) ->
    let complete ~no_labels typer =
      let node, ancestors = Browse.node_at ~skip_recovered:true typer pos in
      let target_type, context =
        Completion.application_context ~verbosity ~prefix node ancestors
      in
      let get_doc =
        if not with_doc then None else
        let project    = Buffer.project buffer in
        let comments   = Buffer.comments buffer in
        let source     = Buffer.unit_name buffer in
        let local_defs = Typer.contents typer in
        Some (
          Track_definition.get_doc ~project ~env:node.BrowseT.t_env ~local_defs
            ~comments ~pos source
        )
      in
      let entries =
        Completion.node_complete ?get_doc ?target_type buffer node prefix
      and context = match context with
        | `Application context when no_labels ->
          `Application {context with Protocol.Compl.labels = []}
        | context -> context
      in
      {Compl. entries = List.rev entries; context }
    in
    let lexer0 = Buffer.lexer buffer in
    let lexer =
      History.seek_backward
        (fun (_,item) -> Lexing.compare_pos pos (Lexer.item_start item) <= 0)
        lexer0
    in
    let lexer =
      History.seek_forward ~last:true
        (fun (_,item) -> Lexing.compare_pos (Lexer.item_end item) pos <= 0)
        lexer
    in
    let need_token, no_labels =
      let open Raw_parser in
      let exns, item = History.focused lexer in
      let loc = Lexer.item_location item in
      let need_token =
        if Parsing_aux.compare_pos pos loc = 0 &&
           (match item with
            | Lexer.Valid (_, (LIDENT _ | UIDENT _), _) -> true
            | _ -> false)
        then
          None
        else
          Some (exns, Lexer.Valid (pos, LIDENT "", pos))
      and no_labels =
        (* Cursor is already over a label, don't suggest another one *)
        match item with
        | Lexer.Valid (_, (LABEL _ | OPTLABEL _), _) -> true
        | _ -> false
      in
      need_token, no_labels
    in
    begin match need_token with
    | None -> with_typer buffer (complete ~no_labels)
    | Some token ->
      (* Setup fake AST *)
      let lexer' = History.fake_insert token lexer in
      let lexer' = History.seek (History.position lexer0 + 1) lexer' in
      ignore (Buffer.update buffer lexer' : [> ]);
      try_finally
        (* Complete on adjusted buffer *)
        (fun () -> with_typer buffer (complete ~no_labels))
        (* Restore original buffer *)
        (fun () -> ignore (Buffer.update buffer lexer0 : [> ]))
    end

  | (Expand_prefix (prefix, pos) : a request) ->
    with_typer buffer @@ fun typer ->
    let env =
      let node, _ = Browse.node_at typer pos in
      node.BrowseT.t_env
    in
    let global_modules = Buffer.global_modules buffer in
    let entries = Completion.expand_prefix env ~global_modules prefix in
    { Compl. entries ; context = `Unknown }

  | (Document (patho, pos) : a request) ->
    with_typer buffer @@ fun typer ->
    let comments = Buffer.comments buffer in
    let env, local_defs =
      let node, _ = Browse.node_at typer pos in
      node.BrowseT.t_env, Typer.contents typer
    in
    let path =
      match patho with
      | Some p -> p
      | None ->
        let lexer = Buffer.lexer buffer in
        let lexer =
          History.seek_backward (fun (_,item) ->
            Lexing.compare_pos pos (Lexer.item_start item) < 0) lexer
        in
        let path = Lexer.reconstruct_identifier ~for_locate:true lexer in
        let path = Lexer.identifier_suffix path in
        let path = List.map ~f:(fun {Location. txt} -> txt) path in
        String.concat ~sep:"." path
    in
    if path = "" then `Invalid_context else
    let source  = Buffer.unit_name buffer in
    let project = Buffer.project buffer in
    Track_definition.get_doc ~project ~env ~local_defs ~comments ~pos source
      (`User_input path)

  | (Locate (patho, ml_or_mli, pos) : a request) ->
    with_typer buffer @@ fun typer ->
    let env, local_defs =
      let node, _ = Browse.node_at typer pos in
      node.BrowseT.t_env, Typer.contents typer
    in
    let path =
      match patho with
      | Some p -> p
      | None ->
        let lexer = Buffer.lexer buffer in
        let lexer =
          History.seek_backward (fun (_,item) ->
            Lexing.compare_pos pos (Lexer.item_start item) < 0) lexer
        in
        let path = Lexer.reconstruct_identifier ~for_locate:true lexer in
        let path = Lexer.identifier_suffix path in
        let path = List.map ~f:(fun {Location. txt} -> txt) path in
        String.concat ~sep:"." path
    in
    if path = "" then `Invalid_context else
    let project = Buffer.project buffer in
    begin match
      Track_definition.from_string ~project ~env ~local_defs ~pos ml_or_mli path
    with
    | `Found (file, pos) ->
      Logger.info (Track_definition.section)
        (Option.value ~default:"<local buffer>" file);
      `Found (file, pos)
    | otherwise -> otherwise
    end

  | (Case_analysis ({ Location. loc_start ; loc_end } as loc) : a request) ->
    with_typer buffer @@ fun typer ->
    let env = Typer.env typer in
    Printtyp.wrap_printing_env env ~verbosity @@ fun () ->
    let structures = Typer.contents typer in
    let structures = Browse.of_typer_contents structures in
    let enclosings = Browse.enclosing loc_start structures in
    begin match
        List.drop_while enclosings ~f:(fun t ->
            Lexing.compare_pos t.BrowseT.t_loc.Location.loc_end loc_end < 0
          )
      with
      | [] -> failwith "No node at given range"
      | node :: parents -> Destruct.node ~loc parents node
    end

  | (Outline : a request) ->
    with_typer buffer @@ fun typer ->
    let typed_tree = Typer.contents typer in
    Outline.get (Browse.of_typer_contents typed_tree)

  | (Boundary (dir,pos) : a request) ->
    let get_enclosing_str_item pos browses =
      let enclosings = Browse.enclosing pos browses in
      match
        List.drop_while enclosings ~f:(fun t ->
          match t.BrowseT.t_node with
          | BrowseT.Structure_item _
          | BrowseT.Signature_item _ -> false
          | _ -> true
        )
      with
      | [] -> None
      | item :: _ -> Some item
    in
    with_typer buffer @@ fun typer ->
    let browses  = Browse.of_typer_contents (Typer.contents typer) in
    Option.bind (get_enclosing_str_item pos browses) ~f:(fun item ->
        match dir with
        | `Current -> Some item.BrowseT.t_loc
        | `Prev ->
          let pos = item.BrowseT.t_loc.Location.loc_start in
          let pos = Lexing.({ pos with pos_cnum = pos.pos_cnum - 1 }) in
          let item= get_enclosing_str_item pos browses in
          Option.map item ~f:(fun i -> i.BrowseT.t_loc)
        | `Next ->
          let pos = item.BrowseT.t_loc.Location.loc_end in
          let pos = Lexing.({ pos with pos_cnum = pos.pos_cnum + 1 }) in
          let item= get_enclosing_str_item pos browses in
          Option.map item ~f:(fun i -> i.BrowseT.t_loc)
      )

  | (Errors : a request) ->
    begin
      with_typer buffer @@ fun typer ->
      Printtyp.wrap_printing_env (Typer.env typer) ~verbosity @@ fun () ->
      try
        let typer = Buffer.typer buffer in
        let cmp (l1,_) (l2,_) =
          Lexing.compare_pos l1.Location.loc_start l2.Location.loc_start in
        let err exns =
          List.filter ~f:(fun (l,err) ->
            not l.Location.loc_ghost || err.Error_report.where <> "warning"
          ) @@
          List.sort_uniq ~cmp (List.map ~f:Error_report.of_exn exns)
        in
        let err_lexer  = err (Buffer.lexer_errors buffer) in
        let err_parser = err (Buffer.parser_errors buffer) in
        let err_typer  =
          (* When there is a cmi error, we will have a lot of meaningless errors,
           * there is no need to report them. *)
          let exns = Typer.exns typer @ Typer.delayed_checks typer in
          let exns =
            let cmi_error = function Cmi_format.Error _ -> true | _ -> false in
            try [ List.find exns ~f:cmi_error ]
            with Not_found -> exns
          in
          err exns
        in
        (* Return parsing warnings & first parsing error,
           or type errors if no parsing errors *)
        let rec extract_warnings acc = function
          | (_,{Error_report. where = "warning"; _ }) as err :: errs ->
            extract_warnings (err :: acc) errs
          | err :: _ ->
            List.rev (err :: acc),
            List.take_while ~f:(fun err' -> cmp err' err < 0) err_typer
          | [] ->
            List.rev acc, err_typer in
        (* Filter duplicate error messages *)
        let err_parser, err_typer = extract_warnings [] err_parser in
        let errors =
          List.map ~f:snd @@
          List.merge ~cmp err_lexer @@
          List.merge ~cmp err_parser err_typer
        in
        Error_report.flood_barrier errors
      with exn -> match Error_report.strict_of_exn exn with
        | None -> raise exn
        | Some (_loc, err) -> [err]
    end

  | (Dump `Parser : a request) ->
    Merlin_recover.dump (Buffer.recover buffer);

  | (Dump (`Typer `Input) : a request) ->
    with_typer buffer @@ fun typer ->
    let ppf, to_string = Format.to_string () in
    Typer.dump ppf typer;
    `String (to_string ())

  | (Dump (`Typer `Output) : a request) ->
    with_typer buffer @@ fun typer ->
    let ppf, to_string = Format.to_string () in
    List.iter (fun (content,_) -> match content with
        | `Sg sg -> Printtyped.interface ppf sg
        | `Str str -> Printtyped.implementation ppf str
        | `Fail (_,loc) ->
          Format.fprintf ppf "<failed to type at %a>\n"
            Location.print loc
      ) (Typer.contents typer);
    `String (to_string ())

  | (Dump `Recover : a request) ->
    Merlin_recover.dump_recoverable (Buffer.recover buffer);

  | (Dump (`Env (kind, pos)) : a request) ->
    with_typer buffer @@ fun typer ->
    let env = match pos with
      | None -> Typer.env typer
      | Some pos ->
        let node, _ = Browse.node_at typer pos in
        node.BrowseT.t_env
    in
    let sg = Browse_misc.signature_of_env ~ignore_extensions:(kind = `Normal) env in
    let aux item =
      let ppf, to_string = Format.to_string () in
      Printtyp.signature ppf [item];
      let content = to_string () in
      let ppf, to_string = Format.to_string () in
      match Raw_compat.signature_loc item with
      | Some loc ->
        Location.print_loc ppf loc;
        let loc = to_string () in
        `List [`String loc ; `String content]
      | None -> `String content
    in
    `List (List.map ~f:aux sg)

  | (Dump `Browse : a request) ->
    with_typer buffer @@ fun typer ->
    let structures = Typer.contents typer in
    let structures = Browse.of_typer_contents structures in
    Browse_misc.dump_ts structures

  | (Dump `Tokens : a request) ->
    let tokens = Buffer.lexer buffer in
    let tokens = History.seek_backward (fun _ -> true) tokens in
    let tokens = History.tail tokens in
    `List (List.filter_map tokens
             ~f:(fun (_exns,item) -> match item with
             | Lexer.Error _ -> None
             | Lexer.Valid (s,t,e) ->
               let t = Raw_parser_values.symbol_of_token t in
               let t = Raw_parser_values.class_of_symbol t in
               let t = Raw_parser_values.string_of_class t in
               Some (`Assoc [
                   "start", Lexing.json_of_position s;
                   "end", Lexing.json_of_position e;
                   "token", `String t;
                 ])
             )
          )

  | (Dump `Flags : a request) ->
    let flags = Project.get_flags (Buffer.project buffer) in
    let assoc =
      List.map flags ~f:(fun (src, flag_lists) ->
        let l = List.concat_map flag_lists ~f:(List.map ~f:(fun s -> `String s)) in
        src, `List l
      )
    in
    `Assoc assoc

  | (Dump `Warnings : a request) ->
    with_typer buffer @@ fun _typer ->
    Warnings.dump ()

  | (Dump _ : a request) ->
    failwith "TODO"

  | (Which_path xs : a request) ->
    begin
      let project = Buffer.project buffer in
      let rec aux = function
        | [] -> raise Not_found
        | x :: xs ->
          try
            find_in_path_uncap (Project.source_path project) x
          with Not_found -> try
            find_in_path_uncap (Project.build_path project) x
          with Not_found ->
            aux xs
      in
      aux xs
    end

  | (Which_with_ext exts : a request) ->
    let project = Buffer.project buffer in
    let path = Path_list.to_strict_list (Project.source_path project) in
    let with_ext ext = modules_in_path ~ext path in
    List.concat_map ~f:with_ext exts

  | (Flags_get : a request) ->
    let project = Buffer.project buffer in
    Merlin_lib.Project.User.get_flags project

  | (Project_get : a request) ->
    let project = Buffer.project buffer in
    (List.map ~f:fst (Project.get_dot_merlins project),
     match Project.get_dot_merlins_failure project with
     | [] -> `Ok
     | failures -> `Failures failures)

  | (Findlib_list : a request) ->
    Fl_package_base.list_packages ()

  | (Extension_list kind : a request) ->
    let project = Buffer.project buffer in
    let enabled = Project.extensions project in
    let set = match kind with
      | `All -> Extension.all
      | `Enabled -> enabled
      | `Disabled -> String.Set.diff Extension.all enabled
    in
    String.Set.to_list set

  | (Path_list `Build : a request) ->
    let project = Buffer.project buffer in
    Path_list.to_strict_list (Project.build_path project)

  | (Path_list `Source : a request) ->
    let project = Buffer.project buffer in
    Path_list.to_strict_list (Project.source_path project)

  | (Occurrences (`Ident_at pos) : a request) ->
    with_typer buffer @@ fun typer ->
    let str = Typer.contents typer in
    let str = Browse.of_typer_contents str in
    let node = match Browse.enclosing pos str with
      | node :: _ -> node
      | [] -> BrowseT.dummy
    in
    let get_loc {Location.txt = _; loc} = loc in
    let ident_occurrence () =
      let paths = BrowseT.node_paths node.BrowseT.t_node in
      let under_cursor p = Parsing_aux.compare_pos pos (get_loc p) = 0 in
      Logger.infojf (Logger.section "occurences") ~title:"Occurrences paths"
        (fun paths ->
          let dump_path ({Location.txt; loc} as p) =
            let ppf, to_string = Format.to_string () in
            Printtyp.path ppf txt;
            `Assoc [
              "start", Lexing.json_of_position loc.Location.loc_start;
              "end", Lexing.json_of_position loc.Location.loc_end;
              "under_cursor", `Bool (under_cursor p);
              "path", `String (to_string ())
            ]
          in
          `List (List.map ~f:dump_path paths)
        ) paths;
      match List.filter paths ~f:under_cursor with
      | [] -> []
      | (path :: _) ->
        let path = path.Location.txt in
        let ts = List.concat_map ~f:(Browse.all_occurrences path) str in
        let loc (_t,paths) = List.map ~f:get_loc paths in
        List.concat_map ~f:loc ts

    and constructor_occurrence d =
      let ts = List.concat_map str
          ~f:(Browse.all_constructor_occurrences (node,d)) in
      List.map ~f:get_loc ts

    in
    let locs = match BrowseT.is_constructor node with
      | Some d -> constructor_occurrence d.Location.txt
      | None -> ident_occurrence ()
    in
    let loc_start l = l.Location.loc_start in
    let cmp l1 l2 = Lexing.compare_pos (loc_start l1) (loc_start l2) in
    List.sort ~cmp locs

  | (Version : a request) ->
    Main_args.version_spec

  | (Idle_job : a request) ->
    Buffer.idle_job buffer

  | _ -> raise Unhandled_command

  : a)

let dispatch_sync (state : state) =
  fun (type a) (request : a request) ->
  (match request with
  | (Tell (`Start pos) : a request) ->
    let lexer = Buffer.start_lexing ?pos state.buffer in
    state.lexer <- Some lexer;
    buffer_update state (Lexer.history lexer);
    cursor_state state

  | (Tell (`File _ | `Source _ | `Eof as source) : a request) ->
    let source = match source with
      | `Eof -> Some ""
      | `Source "" -> None
      | `Source source -> Some source
      | `File path ->
        match Misc.file_contents path with
        | "" -> None
        | source -> Some source
    in
    begin match source with
      | None -> cursor_state state
      | Some source ->
        let lexer = match state.lexer with
          | Some lexer ->
            assert (not (Lexer.eof lexer));
            lexer
          | None ->
            let lexer = Buffer.start_lexing state.buffer in
            state.lexer <- Some lexer; lexer in
        assert (Lexer.feed lexer source);
        buffer_update state (Lexer.history lexer);
        (* Stop lexer on EOF *)
        if Lexer.eof lexer then state.lexer <- None;
        cursor_state state
    end

  | (Tell `Marker : a request) ->
    let lexer = match state.lexer with
      | Some lexer ->
        assert (not (Lexer.eof lexer));
        lexer
      | None ->
        let lexer = Buffer.start_lexing state.buffer in
        state.lexer <- Some lexer; lexer
    in
    Lexer.put_mark lexer (Buffer.get_mark state.buffer);
    cursor_state state

  | (Drop : a request) ->
    let lexer = Buffer.lexer state.buffer in
    buffer_freeze state (History.drop_tail lexer);
    cursor_state state

  | (Seek `Position : a request) ->
    cursor_state state

  | (Seek (`Before pos) : a request) ->
    let items = Buffer.lexer state.buffer in
    (* true while i is before pos *)
    let until_after pos (_,i) =
      Lexing.compare_pos (Lexer.item_start i) pos < 0 in
    (* true while i is after pos *)
    let until_before pos (_,i) =
      Lexing.compare_pos (Lexer.item_start i) pos >= 0 in
    let items = History.seek_forward (until_after pos) items in
    let items = History.seek_backward (until_before pos) items in
    buffer_freeze state items;
    cursor_state state

  | (Seek (`Exact pos) : a request) ->
    let items = Buffer.lexer state.buffer in
    (* true while i is before pos *)
    let until_after pos (_,i) =
      Lexing.compare_pos (Lexer.item_start i) pos < 0 in
    (* true while i is after pos *)
    let until_before pos (_,i) =
      Lexing.compare_pos (Lexer.item_end i) pos > 0 in
    let items = History.seek_forward (until_after pos) items in
    let items = History.seek_backward (until_before pos) items in
    buffer_freeze state items;
    cursor_state state

  | (Seek `End : a request) ->
    let items = Buffer.lexer state.buffer in
    let items = History.seek_forward (fun _ -> true) items in
    buffer_freeze state items;
    cursor_state state

  | (Seek `Marker : a request) ->
    begin match Option.bind state.lexer ~f:Lexer.get_mark with
    | None -> ()
    | Some mark ->
      let recoveries = Buffer.recover_history state.buffer in
      let diff = ref None in
      let check_item (lex_item,recovery) =
        let parser = Recover.parser recovery in
        let result = Parser.has_marker ?diff:!diff parser mark in
        diff := Some (parser,result);
        not result
      in
      if check_item (History.focused recoveries) then
        let recoveries = History.move (-1) recoveries in
        let recoveries = History.seek_backward check_item recoveries in
        let recoveries = History.move 1 recoveries in
        let item, _ = History.focused recoveries in
        let items = Buffer.lexer state.buffer in
        let items = History.seek_backward (fun (_,i) -> i != item) items in
        buffer_freeze state items;
    end;
    cursor_state state

  | (Refresh : a request) ->
    checkout_buffer_cache := [];
    Project.invalidate ~flush:true (Buffer.project state.buffer)

  | (Flags (`Add flags) : a request) ->
    let project = Buffer.project state.buffer in
    Merlin_lib.Project.User.add_flags project flags

  | (Flags `Clear : a request) ->
    let project = Buffer.project state.buffer in
    Merlin_lib.Project.User.clear_flags project

  | (Findlib_use packages : a request) ->
    let project = Buffer.project state.buffer in
    Project.User.load_packages project packages

  | (Extension_set (action,exts) : a request) ->
    let enabled = match action with
      | `Enabled  -> true
      | `Disabled -> false
    in
    let project = Buffer.project state.buffer in
    begin match
      List.filter_map exts ~f:(Project.User.set_extension project ~enabled)
    with
    | [] -> `Ok
    | lst -> `Failures lst
    end

  | (Path (var,action,paths) : a request) ->
    let project = Buffer.project state.buffer in
    List.iter paths ~f:(Project.User.path project ~action ~var ?cwd:None)

  | (Path_reset : a request) ->
    let project = Buffer.project state.buffer in
    Project.User.reset project

  | _ -> raise Unhandled_command

  : a)

let dispatch_reset (state : state) =
  fun (type a) (request : a request) ->
  (match request with
  | (Checkout (ft,path,dot_merlins) : a request) ->
    let ft = match ft, path with
      | (`ML | `MLI as ft), _  -> ft
      | `Auto, Some path when Filename.check_suffix path ".mli" -> `MLI
      | `Auto, _ -> `ML
    in
    let buffer = checkout_buffer ?dot_merlins ?path ft in
    state.lexer <- None;
    state.buffer <- buffer;
    cursor_state state

  | _ -> raise Unhandled_command
  : a)


let dispatch state req =
  let verbosity = track_verbosity state req in
  try dispatch_reset state req
  with Unhandled_command ->
    try dispatch_sync state req
    with Unhandled_command ->
      try dispatch_query ~verbosity state.buffer req
      with Unhandled_command ->
        failwith "Unhandled command"
