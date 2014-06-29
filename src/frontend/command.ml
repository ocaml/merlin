(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013  Frédéric Bour  <frederic.bour(_)lakaban.net>
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
  mutable project : Project.t;
  mutable buffer : Buffer.t;
  mutable lexer : Lexer.t option;
}

let store : (string, Project.t) Hashtbl.t = Hashtbl.create 3
let project_by_key key =
  try Hashtbl.find store key
  with Not_found ->
    let project = Project.create () in
    Hashtbl.replace store key project;
    project

let new_state () =
  let project = project_by_key "" in
  let buffer = Buffer.create project Parser.implementation in
  {project; buffer; lexer = None}

let section = Logger.section "command"

let cursor_state state =
  let cursor, marker =
    match state.lexer with
    | None ->
      Lexer.item_end (History.focused (Buffer.lexer state.buffer)),
      false
    | Some lexer ->
      Lexer.position lexer,
      Buffer.has_mark state.buffer (Lexer.get_mark lexer)
  in
  { cursor; marker }

let buffer_changed state =
  state.lexer <- None

let buffer_update state items =
  Buffer.update state.buffer items;
  buffer_changed state

let dispatch (state : state) =
  fun (type a) (request : a request) ->
  (match request with
  | (Tell (`Start pos) : a request) ->
    let lexer = Buffer.start_lexing ?pos state.buffer in
    state.lexer <- Some lexer;
    ignore (Buffer.update state.buffer (Lexer.history lexer));
    cursor_state state

  | (Tell (`Source source) : a request) ->
    let lexer = match state.lexer with
      | Some lexer ->
        assert (not (Lexer.eof lexer));
        lexer
      | None ->
        let lexer = Buffer.start_lexing state.buffer in
        state.lexer <- Some lexer; lexer
    in
    assert (Lexer.feed lexer source);
    ignore (Buffer.update state.buffer (Lexer.history lexer));
    (* Stop lexer on EOF *)
    if Lexer.eof lexer then state.lexer <- None;
    cursor_state state

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

  | (Type_expr (source, pos) : a request) ->
    let typer = Buffer.typer state.buffer in
    let env = match pos with
      | None -> Typer.env typer
      | Some pos -> (Completion.node_at typer pos).BrowseT.t_env
    in
    let ppf, to_string = Format.to_string () in
    ignore (Type_utils.type_in_env env ppf source : bool);
    to_string ()

  | (Type_enclosing ((expr, offset), pos) : a request) ->
    let open BrowseT in
    let open Typedtree in
    let typer = Buffer.typer state.buffer in
    let structures = Typer.structures typer in
    let structures = Browse.of_structures structures in
    let path = Browse.enclosing pos structures in
    let aux = function
      | { t_loc; t_env;
          t_node = ( Expression {exp_type = t}
                   | Pattern {pat_type = t}
                   | Core_type {ctyp_type = t } )
        } ->
        let ppf, to_string = Format.to_string () in
        Printtyp.wrap_printing_env t_env
          (fun () -> Printtyp.type_scheme ppf t);
        Some (t_loc, to_string ())
      | { t_loc; t_env;
          t_node = Type_declaration { typ_id = id; typ_type = t} } ->
        let ppf, to_string = Format.to_string () in
        Printtyp.wrap_printing_env t_env
          (fun () -> Printtyp.type_declaration id ppf t);
        Some (t_loc, to_string ())
      | { t_loc; t_env;
          t_node = ( Module_expr {mod_type = m}
                   | Module_type {mty_type = m}
                   | Module_declaration {md_type = {mty_type = m}}
                   | Module_type_declaration {mtd_type = Some {mty_type = m}} )
        } ->
        let ppf, to_string = Format.to_string () in
        Printtyp.wrap_printing_env t_env
          (fun () -> Printtyp.modtype ppf m);
        Some (t_loc, to_string ())
      | _ -> None
    in
    let result = List.filter_map ~f:aux path in
    (* enclosings of cursor in given expression *)
    let exprs =
      let len = String.length expr in
      let rec aux acc i =
        if i >= len then
          List.rev (expr :: acc)
        else if expr.[i] = '.' then
          aux (String.sub expr ~pos:0 ~len:i :: acc) (succ i)
        else
          aux acc (succ i)
      in
      aux [] offset
    in
    let small_enclosings =
      let node = Completion.node_at typer pos in
      let env = node.BrowseT.t_env in
      let loc_start =
        let l, c = Lexing.split_pos pos in
        Lexing.make_pos (l, c - offset)
      in
      let shift loc int =
        let l, c = Lexing.split_pos loc in
        Lexing.make_pos (l, c + int)
      in
      List.filter_map exprs ~f:(fun source ->
        try
          let loc = { Location.
            loc_start ;
            loc_end = shift loc_start (String.length source) ;
            loc_ghost = false ;
          }
          in
          let ppf, to_string = Format.to_string () in
          if Type_utils.type_in_env env ppf source then
            Some (loc, to_string ())
          else
            None
        with _ ->
          None
      )
    in
    List.filter_dup'
      ~equiv:(fun ({Location. loc_start; loc_end}, text) ->
        Lexing.split_pos loc_start, Lexing.split_pos loc_end, text)
      (small_enclosings @ result)

  | (Enclosing pos : a request) ->
    let open BrowseT in
    let typer = Buffer.typer state.buffer in
    let structures = Typer.structures typer in
    let structures = Browse.of_structures structures in
    let path = Browse.enclosing pos structures in
    List.map (fun t -> t.BrowseT.t_loc) path

  | (Complete_prefix (prefix, pos) : a request) ->
    let node = Completion.node_at (Buffer.typer state.buffer) pos in
    let compl = Completion.node_complete state.project node prefix in
    List.rev compl

  | (Locate (path, opt_pos) : a request) ->
    let env, local_defs =
      let typer = Buffer.typer state.buffer in
      match opt_pos with
      | None     -> Typer.env typer, []
      | Some pos ->
        let node = Completion.node_at typer pos in
        node.BrowseT.t_env, Typer.structures typer
    in
    begin match
      Track_definition.from_string ~project:state.project ~env ~local_defs path
    with
    | `Found (file, pos) ->
      Logger.info (Logger.section "locate")
        (Option.value ~default:"<local buffer>" file);
      `Found (file, pos)
    | otherwise -> otherwise
    end

  | (Outline : a request) ->
    let typed_tree = Typer.structures (Buffer.typer state.buffer) in
    Outline.get typed_tree

  | (Drop : a request) ->
    let lexer = Buffer.lexer state.buffer in
    Buffer.update state.buffer (History.drop_tail lexer);
    buffer_changed state;
    cursor_state state

  | (Seek `Position : a request) ->
    cursor_state state

  | (Seek (`Before pos) : a request) ->
    let items = Buffer.lexer state.buffer in
    (* true while i is before pos *)
    let until_after pos i = Lexing.compare_pos (Lexer.item_start i) pos < 0 in
    (* true while i is after pos *)
    let until_before pos i = Lexing.compare_pos (Lexer.item_start i) pos >= 0 in
    let items = History.seek_forward (until_after pos) items in
    let items = History.seek_backward (until_before pos) items in
    buffer_update state items;
    cursor_state state

  | (Seek (`Exact pos) : a request) ->
    let items = Buffer.lexer state.buffer in
    (* true while i is before pos *)
    let until_after pos i = Lexing.compare_pos (Lexer.item_start i) pos < 0 in
    (* true while i is after pos *)
    let until_before pos i = Lexing.compare_pos (Lexer.item_end i) pos > 0 in
    let items = History.seek_forward (until_after pos) items in
    let items = History.seek_backward (until_before pos) items in
    buffer_update state items;
    cursor_state state

  | (Seek `End : a request) ->
    let items = Buffer.lexer state.buffer in
    let items = History.seek_forward (fun _ -> true) items in
    buffer_update state items;
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
        let items = History.seek_backward ((!=) item) items in
        buffer_update state items;
    end;
    cursor_state state

  | (Boundary (dir,pos) : a request) ->
    failwith "TODO"

  | (Reset (ml,path) : a request) ->
    let parser = match ml with
      | `ML  -> Raw_parser.implementation_state
      | `MLI -> Raw_parser.interface_state
    in
    let buffer = Buffer.create ?path state.project parser in
    buffer_changed state;
    state.buffer <- buffer;
    cursor_state state

  | (Refresh : a request) ->
    Project.invalidate ~flush:true state.project

  | (Errors : a request) ->
    let pexns = Buffer.parser_errors state.buffer in
    let texns = Typer.exns (Buffer.final_typer state.buffer) in
    texns @ pexns

  | (Dump `Parser : a request) ->
    Merlin_recover.dump (Buffer.recover state.buffer);

  | (Dump `Typer_input : a request) ->
    let ppf, to_string = Format.to_string () in
    Typer.dump ppf (Buffer.typer state.buffer);
    `String (to_string ())

  | (Dump `Recover : a request) ->
    Merlin_recover.dump_recoverable (Buffer.recover state.buffer);

  | (Dump (`Env (kind, pos)) : a request) ->
    let typer = Buffer.typer state.buffer in
    let env = match pos with
      | None -> Typer.env typer
      | Some pos -> (Completion.node_at typer pos).BrowseT.t_env
    in
    let sg = Browse_misc.signature_of_env ~ignore_extensions:(kind = `Normal) env in
    let aux item =
      let ppf, to_string = Format.to_string () in
      Printtyp.signature ppf [item];
      let content = to_string () in
      let ppf, to_string = Format.to_string () in
      match Merlin_types_custom.signature_loc item with
      | Some loc ->
        Location.print_loc ppf loc;
        let loc = to_string () in
        `List [`String loc ; `String content]
      | None -> `String content
    in
    `List (List.map ~f:aux sg)

  | (Dump `Browse : a request) ->
    let typer = Buffer.typer state.buffer in
    let structures = Typer.structures typer in
    let structures = Browse.of_structures structures in
    Browse_misc.dump_ts structures

  | (Dump _ : a request) ->
    failwith "TODO"

  | (Which_path xs : a request) ->
    begin
      let rec aux = function
        | [] -> raise Not_found
        | x :: xs ->
          try
            find_in_path_uncap (Project.source_path state.project) x
          with Not_found -> try
            find_in_path_uncap (Project.build_path state.project) x
          with Not_found ->
            aux xs
      in
      aux xs
    end

  | (Which_with_ext exts : a request) ->
    let path = Path_list.to_strict_list (Project.source_path state.project) in
    let with_ext ext = modules_in_path ~ext path in
    List.concat_map ~f:with_ext exts

  | (Project_load (cmd,path) : a request) ->
    let fn = match cmd with
      | `File -> Dot_merlin.read
      | `Find -> Dot_merlin.find
    in
    let dot_merlins =
      try fn path
      with Sys_error s ->
        Logger.debugj section ~title:"project_load"
          (`String s);
        List.Lazy.Nil
    in
    let config = Dot_merlin.parse dot_merlins in
    let key = match config.Dot_merlin.dot_merlins with
      | [] -> ""
      | (a :: _) -> a
    in
    let project = project_by_key key in
    let failures = Project.set_dot_merlin project (Some config) in
    state.project <- project;
    (config.Dot_merlin.dot_merlins, failures)

  | (Findlib_list : a request) ->
    Fl_package_base.list_packages ()

  | (Findlib_use packages : a request) ->
    Project.User.load_packages state.project packages

  | (Extension_list kind : a request) ->
    let enabled = Project.extensions state.project in
    let set = match kind with
      | `All -> Extension.all
      | `Enabled -> enabled
      | `Disabled -> String.Set.diff Extension.all enabled
    in
    String.Set.to_list set

  | (Extension_set (action,extensions) : a request) ->
    let enabled = match action with
      | `Enabled  -> true
      | `Disabled -> false
    in
    List.iter ~f:(Project.User.set_extension state.project ~enabled)
      extensions

  | (Path (var,action,pathes) : a request) ->
    List.iter pathes
      ~f:(Project.User.path state.project ~action ~var ?cwd:None)

  | (Path_list `Build : a request) ->
    Path_list.to_strict_list (Project.build_path state.project)

  | (Path_list `Source : a request) ->
    Path_list.to_strict_list (Project.source_path state.project)

  | (Path_reset : a request) ->
    Project.User.reset state.project

  | (Occurences (`Ident_at pos) : a request) ->
    let str = Typer.structures (Buffer.typer state.buffer) in
    let str = Browse.of_structures str in
    let node = match Browse.enclosing pos str with
      | node :: _ -> node
      | [] -> BrowseT.dummy
    in
    let get_loc {Location.txt = _; loc} = loc in
    let ident_occurence () =
      let paths =
        match node.BrowseT.t_node with
        | BrowseT.Expression e -> BrowseT.expression_paths e
        | BrowseT.Pattern p -> BrowseT.pattern_paths p
        | _ -> []
      in
      let under_cursor p = Parsing_aux.compare_pos pos (get_loc p) = 0 in
      Logger.infojf section ~title:"Occurences paths"
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
        let ts = List.concat_map ~f:(Browse.all_occurences path) str in
        let loc (_t,paths) = List.map ~f:get_loc paths in
        List.concat_map ~f:loc ts

    and constructor_occurence d =
      let ts = List.concat_map str
          ~f:(Browse.all_constructor_occurences (node,d)) in
      List.map ~f:get_loc ts

    in
    let locs = match BrowseT.is_constructor node with
      | Some d -> constructor_occurence d.Location.txt
      | None -> ident_occurence ()
    in
    let loc_start l = l.Location.loc_start in
    let cmp l1 l2 = Lexing.compare_pos (loc_start l1) (loc_start l2) in
    List.sort ~cmp locs

  | (Version : a request) ->
    Main_args.version_spec

  : a)
