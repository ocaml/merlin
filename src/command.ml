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

type state = State.t = {
  pos      : Lexing.position;
  tokens   : Outline.token list;
  comments : Lexer.comment list;
  outlines : Outline.t;
  chunks   : Chunk.t;
  types    : Typer.t;
}

type handler = Protocol.io -> state -> Json.json list -> state * Json.json
type t = { name : string ; handler : handler }
let invalid_arguments () = failwith "invalid arguments"

let commands : (string,t) Hashtbl.t = Hashtbl.create 11
let register cmd = Hashtbl.add commands cmd.name cmd

let command_tell = {
  name = "tell";

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
            | `List [`String "tell" ; `String "struct" ; `String source] ->
              source
            | `List [`String "tell" ; `String "end" ; `String source] ->
              eod := true; source
            | `List [`String "tell" ; `String ("end"|"struct") ; `Null] ->
              eot := true; ""
            | _ -> invalid_arguments ()
          with
            Stream.Failure -> invalid_arguments ()
        end
      in
      let rec loop state =
        let bufpos = ref state.pos in
        let tokens, outlines, chunks, types =
          state.tokens,
          (History.cutoff state.outlines),
          (History.cutoff state.chunks),
          (History.cutoff state.types)
        in
        let tokens, outlines =
          Outline.parse ~bufpos tokens outlines lexbuf
        in
        let chunks = Chunk.sync outlines chunks in
        let types = Typer.sync chunks types in
        let pos = !bufpos in
          (* If token list didn't change, move forward anyway 
           * to prevent getting stuck *)
        let stuck = state.tokens = tokens in
        let tokens =
          if stuck
          then (try List.tl tokens with _ -> tokens)
          else tokens
        in
        let state' = { tokens ; comments = [] ; outlines ; chunks ; types ; pos } in
        if !eod || (!eot && (stuck || tokens = []))
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
            Browse_misc.print_constructor ppf c
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
    let {Browse.env} = State.node_at state (Protocol.pos_of_json jpos) in
    let ppf, to_string = Misc.ppf_to_string () in
    type_in_env env ppf expr;
    state, `String (to_string ())

  | [`String "at" ; jpos] ->
    let pos = Protocol.pos_of_json jpos in
    let structures = Misc.list_concat_map
      (fun (str,sg) -> Browse.structure str)
      (Typer.trees state.types)
    in
    let node = match Browse.nearest_before pos structures with
      | Some node -> node
      | None -> raise Not_found
    in
    let ppf, to_string = Misc.ppf_to_string () in
    begin match node.Browse.context with
      | Browse.Other -> raise Not_found
      | Browse.Expr t | Browse.Pattern t | Browse.Type t ->
        Printtyp.type_scheme ppf t
      | Browse.TypeDecl (ident, t) ->
        Printtyp.type_declaration ident ppf t
      | Browse.Module m -> Printtyp.modtype ppf m
      | Browse.Modtype (ident, m) ->
        Printtyp.modtype_declaration ident ppf m
      | Browse.Class (ident, cd) ->
        Printtyp.class_declaration ident ppf cd
      | Browse.ClassType (ident, ctd) ->
        Printtyp.cltype_declaration ident ppf ctd
      | Browse.MethodCall (obj, m) ->
        match State.find_method node.Browse.env m obj with
        | Some t -> Printtyp.type_scheme ppf t
        | None -> Format.pp_print_string ppf "Unknown method"
    end;
    state, Protocol.with_location node.Browse.loc
      ["type", `String (to_string ())]

  | [`String "enclosing"; jpos] ->
    let pos = Protocol.pos_of_json jpos in
    let aux = function
      | { Browse. loc ;
          context = (Browse.Expr t | Browse.Pattern t | Browse.Type t) } ->
        let ppf, to_string = Misc.ppf_to_string () in
        Printtyp.type_scheme ppf t;
        Some (Protocol.with_location loc ["type", `String (to_string ())])
      | { Browse. loc ; context = Browse.TypeDecl (id,t) } ->
        let ppf, to_string = Misc.ppf_to_string () in
        Printtyp.type_declaration id ppf t;
        Some (Protocol.with_location loc ["type", `String (to_string ())])
      | _ -> None
    in
    let structures = Misc.list_concat_map
      (fun (str,sg) -> Browse.structure str)
      (Typer.trees state.types)
    in
    let path = Browse.enclosing pos structures in
    let result = Misc.list_filter_map aux path in
    state, `List [`Int (List.length path); `List result]

  | _ -> invalid_arguments ()
  end;
}

let command_complete = {
  name = "complete";

  handler =
  begin fun _ state -> function
  | [`String "prefix" ; `String prefix] ->
    let node = Browse.({dummy with env = Typer.env state.types}) in
    let compl = State.node_complete node prefix in
    state, `List (List.rev compl)
  | [`String "prefix" ; `String prefix ; `String "at" ; jpos ] ->
    let node = State.node_at state (Protocol.pos_of_json jpos) in
    let compl = State.node_complete node prefix in
    state, `List (List.rev compl)
  | _ -> invalid_arguments ()
  end;
}

let command_locate = {
  name = "locate";

  handler = begin fun _ state args ->
    let path, node =
      match args with
      | [ `String path ] ->
        path, Browse.({ dummy with env = Typer.env state.types })
      | [ `String path ; `String "at" ; jpos ] ->
        path, State.node_at state (Protocol.pos_of_json jpos)
      | _ -> invalid_arguments ()
    in
    match State.locate node path with
    | None -> state, `String "Not found"
    | Some (file, loc) ->
      let pos = loc.Location.loc_start in
      state, `Assoc [ "file", `String file ; "pos", Protocol.pos_to_json pos ]
  end
}

let command_seek = {
  name = "seek";

  handler =
  begin fun _ state -> function
  | [`String "position"] ->
    state, Protocol.pos_to_json state.pos

  | [`String "before" ; jpos] ->
    let pos = Protocol.pos_of_json jpos in
    let cmp o = Location.compare_pos pos (Outline.item_loc o) in
    let outlines = state.outlines in
    let outlines = History.seek_forward (fun i -> cmp i > 0) outlines in
    let outlines = History.seek_backward
      (function { Outline.kind = (Outline_utils.Syntax_error _ | Outline_utils.Unterminated)} -> true
                | i -> cmp i <= 0)
      outlines
    in
    let outlines, chunks = History.Sync.rewind fst outlines state.chunks in
    let chunks, types = History.Sync.rewind fst chunks state.types in
    let pos =
      match Outline.location outlines with
        | l when l = Location.none -> State.initial.pos
        | p -> p.Location.loc_end
    in
    { tokens = [] ; comments = [] ; outlines ; chunks ; types ; pos },
    Protocol.pos_to_json pos

  | [`String "exact" ; jpos] ->
    let pos = Protocol.pos_of_json jpos in
    let cmp o = Location.compare_pos pos (Outline.item_loc o) in
    let outlines = state.outlines in
    let outlines = History.seek_backward (fun i -> cmp i < 0) outlines in
    let outlines = History.seek_forward (fun i -> cmp i >= 0) outlines in
    let outlines, chunks = History.Sync.rewind fst outlines state.chunks in
    let chunks, types    = History.Sync.rewind fst chunks   state.types  in
    let pos =
      match Outline.location outlines with
      | l when l = Location.none -> State.initial.pos
      | p -> p.Location.loc_end
    in
    { tokens = [] ; comments = [] ; outlines ; chunks ; types ; pos },
    Protocol.pos_to_json pos

  | [`String "end"] ->
    let outlines = History.seek_forward (fun _ -> true) state.outlines in
    let chunks = History.Sync.right fst outlines state.chunks in
    let types  = History.Sync.right fst chunks state.types in
    let pos =
      match Outline.location outlines with
      | l when l = Location.none -> State.initial.pos
      | p -> p.Location.loc_end
    in
    { tokens = [] ; comments = [] ; outlines ; chunks ; types ; pos },
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
      | l when l = Location.none -> State.initial.pos
      | p -> p.Location.loc_end
    in
    { tokens = [] ; comments = [] ; outlines ; chunks ; types ; pos },
    Protocol.pos_to_json pos
  | _ -> invalid_arguments ()
  end;
}

let command_boundary = {
  name = "boundary";
  handler = begin
    let prev2 x = match History.backward x with
      | Some (_, y) -> History.prev y
      | None -> None
    in
    let command_of_string = function
      | "next" -> History.next
      | "prev" -> prev2
      | "current" -> History.prev
      | _ -> invalid_arguments ()
    in
    let outlines_of_pos state pos =
      let cmp o = Location.compare_pos pos (Outline.item_loc o) in
      let outlines = state.outlines in
      let outlines = History.seek_backward (fun i -> cmp i < 0) outlines in
      let outlines = History.seek_forward (fun i -> cmp i >= 0) outlines in
      fst (History.Sync.rewind fst outlines state.chunks)
    in
    fun _ state args ->
      let (f, pos) = match args with
        | [ `String cmd; `String "at"; jpos] ->
          (command_of_string cmd, Protocol.pos_of_json jpos)
        | [ `String cmd ] -> (command_of_string cmd, state.pos)
        | [ ] -> (History.prev, state.pos)
        | [ `String "at"; jpos ] -> (History.prev, Protocol.pos_of_json jpos)
        | _ -> invalid_arguments ()
      in
      match f (outlines_of_pos state pos) with
      | None -> state, `Null
      | Some {Outline.loc={Location.loc_start; loc_end}} ->
        state, `List (List.map Protocol.pos_to_json [loc_start; loc_end])
  end
}

let command_reset = {
  name = "reset";

  handler =
  begin fun _ state -> function
  | [] -> State.initial, Protocol.pos_to_json State.initial.pos
  | [`String "name"; `String pos_fname] ->
    { State.initial with pos =
      { State.initial.pos with Lexing.pos_fname } },
    Protocol.pos_to_json State.initial.pos 
  | _ -> invalid_arguments ()
  end
}

let command_refresh = {
  name = "refresh";

  handler =
  begin fun _ state -> function
  | [] ->
    State.reset_global_modules ();
    Env.reset_cache ();
    let types = Typer.sync state.chunks History.empty in
    {state with types}, `Bool true
  | _ -> invalid_arguments ()
  end;
}

let command_cd = {
  name = "cd";

  handler =
  begin fun _ state -> function
  | [`String s] -> Sys.chdir s; state, `Bool true
  | _ -> invalid_arguments ()
  end;
}

let command_errors = {
  name = "errors";

  handler =
  begin fun _ state -> function
  | [] -> state, `List (List.map snd 
                          (Error_report.to_jsons (State.exns state)))
  | _ -> invalid_arguments ()
  end;
}

let command_dump = {
  name = "dump";

  handler =
  let pr_item_desc items =
    (List.map (fun (s,i) -> `List [`String s;`Int i]) (Chunk.dump_chunk items))
  in
  begin fun _ state -> function
  | [`String "env"] ->
      let sg = Browse_misc.signature_of_env (Typer.env state.types) in
      let aux item =
        let ppf, to_string = Misc.ppf_to_string () in
        Printtyp.signature ppf [item];
        let content = to_string () in
        let ppf, to_string = Misc.ppf_to_string () in
        match Browse_misc.signature_loc item with
          | Some loc ->
              Location.print_loc ppf loc;
              let loc = to_string () in
              `List [`String loc ; `String content]
          | None -> `String content
      in
      state, `List (List.map aux sg)
  | [`String "env" ; `String "at" ; jpos ] ->
    let {Browse.env} = State.node_at state 
        (Protocol.pos_of_json jpos) in
    let sg = Browse_misc.signature_of_env env in
    let aux item =
      let ppf, to_string = Misc.ppf_to_string () in
      Printtyp.signature ppf [item];
      let content = to_string () in
      let ppf, to_string = Misc.ppf_to_string () in
      match Browse_misc.signature_loc item with
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
        match Browse_misc.signature_loc item with
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
        (fun (str,sg) -> Browse.structure str)
        (Typer.trees state.types)
      in
      state, Browse_misc.dump_ts structures
  | [`String "outline"] ->
    let outlines = History.prevs state.outlines in
    let aux item =
      let tokens =
        List.map (fun (t,_,_) -> `String (Chunk_parser_utils.token_to_string t))
          item.Outline.tokens
      in
      `List [`String (Outline_utils.kind_to_string item.Outline.kind);
             `List tokens]
    in
    state, `List (List.rev_map aux outlines) 


  | [`String "exn"] ->
    let exns = State.exns state in
    state, `List (List.rev_map (fun e -> `String (Printexc.to_string e)) exns)

  | _ -> invalid_arguments ()
  end;
}

let command_which = {
  name = "which";

  handler =
  begin fun _ state -> function
  | [`String "path" ; `String s] ->
      let filename =
        try Misc.find_in_path_uncap !State.source_path s
        with Not_found ->
          Misc.find_in_path_uncap !Config.load_path s
      in
      state, `String filename
  | [`String "with_ext" ; `String ext] ->
      let results = Misc.modules_in_path ~ext !State.source_path in
      state, `List (List.map (fun s -> `String s) results)
  | _ -> invalid_arguments ()
  end;
}

let load_packages packages =
  let packages = Findlib.package_deep_ancestors [] packages in
  let path = List.map Findlib.package_directory packages in
  Config.load_path := Misc.list_filter_dup (path @ !Config.load_path);
  State.reset_global_modules ()

let command_find = {
  name = "find";

  handler =
  begin fun _ state -> function
      (* Recommended form *)
  | [`String "use" ; `List packages]
      (* FIXME: Deprecated *)
  | (`String "use" :: packages) ->
      let packages = List.map
        (function `String pkg -> pkg | _ -> invalid_arguments ())
        packages
      in
      load_packages packages;
      state, `Bool true
  | [`String "list"] ->
      state, `List (List.rev_map (fun s -> `String s) (Fl_package_base.list_packages ()))
  | _ -> invalid_arguments ()
  end;
}

let command_help = {
  name = "help";

  handler =
  begin fun _ state -> function
  | [] ->
      let helps = Hashtbl.fold
        (fun name _ cmds -> `String name :: cmds)
        commands []
      in
      state, `List helps
  | _ -> invalid_arguments ()
  end;
}

let _ = List.iter register [
  command_tell; command_seek; command_reset; command_refresh;
  command_cd; command_type; command_complete; command_boundary;
  command_locate;
  command_errors; command_dump;
  command_which; command_find;
  command_help;
]
