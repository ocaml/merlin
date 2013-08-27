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

open Misc
open Protocol

type state = State.t = {
  pos      : Lexing.position;
  tokens   : Outline.token list;
  comments : Lexer.comment list;
  outlines : Outline.t;
  chunks   : Chunk.t;
  types    : Typer.t;
}
module VPrinttyp = State.Verbose_print

(*let track_verbosity handler : handler =
  let last = ref (History.Sync.origin, []) in
  fun io st args ->
    let action = 
      let (sync,args') = !last in
      let ol = st.outlines in
      let ol = match History.backward ol with None -> ol | Some (_, h) -> h in
      if args = args' && History.Sync.(same sync (at ol))
      then `Incr
      else (last := (History.Sync.at ol, args); `Clear)
    in
    ignore (State.verbosity action);
    handler io st args*)

let load_packages packages =
  let packages = Findlib.package_deep_ancestors [] packages in
  let path = List.map Findlib.package_directory packages in
  Config.load_path := Misc.list_filter_dup (path @ !Config.load_path);
  Extensions_utils.register_packages packages;
  State.reset_global_modules ()

module Path_utils = struct
  (* Search path (-I) handling *)
  let default_build_paths =
    let open Config in
    lazy ("." :: List.rev !Clflags.include_dirs @ !load_path)
  
  let build  = Config.load_path,  default_build_paths 
  let source = State.source_path, lazy ["."]

  let set_default_path () =
    Config.load_path := Lazy.force default_build_paths
  
  let modify ~action ~var ~kind ?cwd path =
    let r,_= match var with `Source -> source | `Build -> build in
    let d =
      if kind = `Relative 
      then path
      else Misc.canonicalize_filename ?cwd
            (Misc.expand_directory Config.standard_library path)
    in
    r := List.filter ((<>) d) !r;
    match action with
    | `Add -> r := d :: !r
    | `Rem -> ()
end

let set_default_path = Path_utils.set_default_path

module Type_utils = struct
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
            VPrinttyp.type_scheme ppf exp.exp_type;
        | _ -> failwith "unhandled expression"
      end
    in
    Printtyp.wrap_printing_env env
    begin fun () -> match Chunk_parser.top_expr Lexer.token lexbuf with
      | { Parsetree.pexp_desc = Parsetree.Pexp_construct (longident,None,_) } ->
        begin
          try let _, c = Env.lookup_constructor longident.Asttypes.txt env in
            Browse_misc.print_constructor ppf c
          with Not_found ->
          try let _, m = Env.lookup_module longident.Asttypes.txt env in
           Printtyp.modtype ppf m
          with Not_found ->
          try let p, m = Env.lookup_modtype longident.Asttypes.txt env in
            VPrinttyp.modtype_declaration (Ident.create (Path.last p)) ppf 
              (State.verbose_sig env m)
          with Not_found ->
            ()
        end
      | { Parsetree.pexp_desc = Parsetree.Pexp_ident longident } as e ->
        begin
          try print_expr e
          with exn ->
          try let p, t = Env.lookup_type longident.Asttypes.txt env in
           VPrinttyp.type_declaration (Ident.create (Path.last p)) ppf t
          with _ ->
            raise exn
        end
      | e -> print_expr e
    end
end

let dispatch (i,o : IO.io) (state : state) = 
  fun (type a) (request : a request) ->
  (match request with
  | (Tell (`Source source) : a request) ->
  begin
    Env.reset_missing_cmis ();
    let eod = ref false and eot = ref false in
    let lexbuf = Misc.lex_strings source
      begin fun () ->
        if !eot then ""
        else try
          o (Return (request, false));
          let request = Stream.next i in
          match request with
          | Request (Tell (`Source source)) -> source
          | Request (Tell (`More source)) -> eod := true; source
          | Request (Tell `End) -> eot := true; ""
          | _ -> IO.invalid_arguments ()
        with
          Stream.Failure -> IO.invalid_arguments ()
      end
    in
    let rec loop first state =
      let bufpos = ref state.pos in
      let tokens, outlines, chunks, types =
        state.tokens,
        (History.cutoff state.outlines),
        (History.cutoff state.chunks),
        (History.cutoff state.types)
      in
      let tokens, outlines =
        let default tokens =
          Outline.parse ~bufpos tokens outlines lexbuf
        in
        if not first
        then default tokens
        else
          match History.backward outlines with
          | None -> default tokens
          | Some (o, os) ->
            let tokens', outlines' =
              Outline.parse ~bufpos (o.Outline.tokens @ tokens)
                (History.cutoff os) lexbuf
            in
            match History.prev outlines' with
            (* Parsing is stable *)
            | Some o' when o.Outline.tokens = o'.Outline.tokens ->
              default tokens'
            (* Parsing is not stable *)
            | _ ->
              tokens', outlines'
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
      else loop false state'
    in
    let state = loop true state in
    state, true
  end
  | (Tell _ : a request) -> IO.invalid_arguments ()

  | (Type_expr (source, None) : a request) ->
    let env = Typer.env state.types in
    let ppf, to_string = Misc.ppf_to_string () in
    Type_utils.type_in_env env ppf source;
    state, to_string ()

  | (Type_expr (source, Some pos) : a request) ->
    let {Browse.env} = State.node_at state pos in
    let ppf, to_string = Misc.ppf_to_string () in
    Type_utils.type_in_env env ppf source;
    state, to_string ()

  | (Type_at pos : a request) ->
    let structures = Misc.list_concat_map
      (fun (str,sg) -> Browse.structure str)
      (Typer.trees state.types)
    in
    let node = match Browse.nearest_before pos structures with
      | Some node -> node
      | None -> raise Not_found
    in
    let ppf, to_string = Misc.ppf_to_string () in
    Printtyp.wrap_printing_env node.Browse.env
    begin fun () -> match node.Browse.context with
      | Browse.NamedOther _ (* FIXME *)
      | Browse.Other -> raise Not_found
      | Browse.Expr t | Browse.Pattern (_, t) | Browse.Type t ->
        VPrinttyp.type_scheme ppf t
      | Browse.TypeDecl (ident, t) ->
        VPrinttyp.type_declaration ident ppf t
      | Browse.Module (_, m) -> Printtyp.modtype ppf m
      | Browse.Modtype (ident, m) ->
        VPrinttyp.modtype_declaration ident ppf m
      | Browse.Class (ident, cd) ->
        Printtyp.class_declaration ident ppf cd
      | Browse.ClassType (ident, ctd) ->
        Printtyp.cltype_declaration ident ppf ctd
      | Browse.MethodCall (obj, m) ->
        match State.find_method node.Browse.env m obj with
        | Some t -> VPrinttyp.type_scheme ppf t
        | None -> Format.pp_print_string ppf "Unknown method"
    end;
    state, (node.Browse.loc, to_string ())

  | (Type_enclosing pos : a request) ->
    let aux = function
      | {Browse. loc; env;
          context = (Browse.Expr t | Browse.Pattern (_, t) | Browse.Type t)} ->
        let ppf, to_string = Misc.ppf_to_string () in
        Printtyp.wrap_printing_env env
          (fun () -> VPrinttyp.type_scheme ppf t);
        Some (loc, to_string ())
      | {Browse. loc; env; context = Browse.TypeDecl (id,t)} ->
        let ppf, to_string = Misc.ppf_to_string () in
        Printtyp.wrap_printing_env env
          (fun () -> VPrinttyp.type_declaration id ppf t);
        Some (loc, to_string ())
      | {Browse. loc; env; context = Browse.Module (_,m)} ->
        let ppf, to_string = Misc.ppf_to_string () in
        Printtyp.wrap_printing_env env
          (fun () -> Printtyp.modtype ppf m);
        Some (loc, to_string ())
      | _ -> None
    in
    let structures = Misc.list_concat_map
      (fun (str,sg) -> Browse.structure str)
      (Typer.trees state.types)
    in
    let path = Browse.enclosing pos structures in
    let result = Misc.list_filter_map aux path in
    state, (List.length path, result)

  | (Complete_prefix (prefix, None) : a request) ->
    let node = Browse.({dummy with env = Typer.env state.types}) in
    let compl = State.node_complete node prefix in
    state, `List (List.rev compl)

  | (Complete_prefix (prefix, Some pos) : a request) ->
    let node = State.node_at state pos in
    let compl = State.node_complete node prefix in
    state, `List (List.rev compl)

  | (Locate (path, opt_pos) : a request) ->
    let node =
      match opt_pos with
      | None -> Browse.({ dummy with env = Typer.env state.types })
      | Some pos -> State.node_at state pos 
    in
    begin match State.locate node path with
    | None -> state, None
    | Some (file, loc) ->
      let pos = loc.Location.loc_start in
      state, Some (file, pos)
    end

  | (Drop : a request) ->
    let outlines, chunks, types =
      (History.cutoff state.outlines),
      (History.cutoff state.chunks),
      (History.cutoff state.types)
    in
    {state with tokens = []; outlines; chunks; types}, state.pos

  | (Seek `Position : a request) ->
    state, state.pos

  | (Seek (`Before pos) : a request) ->
    let cmp o = Location.compare_pos pos (Outline.item_loc o) in
    let outlines = state.outlines in
    let outlines = History.seek_forward (fun i -> cmp i > 0) outlines in
    let outlines = History.seek_backward
      (function { Outline.kind = (Outline_utils.Syntax_error _ | Outline_utils.Unterminated)} -> true
                | i -> cmp i <= 0)
      outlines
    in
    let chunks           = History.Sync.right  fst outlines state.chunks in
    let outlines, chunks = History.Sync.rewind fst outlines chunks       in
    let types            = History.Sync.right  fst chunks   state.types  in
    let chunks, types    = History.Sync.rewind fst chunks   types        in
    let pos =
      match Outline.location outlines with
        | l when l = Location.none -> State.initial.pos
        | p -> p.Location.loc_end
    in
    {tokens = []; comments = []; outlines; chunks; types; pos}, pos

  | (Seek (`Exact pos) : a request) ->
    let cmp o = Location.compare_pos pos (Outline.item_loc o) in
    let outlines = state.outlines in
    let outlines = History.seek_backward (fun i -> cmp i < 0) outlines in
    let outlines = History.seek_forward (fun i -> cmp i >= 0) outlines in
    let chunks           = History.Sync.right  fst outlines state.chunks in
    let outlines, chunks = History.Sync.rewind fst outlines chunks       in
    let types            = History.Sync.right  fst chunks   state.types  in
    let chunks, types    = History.Sync.rewind fst chunks   types        in
    let pos =
      match Outline.location outlines with
      | l when l = Location.none -> State.initial.pos
      | p -> p.Location.loc_end
    in
    {tokens = []; comments = []; outlines; chunks; types; pos}, pos

  | (Seek `End : a request) ->
    let outlines = History.seek_forward (fun _ -> true) state.outlines in
    let chunks = History.Sync.right fst outlines state.chunks in
    let types  = History.Sync.right fst chunks state.types in
    let pos =
      match Outline.location outlines with
      | l when l = Location.none -> State.initial.pos
      | p -> p.Location.loc_end
    in
    {tokens = []; comments = []; outlines; chunks; types; pos}, pos

  | (Seek `Maximize_scope : a request) ->
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
    {tokens = []; comments = []; outlines; chunks; types; pos}, pos

  | (Boundary (dir,pos) : a request) ->
    let prev2 x = match History.backward x with
      | Some (_, y) -> History.prev y
      | None -> None
    in
    let command = function
      | `Next -> History.next
      | `Prev -> prev2
      | `Current -> History.prev
    in
    let outlines_of_pos state pos =
      let cmp o = Location.compare_pos pos (Outline.item_loc o) in
      let outlines = state.outlines in
      let outlines = History.seek_backward (fun i -> cmp i < 0) outlines in
      let outlines = History.seek_forward (fun i -> cmp i >= 0) outlines in
      fst (History.Sync.rewind fst outlines state.chunks)
    in
    let pos = match pos with
      | Some pos -> pos
      | None -> state.pos
    in
    begin match (command dir) (outlines_of_pos state pos) with
    | None -> state, None
    | Some o -> state, Some o.Outline.loc
    end

  | (Reset None : a request) ->
    State.initial, ()

  | (Reset (Some pos_fname) : a request) ->
    { State.initial with pos =
      { State.initial.pos with Lexing.pos_fname } },
    ()

  | (Refresh `Full : a request) ->
    State.reset_global_modules ();
    Env.reset_cache ();
    let types = Typer.sync state.chunks History.empty in
    {state with types}, true

  | (Refresh `Quick : a request) ->
    State.quick_refresh_modules state

  | (Cd dir : a request) ->
    Sys.chdir dir;
    State.reset_global_modules ();
    state, ()

  | (Errors : a request) ->
    state, State.exns state

  | (Dump (`Env None) : a request) ->
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

  | (Dump (`Env (Some pos)) : a request) ->
    let {Browse.env} = State.node_at state pos in
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

  | (Dump `Sig : a request) ->
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

  | (Dump `Chunks : a request) ->
    let pr_item_desc items = List.map 
        (fun (s,i) -> `List [`String s;`Int i])
        (Chunk.dump_chunk items)
    in
    state, `List (pr_item_desc state.chunks)

  | (Dump `Tree : a request) ->
      let structures = Misc.list_concat_map
        (fun (str,sg) -> Browse.structure str)
        (Typer.trees state.types)
      in
      state, Browse_misc.dump_ts structures

  | (Dump `Outline : a request) ->
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

  | (Dump `Exn : a request) ->
    let exns = State.exns state in
    state, `List (List.rev_map (fun e -> `String (Printexc.to_string e)) exns)

  | (Which_path s : a request) ->
    let filename =
      try Misc.find_in_path_uncap !State.source_path s
      with Not_found ->
        Misc.find_in_path_uncap !Config.load_path s
    in
    state, filename

  | (Which_with_ext ext : a request) ->
    state, Misc.modules_in_path ~ext !State.source_path

  | (Findlib_use packages : a request) ->
    load_packages packages;
    state, ()

  | (Findlib_list : a request) ->
    state, (Fl_package_base.list_packages ())

  | (Extension_list `All : a request) ->
    state, (Extensions_utils.all_extensions ())

  | (Extension_list `Enabled : a request) ->
    state, (Extensions_utils.enabled ())

  | (Extension_list `Disabled : a request) ->
    state, (Extensions_utils.disabled ())

  | (Extension_set (action,extensions) : a request) ->
    let enabled = action = `Enabled in
    List.iter (Extensions_utils.set_extension ~enabled)
              extensions;
    state, ()

  | (Path (var,kind,action,pathes) : a request) -> 
    List.iter (Path_utils.modify ~action ~kind ~var) pathes;
    State.reset_global_modules ();
    state, true 

  | (Path_list `Build : a request) ->
    state, !(fst Path_utils.build)

  | (Path_list `Source : a request) ->
    state, !(fst Path_utils.source)

  | (Path_reset var : a request) ->
    let reset (v,lazy l) = v := l in
    if var = `Both || var = `Build  then reset Path_utils.build;
    if var = `Both || var = `Source then reset Path_utils.source;
    State.reset_global_modules ();
    state, ()

  | (Project_load (cmd,path) : a request) ->
    let f = match cmd with
      | `File -> Dot_merlin.read
      | `Find -> Dot_merlin.find 
    in
    let dot_merlins = f path in
    let path_modify action var ~cwd path = 
      Path_utils.modify ~action ~var ~kind:`Absolute ~cwd path in
    state, (Dot_merlin.exec ~path_modify ~load_packages dot_merlins)

  : state * a)

