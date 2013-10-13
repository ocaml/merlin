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

type step = State.step = {
  outlines : Outline.t;
  chunks   : Chunk.t;
  types    : Typer.t;
}

type state = State.t = {steps  : step History.t}

module VPrinttyp = State.Verbose_print

module Path_utils : sig
  val set_local_path : string -> unit
  val set_dot_merlin : Dot_merlin.path_config -> unit
  val user_path : action:[`Add | `Rem] ->
                  var:[`Build | `Source] ->
                  ?cwd:string -> string -> unit

  val load_packages : string list -> unit

  val source_path : Path_list.t
  val build_path  : Path_list.t

  val reset : unit -> unit
  val init  : unit -> unit
end = struct
  (* 1. Local path *)
  let local_path = ref []
  let set_local_path path =
    local_path := [path]

  (* 2a. Dot merlin packages *)
  let dot_merlin_packages = ref []
  (* 2b. User packages *)
  let user_packages = ref []
  let load_packages pkgs =
    Extensions_utils.register_packages pkgs;
    user_packages := 
      list_filter_dup (Dot_merlin.packages_path pkgs @ !user_packages)

  (* 2c. Dot merlin path *)
  let dot_merlin_build = ref []
  let dot_merlin_source = ref []

  let set_dot_merlin config =
    dot_merlin_build    := config.Dot_merlin.build_path;
    dot_merlin_source   := config.Dot_merlin.source_path;
    Extensions_utils.register_packages config.Dot_merlin.packages;
    dot_merlin_packages := Dot_merlin.(packages_path config.packages);
    State.reset_global_modules ()

  (* 3. User path *)
  let user_source = ref []
  let user_build  = ref []

  let user_path ~action ~var ?cwd path =
    let r = match var with `Source -> user_source | `Build -> user_build in
    let d = canonicalize_filename ?cwd
              (expand_directory Config.standard_library path)
    in
    r := List.filter ~f:((<>) d) !r;
    match action with
    | `Add -> r := d :: !r
    | `Rem -> ()

  (* Default ocaml library path *)
  let default_path = ref []

  let build_path =
    Path_list.of_list (List.map ~f:Path_list.of_string_list_ref [
      user_build;
      dot_merlin_build;
      user_packages;
      dot_merlin_packages;
      local_path;
      default_path;
    ])

  let source_path =
    Path_list.of_list (List.map ~f:Path_list.of_string_list_ref [
      user_source;
      dot_merlin_source;
      local_path;
    ])

  (* Initialize the search path.
   The current directory is always searched first,
   then the directories specified with the -I option (in command-line order),
   then the standard library directory (unless the -nostdlib option is given).
  *)
  let init () =
    let dirs =
      if !Clflags.use_threads then "+threads" :: !Clflags.include_dirs
      else if !Clflags.use_vmthreads then "+vmthreads" :: !Clflags.include_dirs
      else !Clflags.include_dirs in
    let exp_dirs = List.map (expand_directory Config.standard_library) dirs in
    let exp_dirs = List.rev_append exp_dirs (Clflags.std_include_dir ()) in
    default_path := exp_dirs;
    Config.load_path := build_path;
    Env.reset_cache ()

  let reset () =
    List.iter (fun p -> p := [])
      [dot_merlin_packages;
       dot_merlin_build;
       dot_merlin_source;
       user_packages;
       user_build;
       user_source;
      ];
    State.reset_global_modules ()
end

let init_path = Path_utils.init

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
          try let c = Merlin_types.lookup_constructor longident.Asttypes.txt env in
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

let track_verbosity =
  let tag (Request r) = Obj.tag (Obj.repr r) in
  let h = Hashtbl.create 21 in
  fun st a_request ->
  let tag = tag a_request in
  let cell =
    try Hashtbl.find h tag
    with Not_found ->
      let cell = ref (Misc.Sync.none (),a_request) in
      Hashtbl.add h tag cell;
      cell
  in
  let sync, a_request' = !cell in
  let steps' = History.focused (History.move (-2) st.steps) in
  let action =
    if a_request = a_request' && Sync.same steps' sync
    then `Incr
    else (cell := (Sync.make steps', a_request); `Clear)
  in
  ignore (State.verbosity action)

let location {steps} = Outline.location (History.focused steps).outlines
let position state = (location state).Location.loc_end
let new_step outline steps =
  History.insert (State.step (History.focused steps) outline) steps

let tell i o state request number_of_definitions source =
  Env.reset_missing_cmis ();
  let number_of_definitions = ref number_of_definitions in
  let eod = ref false and eot = ref false in
  let lexbuf = Misc.lex_strings source ~position:(position state)
    begin fun () ->
      if !eot then ""
      else try
        o (Return (request, None));
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
  let onestep first tokens steps =
    let step = History.focused steps in
    let tokens', outline =
      Outline.parse tokens step.outlines lexbuf in
    let stuck = tokens = tokens' in
    let tokens' =
      if stuck
      then (try List.tl tokens' with _ -> tokens')
      else tokens'
    in
    if !eod && not first then decr number_of_definitions;
    let finished =
      (!eod && !number_of_definitions <= 0) || (!eot && (stuck || tokens' = []))
    in
    if finished
    then None, outline
    else Some tokens', outline
  in
  let rec loop steps = function
    | None -> steps
    | Some tokens ->
      let next_tokens, outline = onestep false tokens steps in
      let steps = match outline with
        | None -> steps
        | Some outline -> new_step outline steps
      in
      loop steps next_tokens
  in
  let first steps =
    match Outline.tokens (History.focused steps).outlines with
    | (_ :: _) as tokens
      (* If length > 10000, we are probably just after a big structure.
       * In this case we don't want to reparse the whole chunk. *)
      when length_lessthan 10000 tokens <> None ->
      let steps' = History.move (-1) steps in
      begin match onestep true tokens steps' with
      | tokens', None -> loop steps tokens'
      | tokens', Some outline
        when Outline.tokens outline = tokens ->
        loop steps tokens'
      | tokens', Some outline ->
        loop (new_step outline steps) tokens'
      end
    | _ -> loop steps (Some [])
  in
  let state = {steps = first state.steps} in
  state, Some (position state)


let dispatch (i,o : IO.io) (state : state) =
  fun (type a) (request : a request) ->
  track_verbosity state (Request request);
  let step = History.focused state.steps in
  (match request with
  | (Tell (`Source source) : a request) ->
    tell i o state request 0 source
  | (Tell (`Definitions defs) : a request) ->
    tell i o state request defs "" 
  | (Tell _ : a request) -> IO.invalid_arguments ()
  | (Type_expr (source, None) : a request) ->
    let env = Typer.env (History.focused state.steps).types in
    let ppf, to_string = Misc.ppf_to_string () in
    Type_utils.type_in_env env ppf source;
    state, to_string ()

  | (Type_expr (source, Some pos) : a request) ->
    let {Browse.env} = State.node_at state pos in
    let ppf, to_string = Misc.ppf_to_string () in
    Type_utils.type_in_env env ppf source;
    state, to_string ()

  | (Type_enclosing ((expr, offset), pos) : a request) ->
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
    (* usual enclosings in typed tree. *)
    let structures = State.browse step in
    let path = Browse.enclosing pos structures in
    let result = List.filter_map ~f:aux path in
    (* enclosings of cursor in given expression *)
    let exprs =
      let len = String.length expr in
      let rec aux acc i =
        if i >= len then
          List.rev (expr :: acc)
        else if expr.[i] = '.' then
          aux (String.sub expr ~pos:0 ~len:i (* i-1 ? *) :: acc) (succ i)
        else
          aux acc (succ i)
      in
      aux [] offset
    in
    let small_enclosings =
      let {Browse.env} = State.node_at state pos in
      let loc_start =
        let l, c = Misc.split_pos pos in
        Misc.make_pos (l, c - offset)
      in
      let shift loc int =
        let l, c = Misc.split_pos loc in
        Misc.make_pos (l, c + int)
      in
      List.filter_map exprs ~f:(fun source ->
        try
          let loc = { Location.
            loc_start ;
            loc_end = shift loc_start (String.length source) ;
            loc_ghost = false ;
          }
          in
          let ppf, to_string = Misc.ppf_to_string () in
          Type_utils.type_in_env env ppf source;
          Some (loc, to_string ())
        with _ ->
          None
      )
    in
    state, small_enclosings @ result

  | (Complete_prefix (prefix, None) : a request) ->
    let node = Browse.({dummy with env = Typer.env step.types}) in
    let compl = State.node_complete node prefix in
    state, List.rev compl

  | (Complete_prefix (prefix, Some pos) : a request) ->
    let node = State.node_at state pos in
    let compl = State.node_complete node prefix in
    state, List.rev compl

  | (Locate (path, opt_pos) : a request) ->
    let node, local_modules, local_defs =
      match opt_pos with
      | None -> Browse.({ dummy with env = Typer.env step.types }), [], []
      | Some pos -> (
          State.node_at state pos,
          List.map (State.local_modules_at state pos)
            ~f:(fun { Location. txt ; loc } -> txt, loc),
          State.str_items_before state pos
        )
    in
    begin match
      Track_definition.from_string
        ~sources:Path_utils.source_path
        ~env:(node.Browse.env)
        ~local_defs
        ~local_modules
        path
    with
    | None -> state, None
    | Some (file, loc) ->
      Logger.log `locate
        (sprintf "--> %s" (match file with None -> "<local buffer>" | Some f -> f)) ;
      let pos = loc.Location.loc_start in
      state, Some (file, pos)
    end

  | (Drop : a request) ->
    let state = {steps = History.modify (fun x -> x) state.steps} in
    state, position state

  | (Seek `Position : a request) ->
    state, position state

  | (Seek (`Before pos) : a request) ->
    let inv step = Outline.invalid step.outlines in
    let cmp step = Merlin_parsing.compare_pos pos (Outline.location step.outlines) in
    let steps = state.steps in
    let steps = History.seek_forward (fun i -> inv i || cmp i > 0) steps in
    let steps = History.seek_backward
      (fun step -> match step.outlines with
       (*| {Outline.tokens = []} -> true*)
       | _ -> inv step || cmp step <= 0)
      steps
    in
    let state = {steps} in
    state, position state

  | (Seek (`Exact pos) : a request) ->
    let inv step = Outline.invalid step.outlines in
    let cmp step = Merlin_parsing.compare_pos pos (Outline.location step.outlines) in
    let steps = state.steps in
    let steps = History.seek_backward (fun i -> inv i || cmp i < 0) steps in
    let steps = History.seek_forward (fun i -> inv i || cmp i > 0) steps in
    let state = {steps} in
    state, position state

  | (Seek `End : a request) ->
    let steps = state.steps in
    let steps = History.seek_forward (fun _ -> true) steps in
    let state = {steps} in
    state, position state

  | (Seek `Maximize_scope : a request) ->
    let rec loop steps =
      let steps' = History.move 1 steps in
      if Outline.Spine.position (History.focused steps').outlines <=
         Outline.Spine.position (History.focused steps).outlines
      then steps
      else loop steps'
    in
    let steps = loop state.steps in
    let state = {steps} in
    state, position state

  | (Boundary (dir,pos) : a request) ->
    let count = match dir with
      | `Next    -> 1
      | `Prev    -> -1
      | `Current -> 0
    in
    let move steps =
      let steps = History.move count steps in
      if count <> 0 && History.focused steps == History.focused state.steps
      then None
      else Some steps
    in
    let steps_at_pos steps = function
      | None -> steps
      | Some pos ->
        let cmp step = Merlin_parsing.compare_pos pos
                         (Outline.location step.outlines)
        in
        let steps = History.seek_backward (fun i -> cmp i < 0) steps in
        let steps = History.seek_forward (fun i -> cmp i > 0) steps in
        steps
    in
    state,
    begin match move (steps_at_pos state.steps pos) with
    | None -> None
    | Some steps ->
      Some (Outline.location (History.focused steps).outlines)
    end

  | (Reset None : a request) ->
    State.initial_str "", ()

  | (Reset (Some name) : a request) ->
    State.initial_str name, ()

  | (Refresh `Full : a request) ->
    State.reset_global_modules ();
    Env.reset_cache ();
    State.retype state, true

  | (Refresh `Quick : a request) ->
    State.quick_refresh_modules state

  | (Cd dir : a request) ->
    Sys.chdir dir;
    State.reset_global_modules ();
    state, ()

  | (Errors : a request) ->
    state, State.exns state

  | (Dump (`Env None) : a request) ->
    let sg = Browse_misc.signature_of_env (Typer.env step.types) in
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
    state, `List (List.map ~f:aux sg)

  | (Dump `Sig : a request) ->
      let trees = Typer.trees step.types in
      let sg = Misc.list_concat_map (fun {Location.txt} -> txt.Typedtree.str_type) trees in
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
      state, `List (List.map ~f:aux sg)

  | (Dump `Chunks : a request) ->
    let pr_item_desc items = List.map
        (fun s -> `String s)
        (Chunk.Spine.dump items)
    in
    state, `List (pr_item_desc (History.focused state.steps).chunks)

  | (Dump `Tree : a request) ->
    let structures = State.browse step in
    state, Browse_misc.dump_ts structures

  | (Dump `Outline : a request) ->
    let print_item label _ tokens=
      let tokens =
        String.concat " "
          (List.map tokens ~f:(fun (t,_,_) ->
            (Chunk_parser_utils.token_to_string t)))
      in
      label ^ "(" ^ tokens ^ ")"
    in
    let outlines = (History.focused state.steps).outlines in
    state, `List (List.map ~f:(fun s -> `String s)
                    (Outline.Spine.dump outlines
                       ~sig_item:print_item ~str_item:print_item))
  | (Dump `History : a request) ->
    state,
    let entry s =
      let {Location. loc_start; loc_end} = Outline.location s.outlines in
      let l1,c1 = Misc.split_pos loc_start in
      let l2,c2 = Misc.split_pos loc_end   in
      let tokens = Outline.tokens s.outlines in
      let tokens = List.map (fun (tok,_,_) ->
          `String (Chunk_parser_utils.token_to_string tok)) tokens
      in
      `List [`String (Printf.sprintf "%d:%d-%d:%d" l1 c1 l2 c2); `List tokens]
    in
    let rec aux acc = function
      | History.One x -> entry x :: acc
      | History.More (x,xs) ->  aux (entry x :: acc) xs
    in
    `Assoc ["head", `List (aux [] (History.head state.steps))
           ;"tail", `List (List.map ~f:entry (History.tail state.steps))]


  | (Dump `Exn : a request) ->
    let exns = State.exns state in
    state, `List (List.rev_map ~f:(fun e -> `String (Printexc.to_string e)) exns)

  | (Which_path s : a request) ->
    let filename =
      try Misc.find_in_path_uncap Path_utils.source_path s
      with Not_found ->
        Misc.find_in_path_uncap !Config.load_path s
    in
    state, filename

  | (Which_with_ext ext : a request) ->
    state, Misc.modules_in_path ~ext
            (Path_list.to_strict_list Path_utils.source_path)

  | (Findlib_use packages : a request) ->
    Path_utils.load_packages packages;
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
    List.iter extensions ~f:(Extensions_utils.set_extension ~enabled) ;
    state, ()

  | (Path (var,action,pathes) : a request) ->
    List.iter ~f:(Path_utils.user_path ~action ~var ?cwd:None) pathes;
    State.reset_global_modules ();
    state, true

  | (Path_list `Build : a request) ->
    state, Path_list.to_strict_list Path_utils.build_path

  | (Path_list `Source : a request) ->
    state, Path_list.to_strict_list Path_utils.source_path

  | (Path_reset : a request) ->
    Path_utils.reset ();
    State.reset_global_modules ();
    state, ()

  | (Project_load (cmd,path) : a request) ->
    let f = match cmd with
      | `File -> Dot_merlin.read
      | `Find -> Dot_merlin.find
    in
    let dot_merlins = f path in
    let config = Dot_merlin.exec dot_merlins in
    Path_utils.set_dot_merlin config;
    state, config.Dot_merlin.dot_merlins

  : state * a)
