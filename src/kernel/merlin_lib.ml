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

(* Stateful parts:
   - lexer keywords, done Lexer
   - typer snapshot & env, done Typer
   - compiler path, done Project
   - compiler flags, done Project
*)
module Lexer  = Merlin_lexer
module Parser = Merlin_parser
module Typer  = Merlin_typer
module Recover = Merlin_recover

(* Project configuration *)
module Project : sig
  type t

  (* A global store mapping (.merlin-)path to projects *)
  val get : string list -> t * [`Fresh | `Cached]

  (* Current buffer path *)
  val set_local_path : t -> string list -> unit

  (* Project-wide configuration *)
  val autoreload_dot_merlin : t -> unit
  val get_dot_merlins_failure : t -> (string * exn) list

  (* paths of dot_merlins with mtime at time of load *)
  val get_dot_merlins : t -> (string * float) list

  (* Config override by user *)
  module User : sig
    val reset : t -> unit
    val path : t -> action:[`Add|`Rem] -> var:[`Build|`Source] -> ?cwd:string -> string -> unit
    val load_packages : t -> string list -> [`Ok | `Failures of (string * exn) list]
    val set_extension : t -> enabled:bool -> string -> (string * exn) option
    val add_flags : t -> string list -> [`Ok | `Failures of (string * exn) list]
    val clear_flags : t -> [`Ok | `Failures of (string * exn) list]
  end

  (* Path configuration *)
  val source_path : t -> Path_list.t
  val build_path  : t -> Path_list.t
  val cmt_path    : t -> Path_list.t

  (* List all top modules of current project *)
  val global_modules : t -> string list

  (* Force recomputation of top modules *)
  val flush_global_modules : t -> unit

  (* Enabled extensions *)
  val extensions: t -> Extension.set

  (* Lexer keywords for current config *)
  val keywords: t -> Lexer.keywords

  (* Make global state point to current project *)
  val setup : t -> unit

  (* Invalidate cache *)
  val validity_stamp: t -> bool ref
  val invalidate: ?flush:bool -> t -> unit
  val flush_cache: t -> unit

end = struct

  type config = {
    mutable cfg_extensions : Extension.set;
    mutable cfg_flags      : string list list;
    mutable cfg_ppxs       : Ppxsetup.t;
    cfg_path_build  : string list ref;
    cfg_path_source : string list ref;
    cfg_path_cmi    : string list ref;
    cfg_path_cmt    : string list ref;
    cfg_path_pkg    : string list ref;
  }

  let empty_config () = {
    cfg_extensions = String.Set.empty;
    cfg_flags = [];
    cfg_ppxs  = Ppxsetup.empty;
    cfg_path_build  = ref [];
    cfg_path_source = ref [];
    cfg_path_cmi    = ref [];
    cfg_path_cmt    = ref [];
    cfg_path_pkg    = ref [];
  }

  let reset_config cfg =
    cfg.cfg_path_build := [];
    cfg.cfg_path_source := [];
    cfg.cfg_path_cmi := [];
    cfg.cfg_path_cmt := [];
    cfg.cfg_path_pkg := []

  type t = {
    mutable dot_merlins_path : (string * float) list;
    mutable dot_merlins : (string * float) list;
    mutable dot_merlins_failures : (string * exn) list;

    dot_config : config;
    user_config : config;

    mutable flags : Clflags.set;
    mutable warnings : Warnings.set;

    local_path : string list ref;

    source_path : Path_list.t;
    build_path  : Path_list.t;
    cmt_path    : Path_list.t;

    mutable global_modules: string list option;
    mutable keywords_cache: Lexer.keywords * Extension.set;
    mutable validity_stamp: bool ref
  }

  let flush_global_modules project =
    project.global_modules <- None

  let global_modules project =
    match project.global_modules with
    | Some lst -> lst
    | None ->
      let lst =
        Misc.modules_in_path ~ext:".cmi"
          (Misc.Path_list.to_strict_list project.build_path)
      in
      project.global_modules <- Some lst;
      lst

  let source_path p = p.source_path
  let build_path  p = p.build_path
  let cmt_path    p = p.cmt_path

  let set_local_path project path =
    if path != !(project.local_path) then begin
      project.local_path := path;
      flush_global_modules project
    end

  let update_ppxs prj =
    let ppxs = Ppxsetup.union
        prj.user_config.cfg_ppxs
        prj.dot_config.cfg_ppxs in
    prj.flags.Clflags.ppx <-
      Ppxsetup.union prj.flags.Clflags.ppx ppxs

  let update_flags prj =
    let cl = Clflags.arg_spec prj.flags in
    let w  = Warnings.arg_spec prj.warnings in
    let spec = cl @ w in
    let process_flags flags =
      let failures = ref [] in
      let rec loop ?(current=(ref 0)) flags =
        try Arg.parse_argv ~current flags spec (fun flg -> raise (Arg.Bad flg)) "" with
        | Arg.Bad _ ->
          Logger.info Logger.Section.project_load ~title:"flags"
            (sprintf "unknown flag: %s" flags.(!current));
          failures := (flags.(!current), Arg.Bad flags.(!current)) :: !failures ;
          loop ~current flags
        | Arg.Help _ -> (* ignore *)
          loop ~current flags
      in
      loop (Array.of_list ("merlin" :: flags)) ;
      !failures
    in
    let failures =
      List.fold_left prj.dot_config.cfg_flags ~init:[] ~f:(fun acc lst ->
        List.rev_append (process_flags lst) acc
      )
    in
    let failures =
      List.fold_left prj.user_config.cfg_flags ~init:failures ~f:(fun acc lst ->
        List.rev_append (process_flags lst) acc
      )
    in
    update_ppxs prj;
    Clflags.set := prj.flags ;
    Warnings.set := prj.warnings ;
    failures

  let invalidate ?(flush=true) project =
    if flush then Cmi_cache.flush ();
    project.validity_stamp := false;
    project.validity_stamp <- ref true

  (* Config override by user *)
  module User = struct
    let reset project =
      let cfg = project.user_config in
      cfg.cfg_path_build := [];
      cfg.cfg_path_source := [];
      cfg.cfg_path_cmi := [];
      cfg.cfg_path_cmt := [];
      cfg.cfg_flags <- [];
      cfg.cfg_extensions <- String.Set.empty

    let path project ~action ~var ?cwd path =
      let cfg = project.user_config in
      let r = match var with
        | `Source -> cfg.cfg_path_source
        | `Build  -> cfg.cfg_path_build
      in
      let d = canonicalize_filename ?cwd
                (expand_directory Config.standard_library path)
      in
      r := List.filter ~f:((<>) d) !r;
      begin match action with
      | `Add -> r := d :: !r
      | `Rem -> ()
      end;
      flush_global_modules project

    let load_packages project pkgs =
      let result, path, ppxs = Dot_merlin.path_of_packages pkgs in
      let cfg = project.user_config in
      let rpath = cfg.cfg_path_pkg in
      rpath := List.filter_dup (path @ !rpath);
      cfg.cfg_ppxs <- Ppxsetup.union ppxs cfg.cfg_ppxs;
      update_ppxs project;
      result

    let add_flags project flags =
      project.user_config.cfg_flags <- flags :: project.user_config.cfg_flags ;
      invalidate ~flush:false project ;
      match update_flags project with
      | [] -> `Ok
      | lst -> `Failures lst

    let clear_flags project =
      project.user_config.cfg_flags <- [] ;
      invalidate ~flush:false project ;
      match update_flags project with
      | [] -> `Ok
      | lst -> `Failures lst

    let set_extension project ~enabled path =
      let cfg = project.user_config in
      if String.Set.mem path Extension.all then
        let f  = String.Set.(if enabled then add else remove) in
        let () = cfg.cfg_extensions <- f path cfg.cfg_extensions in
        None
      else begin
        Logger.info Logger.Section.project_load ~title:"EXT"
          (sprintf "unknown extensions: \"%s\"" path) ;
        Some (path, Extension.Unknown)
      end
  end

  let set_dot_merlin project paths =
    project.dot_merlins_path <- List.map (fun p -> p, file_mtime p) paths;
    let module Dm = Dot_merlin in
    let rec aux = function
      | [] -> List.Lazy.Nil
      | x :: xs -> Dm.read ~tail:(lazy (aux xs)) x
    in
    let dm = Dm.parse (aux paths) in
    let cfg = project.dot_config in
    let result, path_pkg, ppxs = Dot_merlin.path_of_packages dm.Dm.packages in
    project.dot_merlins <- List.map dm.Dm.dot_merlins
        ~f:(fun file -> file, file_mtime file);
    cfg.cfg_path_pkg := List.filter_dup path_pkg;
    cfg.cfg_path_build := dm.Dm.build_path;
    cfg.cfg_path_source := dm.Dm.source_path;
    cfg.cfg_path_cmi := dm.Dm.cmi_path;
    cfg.cfg_path_cmt := dm.Dm.cmt_path;
    cfg.cfg_flags <- dm.Dm.flags;
    cfg.cfg_ppxs <- ppxs;
    let known_extensions, unknown_extensions =
      List.partition dm.Dm.extensions ~f:(fun ext ->
        String.Set.mem ext Extension.all
      )
    in
    List.iter unknown_extensions ~f:(fun ext ->
      Logger.info Logger.Section.project_load ~title:"EXT"
        (sprintf "unknown extensions: \"%s\"" ext)
    ) ;
    cfg.cfg_extensions <- String.Set.of_list known_extensions;
    flush_global_modules project;
    project.dot_merlins_failures <-
      (match result  with `Ok -> [] | `Failures l -> l) @
      (List.map unknown_extensions ~f:(fun e -> e, Extension.Unknown)) @
      update_flags project;
    invalidate project

  let create dot_merlins =
    let dot_config = empty_config () in
    let user_config = empty_config () in
    let local_path = ref [] in
    let prepare l = Path_list.(of_list (List.map ~f:of_string_list_ref l)) in
    let flags = Clflags.copy Clflags.initial in
    let project =
      { dot_merlins_path = [];
        dot_merlins = [];
        dot_merlins_failures = [];
        dot_config; user_config; flags;
        warnings = Warnings.copy Warnings.initial;
        local_path;
        source_path = prepare [
            user_config.cfg_path_source;
            local_path;
            dot_config.cfg_path_source;
            (* Experimental: used by locate *)
            user_config.cfg_path_pkg;
            dot_config.cfg_path_pkg;
          ];
        build_path = prepare [
            user_config.cfg_path_cmi;
            user_config.cfg_path_build;
            local_path;
            dot_config.cfg_path_cmi;
            dot_config.cfg_path_build;
            user_config.cfg_path_pkg;
            dot_config.cfg_path_pkg;
            flags.Clflags.include_dirs;
            flags.Clflags.std_include;
          ];
        cmt_path = prepare [
            user_config.cfg_path_cmt;
            user_config.cfg_path_build;
            local_path;
            dot_config.cfg_path_cmt;
            dot_config.cfg_path_build;
            user_config.cfg_path_pkg;
            dot_config.cfg_path_pkg;
            flags.Clflags.include_dirs;
            flags.Clflags.std_include;
          ];
        global_modules = None;
        keywords_cache = Raw_lexer.keywords [], String.Set.empty;
        validity_stamp = ref true;
      }
    in
    set_dot_merlin project dot_merlins;
    project

  let store : (string list, t) Hashtbl.t = Hashtbl.create 3
  let get path =
    try Hashtbl.find store path, `Cached
    with Not_found ->
      let project = create path in
      Hashtbl.replace store path project;
      project, `Fresh

  let autoreload_dot_merlin project =
    let out_of_date (path, mtime) =
      let mtime' = file_mtime path in
      mtime <> mtime' &&
      (classify_float mtime, classify_float mtime') <> (FP_nan,FP_nan)
    in
    if List.exists ~f:out_of_date project.dot_merlins ||
       List.exists ~f:out_of_date project.dot_merlins_path
    then
      try
        set_dot_merlin project (List.map fst project.dot_merlins_path);
      with exn ->
        project.dot_merlins_failures <- ["reloading", exn]

  let get_dot_merlins project =
    project.dot_merlins

  let get_dot_merlins_failure project =
    project.dot_merlins_failures

  (* Make global state point to current project *)
  let setup project =
    Config.load_path := project.build_path

  (* Enabled extensions *)
  let extensions project =
    String.Set.union
      project.dot_config.cfg_extensions
      project.user_config.cfg_extensions

  (* Lexer keywords for current config *)
  let keywords project =
    let set = extensions project in
    match project.keywords_cache with
    | kw, set' when String.Set.equal set set' -> kw
    | _ ->
      let kw = Extension.keywords set in
      project.keywords_cache <- kw, set;
      kw

  let validity_stamp p =
    assert !(p.validity_stamp);
    p.validity_stamp

  let flush_cache project =
    Cmi_cache.flush ();
end

module Buffer : sig
  type t
  val create: ?dot_merlins:string list -> ?path:string -> Parser.state -> t

  val project: t -> Project.t

  val lexer: t -> (exn list * Lexer.item) History.t
  val update: t -> (exn list * Lexer.item) History.t -> [`Nothing_done | `Updated]
  val start_lexing: ?pos:Lexing.position -> t -> Lexer.t
  val lexer_errors: t -> exn list

  val parser: t -> Parser.t
  val parser_errors: t -> exn list
  val recover: t -> Recover.t
  val recover_history : t -> (Lexer.item * Recover.t) History.t

  val typer: t -> Typer.t

  val get_mark: t -> Parser.frame option
  val has_mark: t -> Parser.frame option -> bool

  val is_implementation : t -> bool

  (* All top modules of current project, with current module removed *)
  val global_modules: t -> string list
end = struct
  type t = {
    kind: Parser.state;
    path: string option;
    dot_merlins: string list;
    unit_name : string;
    mutable project : Project.t;
    mutable stamp : bool ref;
    mutable keywords: Lexer.keywords;
    mutable lexer: (exn list * Lexer.item) History.t;
    mutable recover: (Lexer.item * Recover.t) History.t;
    mutable typer: Typer.t;
  }

  let invalidate t =
    t.stamp := false;
    t.stamp <- ref true

  let is_implementation { kind ; _ } = kind = Parser.implementation

  let initial_step kind (_,token) =
    let input = match token with
      | Lexer.Valid (s,t,e) -> s,t,e
      | _ -> assert false
    in
    (token, Recover.fresh (Parser.from kind input))

  let find_dot_merlins dot_merlins =
    List.filter_map ~f:Dot_merlin.find dot_merlins

  let autoreload_dot_merlin buffer =
    let project' = buffer.project in
    let project, status = Project.get (find_dot_merlins buffer.dot_merlins) in
    buffer.project <- project;
    match status with
    | `Fresh -> invalidate buffer
    | `Cached ->
      Project.autoreload_dot_merlin project;
      if project' != project then
        invalidate buffer

  let create ?(dot_merlins=[]) ?path kind =
    let path, filename = match path with
      | None -> None, "*buffer*"
      | Some path -> Some (Filename.dirname path), Filename.basename path
    in
    let dot_merlins = match dot_merlins, path with
      | [], Some path -> [path]
      | [], None -> []
      | xs, cwd -> List.map ~f:(Misc.canonicalize_filename ?cwd) xs
    in
    let unit_name =
      try String.sub filename ~pos:0 ~len:(String.index filename '.')
      with Not_found -> filename
    in
    let unit_name = String.capitalize unit_name in
    let lexer = Lexer.empty ~filename in
    let project =
      match Project.get (find_dot_merlins dot_merlins) with
      | project, `Fresh -> project
      | project, `Cached ->
        ignore (Project.autoreload_dot_merlin project);
        project
    in
    let stamp = ref true in
    Project.setup project;
    {
      dot_merlins; path; project; lexer; kind; unit_name; stamp;
      keywords = Project.keywords project;
      recover = History.initial (initial_step kind (History.focused lexer));
      typer = Typer.fresh
          ~unit_name ~stamp:[Project.validity_stamp project; stamp]
          (Project.extensions project);
    }

  let setup buffer =
    autoreload_dot_merlin buffer;
    begin match buffer.path with
      | Some path -> Project.set_local_path buffer.project [path]
      | None -> ()
    end;
    Project.setup buffer.project

  let project t = t.project

  let lexer b = b.lexer
  let lexer_errors b = fst (History.focused b.lexer)
  let recover_history b = b.recover
  let recover b = snd (History.focused b.recover)
  let parser b = Recover.parser (recover b)
  let parser_errors b = Recover.exns (recover b)

  let typer b =
    setup b;
    let valid = Typer.is_valid b.typer &&
                String.Set.equal
                  (Typer.extensions b.typer)
                  (Project.extensions b.project) in
    if not valid then
        b.typer <- Typer.fresh
            ~unit_name:b.unit_name
            ~stamp:[Project.validity_stamp b.project; b.stamp]
            (Project.extensions b.project);
    b.typer <- Typer.update (parser b) b.typer;
    b.typer

  let update t l =
    t.lexer <- l;
    let strong_check (_,token) (token',_) = token == token' in
    let weak_check (_,token) (token',_) = Lexer.equal token token' in
    let init token = initial_step t.kind token in
    let strong_fold (_,token) (_,recover) = token, Recover.fold token recover in
    let weak_update (_,token) (_,recover) = (token,recover) in
    let recover', updated = History.sync t.lexer (Some t.recover)
        ~init ~strong_check ~strong_fold ~weak_check ~weak_update; in
    t.recover <- recover';
    updated

  let start_lexing ?pos b =
    let kw = Project.keywords b.project in
    if kw != b.keywords then begin
      b.keywords <- kw;
      ignore (update b (History.drop_tail (History.seek_backward
                                             (fun _ -> true) b.lexer)))
    end
    else begin
      let pos_pred = match pos with
        | None -> (fun _ -> false)
        | Some pos ->
          let line, _ = Lexing.split_pos pos in
          (fun cur -> let line', _ = Lexing.split_pos cur in line <= line')
      in
      let item_pred = function
        | _, Lexer.Valid (cur,_,_) when pos_pred cur -> true
        | _, Lexer.Valid (p,_,_) when p = Lexing.dummy_pos -> true
        | _, Lexer.Error _ -> true
        | _ -> false
      in
      let lexer = b.lexer in
      let lexer = History.seek_backward item_pred lexer in
      let lexer = History.move (-1) lexer in
      ignore (update b lexer)
    end;
    Lexer.start kw b.lexer

  let get_mark t = Parser.find_marker (parser t)

  let has_mark t = function
    | None -> false
    | Some frame -> Parser.has_marker (parser t) frame

  let global_modules t =
    setup t;
    List.remove t.unit_name (Project.global_modules t.project)
end
