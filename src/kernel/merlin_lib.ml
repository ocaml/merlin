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

  (** Create a new project *)
  val create: unit -> t

  (* Current buffer path *)
  val set_local_path : t -> string list -> unit

  (* Project-wide configuration *)
  val set_dot_merlin : t -> Dot_merlin.config option -> [`Ok | `Failures of (string * exn) list]
  val reload_dot_merlin : t -> [`Ok | `Failures of (string * exn) list]
  val autoreload_dot_merlin : t -> [`No | `Ok | `Failures of (string * exn) list]

  (* paths of dot_merlins with mtime at time of load *)
  val get_dot_merlins : t -> (string * float) list

  (* Config override by user *)
  module User : sig
    val reset : t -> unit
    val path : t -> action:[`Add|`Rem] -> var:[`Build|`Source] -> ?cwd:string -> string -> unit
    val load_packages : t -> string list -> [`Ok | `Failures of (string * exn) list]
    val set_extension : t -> enabled:bool -> string -> unit
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
    mutable cfg_flags     : string list list;
    cfg_path_build  : string list ref;
    cfg_path_source : string list ref;
    cfg_path_cmi    : string list ref;
    cfg_path_cmt    : string list ref;
    cfg_path_pkg    : string list ref;
  }

  let empty_config () = {
    cfg_extensions = String.Set.empty;
    cfg_flags = [];
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
    mutable dot_merlins : (string * float) list;
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

  let create () =
    let dot_config = empty_config () in
    let user_config = empty_config () in
    let local_path = ref [] in
    dot_config.cfg_extensions <- String.Set.empty;
    let prepare l = Path_list.(of_list (List.map ~f:of_string_list_ref l)) in
    let flags = Clflags.copy Clflags.initial in
    { dot_merlins = [];
      dot_config; user_config; flags;
      warnings = Warnings.copy Warnings.initial;
      local_path;
      source_path = prepare [
          user_config.cfg_path_source;
          dot_config.cfg_path_source;
          local_path;
        ];
      build_path = prepare [
          user_config.cfg_path_cmi;
          user_config.cfg_path_build;
          dot_config.cfg_path_cmi;
          dot_config.cfg_path_build;
          user_config.cfg_path_pkg;
          dot_config.cfg_path_pkg;
          local_path;
          flags.Clflags.include_dirs;
          flags.Clflags.std_include;
        ];
      cmt_path = prepare [
          user_config.cfg_path_cmt;
          dot_config.cfg_path_cmt;
          user_config.cfg_path_build;
          dot_config.cfg_path_build;
          user_config.cfg_path_pkg;
          dot_config.cfg_path_pkg;
          local_path;
          flags.Clflags.include_dirs;
          flags.Clflags.std_include;
        ];
      global_modules = None;
      keywords_cache = Raw_lexer.keywords [], String.Set.empty;
      validity_stamp = ref true;
    }

  let source_path p = p.source_path
  let build_path  p = p.build_path
  let cmt_path    p = p.cmt_path

  let set_local_path project path =
    if path != !(project.local_path) then begin
      project.local_path := path;
      flush_global_modules project
    end

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
      let result, path = Dot_merlin.path_of_packages pkgs in
      let rpath = project.user_config.cfg_path_pkg in
      rpath := List.filter_dup (path @ !rpath);
      result

    let set_extension project ~enabled path =
      let cfg = project.user_config in
      let f = String.Set.(if enabled then add else remove) in
      cfg.cfg_extensions <- f path cfg.cfg_extensions
  end

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
    Clflags.set := prj.flags ;
    Warnings.set := prj.warnings ;
    failures

  let set_dot_merlin project dm =
    let module Dm = Dot_merlin in
    let dm = match dm with | Some dm -> dm | None -> Dm.empty_config in
    let cfg = project.dot_config in
    let result, path_pkg = Dot_merlin.path_of_packages dm.Dm.packages in
    project.dot_merlins <- List.map dm.Dm.dot_merlins
        ~f:(fun file -> file, file_mtime file);
    cfg.cfg_path_pkg := List.filter_dup (path_pkg @ !(cfg.cfg_path_pkg));
    cfg.cfg_path_build := dm.Dm.build_path;
    cfg.cfg_path_source := dm.Dm.source_path;
    cfg.cfg_path_cmi := dm.Dm.cmi_path;
    cfg.cfg_path_cmt := dm.Dm.cmt_path;
    cfg.cfg_flags <- dm.Dm.flags;
    cfg.cfg_extensions <- String.Set.(of_list dm.Dm.extensions);
    flush_global_modules project;
    match result, update_flags project with
    | _, [] -> result
    | `Ok, lst -> `Failures lst
    | `Failures l1, l2 -> `Failures (List.rev_append l1 l2)

  let reload_dot_merlin project =
    try match project.dot_merlins with
      | [] -> `Ok
      | (path, _) :: _ ->
        let files = Dot_merlin.read ~path in
        let dot_merlins = Dot_merlin.parse files in
        set_dot_merlin project (Some dot_merlins)
    with exn -> `Failures ["reloading", exn]

  let get_dot_merlins project =
    project.dot_merlins

  let autoreload_dot_merlin project =
    match project.dot_merlins with
    | [] -> `No
    | (path, mtime) :: _ ->
      let mtime' = file_mtime path in
      if mtime <> mtime' &&
         (classify_float mtime, classify_float mtime') <> (FP_nan,FP_nan) then
        (reload_dot_merlin project :> [> `Ok | `Failures of _])
      else
        `No

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

  let invalidate ?(flush=true) project =
    if flush then Cmi_cache.flush ();
    project.validity_stamp := false;
    project.validity_stamp <- ref true

  let flush_cache project =
    Cmi_cache.flush ();
end

module Buffer : sig
  type t
  val create: ?path:string -> Project.t -> Parser.state -> t

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
    project : Project.t;
    unit_name : string;
    mutable keywords: Lexer.keywords;
    mutable lexer: (exn list * Lexer.item) History.t;
    mutable recover: (Lexer.item * Recover.t) History.t;
    mutable typer: Typer.t;
  }

  let is_implementation { kind ; _ } = kind = Parser.implementation

  let initial_step kind (_,token) =
    let input = match token with
      | Lexer.Valid (s,t,e) -> s,t,e
      | _ -> assert false
    in
    (token, Recover.fresh (Parser.from kind input))

  let create ?path project kind =
    let path, filename = match path with
      | None -> None, "*buffer*"
      | Some path -> Some (Filename.dirname path), Filename.basename path
    in
    let unit_name =
      try String.sub filename ~pos:0 ~len:(String.index filename '.')
      with Not_found -> filename
    in
    let unit_name = String.capitalize unit_name in
    let lexer = Lexer.empty ~filename in
    Project.setup project;
    {
      path; project; lexer; kind; unit_name;
      keywords = Project.keywords project;
      recover = History.initial (initial_step kind (History.focused lexer));
      typer = Typer.fresh
          ~unit_name ~stamp:(Project.validity_stamp project)
          (Project.extensions project);
    }

  let setup buffer =
    begin match buffer.path with
      | Some path -> Project.set_local_path buffer.project [path]
      | None -> ()
    end;
    Project.setup buffer.project

  let lexer b = b.lexer
  let lexer_errors b = fst (History.focused b.lexer)
  let recover_history b = b.recover
  let recover b = snd (History.focused b.recover)
  let parser b = Recover.parser (recover b)
  let parser_errors b = Recover.exns (recover b)

  let typer b =
    setup b;
    let valid = Typer.is_valid b.typer in
    let valid = valid &&
                String.Set.equal
                  (Typer.extensions b.typer)
                  (Project.extensions b.project) &&
                (Project.autoreload_dot_merlin b.project = `No) in
    if not valid then b.typer <- Typer.fresh
          ~unit_name:b.unit_name
          ~stamp:(Project.validity_stamp b.project)
          (Project.extensions b.project);
    b.typer <- Typer.update (parser b) b.typer;
    b.typer

  let fresh_typer b =
    setup b;
    let typer = Typer.fresh
        ~unit_name:b.unit_name
        ~stamp:(Project.validity_stamp b.project)
        (Project.extensions b.project) in
    Typer.update (parser b) typer

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
