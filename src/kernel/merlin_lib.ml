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
    { dot_config; user_config; flags;
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

  let set_dot_merlin project dm =
    let module Dm = Dot_merlin in
    let dm = match dm with | Some dm -> dm | None -> Dm.empty_config in
    let cfg = project.dot_config in
    let result, path_pkg = Dot_merlin.path_of_packages dm.Dm.packages in
    cfg.cfg_path_pkg := List.filter_dup (path_pkg @ !(cfg.cfg_path_pkg));
    cfg.cfg_path_build := dm.Dm.build_path;
    cfg.cfg_path_source := dm.Dm.source_path;
    cfg.cfg_path_cmi := dm.Dm.cmi_path;
    cfg.cfg_path_cmt := dm.Dm.cmt_path;
    cfg.cfg_flags <- dm.Dm.flags;
    cfg.cfg_extensions <- String.Set.(of_list dm.Dm.extensions);
    flush_global_modules project;
    result

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
end

module Buffer : sig
  type t
  val create: ?path:string -> Project.t -> Parser.state -> t

  val lexer: t -> Lexer.item History.t
  val update: ?ignore_eof:bool -> t -> Lexer.item History.t -> unit
  val start_lexing: t -> Lexer.t

  val parser: t -> Parser.t
  val parser_errors: t -> exn list
  val recover: t -> Recover.t
  val path: t -> Parser.path
  val typer: t -> Typer.t
  val fresh_typer: t -> Typer.t
end = struct
  type t = {
    kind: Parser.state;
    path: string option;
    project : Project.t;
    mutable keywords: Lexer.keywords;
    mutable lexer: Lexer.item History.t;
    mutable parser: (Lexer.item * Merlin_recover.t) History.t;
    mutable typer: Merlin_typer.t;
    mutable parser_path: Parser.Path.t;
    mutable validity_stamp: bool ref;
  }

  let initial_step kind token =
    let input = match token with
      | Lexer.Valid (s,t,e) -> s,t,e
      | _ -> assert false
    in
    (token, Merlin_recover.fresh (Parser.from kind input))

  let create ?path project kind =
    let path, filename = match path with
      | None -> None, "*buffer*"
      | Some path -> Some (Filename.dirname path), Filename.basename path
    in
    let lexer = Lexer.empty ~filename in
    Project.setup project;
    {
      path; project; lexer; kind;
      keywords = Project.keywords project;
      parser = History.initial (initial_step kind (History.focused lexer));
      parser_path = Parser.Path.empty;
      typer = Merlin_typer.fresh (Project.extensions project);
      validity_stamp = Project.validity_stamp project;
    }

  let setup buffer =
    begin match buffer.path with
      | Some path -> Project.set_local_path buffer.project [path]
      | None -> ()
    end;
    Project.setup buffer.project

  let lexer b = b.lexer
  let recover b = snd (History.focused b.parser)
  let parser b = Merlin_recover.parser (recover b)
  let parser_errors b = Merlin_recover.exns (recover b)

  let path b =
    let parser_path = Parser.Path.update' (parser b) b.parser_path in
    b.parser_path <- parser_path;
    Parser.Path.get parser_path

  let typer b =
    setup b;
    let need_refresh = not !(b.validity_stamp) in
    if need_refresh then
      b.validity_stamp <- Project.validity_stamp b.project;
    let need_refresh = need_refresh ||
                       not (Merlin_typer.is_valid b.typer) ||
                       not (String.Set.equal
                              (Merlin_typer.extensions b.typer)
                              (Project.extensions b.project))
    in
    if need_refresh then
      b.typer <- Merlin_typer.fresh (Project.extensions b.project);
    let typer = Merlin_typer.update (parser b) b.typer  in
    b.typer <- typer;
    typer

  let fresh_typer b =
    setup b;
    let typer = Merlin_typer.fresh (Project.extensions b.project) in
    Merlin_typer.update (parser b) typer

  let update ?(ignore_eof=true) t l =
    t.lexer <- l;
    let check token (token',_) = token == token' in
    let init token = initial_step t.kind token in
    let fold token (_,recover) =
      match token with
      | Lexer.Valid (_,Raw_parser.EOF,_)
        when ignore_eof -> (token,recover)
      | _ -> (token,Merlin_recover.fold token recover)
    in
    t.parser <- History.sync ~check ~init ~fold t.lexer (Some t.parser)

  let start_lexing b =
    let kw = Project.keywords b.project in
    if kw != b.keywords then begin
      b.keywords <- kw;
      ignore (update b (History.drop_tail (History.seek_backward
                                             (fun _ -> true) b.lexer)))
    end else begin
      ignore (update b (History.seek_backward
                          (function ( Lexer.Valid (_,Raw_parser.EOF,_)
                                    | Lexer.Error _) -> true
                                  | _ -> false)
                          b.lexer))
    end;
    Lexer.start kw b.lexer
end
