open Std
open Misc

module Lexer: sig
  type keywords = Raw_lexer.keywords

  (* Lexing is split in two steps.

     First the list of tokens is represented by a [item History.t].
     It's a pure value, independent of the context.

     Second the process of lexing is represented by values of type [t].  You
     resume the process from an arbitrary list of tokens, feeding it with one
     or more string, and you can extract the current list of tokens and cursor
     position at any time.
     Beware, the cursor may be in the middle of a not yet determined token.

     The process ultimately ends when fed with the empty string, representing
     EOF.
  *)

  (* Lexing step *)
  type item =
    | Valid of Lexing.position * Raw_parser.token * Lexing.position
    | Error of Raw_lexer.error * Location.t

  (** Create an empty list new lexer *)
  val empty: filename:string -> item History.t

  (** Prepare for lexing.
      Returns the start position (end position of last valid token), and a
      lexing function that will append at most one token to the history at each
      call. *)
  type t
  val history: t -> item History.t
  val start: keywords -> item History.t -> t
  val position: t -> Lexing.position
  val feed: t -> string -> bool
  val eof: t -> bool
end = struct
  type keywords = Raw_lexer.keywords

  (* Lexing step *)
  type item =
    | Valid of Lexing.position * Raw_parser.token * Lexing.position
    | Error of Raw_lexer.error * Location.t

  (** Create an empty list new lexer *)
  let empty ~filename =
    let pos =
      { Lexing.
        pos_fname = filename;
        pos_lnum  = 1;
        pos_bol   = 0;
        pos_cnum  = 0;
      }
    in
    History.initial (Valid (pos, Raw_parser.ENTRYPOINT, pos))

  type t = {
    (* Result *)
    mutable history: item History.t;
    (* Input buffer *)
    refill: string option ref;
    (* Lexer data *)
    state: Raw_lexer.state;
    lexbuf: Lexing.lexbuf;
    mutable resume: (unit -> Raw_parser.token Raw_lexer.result) option;
  }

  let history t = t.history

  (** Prepare for lexing.
      Returns the start position (end position of last valid token), and a
      lexing function that will append at most one token to the history at each
      call. *)
  let make_lexbuf refill position =
    Lexing.from_strings ~position ""
      (fun () ->
         match !refill with
         | Some s -> refill := None; s
         | None -> "")

  let start keywords history =
    let position = match History.focused history with
      | Valid (_,_,p) -> p
      | Error (_,l) -> l.Location.loc_end
    in
    let refill = ref None in
    let lexbuf = make_lexbuf refill position in
    {
      state = Raw_lexer.make keywords;
      resume = None;
      history; refill; lexbuf;
    }

  let position t = Lexing.immediate_pos t.lexbuf

  let feed t str =
    if not t.lexbuf.Lexing.lex_eof_reached then begin
      t.refill := Some str;
      let append item =
        t.history <- History.insert item t.history
      in
      let rec aux = function
        (* Lexer interrupted, there is data to refill: continue. *)
        | Raw_lexer.Refill f when !(t.refill) <> None ->
          aux (f ())
        (* Lexer interrupted, nothing to refill, return to caller. *)
        | Raw_lexer.Refill r ->
          t.resume <- Some r
        (* EOF Reached: notify EOF to parser, stop now *)
        | Raw_lexer.Return Raw_parser.EOF ->
          begin match History.focused t.history with
            | Valid (_,Raw_parser.EOF,_) -> ()
            | _ ->
              append (Valid (t.lexbuf.Lexing.lex_start_p,
                             Raw_parser.EOF,
                             t.lexbuf.Lexing.lex_curr_p));
          end
        | Raw_lexer.Return token ->
          append (Valid (t.lexbuf.Lexing.lex_start_p,
                         token,
                         t.lexbuf.Lexing.lex_curr_p));
          continue ()
        | Raw_lexer.Error (e,l) ->
          append (Error (e,l));
          continue ()
      and continue () =
        aux (Raw_lexer.token t.state t.lexbuf)
      in
      begin match t.resume with
      | Some f ->
        t.resume <- None;
        aux (f ())
      | None -> continue ()
      end;
      true
    end
    else
      false

  let eof t = t.lexbuf.Lexing.lex_eof_reached
end

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

  (* TEMPORARY *)
  val chosen_protocol : string option
end = struct

  (** Mimic other OCaml tools entry point *)
  module Flags = struct

    let chosen_protocol = ref None

    (* Parse arguments specified on commandline *)
    module Initial = Top_options.Make (struct
      let _projectfind path =
        let dot_merlins = Dot_merlin.find path in
        begin match Dot_merlin.project_name dot_merlins with
        | Some name -> print_endline name
        | None -> ()
        end;
        exit 0
      let _protocol p =
        chosen_protocol := Some p
    end)

    let () =
      (* Parse arguments on commandline *)
      Arg.parse Initial.list Top_options.unexpected_argument
        "Usage: ocamlmerlin [options]\noptions are:"

    (* Save flags, so as to restore them when changing project *)
    let initial_flags = Clflags.snapshot ()
    let reset_flags () = Clflags.restore initial_flags

    (* Parse flags specified at runtime (project-wide or entered by user) *)
    let err_log msg = Logger.error `dot_merlin msg

    module Incremental = Top_options.Make (struct
        let _projectfind _ =
          err_log "unsupported flag \"-project-find\" (ignored)"
        let _protocol _ =
          err_log "unsupported flag \"-protocol\" (ignored)"
    end)

    let enable_flags flags =
      begin try
        Arg.parse_argv ~current:(ref (-1)) (Array.of_list flags) Incremental.list
          Top_options.unexpected_argument "error..."
      with (* FIXME *)
      | Arg.Bad msg -> err_log msg
      | Arg.Help msg -> err_log msg
      end

    let default_path =
      let dirs =
        if !Clflags.use_threads then "+threads" :: !Clflags.include_dirs
        else if !Clflags.use_vmthreads then "+vmthreads" :: !Clflags.include_dirs
        else !Clflags.include_dirs in
      let exp_dirs = List.map (expand_directory Config.standard_library) dirs in
      let exp_dirs = List.rev_append exp_dirs (Clflags.std_include_dir ()) in
      ref exp_dirs
  end
  let chosen_protocol = !Flags.chosen_protocol

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

    mutable flags : Clflags.snapshot;

    local_path : string list ref;

    source_path : Path_list.t;
    build_path  : Path_list.t;
    cmt_path    : Path_list.t;

    mutable global_modules: string list option;

    mutable keywords_cache : Lexer.keywords * Extension.set;
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
    dot_config.cfg_extensions <- Extension.default;
    let prepare l = Path_list.(of_list (List.map ~f:of_string_list_ref l)) in
    { dot_config; user_config;
      flags = Flags.initial_flags;
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
          Flags.default_path;
        ];
      cmt_path = prepare [
          user_config.cfg_path_cmt;
          dot_config.cfg_path_cmt;
          user_config.cfg_path_build;
          dot_config.cfg_path_build;
          user_config.cfg_path_pkg;
          dot_config.cfg_path_pkg;
          local_path;
          Flags.default_path;
        ];
      global_modules = None;
      keywords_cache = Raw_lexer.keywords [], String.Set.empty;
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
    cfg.cfg_extensions <-
      String.Set.(union Extension.default (of_list dm.Dm.extensions));
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
end
let chosen_protocol = Project.chosen_protocol

module Parser = Merlin_parser

module Buffer : sig
  type t
  val create: ?path:string -> Project.t -> Parser.state -> t

  val lexer: t -> Lexer.item History.t
  val update: t -> Lexer.item History.t -> Parser.t
  val start_lexing: t -> Lexer.t

  val parser: t -> Parser.t
  val path: t -> Parser.path
end = struct
  type step = {
    token: Lexer.item;
    parser: Parser.t;
    parser_path: Parser.Path.t;
  }

  type t = {
    path: string option;
    project : Project.t;
    env: Env.cache;
    btype: Btype.cache;
    mutable keywords: Lexer.keywords;
    mutable lexer: Lexer.item History.t;
    mutable steps: step History.t;
  }

  let create ?path project parser_state =
    let path, filename = match path with
      | None -> None, "*buffer*"
      | Some path -> Some (Filename.dirname path), Filename.basename path
    in
    let lexer = Lexer.empty ~filename in
    let step =
      {
        token = History.focused lexer;
        parser = Parser.from parser_state;
        parser_path = Parser.Path.empty;
      }
    in
    let steps = History.initial step in
    {
      path; project; lexer; steps;
      keywords = Project.keywords project;
      env = Env.new_cache ();
      btype = Btype.new_cache ();
    }

  let setup buffer =
    Project.setup buffer.project;
    begin match buffer.path with
      | Some path -> Project.set_local_path buffer.project [path]
      | None -> ()
    end;
    Env.set_cache buffer.env;
    Btype.set_cache buffer.btype

  let lexer b = b.lexer
  let step b = History.focused b.steps
  let parser b = (step b).parser
  let path b = Parser.Path.get (step b).parser_path

  let update t l =
    t.lexer <- l;
    t.steps <- History.sync
        ~check:(fun a a' -> a == a'.token)
        ~init:(fun _ -> assert false)
        ~fold:(fun token step ->
            let result =
              match token with
              | Lexer.Error _ -> {step with token}
              | Lexer.Valid (s,t,e) ->
                prerr_endline (Parser.Values.Token.to_string t);
                match Parser.feed (s,t,e) step.parser with
                | `Accept _ -> prerr_endline "Accept"; {step with token}
                | `Reject _ -> prerr_endline "Reject"; {step with token}
                | `Step parser ->
                  {
                    token; parser;
                    parser_path = Parser.Path.update' parser step.parser_path
                  }
            in
            Parser.dump Format.std_formatter result.parser;
            result
          )
        t.lexer
        (Some t.steps);
    parser t

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

