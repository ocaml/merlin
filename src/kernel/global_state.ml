open Std
open Misc

(** Mimic other OCaml tools, entry point *)
module Flags = struct

  (* Parse arguments specified on commandline *)
  module Initial = Top_options.Make (struct
    let _projectfind path =
      let dot_merlins = Dot_merlin.find path in
      begin match Dot_merlin.project_name dot_merlins with
      | Some name -> print_endline name
      | None -> ()
      end;
      exit 0
  end)

  let () =
    (* Parse arguments on commandline *)
    Arg.parse Initial.list Top_options.unexpected_argument
      "Usage: ocamlmerlin [options]\noptions are:"

  (* Save flags, so as to restore them when changing project *)
  let initial_flags = Clflags.snapshot ()
  let reset_flags () = Clflags.restore initial_flags

  (* Parse incremental arguments specified during runtime
      (project-wide or manually entered by user) *)
  let err_log msg = Logger.error `dot_merlin msg

  module Incremental = Top_options.Make (struct
    let _projectfind _ = err_log "unsupported flag \"-project-find\" (ignored)" ;
  end)

  let enable_flags flags =
    begin try
      Arg.parse_argv ~current:(ref (-1)) (Array.of_list flags) Incremental.list
        Top_options.unexpected_argument "error..."
    with (* FIXME *)
    | Arg.Bad msg -> err_log msg
    | Arg.Help msg -> err_log msg
    end
end

(** Project configuration management *)
module Project : sig
  (* Current buffer path *)
  val set_local_path : string -> unit

  (* Project-wide configuration *)
  val set_dot_merlin
    : Dot_merlin.path_config -> [`Ok | `Failures of (string * exn) list]

  val reset_project : unit -> unit

  (* Config override by user *)
  val reset_user : unit -> unit
  val user_path : action:[`Add | `Rem] ->
                  var:[`Build | `Source] ->
                  ?cwd:string -> string -> unit

  val user_load_packages
    : string list -> [`Ok | `Failures of (string * exn) list]

  val user_set_extension : enabled:bool -> string -> unit

  (* Output values *)
  val source_path : Path_list.t
  val build_path  : Path_list.t
  val cmt_path    : Path_list.t

  (* List all top modules of current project *)
  val global_modules : unit -> string list
  (* Force recomputing list of global modules *)
  val flush_global_modules : unit -> unit

  val init  : unit -> unit
end = struct

  let global_modules = ref None
  let flush_global_modules () =
    global_modules := None

  (** Extensions **)
  let dot_merlin_extensions = ref []
  let user_extensions = ref []

  let update_extensions () =
    Extensions_utils.set_extensions
      (List.filter_dup (!dot_merlin_extensions @ !user_extensions))

  let user_set_extension ~enabled name =
    (if enabled
     then user_extensions := List.filter_dup (name :: !user_extensions)
     else user_extensions := List.filter ~f:((<>) name) !user_extensions);
    update_extensions ()

  (** Flags **)
  let dot_merlin_flags = ref []
  let user_flags = ref []

  let update_flags () =
    Flags.reset_flags ();
    List.iter Flags.enable_flags !dot_merlin_flags;
    Flags.enable_flags !user_flags

  let dot_merlin_set_flags flags =
    dot_merlin_flags := flags;
    update_flags ()

  let user_set_flags flags =
    user_flags := flags;
    update_flags ()

  (** Path **)
  (* 1. Local path *)
  let local_path = ref []
  let set_local_path path =
    local_path := [path];
    flush_global_modules ()

  (* 2a. Dot merlin packages *)
  let dot_merlin_packages = ref []

  (* 2b. User packages *)
  let user_packages = ref []
  let user_load_packages pkgs =
    let exts = Extensions_utils.extensions_from_packages pkgs in
    user_extensions :=
      List.filter_dup (exts @ !user_extensions);
    update_extensions ();
    let failures, pathes = Dot_merlin.packages_path pkgs in
    user_packages := List.filter_dup (pathes @ !user_packages);
    failures

  (* 2c. Dot merlin path *)
  let dot_merlin_cmi    = ref []
  let dot_merlin_cmt    = ref []
  let dot_merlin_build  = ref []
  let dot_merlin_source = ref []

  let set_dot_merlin config =
    dot_merlin_set_flags config.Dot_merlin.flags;
    dot_merlin_build  := config.Dot_merlin.build_path;
    dot_merlin_source := config.Dot_merlin.source_path;
    dot_merlin_cmi    := config.Dot_merlin.cmi_path;
    dot_merlin_cmt    := config.Dot_merlin.cmt_path;
    let exts = Extensions_utils.extensions_from_packages
        config.Dot_merlin.packages in
    dot_merlin_extensions := exts @ config.Dot_merlin.extensions;
    update_extensions ();
    let failures, pathes = Dot_merlin.(packages_path config.packages) in
    dot_merlin_packages := pathes;
    flush_global_modules ();
    failures

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
      dot_merlin_cmi;
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

  let cmt_path =
    Path_list.of_list (List.map ~f:Path_list.of_string_list_ref [
      dot_merlin_cmt;
      user_build;
      dot_merlin_build;
      user_packages;
      dot_merlin_packages;
      local_path;
      default_path;
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

  let reset_project () =
    List.iter (fun p -> p := [])
      [dot_merlin_packages;
       dot_merlin_build;
       dot_merlin_source;
      ];
    flush_global_modules ()

  let reset_user () =
    List.iter (fun p -> p := [])
      [user_packages;
       user_build;
       user_source;
      ];
    flush_global_modules ()

  let global_modules () =
    match !global_modules with
    | Some lst -> lst
    | None ->
      let lst = Misc.modules_in_path ~ext:".cmi"
                  (Misc.Path_list.to_strict_list build_path)
      in
      global_modules := Some lst;
      lst
end

let () =
  (* Setup logging *)
  begin try
    let dest = Sys.getenv "MERLIN_LOG" in
    Logger.set_default_destination dest ;
    Logger.monitor ~dest `protocol
  with _ ->
    ()
  end;
  at_exit Logger.shutdown;
  (* Initialize path management *)
  Project.init ();
  (* Initialize findlib *)
  Findlib.init ()
