open Std

(** {1 OCaml commandline parsing} *)

type ocaml = {
  include_dirs         : string list;
  no_std_include       : bool;
  unsafe               : bool;
  classic              : bool;
  principal            : bool;
  real_paths           : bool;
  threads              : [ `None | `Threads | `Vmthreads ];
  recursive_types      : bool;
  strict_sequence      : bool;
  applicative_functors : bool;
  unsafe_string        : bool;
  nopervasives         : bool;
  strict_formats       : bool;
  open_modules         : string list;
  ppx                  : string list;
  pp                   : string;
  warnings             : Warnings.state;
}

let dump_warnings st =
  let st' = Warnings.backup () in
  Warnings.restore st;
  Misc.try_finally Warnings.dump
    (fun () -> Warnings.restore st')

let dump_ocaml x = `Assoc [
    "include_dirs"         , `List (List.map ~f:Json.string x.include_dirs);
    "no_std_include"       , `Bool x.no_std_include;
    "unsafe"               , `Bool x.unsafe;
    "classic"              , `Bool x.classic;
    "principal"            , `Bool x.principal;
    "real_paths"           , `Bool x.real_paths;
    "recursive_types"      , `Bool x.recursive_types;
    "strict_sequence"      , `Bool x.strict_sequence;
    "applicative_functors" , `Bool x.applicative_functors;
    "unsafe_string"        , `Bool x.unsafe_string;
    "nopervasives"         , `Bool x.nopervasives;
    "strict_formats"       , `Bool x.strict_formats;
    "open_modules"         , `List (List.map ~f:Json.string x.open_modules);
    "ppx"                  , `List (List.map ~f:Json.string x.ppx);
    "pp"                   , `String x.pp;
    "warnings"             , dump_warnings x.warnings;
  ]

(** Some paths can be resolved relative to a current working directory *)

let cwd = ref None

let resolve_relative_path ?cwd:path f =
  let_ref cwd path f

let canonicalize_filename path =
  Misc.canonicalize_filename ?cwd:!cwd path

let marg_path f =
  Marg.param "path" (fun path acc -> f (canonicalize_filename path) acc)

let marg_exec_path f =
  Marg.param "path" (fun path acc ->
      (* Don't canonicalize if relative path can be resolved by looking up
         PATH. *)
      if Filename.basename path = path then
        f path acc
      else
        f (canonicalize_filename path) acc)

(** {1 Findlib configuration} *)

type findlib = {
  conf : string option;
  path : string list;
  toolchain : string option;
}

let dump_findlib x = `Assoc [
    "conf", Json.option Json.string x.conf;
    "path", `List (List.map ~f:Json.string x.path);
    "toolchain", Json.option Json.string x.toolchain;
  ]

(** {1 Merlin high-level settings} *)

type flag_list = {
  flag_cwd : string option;
  flag_list : string list;
}

let flag_list ?(cwd=(!cwd)) flag_list =
  { flag_cwd = cwd; flag_list }

type merlin = {
  build_path  : string list;
  source_path : string list;
  cmi_path    : string list;
  cmt_path    : string list;
  extensions  : string list;
  suffixes    : (string * string) list;
  stdlib      : string option;
  reader      : string list;
  protocol    : [`Json | `Sexp];
  log_file    : string option;
  trace       : bool;

  exclude_query_dir : bool;

  flags_to_apply    : flag_list list;
  packages_to_load  : string list;

  flags_applied    : flag_list list;
  dotmerlin_loaded : string list;
  packages_loaded  : string list;

  packages_path : string list;
  packages_ppx  : Ppxsetup.t;

  failures    : string list;

  extension_to_reader : (string * string) list

}

let dump_merlin x =
  let dump_flag_list { flag_cwd; flag_list } =
    `Assoc [
      "cwd", Json.(option string) flag_cwd;
      "flags", `List (List.map ~f:Json.string flag_list);
    ]
  in
  `Assoc [
    "build_path"   , `List (List.map ~f:Json.string x.build_path);
    "source_path"  , `List (List.map ~f:Json.string x.source_path);
    "cmi_path"     , `List (List.map ~f:Json.string x.cmi_path);
    "cmt_path"     , `List (List.map ~f:Json.string x.cmt_path);
    "flags_applied", `List (List.map ~f:dump_flag_list x.flags_applied);
    "extensions"   , `List (List.map ~f:Json.string x.extensions);
    "suffixes"     , `List (
      List.map ~f:(fun (impl,intf) -> `Assoc [
          "impl", `String impl;
          "intf", `String intf;
        ]) x.suffixes
    );
    "stdlib"       , Json.option Json.string x.stdlib;
    "reader"       , `List (List.map ~f:Json.string x.reader);
    "protocol"     , (match x.protocol with
        | `Json -> `String "json"
        | `Sexp -> `String "sexp"
      );
    "log_file"     , Json.option Json.string x.log_file;
    "trace"        , `Bool x.trace;
    "flags_to_apply"   , `List (List.map ~f:dump_flag_list x.flags_to_apply);
    "packages_to_load" , `List (List.map ~f:Json.string x.packages_to_load);
    "dotmerlin_loaded" , `List (List.map ~f:Json.string x.dotmerlin_loaded);
    "packages_loaded"  , `List (List.map ~f:Json.string x.packages_loaded);
    "packages_path"    , `List (List.map ~f:Json.string x.packages_path);

    "failures"         , `List (List.map ~f:Json.string x.failures);
    "assoc_suffixes"   , `List (
      List.map ~f:(fun (suffix,reader) -> `Assoc [
          "extension", `String suffix;
          "reader", `String reader;
        ]) x.extension_to_reader
    )
  ]

type query = {
  filename  : string;
  directory : string;
  printer_width : int;
  verbosity : int;
}

let dump_query x = `Assoc [
    "filename"  , `String x.filename;
    "directory" , `String x.directory;
    "printer_width", `Int x.printer_width;
    "verbosity" , `Int x.verbosity;
  ]

type t = {
  ocaml   : ocaml;
  findlib : findlib;
  merlin  : merlin;
  query   : query;
}

let dump x = `Assoc [
    "ocaml"   , dump_ocaml x.ocaml;
    "findlib" , dump_findlib x.findlib;
    "merlin"  , dump_merlin x.merlin;
    "query"   , dump_query x.query;
  ]

let arguments_table = Hashtbl.create 67

let stdlib =
  let env =
    try Some (Sys.getenv "OCAMLLIB")
    with Not_found ->
    try Some (Sys.getenv "CAMLLIB")
    with Not_found -> None
  in
  fun config ->
    match config.merlin.stdlib with
    | Some stdlib -> stdlib
    | None -> match env with
      | Some stdlib -> stdlib
      | None ->
        Mconfig_dot.standard_library
          ?conf:config.findlib.conf
          ~path:config.findlib.path
          ?toolchain:config.findlib.toolchain ()

let normalize_step t =
  let merlin = t.merlin and findlib = t.findlib in
  let open Mconfig_dot in
  if merlin.packages_to_load <> [] then
    let path, ppx, failures = path_of_packages
        ?conf:findlib.conf
        ~path:findlib.path
        ?toolchain:findlib.toolchain
        merlin.packages_to_load
    in
    { t with merlin =
               { merlin with
                 packages_to_load = [];
                 packages_loaded = merlin.packages_to_load @ merlin.packages_loaded;
                 packages_path = path @ merlin.packages_path;
                 packages_ppx  = Ppxsetup.union ppx merlin.packages_ppx;
                 failures = failures @ merlin.failures
               }
    }
  else if merlin.flags_to_apply <> [] then
    let flagss = merlin.flags_to_apply in
    let t = {t with merlin = { merlin with
                               flags_to_apply = [];
                               flags_applied = flagss @ merlin.flags_applied;
                             } }
    in
    let failures = ref [] in
    let warning failure = failures := failure :: !failures in
    let t = List.fold_left ~f:(fun t {flag_cwd; flag_list} ->
        fst (resolve_relative_path ?cwd:flag_cwd
               (Marg.parse_all ~warning arguments_table [] flag_list t))
      ) ~init:t flagss
    in
    {t with merlin = {t.merlin with failures = !failures @ t.merlin.failures}}
  else
    t

let is_normalized t =
  let merlin = t.merlin in
  merlin.flags_to_apply = [] &&
  merlin.packages_to_load = []

let rec normalize trace t =
  if is_normalized t then
    (Logger.logj "Mconfig" "normalize" (fun () -> dump t); t)
  else normalize trace (normalize_step t)

let load_dotmerlins ~filenames t =
  let open Mconfig_dot in
  let stdlib = stdlib t in
  let dot = Mconfig_dot.load ~stdlib filenames in
  let merlin = t.merlin in
  let merlin = {
    merlin with
    build_path = dot.build_path @ merlin.build_path;
    source_path = dot.source_path @ merlin.source_path;
    cmi_path = dot.cmi_path @ merlin.cmi_path;
    cmt_path = dot.cmt_path @ merlin.cmt_path;
    exclude_query_dir = dot.exclude_query_dir || merlin.exclude_query_dir;
    extensions = dot.extensions @ merlin.extensions;
    suffixes = dot.suffixes @ merlin.suffixes;
    stdlib = (if dot.stdlib = None then merlin.stdlib else dot.stdlib);
    reader =
      if dot.reader = []
      then merlin.reader
      else dot.reader;
    flags_to_apply = List.map ~f:flag_list dot.flags @ merlin.flags_to_apply;
    dotmerlin_loaded = dot.dot_merlins @ merlin.dotmerlin_loaded;
    packages_to_load = dot.packages @ merlin.packages_to_load;
  } in
  let findlib = {
    conf = Option.plus dot.findlib t.findlib.conf;
    path = dot.findlib_path @ t.findlib.path;
    toolchain = Option.plus dot.findlib_toolchain t.findlib.toolchain;
  } in
  normalize Trace.null { t with merlin; findlib }

let findlib_flags = [
  (
    "-findlib-conf",
    marg_path (fun conf findlib ->
        let conf = if conf = "" then None else Some conf in
        {findlib with conf}),
    "<path> Path to findlib.conf to use for resolving packages"
  );
  (
    "-findlib-path",
    marg_path (fun path findlib ->
        {findlib with path = path :: findlib.path}),
    "<path> Add <path> to the list of paths considered "
  );
]

let merlin_flags = [
  (
    "-build-path",
    marg_path (fun dir merlin ->
        {merlin with build_path = dir :: merlin.build_path}),
    "<dir> Add <dir> to merlin build path"
  );
  (
    "-source-path",
    marg_path (fun dir merlin ->
        {merlin with source_path = dir :: merlin.source_path}),
    "<dir> Add <dir> to merlin source path"
  );
  (
    "-cmi-path",
    marg_path (fun dir merlin ->
        {merlin with cmi_path = dir :: merlin.cmi_path}),
    "<dir> Add <dir> to merlin cmi path"
  );
  (
    "-cmt-path",
    marg_path (fun dir merlin ->
        {merlin with build_path = dir :: merlin.cmt_path}),
    "<dir> Add <dir> to merlin cmt path"
  );
  (
    "-reader",
    Marg.param "command" (fun reader merlin ->
        {merlin with reader = Shell.split_command reader }),
    "<command> Use <command> as a merlin reader"
  );
  (
    "-assocsuffix",
    Marg.param "suffix:reader"
      (fun assoc_pair merlin ->
         match Misc.rev_string_split ~on:':' assoc_pair with
         | [reader;suffix] ->
              {merlin with
               extension_to_reader = (suffix,reader)::merlin.extension_to_reader}
         | _ -> merlin
      ),
    "Associate suffix with reader"
  );
  (
    "-addsuffix",
    Marg.param "implementation Suffix, interface Suffix"
    (fun suffix_pair merlin ->
      match Misc.rev_string_split ~on:':' suffix_pair with
      | [intf;impl] ->
        {merlin with suffixes = (impl,intf)::merlin.suffixes}
      | _ -> merlin
    ),
    "Add a suffix implementation,interface pair"
  );
  (
    "-extension",
    Marg.param "extension" (fun extension merlin ->
        match Extension.lookup extension with
        | None -> invalid_arg "Unknown extension"
        | Some _ ->
          {merlin with extensions = extension :: merlin.extensions}),
    "<extension> Load merlin syntax extension"
  );
  (
    "-package",
    Marg.param "package" (fun pkg merlin ->
        {merlin with packages_to_load = pkg :: merlin.packages_to_load}),
    "<package> Load findlib package"
  );
  (
    "-flags",
    Marg.param "string" (fun flags merlin ->
        {merlin with flags_to_apply = flag_list (Shell.split_command flags) ::
                                      merlin.flags_to_apply}),
    "<quoted flags> Unescape argument and interpret it as more flags"
  );
  (
    "-protocol",
    Marg.param "protocol" (fun prot merlin ->
        match prot with
        | "json" -> {merlin with protocol = `Json}
        | "sexp" -> {merlin with protocol = `Sexp}
        | _ -> invalid_arg "Valid protocols are 'json' and 'sexp'";
      ),
    "<protocol> Select frontend protocol ('json' or 'sexp')"
  );
  (
    "-log-file",
    Marg.param "file" (fun file merlin -> {merlin with log_file = Some file}),
    "<file> Log messages to specified file ('' for disabling, '-' for stderr)"
  );
  (
    "-trace",
    Marg.bool (fun trace merlin -> {merlin with trace}),
    "<bool> Output a trace of the execution on stderr"
  );
  (
    "-ocamllib-path",
    marg_path (fun path merlin -> {merlin with stdlib = Some path}),
    "<path> Change path of ocaml standard library"
  );
  (
    (* Legacy support for janestreet. Ignored. To be removed soon. *)
    "-attributes-allowed",
    Marg.unit_ignore,
    " DEPRECATED"
  );
]

let query_flags = [
  (
    "-verbosity",
    Marg.param "integer" (fun verbosity query ->
        let verbosity =
          try int_of_string verbosity
          with _ -> invalid_arg "argument should be an integer"
        in
        {query with verbosity}),
    "<integer> Verbosity determines the number of expansions of aliases in answers"
  );
  (
    "-printer-width",
    Marg.param "integer" (fun width query ->
        let printer_width =
          try int_of_string width
          with _ -> invalid_arg "argument should be an integer"
        in
        {query with printer_width}),
    "<integer> Optimal width for formatting types, signatures, etc"
  )
]

let ocaml_ignored_flags = [
  "-a"; "-absname"; "-alias-deps"; "-annot"; "-app-funct"; "-bin-annot";
  "-c"; "-compact"; "-compat-32"; "-config"; "-custom"; "-dalloc";
  "-dclambda"; "-dcmm"; "-dcombine"; "-dcse"; "-dflambda";
  "-dflambda-no-invariants"; "-dflambda-verbose"; "-dinstr"; "-dinterf";
  "-dlambda"; "-dlinear"; "-dlive"; "-dparsetree"; "-dprefer";
  "-drawclambda"; "-drawflambda"; "-drawlambda"; "-dreload"; "-dscheduling";
  "-dsel"; "-dsource"; "-dspill"; "-dsplit"; "-dstartup"; "-dtimings";
  "-dtypedtree"; "-dtypes"; "-dump-pass"; "-fno-PIC"; "-fPIC"; "-g"; "-i";
  "-inlining-report"; "-keep-docs"; "-keep-docs"; "-keep-locs"; "-linkall";
  "-make_runtime"; "-make-runtime"; "-modern"; "-no-alias-deps"; "-noassert";
  "-noautolink"; "-no-check-prims"; "-nodynlink"; "-no-float-const-prop";
  "-no-keep-locs"; "-no-principal"; "-no-rectypes"; "-no-strict-formats";
  "-no-strict-sequence"; "-no-unbox-free-vars-of-clos";
  "-no-unbox-specialised-args"; "-O2"; "-O3"; "-Oclassic"; "-opaque";
  "-output-complete-obj"; "-output-obj"; "-p"; "-pack";
  "-remove-unused-arguments"; "-S"; "-shared"; "-unbox-closures"; "-v";
  "-verbose"; "-where";
]

let ocaml_ignored_parametrized_flags = [
  "-cc"; "-cclib"; "-ccopt"; "-color"; "-dflambda-let"; "-dllib"; "-dllpath";
  "-for-pack"; "-impl"; "-inline-alloc-cost"; "-inline-branch-cost";
  "-inline-branch-factor"; "-inline-call-cost"; "-inline-indirect-cost";
  "-inline-lifting-benefit"; "-inline-max-depth"; "-inline-max-unroll";
  "-inline"; "-inline-prim-cost"; "-inline-toplevel"; "-intf";
  "-intf_suffix"; "-intf-suffix"; "-o"; "-rounds"; "-runtime-variant";
  "-unbox-closures-factor"; "-use-prims"; "-use_runtime"; "-use-runtime";
]

let ocaml_warnings_spec ~error =
  Marg.param "warning specification" (fun spec ocaml ->
      let b' = Warnings.backup () in
      Warnings.restore ocaml.warnings;
      Misc.try_finally (fun () ->
          Warnings.parse_options error spec;
          { ocaml with warnings = Warnings.backup () })
        (fun () -> Warnings.restore b'))

let ocaml_flags = [
  (
    "-I",
    marg_path (fun dir ocaml ->
        {ocaml with include_dirs = dir :: ocaml.include_dirs}),
    "<dir> Add <dir> to the list of include directories"
  );
  (
    "-nostdlib",
    Marg.unit (fun ocaml -> {ocaml with no_std_include = true}),
    " Do not add default directory to the list of include directories"
  );
  (
    "-unsafe",
    Marg.unit (fun ocaml -> {ocaml with unsafe = true}),
    " Do not compile bounds checking on array and string access"
  );
  (
    "-labels",
    Marg.unit (fun ocaml -> {ocaml with classic = false}),
    " Use commuting label mode"
  );
  (
    "-nolabels",
    Marg.unit (fun ocaml -> {ocaml with classic = true}),
    " Ignore non-optional labels in types"
  );
  (
    "-principal",
    Marg.unit (fun ocaml -> {ocaml with principal = true}),
    " Check principality of type inference"
  );
  (
    "-real-paths",
    Marg.unit (fun ocaml -> {ocaml with real_paths = true}),
    " Display real paths in types rather than short ones"
  );
  (
    "-short-paths",
    Marg.unit (fun ocaml -> {ocaml with real_paths = false}),
    " Shorten paths in types"
  );
  (
    "-rectypes",
    Marg.unit (fun ocaml -> {ocaml with recursive_types = true}),
    " Allow arbitrary recursive types"
  );
  (
    "-strict-sequence",
    Marg.unit (fun ocaml -> {ocaml with strict_sequence = true}),
    " Left-hand part of a sequence must have type unit"
  );
  (
    "-no-app-funct",
    Marg.unit (fun ocaml -> {ocaml with applicative_functors = false}),
    " Deactivate applicative functors"
  );
  (
    "-thread",
    Marg.unit (fun ocaml -> {ocaml with threads = `Threads}),
    " Add support for system threads library"
  );
  (
    "-vmthread",
    Marg.unit (fun ocaml -> {ocaml with threads = `None}),
    " Add support for VM-scheduled threads library"
  );
  (
    "-unsafe-string",
    Marg.unit (fun ocaml -> {ocaml with unsafe_string = true}),
    " Make strings mutable (default)"
  );
  (
    "-safe-string",
    Marg.unit (fun ocaml -> {ocaml with unsafe_string = false}),
    " Make strings immutable"
  );
  (
    "-nopervasives",
    Marg.unit (fun ocaml -> {ocaml with nopervasives = true}),
    " Don't open Pervasives module (advanced)"
  );
  (
    "-strict-formats",
    Marg.unit (fun ocaml -> {ocaml with strict_formats = true}),
    " Reject invalid formats accepted by legacy implementations"
  );
  (
    "-open",
    Marg.param "module" (fun md ocaml ->
        {ocaml with open_modules = md :: ocaml.open_modules}),
    "<module>  Opens the module <module> before typing"
  );
  (
    "-ppx",
    marg_exec_path (fun command ocaml ->
        {ocaml with ppx = command :: ocaml.ppx}),
    "<command> Pipe abstract syntax trees through preprocessor <command>"
  );
  (
    "-pp",
    marg_exec_path (fun pp ocaml -> {ocaml with pp}),
    "<command> Pipe sources through preprocessor <command>"
  );
  ( "-w",
    ocaml_warnings_spec ~error:false,
    Printf.sprintf
      "<list>  Enable or disable warnings according to <list>:\n\
      \        +<spec>   enable warnings in <spec>\n\
      \        -<spec>   disable warnings in <spec>\n\
      \        @<spec>   enable warnings in <spec> and treat them as errors\n\
      \     <spec> can be:\n\
      \        <num>             a single warning number\n\
      \        <num1>..<num2>    a range of consecutive warning numbers\n\
      \        <letter>          a predefined set\n\
      \     default setting is %S"
      Warnings.defaults_w
  );
  ( "-warn-error",
    ocaml_warnings_spec ~error:true,
    Printf.sprintf
      "<list> Enable or disable error status for warnings according\n\
      \     to <list>.  See option -w for the syntax of <list>.\n\
      \     Default setting is %S"
      Warnings.defaults_warn_error
  );
]

(** {1 Main configuration} *)

let initial = {
  ocaml = {
    include_dirs         = [];
    no_std_include       = false;
    unsafe               = false;
    classic              = false;
    principal            = false;
    real_paths           = true;
    threads              = `None;
    recursive_types      = false;
    strict_sequence      = false;
    applicative_functors = true;
    unsafe_string        = true;
    nopervasives         = false;
    strict_formats       = false;
    open_modules         = [];
    ppx                  = [];
    pp                   = "";
    warnings             = Warnings.backup ();
  };
  findlib = {
    conf = None;
    path = [];
    toolchain = None;
  };
  merlin = {
    build_path  = [];
    source_path = [];
    cmi_path    = [];
    cmt_path    = [];
    extensions  = [];
    suffixes    = [(".ml", ".mli"); (".re", ".rei")];
    stdlib      = None;
    reader      = [];
    protocol    = `Json;
    log_file    = None;
    trace       = false;

    exclude_query_dir = false;

    flags_to_apply    = [];
    packages_to_load  = [];
    flags_applied     = [];
    dotmerlin_loaded  = [];
    packages_loaded   = [];

    packages_path = [];
    packages_ppx  = Ppxsetup.empty;

    failures = [];
    extension_to_reader = [(".re","reason");(".rei","reason")];
  };
  query = {
    filename = "*buffer*";
    directory = Sys.getcwd ();
    verbosity = 0;
    printer_width = 0;
  }
}

let global_flags = [
  (
    "-filename",
    marg_path (fun path t ->
        let query = t.query in
        let path = Misc.canonicalize_filename path in
        let filename = Filename.basename path in
        let directory = Filename.dirname path in
        let t = {t with query = {query with filename; directory}} in
        load_dotmerlins t ~filenames:[
          let base = "." ^ filename ^ ".merlin" in
          Filename.concat directory base
        ]),
    "<path> Path of the buffer; \
     extension determines the kind of file (interface or implementation), \
     basename is used as name of the module being definer, \
     directory is used to resolve other relative paths"
  );
  (
    "-dot-merlin",
    marg_path (fun dotmerlin t -> load_dotmerlins ~filenames:[dotmerlin] t),
    "<path> Load <path> as a .merlin; if it is a directory, \
     look for .merlin here or in a parent directory"
  );
]

let () =
  List.iter ~f:(fun name -> Hashtbl.add arguments_table name Marg.unit_ignore)
    ocaml_ignored_flags;
  List.iter ~f:(fun name -> Hashtbl.add arguments_table name Marg.param_ignore)
    ocaml_ignored_parametrized_flags;
  let lens prj upd flag : _ Marg.t = fun args a ->
    let cwd' = match !cwd with
      | None when a.query.directory <> "" -> Some a.query.directory
      | cwd -> cwd
    in
    let_ref cwd cwd' @@ fun () ->
    let args, b = flag args (prj a) in
    args, (upd a b)
  in
  let add prj upd (name,flag,_doc) =
    assert (not (Hashtbl.mem arguments_table name));
    Hashtbl.add arguments_table name (lens prj upd flag)
  in
  List.iter
    ~f:(add (fun x -> x.ocaml) (fun x ocaml -> {x with ocaml}))
    ocaml_flags;
  List.iter
    ~f:(add (fun x -> x.findlib) (fun x findlib -> {x with findlib}))
    findlib_flags;
  List.iter
    ~f:(add (fun x -> x.merlin) (fun x merlin -> {x with merlin}))
    merlin_flags;
  List.iter
    ~f:(add (fun x -> x.query) (fun x query -> {x with query}))
    query_flags;
  List.iter
    ~f:(add (fun x -> x) (fun _ x -> x))
    global_flags

let flags_for_completion () =
  List.sort ~cmp:compare (
    "-dot-merlin" :: "-reader" ::
    List.map ~f:(fun (x,_,_) -> x) findlib_flags @
    List.map ~f:(fun (x,_,_) -> x) ocaml_flags
  )

let document_arguments oc =
  let print_doc flags =
    List.iter ~f:(fun (name,_flag,doc) -> Printf.fprintf oc "  %s\t%s\n" name doc)
      flags
  in
  output_string oc "Flags affecting Merlin:\n";
  print_doc merlin_flags;
  print_doc query_flags;
  output_string oc "Flags affecting OCaml frontend:\n";
  print_doc ocaml_flags;
  output_string oc "Flags affecting Findlib behavior:\n";
  print_doc findlib_flags;
  output_string oc "Flags accepted by ocamlc and ocamlopt but not affecting merlin will be ignored.\n"

let source_path config = (
  config.query.directory ::
  config.merlin.source_path @
  config.merlin.packages_path
)

let build_path config = (
  let dirs =
    match config.ocaml.threads with
    | `None -> config.ocaml.include_dirs
    | `Threads -> "+threads" :: config.ocaml.include_dirs
    | `Vmthreads -> "+vmthreads" :: config.ocaml.include_dirs
  in
  let dirs =
    config.merlin.cmi_path @
    config.merlin.build_path @
    config.merlin.packages_path @
    dirs
  in
  let stdlib = stdlib config in
  let exp_dirs =
    List.map ~f:(Misc.expand_directory stdlib) dirs
  in
  let stdlib = if config.ocaml.no_std_include then [] else [stdlib] in
  let dirs = List.rev_append exp_dirs stdlib in
  let result =
    if config.merlin.exclude_query_dir
    then dirs
    else config.query.directory :: dirs
  in
  Logger.logf "Mconfig" "build_path" "%d items in path, %t after deduplication"
    (List.length result)
    (fun () -> string_of_int (List.length (List.filter_dup result)));
  result
)

let cmt_path config = (
  let dirs =
    match config.ocaml.threads with
    | `None -> config.ocaml.include_dirs
    | `Threads -> "+threads" :: config.ocaml.include_dirs
    | `Vmthreads -> "+vmthreads" :: config.ocaml.include_dirs
  in
  let dirs =
    config.merlin.cmt_path @
    config.merlin.build_path @
    config.merlin.packages_path @
    dirs
  in
  let stdlib = stdlib config in
  let exp_dirs =
    List.map ~f:(Misc.expand_directory stdlib) dirs
  in
  let stdlib = if config.ocaml.no_std_include then [] else [stdlib] in
  config.query.directory :: List.rev_append exp_dirs stdlib
)

let global_modules ?(include_current=false) config = (
  let modules = Misc.modules_in_path ~ext:".cmi" (build_path config) in
  if include_current then modules
  else match config.query.filename with
    | "" -> modules
    | filename -> List.remove (Misc.unitname filename) modules
)

(** {1 Accessors for other informations} *)

let filename t = t.query.filename

let unitname t = Misc.unitname t.query.filename
