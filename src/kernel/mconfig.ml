open Std

(** {1 OCaml commandline parsing} *)

type ocaml = {
  include_dirs         : string list;
  no_std_include       : bool;
  unsafe               : bool;
  classic              : bool;
  principal            : bool;
  real_paths           : [ `Real | `Short | `Opened ];
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
    "include_dirs"         , `List (List.map Json.string x.include_dirs);
    "no_std_include"       , `Bool x.no_std_include;
    "unsafe"               , `Bool x.unsafe;
    "classic"              , `Bool x.classic;
    "principal"            , `Bool x.principal;
    "real_paths"           , `String (
      match x.real_paths with
        `Real -> "real" | `Short -> "short" | `Opened -> "opened"
    );
    "recursive_types"      , `Bool x.recursive_types;
    "strict_sequence"      , `Bool x.strict_sequence;
    "applicative_functors" , `Bool x.applicative_functors;
    "unsafe_string"        , `Bool x.unsafe_string;
    "nopervasives"         , `Bool x.nopervasives;
    "strict_formats"       , `Bool x.strict_formats;
    "open_modules"         , `List (List.map Json.string x.open_modules);
    "ppx"                  , `List (List.map Json.string x.ppx);
    "pp"                   , `String x.pp;
    "warnings"             , dump_warnings x.warnings;
  ]

(** {1 Findlib configuration} *)

type findlib = {
  conf : string option;
  path : string list;
}

let dump_findlib x = `Assoc [
    "conf", Json.option Json.string x.conf;
    "path", `List (List.map Json.string x.path);
  ]

let findlib_flags = [
  (
    "-findlib-conf",
    Marg.param "path" (fun conf findlib ->
        let conf = if conf = "" then None else Some conf in
        {findlib with conf}),
    "<path> Path to findlib.conf to use for resolving packages"
  );
  (
    "-findlib-path",
    Marg.param "path" (fun path findlib ->
        {findlib with path = path :: findlib.path}),
    "<path> Add <path> to the list of paths considered "
  );
]

(** {1 Merlin high-level settings} *)

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

  flags_to_apply    : string list list;
  dotmerlin_to_load : string list;
  packages_to_load  : string list;

  flags_applied    : string list list;
  dotmerlin_loaded : string list;
  packages_loaded  : string list;

  packages_path : string list;
  packages_ppx  : Ppxsetup.t;

  failures    : string list;

}

let dump_merlin x = `Assoc [
    "build_path"  , `List (List.map Json.string x.build_path);
    "source_path" , `List (List.map Json.string x.source_path);
    "cmi_path"    , `List (List.map Json.string x.cmi_path);
    "cmt_path"    , `List (List.map Json.string x.cmt_path);
    "flags_applied", `List (List.map (fun l -> `List (List.map Json.string l))
                              x.flags_applied);
    "extensions"  , `List (List.map Json.string x.extensions);
    "suffixes"    , `List (
      List.map (fun (impl,intf) -> `Assoc [
          "impl", `String impl;
          "intf", `String intf;
        ]) x.suffixes
    );
    "stdlib"      , Json.option Json.string x.stdlib;
    "reader"      , `List (List.map Json.string x.reader);
    "protocol"    , (match x.protocol with
        | `Json -> `String "json"
        | `Sexp -> `String "sexp"
      );
    "dotmerlin_to_load", `List (List.map Json.string x.dotmerlin_to_load);
    "packages_to_load" , `List (List.map Json.string x.packages_to_load);
    "dotmerlin_loaded" , `List (List.map Json.string x.dotmerlin_loaded);
    "packages_loaded"  , `List (List.map Json.string x.packages_loaded);
    "packages_path"    , `List (List.map Json.string x.packages_path);
  ]

let merlin_flags = [
  (
    "-build-path",
    Marg.param "directory" (fun dir merlin ->
        {merlin with build_path = dir :: merlin.build_path}),
    "<dir> Add <dir> to merlin build path"
  );
  (
    "-source-path",
    Marg.param "directory" (fun dir merlin ->
        {merlin with source_path = dir :: merlin.build_path}),
    "<dir> Add <dir> to merlin source path"
  );
  (
    "-cmi-path",
    Marg.param "directory" (fun dir merlin ->
        {merlin with cmi_path = dir :: merlin.cmi_path}),
    "<dir> Add <dir> to merlin cmi path"
  );
  (
    "-cmt-path",
    Marg.param "directory" (fun dir merlin ->
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
    "-dot-merlin",
    Marg.param "path" (fun dotmerlin merlin ->
        {merlin with dotmerlin_to_load =
                       dotmerlin :: merlin.dotmerlin_to_load}),
    "<path> Load <path> as a .merlin; if it is a directory, \
     look for .merlin here or in a parent directory"
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
        {merlin with flags_to_apply =
                       Shell.split_command flags :: merlin.flags_to_apply}),
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

let query_flags = [
  (
    "-filename",
    Marg.param "path" (fun path query ->
        let path = Misc.canonicalize_filename path in
        let filename = Filename.basename path in
        let directory = Filename.dirname path in
        {query with filename; directory}),
    "<path> Path of the buffer; \
     extension determines the kind of file (interface or implementation), \
     basename is used as name of the module being definer, \
     directory is used to resolve other relative paths"
  );
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

type t = {
  ocaml   : ocaml;
  findlib : findlib;
  merlin  : merlin;
  query   : query;
}

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
    Marg.param "directory" (fun dir ocaml ->
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
    Marg.unit (fun ocaml -> {ocaml with real_paths = `Real}),
    " Display real paths in types rather than short ones"
  );
  (
    "-short-paths",
    Marg.unit (fun ocaml -> {ocaml with real_paths = `Short}),
    " Shorten paths in types"
  );
  (
    "-opened-paths",
    Marg.unit (fun ocaml -> {ocaml with real_paths = `Opened}),
    " Remove opened prefix from displayed types"
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
    Marg.param "command" (fun command ocaml ->
        {ocaml with ppx = command :: ocaml.ppx}),
    "<command> Pipe abstract syntax trees through preprocessor <command>"
  );
  (
    "-pp",
    Marg.param "command" (fun pp ocaml -> {ocaml with pp}),
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
    real_paths           = `Real;
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
  };
  merlin = {
    build_path  = [];
    source_path = [];
    cmi_path    = [];
    cmt_path    = [];
    extensions  = [];
    suffixes    = [];
    stdlib      = None;
    reader      = [];
    protocol    = `Json;

    flags_to_apply    = [];
    dotmerlin_to_load = [];
    packages_to_load  = [];
    flags_applied     = [];
    dotmerlin_loaded  = [];
    packages_loaded   = [];

    packages_path = [];
    packages_ppx  = Ppxsetup.empty;

    failures = [];
  };
  query = {
    filename = "*buffer*";
    directory = Sys.getcwd ();
    verbosity = 0;
    printer_width = 0;
  }
}

let dump x = `Assoc [
    "ocaml"   , dump_ocaml x.ocaml;
    "findlib" , dump_findlib x.findlib;
    "merlin"  , dump_merlin x.merlin;
    "query"   , dump_query x.query;
  ]

let arguments_table =
  let table = Hashtbl.create 67 in
  List.iter (fun name -> Hashtbl.add table name Marg.unit_ignore)
    ocaml_ignored_flags;
  List.iter (fun name -> Hashtbl.add table name Marg.param_ignore)
    ocaml_ignored_parametrized_flags;
  let add prj upd (name,flag,_doc) =
    assert (not (Hashtbl.mem table name));
    Hashtbl.add table name (Marg.lens prj upd flag)
  in
  List.iter
    (add (fun x -> x.ocaml) (fun x ocaml -> {x with ocaml}))
    ocaml_flags;
  List.iter
    (add (fun x -> x.findlib) (fun x findlib -> {x with findlib}))
    findlib_flags;
  List.iter
    (add (fun x -> x.merlin) (fun x merlin -> {x with merlin}))
    merlin_flags;
  List.iter
    (add (fun x -> x.query) (fun x query -> {x with query}))
    query_flags;
  table

let flags_for_completion () =
  List.sort ~cmp:compare (
    "-dot-merlin" :: "-reader" ::
    List.map (fun (x,_,_) -> x) findlib_flags @
    List.map (fun (x,_,_) -> x) ocaml_flags
  )

let try_parse_argument ~warning args ocaml =
  match args with
  | [] -> None
  | arg :: args ->
    match Hashtbl.find arguments_table arg with
    | exception Not_found -> None
    | action -> match action args ocaml with
      | result -> Some result
      | exception (Failure msg) ->
        warning ("flag " ^ arg ^ " " ^ msg);
        Some (args, ocaml)
      | exception exn ->
        warning ("flag " ^ arg ^ ": error, " ^ Printexc.to_string exn);
        Some (args, ocaml)

let parse_arguments ~warning =
  let rec normal_parsing args ocaml =
    match try_parse_argument ~warning args ocaml with
    | Some (args, ocaml) -> normal_parsing args ocaml
    | None -> match args with
      | _ :: args -> resume_parsing args ocaml
      | [] -> ocaml
  and resume_parsing args ocaml =
    match args with
    | arg :: args when not (Hashtbl.mem arguments_table arg) ->
      normal_parsing args ocaml
    | args -> normal_parsing args ocaml
  in
  normal_parsing

let document_arguments oc =
  let print_doc flags =
    List.iter (fun (name,_flag,doc) -> Printf.fprintf oc "  %s\t%s\n" name doc)
      flags
  in
  output_string oc "Flags affecting Merlin:\n";
  print_doc merlin_flags;
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
  let exp_dirs =
    List.map (Misc.expand_directory Config.standard_library) dirs
  in
  let stdlib =
    if config.ocaml.no_std_include then []
    else [Config.standard_library]
  in
  config.query.directory :: List.rev_append exp_dirs stdlib
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
  let exp_dirs =
    List.map (Misc.expand_directory Config.standard_library) dirs
  in
  let stdlib =
    if config.ocaml.no_std_include then []
    else [Config.standard_library]
  in
  config.query.directory :: List.rev_append exp_dirs stdlib
)

let global_modules ?(include_current=false) config = (
  let modules = Misc.modules_in_path ~ext:".cmi" (build_path config) in
  if include_current then modules
  else match config.query.filename with
    | "" -> modules
    | filename -> List.remove (Misc.unitname filename) modules
)

let normalize_step _trace t =
  let merlin = t.merlin and findlib = t.findlib in
  let open Mconfig_dot in
  if merlin.dotmerlin_to_load <> [] then
    let dot = Mconfig_dot.load merlin.dotmerlin_to_load in
    let merlin = {
      merlin with
      build_path = dot.build_path @ merlin.build_path;
      source_path = dot.build_path @ merlin.source_path;
      cmi_path = dot.cmi_path @ merlin.cmi_path;
      cmt_path = dot.cmt_path @ merlin.cmt_path;
      extensions = dot.extensions @ merlin.extensions;
      suffixes = dot.suffixes @ merlin.suffixes;
      stdlib =
        if dot.stdlib = ""
        then merlin.stdlib
        else Some dot.stdlib;
      reader =
        if dot.reader = []
        then merlin.reader
        else dot.reader;
      flags_to_apply = dot.flags @ merlin.flags_to_apply;
      dotmerlin_to_load = [];
      dotmerlin_loaded = dot.dot_merlins @ merlin.dotmerlin_loaded;
      packages_to_load = dot.packages @ merlin.packages_to_load;
    } in
    { t with merlin }
  else if merlin.packages_to_load <> [] then
    let path, ppx, failures = path_of_packages
        ?conf:findlib.conf
        ~path:findlib.path
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
    let failures = ref merlin.failures in
    let warning failure = failures := failure :: !failures in
    let t = List.fold_left ~f:(fun t flags ->
        fst (Marg.parse_all ~warning arguments_table [] flags t ())
      ) ~init:t flagss
    in
    {t with merlin = {t.merlin with failures = !failures}}
  else
    t

let is_normalized t =
  let merlin = t.merlin in
  merlin.flags_to_apply = [] &&
  merlin.dotmerlin_to_load = [] &&
  merlin.packages_to_load = []

let rec normalize trace t =
  if is_normalized t then
    (Logger.logj "Mconfig" "normalize" (fun () -> dump t); t)
  else normalize trace (normalize_step trace t)
