open Std

type input = {
  source    : Msource.t;
  reader    : Mreader.t;
  config    : Mconfig.t;
  parsetree : Mreader.parsetree;
}

let dump_input t = `Assoc [
    "source", Msource.dump t.source;
    "reader", Mreader.dump t.reader;
    "config", Mconfig.dump t.config;
    "parsetree", `Null
  ]

type command =
    Command : string * Marg.docstring * 'args Marg.spec list * 'args *
              (input -> 'args -> json) ->
    command

let command name ?(doc="") ~spec ~default f =
  Command (name, doc, spec, default, f)

open Mconfig

let queries = [

  command "list-modules"
    ~doc:"list-modules -ext .ml -ext .mli ...\n\
          \tlooks into project source paths for files with an extension \
          matching and prints the corresponding module name"
    ~spec:[
      ("-ext",
       "<extension> file extensions to look for",
       Marg.param "extension" (fun ext exts -> ext :: exts)
      )
    ]
    ~default:[]

    begin fun {config; _} exts ->
      Logger.logj "query" "list-modules"
        (fun () -> `Assoc ["extensions", `List (List.map Json.string exts)]);
      let with_ext ext =
        let modules = Misc.modules_in_path ~ext config.merlin.source_path in
        List.map Json.string modules
      in
      `List (List.concat_map ~f:with_ext exts)
    end
  ;

  command "path-of-source"
    ~doc:"path-of-source -file a.mli -file a.ml\n\
          \tlooks for files with a matching name in the project source and \
          build paths"
    ~spec: [
      ("-file",
       "<filename> filename to look for in project paths",
       Marg.param "filename" (fun file files -> file :: files)
      )
    ]
    ~default:[]

    begin fun {config; _} filenames ->
      let rec check_path file = function
        | [] -> raise Not_found
        | x :: xs ->
          try Misc.find_in_path_uncap x file
          with Not_found -> check_path file xs
      in
      let paths = [
        config.merlin.source_path;
        config.merlin.build_path;
        config.ocaml.include_dirs;
      ] in
      let rec check_filenames = function
        | [] -> `Null
        | x :: xs ->
          try `String (check_path x paths)
          with Not_found -> check_filenames xs
      in
      check_filenames filenames
    end
  ;
]

let rec find_command name = function
  | [] -> raise Not_found
  | (Command (name', _, _, _, _) as command) :: xs ->
    if name = name' then
      command
    else find_command name xs
