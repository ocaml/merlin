open Std

type command =
    Command : string * Marg.docstring * 'args Marg.spec list * 'args *
              (Trace.t * Mconfig.t * Msource.t -> 'args -> json) -> command

let command name ?(doc="") ~spec ~default f =
  Command (name, doc, spec, default, f)

open Mconfig

let marg_position f = Marg.param "position"
    (function
      | "start" -> f `Start
      | "end" -> f `End
      | str -> match int_of_string str with
        | n -> f (`Offset n)
        | exception _ ->
          match
            let offset = String.index str ':' in
            let line = String.sub str ~pos:0 ~len:offset in
            let col = String.sub str ~pos:(offset+1)
                ~len:(String.length str - offset - 1) in
            `Logical (int_of_string line, int_of_string col)
          with
          | pos -> f pos
          | exception _ ->
            failwithf "expecting position, got %S. \
                       position can be start|end|<offset>|<line>:<col>, \
                       where offset, line and col are numbers, \
                       lines are indexed from 1."
              str
    )

let marg_bool f = Marg.param "bool"
    (function
      | "y" | "Y" | "true" | "1" -> f true
      | "n" | "N" | "false" | "0" -> f false
      | str ->
        failwithf "expecting boolean (%s), got %S."
          "y|Y|true|1 / n|N|false|0"
          str
    )

let rec find_command name = function
  | [] -> raise Not_found
  | (Command (name', _, _, _, _) as command) :: xs ->
    if name = name' then
      command
    else find_command name xs

let all_commands = [
(*
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

    begin fun buffer exts ->
      Logger.logj "query" "list-modules"
        (fun () -> `Assoc ["extensions", `List (List.map Json.string exts)]);
      let query = Query_protocol.Which_with_ext exts in
      Query_commands.dispatch buffer
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

    begin fun pipeline filenames ->
      let config = Mpipeline.reader_config pipeline in
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

  command "completion"
    ~doc:"completion -position pos -prefix a.ml [-doc]\n\
          \tTODO"
    ~spec: [
      ("-position",
       "<position> Position to complete",
       marg_position (fun pos (doc,_pos,prefix) -> (doc,pos,prefix))
      );
      ("-doc",
       "<bool> Add docstring to entries",
       marg_bool (fun doc (_doc,pos,prefix) -> (doc,pos,prefix))
      );
      ("-prefix",
       "<string> Prefix to complete",
       Marg.param "string" (fun prefix (doc,pos,_prefix) -> (doc,pos,prefix))
      );
    ]
    ~default:(false,`None,"")
    begin fun pipeline filenames ->
      let config = Mpipeline.reader_config pipeline in
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
      Query_commands.dispatch
      check_filenames filenames
    end
  ; *)
]
