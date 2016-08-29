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

let run buffer query =
  Logger.logj "New_commands.run" "query" (fun () -> Query_json.dump query);
  let result = Query_commands.dispatch buffer query in
  let json = Query_json.json_of_response query result in
  Logger.logj "New_commands.run" "result" (fun () -> json);
  json

let all_commands = [

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

    begin fun buffer extensions ->
      run buffer (Query_protocol.List_modules extensions)
    end
  ;

  command "path-of-source"
    ~doc:"path-of-source -file a.mli -file a.ml\n\
          \tlooks for first file with a matching name in the project source \
          and build paths"
    ~spec: [
      ("-file",
       "<filename> filename to look for in project paths",
       Marg.param "filename" (fun file files -> file :: files)
      )
    ]
    ~default:[]

    begin fun buffer filenames ->
      run buffer (Query_protocol.Path_of_source filenames)
    end
  ;

  command "completion"
    ~doc:"completion -position pos -prefix a.ml [-doc (y|n)]\n\
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
    begin fun buffer (doc,pos,prefix) ->
      match pos with
      | `None -> failwith "-position <pos> is mandatory"
      | #Msource.position as pos ->
        run buffer (Query_protocol.Complete_prefix (prefix,pos,doc))
    end
  ;
]
