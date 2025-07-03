(* {{{ COPYING *(

     This file is part of Merlin, an helper for ocaml editors

     Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                                Thomas Refis  <refis.thomas(_)gmail.com>
                                Simon Castellan  <simon.castellan(_)iuwt.fr>
                                Tomasz Kołodziejski  <tkolodziejski(_)gmail.com>

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

type command =
  | Command :
      string
      * Marg.docstring
      * ([ `Mandatory | `Optional | `Many ] * 'args Marg.spec) list
      * 'args
      * (Mpipeline.t -> 'args -> json)
      -> command

let command name ?(doc = "") ~spec ~default f =
  Command (name, doc, spec, default, f)

let arg ?(kind = `Mandatory) name doc action = (kind, (name, doc, action))
let optional x = arg ~kind:`Optional x
let many x = arg ~kind:`Many x

let marg_position f =
  Marg.param "position" (function
    | "start" -> f `Start
    | "end" -> f `End
    | str -> (
      match int_of_string str with
      | n -> f (`Offset n)
      | exception _ -> (
        match
          let offset = String.index str ':' in
          let line = String.sub str ~pos:0 ~len:offset in
          let col =
            String.sub str ~pos:(offset + 1)
              ~len:(String.length str - offset - 1)
          in
          `Logical (int_of_string line, int_of_string col)
        with
        | pos -> f pos
        | exception _ ->
          failwithf
            "expecting position, got %S. position can be \
             start|end|<offset>|<line>:<col>, where offset, line and col are \
             numbers, lines are indexed from 1."
            str)))

let marg_completion_kind f =
  Marg.param "completion-kind" (function
    | "t" | "type" | "types" -> f `Types
    | "v" | "val" | "value" | "values" -> f `Values
    | "variant" | "variants" | "var" -> f `Variants
    | "c" | "constr" | "constructor" -> f `Constructor
    | "l" | "label" | "labels" -> f `Labels
    | "m" | "mod" | "module" -> f `Modules
    | "mt" | "modtype" | "module-type" -> f `Modules_type
    | "k" | "kw" | "keyword" -> f `Keywords
    | str ->
      failwithf
        "expecting completion kind, got %S. kind can be value, variant, \
         constructor, label, module or module-type"
        str)

let command_is ~name (Command (name', _, _, _, _)) = String.equal name name'

let find_command name = List.find ~f:(command_is ~name)

let find_command_opt name = List.find_opt ~f:(command_is ~name)

let run pipeline query =
  Logger.log ~section:"New_commands" ~title:"run(query)" "%a" Logger.json
    (fun () -> Query_json.dump query);
  let result = Query_commands.dispatch pipeline query in
  let json = Query_json.json_of_response query result in
  json

let all_commands =
  [ command "case-analysis"
      ~spec:
        [ arg "-start" "<position> Where analysis starts"
            (marg_position (fun startp (_startp, endp) -> (startp, endp)));
          arg "-end" "<position> Where analysis ends"
            (marg_position (fun endp (startp, _endp) -> (startp, endp)))
        ]
      ~doc:
        "When the range determined by (-start, -end) positions is an expression,\n\
         this command replaces it with [match expr with _] expression where a \
         branch is introduced for each immediate value constructor of the type \
         that was determined for expr.\n\
         When it is a variable pattern, it is further expanded and new \
         branches are introduced for each possible immediate constructor of \
         this variable.\n\
         The return value has the shape `[{'start': position, 'end': \
         position}, content]`, where content is string.\n"
      ~default:(`Offset (-1), `Offset (-1))
      begin
        fun buffer -> function
          | `Offset -1, _ -> failwith "-start <pos> is mandatory"
          | _, `Offset -1 -> failwith "-end <pos> is mandatory"
          | startp, endp ->
            run buffer (Query_protocol.Case_analysis (startp, endp))
      end;
    command "holes" ~spec:[]
      ~doc:"Returns the list of the positions of all the holes in the file."
      ~default:()
      begin
        fun buffer () -> run buffer Query_protocol.Holes
      end;
    command "construct"
      ~spec:
        [ arg "-position" "<position> Position where construct should happen"
            (marg_position (fun pos (_pos, with_values, depth) ->
                 (pos, with_values, depth)));
          optional "-with-values" "<none|local> Use values from the environment"
            (Marg.param "<none|local>"
               (fun with_values (pos, _with_values, depth) ->
                 match with_values with
                 | "none" -> (pos, None, depth)
                 | "local" -> (pos, Some `Local, depth)
                 | _ -> failwith "-with-values should be one of none or local"));
          optional "-depth" "<int> Depth for the search (defaults to 1)"
            (Marg.param "int" (fun depth (pos, with_values, _depth) ->
                 match int_of_string depth with
                 | depth ->
                   if depth >= 1 then (pos, with_values, Some depth)
                   else failwith "depth should be a positive integer"
                 | exception _ -> failwith "depth should be a positive integer"))
        ]
      ~doc:
        "The construct command returns a list of expressions that could fill a\n\
         hole at '-position' given its inferred type. The '-depth' parameter \
         allows to\n\
         recursively construct terms. Note that when '-depth' > 1 partial \
         results of\n\
         inferior depth will not be returned."
      ~default:(`Offset (-1), None, None)
      begin
        fun buffer (pos, with_values, max_depth) ->
          match pos with
          | `Offset -1 -> failwith "-position <pos> is mandatory"
          | pos ->
            run buffer (Query_protocol.Construct (pos, with_values, max_depth))
      end;
    command "complete-prefix"
      ~spec:
        [ arg "-position" "<position> Position to complete"
            (marg_position (fun pos (txt, _pos, kinds, doc, typ) ->
                 (txt, pos, kinds, doc, typ)));
          optional "-doc" "<bool> Add docstring to entries (default is false)"
            (Marg.bool (fun doc (txt, pos, kinds, _doc, typ) ->
                 (txt, pos, kinds, doc, typ)));
          arg "-prefix" "<string> Prefix to complete"
            (Marg.param "string" (fun txt (_, pos, kinds, doc, typ) ->
                 (txt, pos, kinds, doc, typ)));
          optional "-types" "<bool> Report type information (default is true)"
            (Marg.bool (fun typ (txt, pos, kinds, doc, _typ) ->
                 (txt, pos, kinds, doc, typ)));
          optional "-kind"
            "<completion-kind> Namespace to complete (value, constructor, \
             type, variant, label, module, module-type). Default is decided by \
             cursor context"
            (marg_completion_kind (fun kind (txt, pos, kinds, doc, typ) ->
                 (txt, pos, kind :: kinds, doc, typ)))
        ]
      ~doc:
        "This functions completes an identifier that the user started to type.\n\
         It returns a list of possible completions.\n\
         With '-types y' (default), each completion comes with type information.\n\
         With '-doc y' it tries to lookup OCamldoc, which is slightly more \
         time consuming.\n\n\
         The result has the form:\n\
         ```javascript\n\
         {\n\
        \  'context': (null | ['application',{'argument_type': string, \
         'labels': [{'name':string,'type':string}]}]),\n\
        \  'entries': \
         [{'name':string,'kind':string,'desc':string,'info':string}]\n\
         }\n\
         ```\n\n\
         Context describe where completion is occurring. Only application is \
         distinguished now: that's when one is completing the arguments to a \
         function call. In this case, one gets the type expected at the cursor \
         as well as the other labels.\n\n\
         Entries is the list of possible completion. Each entry is made of:\n\
         - a name, the text that should be put in the buffer if selected\n\
         - a kind, one of `'value'`, `'variant'`, `'constructor'`, `'label'`, \
         `'module'`, `'signature'`, `'type'`, `'method'`, `'#'` (for method \
         calls), `'exn'`, `'class'`\n\
         - a description, most of the time a type or a definition line, to be \
         put next to the name in completion box\n\
         - optional information which might not fit in the completion box, \
         like signatures for modules or documentation string."
      ~default:("", `None, [], false, true)
      begin
        fun buffer (txt, pos, kinds, doc, typ) ->
          match pos with
          | `None -> failwith "-position <pos> is mandatory"
          | #Msource.position as pos ->
            run buffer
              (Query_protocol.Complete_prefix
                 (txt, pos, List.rev kinds, doc, typ))
      end;
    command "document"
      ~doc:
        "Returns OCamldoc documentation as a string.\n\
         If `-identifier ident` is specified, documentation for this ident is \
         looked up from environment at `-position`.\n\
         Otherwise, Merlin looks for the documentation for the entity under \
         the cursor (at `-position`)."
      ~spec:
        [ arg "-position" "<position> Position to complete"
            (marg_position (fun pos (ident, _pos) -> (ident, pos)));
          optional "-identifier" "<string> Identifier"
            (Marg.param "string" (fun ident (_ident, pos) -> (Some ident, pos)))
        ]
      ~default:(None, `None)
      begin
        fun buffer (ident, pos) ->
          match pos with
          | `None -> failwith "-position <pos> is mandatory"
          | #Msource.position as pos ->
            run buffer (Query_protocol.Document (ident, pos))
      end;
    command "syntax-document"
      ~doc:
        "Returns documentation for OCaml syntax for the entity under the cursor"
      ~spec:
        [ arg "-position" "<position> Position to complete"
            (marg_position (fun pos _pos -> pos))
        ]
      ~default:`None
      begin
        fun buffer pos ->
          match pos with
          | `None -> failwith "-position <pos> is mandatory"
          | #Msource.position as pos ->
            run buffer (Query_protocol.Syntax_document pos)
      end;
    command "expand-ppx" ~doc:"Returns the generated code of a PPX."
      ~spec:
        [ arg "-position" "<position> Position to expand"
            (marg_position (fun pos _pos -> pos))
        ]
      ~default:`None
      begin
        fun buffer pos ->
          match pos with
          | `None -> failwith "-position <pos> is mandatory"
          | #Msource.position as pos ->
            run buffer (Query_protocol.Expand_ppx pos)
      end;
    command "enclosing"
      ~spec:
        [ arg "-position" "<position> Position to complete"
            (marg_position (fun pos _pos -> pos))
        ]
      ~doc:
        "Returns a list of locations `{'start': position, 'end': position}` in \
         increasing size of all entities surrounding the position.\n\
         (In a lisp, this would be the locations of all s-exps that contain \
         the cursor.)"
      ~default:`None
      begin
        fun buffer pos ->
          match pos with
          | `None -> failwith "-position <pos> is mandatory"
          | #Msource.position as pos ->
            run buffer (Query_protocol.Enclosing pos)
      end;
    command "errors"
      ~spec:
        [ arg "-lexing" "<bool> Whether to report lexing errors or not"
            (Marg.bool (fun l (_, p, t) -> (l, p, t)));
          arg "-parsing" "<bool> Whether to report parsing errors or not"
            (Marg.bool (fun p (l, _, t) -> (l, p, t)));
          arg "-typing" "<bool> Whether to report typing errors or not"
            (Marg.bool (fun t (l, p, _) -> (l, p, t)))
        ]
      ~doc:
        "Returns a list of errors in current buffer.\n\
         The value is a list where each item as the shape:\n\n\
         ```javascript\n\
         {\n\
         'start' : position,\n\
         'end'   : position,\n\
         'valid' : bool,\n\
         'message' : string,\n\
         'type'  : ('type'|'parser'|'env'|'warning'|'unkown')\n\
         }\n\
         ```\n\n\
         `start` and `end` are omitted if error has no location (e.g. wrong \
         file format), otherwise the editor should probably highlight / mark \
         this range.\n\
         `type` is an attempt to classify the error.\n\
         `valid` is here mostly for informative purpose. It reflects whether \
         Merlin was expecting such an error to be possible or not, and is \
         useful for debugging purposes.\n\
         `message` is the error description to be shown to the user."
      ~default:(true, true, true)
      begin
        fun buffer (lexing, parsing, typing) ->
          run buffer (Query_protocol.Errors { lexing; parsing; typing })
      end;
    command "expand-prefix"
      ~doc:
        "\n\
         The function behaves like `complete-prefix`, but it also handles \
         partial, incorrect, or wrongly spelled prefixes (as determined by \
         some heuristic).\n\
         For instance, `L.ma` can get expanded to `List.map`. This function is \
         a useful fallback if normal completion gave no results.\n\
         Be careful that it always return fully qualified paths, whereas \
         normal completion only completes an identifier (last part of a module \
         path)."
      ~spec:
        [ arg "-position" "<position> Position to complete"
            (marg_position (fun pos (txt, _pos, kinds, typ) ->
                 (txt, pos, kinds, typ)));
          arg "-prefix" "<string> Prefix to complete"
            (Marg.param "string" (fun txt (_prefix, pos, kinds, typ) ->
                 (txt, pos, kinds, typ)));
          optional "-types" "<bool> Report type information (default is false)"
            (Marg.bool (fun typ (txt, pos, kinds, _typ) ->
                 (txt, pos, kinds, typ)));
          optional "-kind"
            "<completion-kind> Namespace to complete (value, constructor, \
             type, variant, label, module, module-type). Default is decided by \
             cursor context"
            (marg_completion_kind (fun kind (txt, pos, kinds, typ) ->
                 (txt, pos, kind :: kinds, typ)))
        ]
      ~default:("", `None, [], false)
      begin
        fun buffer (txt, pos, kinds, typ) ->
          match pos with
          | `None -> failwith "-position <pos> is mandatory"
          | #Msource.position as pos ->
            run buffer
              (Query_protocol.Expand_prefix (txt, pos, List.rev kinds, typ))
      end;
    command "extension-list"
      ~spec:
        [ optional "-status" "<all|enabled|disabled> Filter extensions"
            (Marg.param "<all|enabled|disabled>" (fun status _status ->
                 match status with
                 | "all" -> `All
                 | "enabled" -> `Enabled
                 | "disabled" -> `Disabled
                 | _ ->
                   failwith "-status should be one of all, disabled or enabled"))
        ]
      ~doc:
        "List all known / currently enabled / currently disabled extensions as \
         a list of strings."
      ~default:`All
      begin
        fun buffer status -> run buffer (Query_protocol.Extension_list status)
      end;
    command "findlib-list"
      ~doc:"Returns all known findlib packages as a list of string." ~spec:[]
      ~default:()
      begin
        fun buffer () -> run buffer Query_protocol.Findlib_list
      end;
    command "flags-list" ~spec:[]
      ~doc:
        "Returns supported compiler flags.The purpose of this command is to \
         implement interactive completion of compiler settings in an IDE."
      ~default:()
      begin
        fun _ () ->
          `List (List.map ~f:Json.string (Mconfig.flags_for_completion ()))
      end;
    command "jump"
      ~spec:
        [ arg "-target" "<string> Entity to jump to"
            (Marg.param "string" (fun target (_, pos) -> (target, pos)));
          arg "-position" "<position> Position to complete"
            (marg_position (fun pos (target, _pos) -> (target, pos)))
        ]
      ~doc:
        "This command can be used to assist navigation in a source code buffer.\n\
         Target is a string that can contain one or more of the 'fun', 'let', \
         'module', 'module-type' and 'match' words.\n\
         It returns the starting position of the function, let definition, \
         module or match expression that contains the cursor\n"
      ~default:("", `None)
      begin
        fun buffer (target, pos) ->
          match pos with
          | `None -> failwith "-position <pos> is mandatory"
          | #Msource.position as pos ->
            run buffer (Query_protocol.Jump (target, pos))
      end;
    command "phrase"
      ~spec:
        [ arg "-target" "<next|prev> Entity to jump to"
            (Marg.param "string" (fun target (_, pos) ->
                 match target with
                 | "next" -> (`Next, pos)
                 | "prev" -> (`Prev, pos)
                 | _ -> failwith "-target should be one of 'next' or 'prev'"));
          arg "-position" "<position> Position to complete"
            (marg_position (fun pos (target, _pos) -> (target, pos)))
        ]
      ~doc:
        "Returns the position of the next or previous phrase (top-level \
         definition or module definition)."
      ~default:(`Next, `None)
      begin
        fun buffer (target, pos) ->
          match pos with
          | `None -> failwith "-position <pos> is mandatory"
          | #Msource.position as pos ->
            run buffer (Query_protocol.Phrase (target, pos))
      end;
    command "list-modules"
      ~spec:
        [ many "-ext" "<extension> file extensions to look for"
            (Marg.param "extension" (fun ext exts -> ext :: exts))
        ]
      ~doc:
        "Looks into project source paths for files with an extension matching \
         and prints the corresponding module name."
      ~default:[]
      begin
        fun buffer extensions ->
          run buffer (Query_protocol.List_modules (List.rev extensions))
      end;
    command "locate"
      ~spec:
        [ optional "-prefix" "<string> Prefix to complete"
            (Marg.param "string" (fun txt (_, pos, kind) ->
                 (Some txt, pos, kind)));
          arg "-position" "<position> Position to complete"
            (marg_position (fun pos (prefix, _pos, kind) -> (prefix, pos, kind)));
          optional "-look-for"
            "<interface|implementation> Prefer opening interface or \
             implementation"
            (Marg.param "<interface|implementation>"
               (fun kind (prefix, pos, _) ->
                 match kind with
                 | "mli" | "interface" -> (prefix, pos, `MLI)
                 | "ml" | "implementation" -> (prefix, pos, `ML)
                 | str ->
                   failwithf "expecting interface or implementation, got %S."
                     str))
        ]
      ~doc:
        "Finds the declaration of entity at the specified position, Or \
         referred to by specified string.\n\
         Returns either:\n\
         - if location failed, a `string` describing the reason to the user,\n\
         - `{'pos': position}` if the location is in the current buffer,\n\
         - `{'file': string, 'pos': position}` if definition is located in a \
         different file."
      ~default:(None, `None, `MLI)
      begin
        fun buffer (prefix, pos, lookfor) ->
          match pos with
          | `None -> failwith "-position <pos> is mandatory"
          | #Msource.position as pos ->
            run buffer (Query_protocol.Locate (prefix, lookfor, pos))
      end;
    command "locate-type"
      ~spec:
        [ arg "-position" "<position> Position to complete"
            (marg_position (fun pos _ -> pos))
        ]
      ~doc:"Locate the declaration of the type of the expression" ~default:`None
      begin
        fun buffer pos ->
          match pos with
          | `None -> failwith "-position <pos> is mandatory"
          | #Msource.position as pos ->
            run buffer (Query_protocol.Locate_type pos)
      end;
    command "occurrences"
      ~spec:
        [ arg "-identifier-at" "<position> Position of the identifier"
            (marg_position (fun pos (_pos, scope) -> (`Ident_at pos, scope)));
          optional "-scope" "buffer|project Scope of the query"
            (Marg.param "<buffer|project>" (fun scope (pos, _scope) ->
                 match scope with
                 | "buffer" -> (pos, `Buffer)
                 | "project" -> (pos, `Project)
                 | "renaming" -> (pos, `Renaming)
                 | _ -> failwith "-scope should be one of buffer or project"))
        ]
      ~doc:
        "Returns a list of locations `{'start': position, 'end': position}` of \
         all occurrences in current buffer of the entity at the specified \
         position."
      ~default:(`None, `Buffer)
      begin
        fun buffer -> function
          | `None, _ -> failwith "-identifier-at <pos> is mandatory"
          | `Ident_at pos, scope ->
            run buffer (Query_protocol.Occurrences (`Ident_at pos, scope))
      end;
    command "outline" ~spec:[]
      ~doc:
        "Returns a tree of objects `{'start': position, 'end': position, \
         'name': string, 'kind': string, 'children': subnodes}` describing the \
         content of the buffer."
      ~default:()
      begin
        fun buffer () -> run buffer Query_protocol.Outline
      end;
    command "path-of-source"
      ~doc:
        "Looks for first file with a matching name in the project source and \
         build paths"
      ~spec:
        [ arg "-file" "<filename> filename to look for in project paths"
            (Marg.param "filename" (fun file files -> file :: files))
        ]
      ~default:[]
      begin
        fun buffer filenames ->
          run buffer (Query_protocol.Path_of_source (List.rev filenames))
      end;
    command "refactor-open"
      ~doc:"refactor-open -position pos -action <qualify|unqualify>\n\tTODO"
      ~spec:
        [ arg "-position" "<position> Position to complete"
            (marg_position (fun pos (action, _pos) -> (action, pos)));
          arg "-action" "<qualify|unqualify> Direction of rewriting"
            (Marg.param "<qualify|unqualify>" (fun action (_action, pos) ->
                 match action with
                 | "qualify" -> (Some `Qualify, pos)
                 | "unqualify" -> (Some `Unqualify, pos)
                 | _ -> failwith "invalid -action"))
        ]
      ~default:(None, `None)
      begin
        fun buffer -> function
          | None, _ -> failwith "-action is mandatory"
          | _, `None -> failwith "-position is mandatory"
          | Some action, (#Msource.position as pos) ->
            run buffer (Query_protocol.Refactor_open (action, pos))
      end;
    command "refactoring-extract-region" ~doc:"extract a region as function"
      ~spec:
        [ arg "-start" "<position> Where extracted region start"
            (marg_position (fun start (_start, stop, name) ->
                 (start, stop, name)));
          arg "-end" "<position> Where extracted region end"
            (marg_position (fun stop (start, _stop, name) ->
                 (start, stop, name)));
          optional "-extract-name"
            "<name> Name used by the generated let binding"
            (Marg.param "string" (fun name (start, stop, _name) ->
                 let name =
                   match String.trim name with
                   | "" -> None
                   | n -> Some n
                 in
                 (start, stop, name)))
        ]
      ~default:(`None, `None, None)
      begin
        fun buffer (start, stop, name) ->
          match (start, stop, name) with
          | `None, `None, _ -> failwith "-start <pos> and -end are mandatory"
          | `None, _, _ -> failwith "-start <pos> is mandatory"
          | _, `None, _ -> failwith "-end <pos> is mandatory"
          | (#Msource.position as start), (#Msource.position as stop), name ->
            let raw_source = Mpipeline.raw_source buffer in
            run buffer
              (Query_protocol.Refactor_extract_region
                 (start, stop, name, raw_source))
      end;
    command "search-by-polarity"
      ~doc:"search-by-polarity -position pos -query ident\n\tTODO"
      ~spec:
        [ arg "-position" "<position> Position to complete"
            (marg_position (fun pos (query, _pos) -> (query, pos)));
          arg "-query"
            "<string> Query of the form every input parameters prefixed by `-` \
             and output parameters prefixed by `+`. In example: -string \
             +option will fetch function that takes string and returns an \
             option. (You can't parametrize types in polarity queries)"
            (Marg.param "string" (fun query (_prefix, pos) -> (query, pos)))
        ]
      ~default:("", `None)
      begin
        fun buffer (query, pos) ->
          match pos with
          | `None -> failwith "-position <pos> is mandatory"
          | #Msource.position as pos ->
            run buffer (Query_protocol.Polarity_search (query, pos))
      end;
    command "search-by-type" ~doc:"return a list of values that match a query"
      ~spec:
        [ arg "-position" "<position> to complete"
            (marg_position (fun pos (query, _pos, limit, with_doc) ->
                 (query, pos, limit, with_doc)));
          arg "-query" "<query> to request values"
            (Marg.param "string" (fun query (_query, pos, limit, with_doc) ->
                 (Some query, pos, limit, with_doc)));
          optional "-limit"
            "<int> the maximal amount of results (default is 100)"
            (Marg.int (fun limit (query, pos, _limit, with_doc) ->
                 (query, pos, limit, with_doc)));
          optional "-with-doc" "<bool> include docstring (default is false)"
            (Marg.bool (fun with_doc (query, pos, limit, _with_doc) ->
                 (query, pos, limit, with_doc)))
        ]
      ~default:(None, `None, 100, false)
      begin
        fun buffer (query, pos, limit, with_doc) ->
          match (query, pos) with
          | None, `None ->
            failwith "-position <pos> and -query <string> are mandatory"
          | None, _ -> failwith "-query <string> is mandatory"
          | _, `None -> failwith "-position <pos> is mandatory"
          | Some query, (#Msource.position as pos) ->
            run buffer
              (Query_protocol.Type_search (query, pos, limit, with_doc))
      end;
    command "inlay-hints"
      ~doc:"return a list of inly-hints for additional client (like LSP)"
      ~spec:
        [ arg "-start" "<position> Where inlay-hints generation start"
            (marg_position
               (fun
                 start
                 ( _start,
                   stop,
                   let_binding,
                   pattern_binding,
                   function_params,
                   ghost )
               ->
                 ( start,
                   stop,
                   let_binding,
                   pattern_binding,
                   function_params,
                   ghost )));
          arg "-end" "<position> Where inlay-hints generation stop"
            (marg_position
               (fun
                 stop
                 ( start,
                   _stop,
                   let_binding,
                   pattern_binding,
                   function_params,
                   ghost )
               ->
                 ( start,
                   stop,
                   let_binding,
                   pattern_binding,
                   function_params,
                   ghost )));
          optional "-let-binding" "<bool> Hint let-binding (default is false)"
            (Marg.bool
               (fun
                 let_binding
                 ( start,
                   stop,
                   _let_binding,
                   pattern_binding,
                   function_params,
                   ghost )
               ->
                 ( start,
                   stop,
                   let_binding,
                   pattern_binding,
                   function_params,
                   ghost )));
          optional "-pattern-binding"
            "<bool> Hint pattern-binding (default is false)"
            (Marg.bool
               (fun
                 pattern_binding
                 ( start,
                   stop,
                   let_binding,
                   _pattern_binding,
                   function_params,
                   ghost )
               ->
                 ( start,
                   stop,
                   let_binding,
                   pattern_binding,
                   function_params,
                   ghost )));
          optional "-function-params"
            "<bool> Hint function parameters (default is false)"
            (Marg.bool
               (fun
                 function_params
                 ( start,
                   stop,
                   let_binding,
                   pattern_binding,
                   _function_params,
                   ghost )
               ->
                 ( start,
                   stop,
                   let_binding,
                   pattern_binding,
                   function_params,
                   ghost )));
          optional "-avoid-ghost-location"
            "<bool> Avoid hinting ghost location (default is true)"
            (Marg.bool
               (fun
                 ghost
                 ( start,
                   stop,
                   let_binding,
                   pattern_binding,
                   function_params,
                   _ghost )
               ->
                 ( start,
                   stop,
                   let_binding,
                   pattern_binding,
                   function_params,
                   ghost )))
        ]
      ~default:(`None, `None, false, false, false, true)
      begin
        fun buffer
          ( start,
            stop,
            let_binding,
            pattern_binding,
            function_params,
            avoid_ghost )
        ->
          match (start, stop) with
          | `None, `None -> failwith "-start <pos> and -end are mandatory"
          | `None, _ -> failwith "-start <pos> is mandatory"
          | _, `None -> failwith "-end <pos> is mandatory"
          | (#Msource.position, #Msource.position) as position ->
            let start, stop = position in
            run buffer
              (Query_protocol.Inlay_hints
                 ( start,
                   stop,
                   let_binding,
                   pattern_binding,
                   function_params,
                   avoid_ghost ))
      end;
    command "shape"
      ~doc:
        "This command can be used to assist navigation in a source code buffer.\n\
         It returns a tree of all relevant locations around the cursor.\n\
         It is similar to outline without telling any information about the \
         entity at a given location.\n\
         ```javascript\n\
         shape =\n\
         {\n\
        \  'start' : position,\n\
        \  'end'   : position,\n\
        \  'children' : [shape]\n\
         }\n\
         ```\n"
      ~spec:
        [ arg "-position" "<position> Position "
            (marg_position (fun pos _pos -> pos))
        ]
      ~default:`None
      begin
        fun buffer -> function
          | `None -> failwith "-position <pos> is mandatory"
          | #Msource.position as pos -> run buffer (Query_protocol.Shape pos)
      end;
    command "type-enclosing"
      ~doc:
        "Returns a list of type information for all expressions at given \
         position, sorted by increasing size.\n\
         That is asking for type enlosing around `2` in `string_of_int 2` will \
         return the types of `2 : int` and `string_of_int 2 : string`.\n\n\
         If `-expression` and `-cursor` are specified, the first result will \
         be the type\n\
         relevant to the prefix ending at the `cursor` offset.\n\n\
         `-index` can be used to print only one type information. This is \
         useful to\n\
         query the types lazily: normally, Merlin would return the signature \
         of all\n\
         enclosing modules, which can be very expensive.\n\n\
         The result is returned as a list of:\n\
         ```javascript\n\
         {\n\
        \  'start': position,\n\
        \  'end': position,\n\
        \  'type': string,\n\
        \  // is this expression not in tail position, in tail position, or \
         even a tail call?\n\
        \  'tail': ('no' | 'position' | 'call')\n\
         }\n\
         ```"
      ~spec:
        [ arg "-position" "<position> Position to complete"
            (marg_position (fun pos (expr, cursor, _pos, index) ->
                 (expr, cursor, pos, index)));
          optional "-expression" "<string> Expression to type"
            (Marg.param "string" (fun expr (_expr, cursor, pos, index) ->
                 (expr, cursor, pos, index)));
          optional "-cursor" "<int> Position of the cursor inside expression"
            (Marg.param "int" (fun cursor (expr, _cursor, pos, index) ->
                 match int_of_string cursor with
                 | cursor -> (expr, cursor, pos, index)
                 | exception _ -> failwith "cursor should be an integer"));
          optional "-index" "<int> Only print type of <index>'th result"
            (Marg.param "int" (fun index (expr, cursor, pos, _index) ->
                 match int_of_string index with
                 | index -> (expr, cursor, pos, Some index)
                 | exception _ -> failwith "index should be an integer"))
        ]
      ~default:("", -1, `None, None)
      begin
        fun buffer (expr, cursor, pos, index) ->
          match pos with
          | `None -> failwith "-position <pos> is mandatory"
          | #Msource.position as pos ->
            let expr =
              if expr = "" then None
              else
                let cursor =
                  if cursor = -1 then String.length expr else cursor
                in
                Some (expr, cursor)
            in
            run buffer (Query_protocol.Type_enclosing (expr, pos, index))
      end;
    command "type-expression"
      ~doc:
        "Returns the type of the expression when typechecked in the \
         environment around the specified position."
      ~spec:
        [ arg "-position" "<position> Position to complete"
            (marg_position (fun pos (expr, _pos) -> (expr, pos)));
          arg "-expression" "<string> Expression to type"
            (Marg.param "string" (fun expr (_expr, pos) -> (expr, pos)))
        ]
      ~default:("", `None)
      begin
        fun buffer (expr, pos) ->
          match pos with
          | `None -> failwith "-position <pos> is mandatory"
          | #Msource.position as pos ->
            run buffer (Query_protocol.Type_expr (expr, pos))
      end;
    (* Implemented without support from Query_protocol.  This command might be
       refactored if it proves useful for old protocol too. *)
    command "check-configuration" ~spec:[]
      ~doc:
        "This command checks that merlin project and options are correct.\n\
         The return value has the shape:\n\
         ```javascript\n\
         {\n\
        \  'dot_merlins': [path], // a list of string\n\
        \  'failures': [message]  // a list of string\n\
         }\n\
         ```"
      ~default:()
      begin
        fun pipeline () ->
          let config = Mpipeline.final_config pipeline in
          `Assoc
            [ (* TODO Remove support for multiple configuration files
                 The protocol could be changed to:
                 'config_file': path_to_dot_merlin_or_dune

                 For now, if the configurator is dune, the field 'dot_merlins'
                 will contain the path to the dune file (or jbuild, or dune-project)
              *)
              ( "dot_merlins",
                `List
                  (match Mconfig.(config.merlin.config_path) with
                  | Some path -> [ Json.string path ]
                  | None -> []) );
              ( "failures",
                `List (List.map ~f:Json.string Mconfig.(config.merlin.failures))
              )
            ]
      end;
    command "signature-help" ~doc:"Returns LSP Signature Help response"
      ~spec:
        [ arg "-position" "<position> Position of Signature Help request"
            (marg_position (fun pos (expr, _pos) -> (expr, pos)))
        ]
      ~default:("", `None)
      begin
        fun buffer (_, pos) ->
          match pos with
          | `None -> failwith "-position <pos> is mandatory"
          | #Msource.position as position ->
            let sh =
              { Query_protocol.position;
                trigger_kind = None;
                is_retrigger = false;
                active_signature_help = None
              }
            in
            run buffer (Query_protocol.Signature_help sh)
      end;
    (* Used only for testing *)
    command "dump"
      ~spec:
        [ arg "-what"
            "<source|parsetree|ppxed-source|ppxed-parsetree|typedtree|env|fullenv|browse|tokens|flags|warnings|exn|paths> \
             Information to dump ()"
            (Marg.param "string" (fun what _ -> what))
        ]
      ~default:"" ~doc:"Not for the casual user, used for debugging merlin"
      begin
        fun pipeline what -> run pipeline (Query_protocol.Dump [ `String what ])
      end;
    (* Used only for testing *)
    command "dump-configuration" ~spec:[] ~default:()
      ~doc:"Not for the casual user, used for merlin tests"
      begin
        fun pipeline () -> Mconfig.dump (Mpipeline.final_config pipeline)
      end
  ]
