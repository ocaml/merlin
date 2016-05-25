(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>

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
open Logger

let latest_version : Protocol.protocol_version = `V3
let current_version = ref `V2

let default_context =
  {Protocol.Context.
    document = None; printer_width = None; printer_verbosity = None}

let invalid_arguments () = failwith "invalid arguments"

exception Failure' = Failure
open Protocol

let with_location ?(skip_none=false) loc assoc =
  if skip_none && loc = Location.none then
    `Assoc assoc
  else
    `Assoc (("start", Lexing.json_of_position loc.Location.loc_start) ::
            ("end",   Lexing.json_of_position loc.Location.loc_end) ::
            assoc)

let json_of_error {Error_report. valid; text; where; sub; loc} =
  let of_sub (msg,loc) = with_location ~skip_none:true loc ["message", `String msg] in
  let content = [
    "type"    , `String where;
    "sub"     , `List (List.map ~f:of_sub sub);
    "valid"   , `Bool valid;
    "message" , `String text;
  ] in
  with_location ~skip_none:true loc content

let error_catcher exn =
  match Error_report.strict_of_exn exn with
  | None -> None
  | Some t -> Some (json_of_error t)

let pos_of_json = function
  | `String "start" -> `Start
  | `String "end" -> `End
  | `Int offset -> `Offset offset
  | `Assoc props ->
    begin try match List.assoc "line" props, List.assoc "col" props with
      | `Int line, `Int col -> `Logical (line,col)
      | _ -> failwith "Incorrect position"
      with Not_found -> failwith "Incorrect position"
    end
  | _ -> failwith "Incorrect position"

let optional_position = function
  | [`String "at"; jpos] -> Some (pos_of_json jpos)
  | [] -> None
  | _ -> invalid_arguments ()

let mandatory_position = function
  | [`String "at"; jpos] -> pos_of_json jpos
  | _ -> invalid_arguments ()

let optional_string = function
  | [`String name] -> Some name
  | [] -> None
  | _ -> invalid_arguments ()

let string_list l =
  List.map (function `String s -> s | _ -> invalid_arguments ()) l

let json_of_string_list l =
  `List (List.map (fun s -> `String s) l)

let json_of_type_loc (loc,str,tail) =
  with_location loc [
    "type", `String str;
    "tail", `String (match tail with
        | `No -> "no"
        | `Tail_position -> "position"
        | `Tail_call -> "call")
  ]

let string_of_kind = function
  | `Value       -> "Value"
  | `Variant     -> "Variant"
  | `Constructor -> "Constructor"
  | `Label       -> "Label"
  | `Module      -> "Module"
  | `Modtype     -> "Signature"
  | `Type        -> "Type"
  | `Method      -> "Method"
  | `MethodCall  -> "#"
  | `Exn         -> "Exn"
  | `Class       -> "Class"

let json_of_completion {Compl. name; kind; desc; info} =
  `Assoc ["name", `String name;
          "kind", `String (string_of_kind kind);
          "desc", `String desc;
          "info", `String info]

let json_of_completions {Compl. entries; context } =
  `Assoc [
    "entries", `List (List.map json_of_completion entries);
    "context", (match context with
        | `Unknown -> `Null
        | `Application {Compl. argument_type; labels} ->
          let label (name,ty) = `Assoc ["name", `String name;
                                        "type", `String ty] in
          let a = `Assoc ["argument_type", `String argument_type;
                          "labels", `List (List.map label labels)] in
          `List [`String "application"; a])
  ]

let rec json_of_outline outline =
  let json_of_item { outline_name ; outline_kind ; location ; children } =
    with_location location [
      "name", `String outline_name;
      "kind", `String (string_of_kind outline_kind);
      "children", `List (json_of_outline children);
    ]
  in
  List.map json_of_item outline

let rec json_of_shape { shape_loc; shape_sub } =
  with_location shape_loc [
    "children", `List (List.map ~f:json_of_shape shape_sub);
  ]

let source_or_build = function
  | "source" -> `Source
  | "build"  -> `Build
  | _ -> invalid_arguments ()

let ml_or_mli = function
  | "ml" -> `ML
  | "mli"  -> `MLI
  | _ -> invalid_arguments ()

let auto_ml_or_mli = function
  | "auto" -> `Auto
  | x -> ml_or_mli x

let add_or_remove = function
  | "add"    -> `Add
  | "remove" -> `Rem
  | _ -> invalid_arguments ()

let load_or_find = function
  | "load" -> `File
  | "find" -> `Find
  | _ -> invalid_arguments ()

let with_failures assoc = function
  | `Ok -> assoc
  | `Failures failures ->
    let packages, flags, extensions =
      List.fold_left failures ~init:([],[],[]) ~f:(
        fun (pkgs, flgs, exts) (str,exn) ->
          let str = "\"" ^ str ^ "\"" in
          match exn with
          | Fl_package_base.No_such_package _ -> str :: pkgs, flgs, exts
          | Arg.Bad _ -> pkgs, str :: flgs, exts
          | Extension.Unknown -> pkgs, flgs, str :: exts
          | e -> (str ^ " (" ^ Printexc.to_string e ^ ")") :: pkgs, flgs, exts
      )
    in
    let packages =
      match packages with
      | [] -> []
      | failures ->
        let str = String.concat ~sep:", " failures in
        [ `String ("Failed to load some packages " ^ str) ]
    in
    let flags =
      match flags with
      | [] -> []
      | failures ->
        let str = String.concat ~sep:", " failures in
        [ `String ("Unknown flags " ^ str) ]
    in
    let extensions =
      match extensions with
      | [] -> []
      | failures ->
        let str = String.concat ~sep:", " failures in
        [ `String ("Unknown extensions " ^ str) ]
    in
    ("failures", `List (packages @ flags @ extensions)) :: assoc

let document_of_json =
  let make kind path dot_merlins =
    {Context.dot_merlins;
     kind = auto_ml_or_mli kind;
     path = optional_string path;
    }
  in function
    | (`String "dot_merlin" :: `List dot_merlins :: `String kind :: opt_name) ->
      make kind opt_name (Some (string_list dot_merlins))
    | (`String kind :: opt_name) ->
      make kind opt_name None
    | _ -> invalid_arguments ()

let request_of_json context =
  let request x = Request (context, x) in function
    | (`String "type" :: `String "expression" :: `String expr :: opt_pos) ->
      request (Query (Type_expr (expr, mandatory_position opt_pos)))
    | [`String "type"; `String "enclosing";
       `Assoc [ "expr", `String expr ; "offset", `Int offset] ; jpos] ->
      request (Query (Type_enclosing (Some (expr, offset), pos_of_json jpos)))
    | [`String "type"; `String "enclosing"; `String "at"; jpos] ->
      request (Query (Type_enclosing (None, pos_of_json jpos)))
    | [ `String "case"; `String "analysis"; `String "from"; x; `String "to"; y ] ->
      request (Query (Case_analysis (pos_of_json x, pos_of_json y)))
    | [`String "enclosing"; jpos] ->
      request (Query (Enclosing (pos_of_json jpos)))
    | [`String "complete"; `String "prefix"; `String prefix; `String "at"; jpos] ->
      request (Query (Complete_prefix (prefix, pos_of_json jpos, false)))
    | [`String "complete"; `String "prefix"; `String prefix; `String "at"; jpos;
       `String "with"; `String "doc"] ->
      request (Query (Complete_prefix (prefix, pos_of_json jpos, true)))
    | [`String "expand"; `String "prefix"; `String prefix; `String "at"; jpos] ->
      request (Query (Expand_prefix (prefix, pos_of_json jpos)))
    | (`String "document" :: (`String "" | `Null) :: pos) ->
      request (Query (Document (None, mandatory_position pos)))
    | (`String "document" :: `String path :: pos) ->
      request (Query (Document (Some path, mandatory_position pos)))
    | (`String "locate" :: (`String "" | `Null) :: `String choice :: pos) ->
      request (Query (Locate (None, ml_or_mli choice, mandatory_position pos)))
    | (`String "locate" :: `String path :: `String choice :: pos) ->
      request (Query (Locate (Some path, ml_or_mli choice, mandatory_position pos)))
    | (`String "jump" :: `String target :: pos) ->
      request (Query (Jump (target, mandatory_position pos)))
    | [`String "outline"] ->
      request (Query Outline)
    | [`String "shape"; pos] ->
      request (Query (Shape (pos_of_json pos)))
    | [`String "occurrences"; `String "ident"; `String "at"; jpos] ->
      request (Query (Occurrences (`Ident_at (pos_of_json jpos))))
    | (`String ("reset"|"checkout") :: document) ->
      request (Sync (Checkout (document_of_json document)))
    | [`String "refresh"] ->
      request (Sync Refresh)
    | [`String "errors"] ->
      request (Query Errors)
    | (`String "dump" :: args) ->
      request (Query (Dump args))
    | [`String "which"; `String "path"; `String name] ->
      request (Query (Which_path [name]))
    | [`String "which"; `String "path"; `List names] ->
      request (Query (Which_path (string_list names)))
    | [`String "which"; `String "with_ext"; `String ext] ->
      request (Query (Which_with_ext [ext]))
    | [`String "which"; `String "with_ext"; `List exts] ->
      request (Query (Which_with_ext (string_list exts)))
    | [`String "flags" ; `String "set" ; `List flags ] ->
      request (Sync (Flags_set (string_list flags)))
    | [`String "flags" ; `String "get" ] ->
      request (Query (Flags_get))
    | [`String "find"; `String "use"; `List packages]
    | (`String "find" :: `String "use" :: packages) ->
      request (Sync (Findlib_use (string_list packages)))
    | [`String "find"; `String "list"] ->
      request (Query Findlib_list)
    | [`String "extension"; `String "enable"; `List extensions] ->
      request (Sync (Extension_set (`Enabled,string_list extensions)))
    | [`String "extension"; `String "disable"; `List extensions] ->
      request (Sync (Extension_set (`Disabled,string_list extensions)))
    | [`String "extension"; `String "list"] ->
      request (Query (Extension_list `All))
    | [`String "extension"; `String "list"; `String "enabled"] ->
      request (Query (Extension_list `Enabled))
    | [`String "extension"; `String "list"; `String "disabled"] ->
      request (Query (Extension_list `Disabled))
    | [`String "path"; `String "list";
       `String ("source"|"build" as var)] ->
      request (Query (Path_list (source_or_build var)))
    | [`String "path"; `String "reset"] ->
      request (Sync Path_reset)
    | (`String "path" :: `String ("add"|"remove" as action) ::
       `String ("source"|"build" as var) :: ((`List pathes :: []) | pathes)) ->
      request (Sync (Path (source_or_build var, add_or_remove action, string_list pathes)))
    | [`String "tell"; pos_start; pos_end; `String content] ->
      request (Sync (Tell (pos_of_json pos_start, pos_of_json pos_end, content)))
    | [`String "project"; `String "get"] ->
      request (Query Project_get)
    | [`String "version"] ->
      request (Query Version)
    | [`String "protocol"; `String "version"] ->
      request (Sync (Protocol_version None))
    | [`String "protocol"; `String "version"; `Int n] ->
      request (Sync (Protocol_version (Some n)))
    | _ -> invalid_arguments ()

let json_of_protocol_version : Protocol.protocol_version -> _ = function
  | `V2 -> `Int 2
  | `V3 -> `Int 3

let json_of_query_command (type a) (command : a query_command) (response : a) : json =
  match command, response with
  | Type_expr _, str -> `String str
  | Type_enclosing _, results ->
    `List (List.map json_of_type_loc results)
  | Enclosing _, results ->
    `List (List.map (fun loc -> with_location loc []) results)
  | Complete_prefix _, compl ->
    json_of_completions compl
  | Expand_prefix _, compl ->
    json_of_completions compl
  | Document _, resp ->
    begin match resp with
      | `No_documentation -> `String "No documentation available"
      | `Invalid_context -> `String "Not a valid identifier"
      | `Builtin s ->
        `String (sprintf "%S is a builtin, no documentation is available" s)
      | `Not_found (id, None) -> `String ("didn't manage to find " ^ id)
      | `Not_found (i, Some f) ->
        `String
          (sprintf "%s was supposed to be in %s but could not be found" i f)
      | `Not_in_env str ->
        `String (Printf.sprintf "Not in environment '%s'" str)
      | `File_not_found msg ->
        `String msg
      | `Found doc ->
        `String doc
    end
  | Locate _, resp ->
    begin match resp with
      | `At_origin -> `String "Already at definition point"
      | `Builtin s ->
        `String (sprintf "%S is a builtin, and it is therefore impossible \
                          to jump to its definition" s)
      | `Invalid_context -> `String "Not a valid identifier"
      | `Not_found (id, None) -> `String ("didn't manage to find " ^ id)
      | `Not_found (i, Some f) ->
        `String
          (sprintf "%s was supposed to be in %s but could not be found" i f)
      | `Not_in_env str ->
        `String (Printf.sprintf "Not in environment '%s'" str)
      | `File_not_found msg ->
        `String msg
      | `Found (None,pos) ->
        `Assoc ["pos", Lexing.json_of_position pos]
      | `Found (Some file,pos) ->
        `Assoc ["file",`String file; "pos", Lexing.json_of_position pos]
    end
  | Jump _, resp ->
    begin match resp with
      | `Error str ->
        `String str
      | `Found pos ->
        `Assoc ["pos", Lexing.json_of_position pos]
    end
  | Case_analysis _, ({ Location. loc_start ; loc_end }, str) ->
    let assoc =
      `Assoc [
        "start", Lexing.json_of_position loc_start  ;
        "end", Lexing.json_of_position loc_end ;
      ]
    in
    `List [ assoc ; `String str ]
  | Outline, outlines ->
    `List (json_of_outline outlines)
  | Shape _, shapes ->
    `List (List.map ~f:json_of_shape shapes)
  | Errors, errors ->
    `List (List.map ~f:json_of_error errors)
  | Dump _, json -> json
  | Which_path _, str -> `String str
  | Which_with_ext _, strs -> json_of_string_list strs
  | Flags_get, flags ->
    json_of_string_list flags
  | Findlib_list, strs -> json_of_string_list strs
  | Extension_list _, strs -> json_of_string_list strs
  | Path_list _, strs -> json_of_string_list strs
  | Project_get, (strs, failures) ->
    `Assoc (with_failures ["result", json_of_string_list strs] failures)
  | Occurrences _, locations ->
    `List (List.map locations
             ~f:(fun loc -> with_location loc []))
  | Version, version ->
    `String version
  | Idle_job, b -> `Bool b

let json_of_sync_command (type a) (command : a sync_command) (response : a) : json =
  match command, response with
  | Tell _, () -> `Bool true
  | Checkout _, () -> `Bool true
  | Refresh, () -> `Bool true
  | Flags_set _, failures ->
    `Assoc (with_failures ["result", `Bool true] failures)
  | Findlib_use _, failures ->
    `Assoc (with_failures ["result", `Bool true] failures)
  | Extension_set _, failures ->
    `Assoc (with_failures ["result", `Bool true] failures)
  | Path _, () -> `Bool true
  | Path_reset, () -> `Bool true
  | Protocol_version _, (`Selected v, `Latest vm, version) ->
    `Assoc ["selected", json_of_protocol_version v;
            "latest", json_of_protocol_version vm;
            "merlin",  `String version
           ]

let classify_response = function
  | Failure s | Exception (Failure' s) -> ("failure", `String s)
  | Error error -> ("error", error)
  | Exception exn ->
    begin match error_catcher exn with
      | Some error -> ("error", error)
      | None -> ("exception", `String (Printexc.to_string exn))
    end
  | Return (Query cmd, response) ->
    ("return", json_of_query_command cmd response)
  | Return (Sync cmd, response) ->
    ("return", json_of_sync_command cmd response)

let json_of_response_v2 response =
  let class_, value = classify_response response in
  `List [`String class_; value]

let json_of_response_v3 ~notifications response =
  let class_, value = classify_response response in
  `Assoc [
    "class", `String class_;
    "value", value;
    "notifications",
    `List (List.map (fun (sec,msg) ->
        `Assoc ["section", `String sec; "message", `String msg])
        notifications);
  ]

let json_of_response ~notifications response =
  match !current_version with
  | `V2 -> json_of_response_v2 response
  | `V3 -> json_of_response_v3 ~notifications response

let request_of_json = function
  | `Assoc _ as json ->
    let open Json.Util in
    let document =
      let value = member "document" json in
      let value =
        if value = `Null then
          member "context" json
        else value
      in
      if value = `Null then
        None
      else Some (to_list value |> document_of_json)
    in
    let printer_width = member "printer_width" json |> to_int_option in
    let printer_verbosity = member "printer_verbosity" json |> to_int_option in
    let context = {Context. document; printer_verbosity; printer_width} in
    let query = member "query" json |> to_list in
    request_of_json context query
  | `List jsons -> request_of_json default_context jsons
  | _ -> invalid_arguments ()
