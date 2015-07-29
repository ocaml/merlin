(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2014  Frédéric Bour  <frederic.bour(_)lakaban.net>
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

type io = Protocol.a_request Stream.t * (Protocol.response -> unit)
type low_io = Json.json Stream.t * (Json.json -> unit)
type io_maker =
  on_read:(Unix.file_descr -> unit) -> input:Unix.file_descr ->
  output:Unix.file_descr ->
  low_io

let section = Logger.section "protocol"

let invalid_arguments () = failwith "invalid arguments"

let last_time = ref (Sys.time ())
let log_time fields =
  let old_time = !last_time in
  let new_time = Sys.time () in
  last_time := new_time;
  ("delta", `Int (int_of_float ((new_time -. old_time) *. 1000.))) ::
  fields

let json_log (input,output) =
  let wrap json = `Assoc (log_time ["body", json]) in
  let log_input json =
    Logger.infojf section ~title:"input" wrap json; json
  in
  let log_output json =
    Logger.infojf section ~title:"output" wrap json; json
  in
  let input' = Stream.map ~f:log_input input in
  let output' json = output (log_output json) in
  input', output'

let json_make ~on_read ~input ~output =
  let rec read buf len =
    on_read input;
    try Unix.read input buf 0 len
    with Unix.Unix_error (Unix.EINTR,_,_) ->
      read buf len
  in
  let lexbuf = Lexing.from_function read in
  let input = Json.stream_from_lexbuf (Json.init_lexer ()) lexbuf in
  let output = Unix.out_channel_of_descr output in
  let output' = Json.to_channel output in
  let output json =
    output' json;
    output_char output '\n';
    flush output
  in
  input, output

let makers = ref ["json", ("(default) simple JSON-based protocol", json_make)]

let register_protocol ~name ~desc inst =
  makers := (name, (desc,inst)) :: !makers

let make' = ref json_make
let make ~on_read ~input ~output =
  let io = !make' ~on_read ~input ~output in
  if Logger.is_monitored section then json_log io else io

let select_frontend name =
  try make' := snd (List.assoc name !makers)
  with Not_found ->
    if name <> "help" then
      prerr_endline
        ("Unknown protocol '" ^ name ^ "' (maybe check build configuration)\n");
    prerr_endline "Choose protocol to use for communication. Known protocols:";
    List.iter (fun (name, (desc, _)) ->
        prerr_endline (name ^ "\t" ^ desc))
      !makers;
    exit 1

module Protocol_io = struct
  exception Failure' = Failure
  open Protocol

  let json_of_error {Error_report. valid; text; where; loc} =
    let content = ["valid", `Bool valid; "message", `String text] in
    let content =
      if loc = Location.none then content else
      ("start", Lexing.json_of_position loc.Location.loc_start) ::
      ("end"  , Lexing.json_of_position loc.Location.loc_end) ::
      content
    in
    let content = ("type", `String where) :: content in
    `Assoc content

  let error_catcher exn =
    match Error_report.error_catcher exn with
    | None -> None
    | Some (loc,t) -> Some (loc, json_of_error t)

  let make_pos (pos_lnum, pos_cnum) =
    Lexing.({ pos_fname = "" ; pos_lnum ; pos_cnum ; pos_bol = 0 })

  let pos_of_json = function
    | `Assoc props ->
      begin try match List.assoc "line" props, List.assoc "col" props with
        | `Int line, `Int col -> make_pos (line,col)
        | _ -> failwith "Incorrect position"
      with Not_found -> failwith "Incorrect position"
      end
    | _ -> failwith "Incorrect position"

  let with_location loc assoc =
    `Assoc (("start", Lexing.json_of_position loc.Location.loc_start) ::
            ("end",   Lexing.json_of_position loc.Location.loc_end) ::
            assoc)

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

  let json_of_cursor_state {cursor; marker} =
    `Assoc [
      "cursor", Lexing.json_of_position cursor;
      "marker", `Bool marker;
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

  let context_of_json = function
    | (`String "dot_merlin" :: `List dot_merlins :: `String kind :: opt_name) ->
      auto_ml_or_mli kind, optional_string opt_name, Some (string_list dot_merlins)
    | (`String kind :: opt_name) ->
      auto_ml_or_mli kind, optional_string opt_name, None
    | _ -> invalid_arguments ()

  let request_of_json = function
    | (`String "tell" :: `String "start" :: opt_pos) ->
      Request (Tell (`Start (optional_position opt_pos)))
    | [`String "tell"; `String "source"; `String source] ->
      Request (Tell (`Source source))
    | [`String "tell"; `String "file"; `String path] ->
      Request (Tell (`File path))
    | [`String "tell"; `String "eof"] ->
      Request (Tell `Eof)
    | [`String "tell"; `String "marker"] ->
      Request (Tell `Marker)
    | (`String "type" :: `String "expression" :: `String expr :: opt_pos) ->
      Request (Type_expr (expr, optional_position opt_pos))
    | [`String "type"; `String "enclosing";
        `Assoc [ "expr", `String expr ; "offset", `Int offset] ; jpos] ->
      Request (Type_enclosing (Some (expr, offset), pos_of_json jpos))
    | [`String "type"; `String "enclosing"; `String "at"; jpos] ->
      Request (Type_enclosing (None, pos_of_json jpos))
    | [ `String "case"; `String "analysis"; `String "from"; x; `String "to"; y ] ->
      let loc_start = pos_of_json x in
      let loc_end = pos_of_json y in
      let loc_ghost = true in
      Request (Case_analysis ({ Location. loc_start ; loc_end ; loc_ghost }))
    | [`String "enclosing"; jpos] ->
      Request (Enclosing (pos_of_json jpos))
    | [`String "complete"; `String "prefix"; `String prefix; `String "at"; jpos] ->
      Request (Complete_prefix (prefix, pos_of_json jpos, false))
    | [`String "complete"; `String "prefix"; `String prefix; `String "at"; jpos;
       `String "with"; `String "doc"] ->
      Request (Complete_prefix (prefix, pos_of_json jpos, true))
    | [`String "expand"; `String "prefix"; `String prefix; `String "at"; jpos] ->
      Request (Expand_prefix (prefix, pos_of_json jpos))
    | (`String "document" :: (`String "" | `Null) :: pos) ->
      Request (Document (None, mandatory_position pos))
    | (`String "document" :: `String path :: pos) ->
      Request (Document (Some path, mandatory_position pos))
    | (`String "locate" :: (`String "" | `Null) :: `String choice :: pos) ->
      Request (Locate (None, ml_or_mli choice, mandatory_position pos))
    | (`String "locate" :: `String path :: `String choice :: pos) ->
      Request (Locate (Some path, ml_or_mli choice, mandatory_position pos))
    | [`String "outline"] ->
      Request Outline
    | [`String "drop"] ->
      Request Drop
    | [`String "seek"; `String "position"] ->
      Request (Seek `Position)
    | [`String "seek"; `String "marker"] ->
      Request (Seek `Marker)
    | [`String "occurrences"; `String "ident"; `String "at"; jpos] ->
      Request (Occurrences (`Ident_at (pos_of_json jpos)))
    | [`String "seek"; `String "before"; jpos] ->
      Request (Seek (`Before (pos_of_json jpos)))
    | [`String "seek"; `String "exact"; jpos] ->
      Request (Seek (`Exact (pos_of_json jpos)))
    | [`String "seek"; `String "end"] ->
      Request (Seek `End)
    | (`String "boundary" :: `String "next" :: opt_pos) ->
      Request (Boundary (`Next, mandatory_position opt_pos))
    | (`String "boundary" :: `String "prev" :: opt_pos) ->
      Request (Boundary (`Prev, mandatory_position opt_pos))
    | (`String "boundary" :: `String "current" :: opt_pos)
    | (`String "boundary" :: opt_pos) ->
      Request (Boundary (`Current, mandatory_position opt_pos))
    | (`String ("reset"|"checkout") :: context) ->
      Request (Checkout (context_of_json context))
    | [`String "refresh"] ->
      Request Refresh
    | [`String "errors"] ->
      Request Errors
    | (`String "dump" :: `String "env" :: opt_pos) ->
      Request (Dump (`Env (`Normal, optional_position opt_pos)))
    | (`String "dump" :: `String "full_env" :: opt_pos) ->
      Request (Dump (`Env (`Full, optional_position opt_pos)))
    | [`String "dump"; `String "sig"] ->
      Request (Dump `Sig)
    | [`String "dump"; `String "parser"] ->
      Request (Dump `Parser)
    | [`String "dump"; `String "recover"] ->
      Request (Dump `Recover)
    | [`String "dump"; `String "exn"] ->
      Request (Dump `Exn)
    | [`String "dump"; `String "browse"] ->
      Request (Dump `Browse)
    | [`String "dump"; `String "typer"; `String "input"] ->
      Request (Dump (`Typer `Input))
    | [`String "dump"; `String "typer"; `String "output"] ->
      Request (Dump (`Typer `Output))
    | [`String "dump"; `String "tokens"] ->
      Request (Dump `Tokens)
    | [`String "dump"; `String "flags"] ->
      Request (Dump `Flags)
    | [`String "dump"; `String "warnings"] ->
      Request (Dump `Warnings)
    | [`String "which"; `String "path"; `String name] ->
      Request (Which_path [name])
    | [`String "which"; `String "path"; `List names] ->
      Request (Which_path (string_list names))
    | [`String "which"; `String "with_ext"; `String ext] ->
      Request (Which_with_ext [ext])
    | [`String "which"; `String "with_ext"; `List exts] ->
      Request (Which_with_ext (string_list exts))
    | [`String "flags" ; `String "add" ; `List flags ] ->
      Request (Flags (`Add (string_list flags)))
    | [`String "flags" ; `String "clear" ] ->
      Request (Flags `Clear)
    | [`String "flags" ; `String "get" ] ->
      Request (Flags_get)
    | [`String "find"; `String "use"; `List packages]
    | (`String "find" :: `String "use" :: packages) ->
      Request (Findlib_use (string_list packages))
    | [`String "find"; `String "list"] ->
      Request Findlib_list
    | [`String "extension"; `String "enable"; `List extensions] ->
      Request (Extension_set (`Enabled,string_list extensions))
    | [`String "extension"; `String "disable"; `List extensions] ->
      Request (Extension_set (`Disabled,string_list extensions))
    | [`String "extension"; `String "list"] ->
      Request (Extension_list `All)
    | [`String "extension"; `String "list"; `String "enabled"] ->
      Request (Extension_list `Enabled)
    | [`String "extension"; `String "list"; `String "disabled"] ->
      Request (Extension_list `Disabled)
    | [`String "path"; `String "list";
                       `String ("source"|"build" as var)] ->
      Request (Path_list (source_or_build var))
    | [`String "path"; `String "reset"] ->
      Request Path_reset
    | (`String "path" :: `String ("add"|"remove" as action) ::
         `String ("source"|"build" as var) :: ((`List pathes :: []) | pathes)) ->
      Request (Path (source_or_build var, add_or_remove action, string_list pathes))
    | [`String "project"; `String "get"] ->
      Request (Project_get)
    | [`String "version"] ->
      Request (Version)
    | _ -> invalid_arguments ()

  let json_of_response = function
    | Failure s | Exception (Failure' s) -> `List [`String "failure"; `String s]
    | Error error -> `List [`String "error"; error]
    | Exception exn ->
      begin match error_catcher exn with
      | Some (_,error) -> `List [`String "error"; error]
      | None -> `List [`String "exception"; `String (Printexc.to_string exn)]
      end
    | Return (request, response) ->
      `List [`String "return";
      begin match request, response with
        | Tell _, cursor ->
          json_of_cursor_state cursor
        | Seek _, cursor ->
          json_of_cursor_state cursor
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
        | Drop, cursor ->
          json_of_cursor_state cursor
        | Boundary _, Some {Location. loc_start; loc_end} ->
          `List (List.map Lexing.json_of_position [loc_start; loc_end])
        | Boundary _, None ->
          `Null
        | Checkout _, cursor ->
          json_of_cursor_state cursor
        | Refresh, () -> `Bool true
        | Errors, errors ->
          `List (List.map ~f:json_of_error errors)
        | Dump _, json -> json
        | Which_path _, str -> `String str
        | Which_with_ext _, strs -> json_of_string_list strs
        | Flags _, failures ->
          `Assoc (with_failures ["result", `Bool true] failures)
        | Flags_get, flags ->
          `List (List.map json_of_string_list flags)
        | Findlib_use _, failures ->
          `Assoc (with_failures ["result", `Bool true] failures)
        | Findlib_list, strs -> json_of_string_list strs
        | Extension_list _, strs -> json_of_string_list strs
        | Extension_set _, failures ->
          `Assoc (with_failures ["result", `Bool true] failures)
        | Path _, () -> `Bool true
        | Path_list _, strs -> json_of_string_list strs
        | Path_reset, () -> `Bool true
        | Project_get, (strs, failures) ->
          `Assoc (with_failures ["result", json_of_string_list strs] failures)
        | Occurrences _, locations ->
          `List (List.map locations
                   ~f:(fun loc -> with_location loc []))
        | Idle_job, b -> `Bool b
        | Version, version ->
          `String version
      end]

  let request_of_json = function
    | `Assoc _ as json ->
      let open Json.Util in
      let ctx = member "context" json |> to_list in
      let query = member "query" json |> to_list in
      begin match request_of_json query with
        | Request request -> Context_request (context_of_json ctx, request)
        | _ -> assert false
      end
    | `List jsons -> request_of_json jsons
    | _ -> invalid_arguments ()
end

(* Used when dumping state as raw json *)
let with_location = Protocol_io.with_location

let lift (i,o : low_io) : io =
  (Stream.map ~f:Protocol_io.request_of_json i,
   (fun x -> o (Protocol_io.json_of_response x)))
