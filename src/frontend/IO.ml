(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013  Frédéric Bour  <frederic.bour(_)lakaban.net>
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

type io = Protocol.a_request Stream.t * (Protocol.response -> unit)
type low_io = Json.json Stream.t * (Json.json -> unit)
type io_maker = input:in_channel -> output:out_channel -> low_io

let section = Logger.(`protocol)

let invalid_arguments () = failwith "invalid arguments"

let json_log (input,output) =
  let last_time = ref (Sys.time ()) in
  let log_time () =
    let old_time = !last_time in
    let new_time = Sys.time () in
    last_time := new_time;
    Printf.sprintf "time %f, delta %f" new_time (new_time -. old_time)
  in
  let mk_prefix s =
    if Clflags.timed_logs () then sprintf "// %s %s\n " s (log_time ()) else s
  in
  let log_input json =
    Logger.log section ~prefix:(mk_prefix "<") (Json.to_string json); json
  in
  let log_output json =
    Logger.log section ~prefix:(mk_prefix ">") (Json.to_string json); json
  in
  let input' = Stream.map ~f:log_input input in
  let output' json = output (log_output json) in
  input', output'

let json_make ~input ~output =
  let input   = Json.stream_from_channel input in
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
let make ~input ~output =
  let io = !make' ~input ~output in
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

  let json_of_pos pos =
    Lexing.(`Assoc ["line", `Int pos.pos_lnum;
                    "col", `Int (pos.pos_cnum - pos.pos_bol)])

  let json_of_error {Error_report. valid; text; where; loc} =
    let content = ["valid", `Bool valid; "message", `String text] in
    let content =
      if loc = Location.none then content else
      ("start", json_of_pos loc.Location.loc_start) ::
      ("end"  , json_of_pos loc.Location.loc_end) ::
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
    `Assoc (("start", json_of_pos loc.Location.loc_start) ::
            ("end",   json_of_pos loc.Location.loc_end) ::
            assoc)

  let optional_position = function
    | [`String "at"; jpos] -> Some (pos_of_json jpos)
    | [] -> None
    | _ -> invalid_arguments ()

  let optional_string = function
    | [`String name] -> Some name
    | [] -> None
    | _ -> invalid_arguments ()

  let string_list l =
    List.map (function `String s -> s | _ -> invalid_arguments ()) l

  let json_of_string_list l =
    `List (List.map (fun s -> `String s) l)

  let json_of_type_loc (loc,str) =
    with_location loc ["type", `String str]

  let string_of_kind = function
    | `Value       -> "Value"
    | `Constructor -> "Constructor"
    | `Label       -> "Label"
    | `Module      -> "Module"
    | `Modtype     -> "Signature"
    | `Type        -> "Type"
    | `MethodCall  -> "#"
    | `Exn         -> "Exn"

  let json_of_completion {Protocol. name; kind; desc; info} =
    `Assoc ["name", `String name;
            "kind", `String (string_of_kind kind);
            "desc", `String desc;
            "info", `String info]

  let rec json_of_outline outline =
    let json_of_item {Protocol. name ; kind ; pos ; children } =
      `Assoc [
        "name", `String name;
        "kind", `String (string_of_kind kind);
        "pos", json_of_pos pos;
        "children", `List (json_of_outline children);
      ]
    in
    List.map json_of_item outline

  let json_of_cursor_state {cursor; marker} =
    `Assoc [
      "cursor", json_of_pos cursor;
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

  let add_or_remove = function
    | "add"    -> `Add
    | "remove" -> `Rem
    | _ -> invalid_arguments ()

  let load_or_find = function
    | "load" -> `File
    | "find" -> `Find
    | _ -> invalid_arguments ()

  let with_package_failures assoc = function
    | `Ok -> assoc
    | `Failures failures ->
      let failures = List.map failures
          ~f:(fun (str,exn) ->
              let str = "\"" ^ str ^ "\"" in
              let str = match exn with
                | Fl_package_base.No_such_package _ -> str
                | exn -> str ^ " (" ^ Printexc.to_string exn ^ ")"
              in
              str)
      in
      let str = String.concat ~sep:", " failures in
      ("failures", `String ("Failed to load some packages " ^ str)) :: assoc

  let request_of_json = function
    | (`String "tell" :: `String "start" :: opt_pos) ->
      Request (Tell (`Start (optional_position opt_pos)))
    | [`String "tell"; `String "source"; `String source] ->
      Request (Tell (`Source source))
    | [`String "tell"; `String "eof"] ->
      Request (Tell (`Source ""))
    | [`String "tell"; `String "marker"] ->
      Request (Tell `Marker)
    | (`String "type" :: `String "expression" :: `String expr :: opt_pos) ->
      Request (Type_expr (expr, optional_position opt_pos))
    | [`String "type"; `String "enclosing";
        `Assoc [ "expr", `String expr ; "offset", `Int offset] ; jpos] ->
      Request (Type_enclosing ((expr, offset), pos_of_json jpos))
    | [`String "complete"; `String "prefix"; `String prefix; `String "at"; jpos] ->
      Request (Complete_prefix (prefix, pos_of_json jpos))
    | (`String "locate" :: `String path :: opt_pos) ->
      Request (Locate (path, optional_position opt_pos))
    | [`String "outline"] ->
      Request Outline
    | [`String "drop"] ->
      Request Drop
    | [`String "seek"; `String "position"] ->
      Request (Seek `Position)
    | [`String "occurences"; `String "ident"; `String "at"; jpos] ->
      Request (Occurences (`Ident_at (pos_of_json jpos)))
    | [`String "seek"; `String "before"; jpos] ->
      Request (Seek (`Before (pos_of_json jpos)))
    | [`String "seek"; `String "exact"; jpos] ->
      Request (Seek (`Exact (pos_of_json jpos)))
    | [`String "seek"; `String "end"] ->
      Request (Seek `End)
    | (`String "boundary" :: `String "next" :: opt_pos) ->
      Request (Boundary (`Next, optional_position opt_pos))
    | (`String "boundary" :: `String "prev" :: opt_pos) ->
      Request (Boundary (`Prev, optional_position opt_pos))
    | (`String "boundary" :: `String "current" :: opt_pos)
    | (`String "boundary" :: opt_pos) ->
      Request (Boundary (`Current, optional_position opt_pos))
    | (`String "reset" :: `String kind :: opt_name) ->
      Request (Reset (ml_or_mli kind, optional_string opt_name))
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
      Request (Dump `Typer_input)
    | [`String "which"; `String "path"; `String name] ->
      Request (Which_path name)
    | [`String "which"; `String "with_ext"; `String ext] ->
      Request (Which_with_ext ext)
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
    | [`String "project"; `String ("load"|"find" as action); `String path] ->
      Request (Project_load (load_or_find action, path))
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
        | Complete_prefix _, compl_list ->
          `List (List.map json_of_completion compl_list)
        | Locate _, None ->
          `String "Not found"
        | Locate _, Some (None,pos) ->
          `Assoc ["pos",json_of_pos pos]
        | Locate _, Some (Some file,pos) ->
          `Assoc ["file",`String file; "pos",json_of_pos pos]
        | Outline, outlines ->
          `List (json_of_outline outlines)
        | Drop, cursor ->
          json_of_cursor_state cursor
        | Boundary _, Some {Location. loc_start; loc_end} ->
          `List (List.map json_of_pos [loc_start; loc_end])
        | Boundary _, None ->
          `Null
        | Reset _, cursor ->
          json_of_cursor_state cursor
        | Refresh, () -> `Bool true
        | Errors, exns ->
          `List (List.map (fun (_,err) -> json_of_error err)
                          (Error_report.of_exns exns))
        | Dump _, json -> json
        | Which_path _, str -> `String str
        | Which_with_ext _, strs -> json_of_string_list strs
        | Findlib_use _, failures ->
          `Assoc (with_package_failures ["result", `Bool true] failures)
        | Findlib_list, strs -> json_of_string_list strs
        | Extension_list _, strs -> json_of_string_list strs
        | Extension_set _, () -> `Bool true
        | Path _, () -> `Bool true
        | Path_list _, strs -> json_of_string_list strs
        | Path_reset, () -> `Bool true
        | Project_load _, (strs, failures) ->
          `Assoc (with_package_failures ["result", json_of_string_list strs] failures)
        | Occurences _, locations ->
          `List (List.map locations
                   ~f:(fun loc -> with_location loc []))
      end]

  let request_of_json = function
    | `List jsons -> request_of_json jsons
    | _ -> invalid_arguments ()
end

(* Used when dumping state as raw json *)
let with_location = Protocol_io.with_location

let lift (i,o : low_io) : io =
  (Stream.map ~f:Protocol_io.request_of_json i,
   (fun x -> o (Protocol_io.json_of_response x)))
