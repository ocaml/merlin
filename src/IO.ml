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

type io = Protocol.a_request Stream.t * (Protocol.response -> unit)
type low_io = Json.json Stream.t * (Json.json -> unit)
type io_maker = input:in_channel -> output:out_channel -> low_io

let section = Logger.(`protocol)

exception Protocol_failure of string

let invalid_arguments () = failwith "invalid arguments"

let stream_map s f =
  Stream.from (fun _ -> 
      try Some (f (Stream.next s)) 
      with Stream.Failure -> None)

let json_of_completion {Protocol. name; kind; desc; info} =
  let kind = match kind with
    | `Value       -> "Value"
    | `Constructor -> "Constructor"
    | `Label       -> "Label"
    | `Module      -> "Module"
    | `Modtype     -> "Signature"
    | `Type        -> "Type"
    | `MethodCall  -> "#"
  in
  `Assoc ["name", `String name;
          "kind", `String kind;
          "desc", `String desc;
          "info", `String info]

let json_log (input,output) =
  let log_input json = Logger.log section ~prefix:"<" (Json.to_string json); json in
  let log_output json = Logger.log section ~prefix:">" (Json.to_string json); json in
  let input' =
    Stream.from
    begin fun _ ->
      try Some (log_input (Stream.next input))
      with Stream.Failure -> None
    end
  in
  let output' json = output (log_output json) in
  input', output'

let json_make ~input ~output =
  let input  = Json.stream_from_channel input in
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

let return l = `List [`String "return" ; l]

let pos_to_json pos =
  Lexing.(`Assoc ["line", `Int pos.pos_lnum;
                  "col", `Int (pos.pos_cnum - pos.pos_bol)])

let error_to_json {Error_report. valid; text; where; loc} =
  let content = ["valid", `Bool valid; "message", `String text] in
  let content =
    if loc = Location.none then content else
    ("start", pos_to_json loc.Location.loc_start) ::
    ("end"  , pos_to_json loc.Location.loc_end) ::
    content
  in
  let content = ("type", `String where) :: content in
  `Assoc content

let error_catcher exn = 
  match Error_report.error_catcher exn with
  | None -> None
  | Some (loc,t) -> Some (loc, error_to_json t)

let fail = function
  | Protocol_failure s ->
    prerr_endline ("Fatal protocol failure. " ^ s);
    exit (-1)
  | Failure s -> `List [`String "failure"; `String s]
  | exn -> match error_catcher exn with
      | Some (_,error) -> `List [`String "error"; error]
      | None -> `List [`String "exception"; `String (Printexc.to_string exn)]

let protocol_failure s = raise (Protocol_failure s)

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
  `Assoc (("start", pos_to_json loc.Location.loc_start) ::
          ("end",   pos_to_json loc.Location.loc_end) ::
          assoc)

let optional_position = function
  | [`String "at"; jpos] -> Some (pos_of_json jpos)
  | [] -> None
  | _ -> invalid_arguments ()
let string_list l =
  List.map (function `String s -> s | _ -> invalid_arguments ()) l
let json_of_string_list l =
  `List (List.map (fun s -> `String s) l)
let json_of_type_loc (loc,str) =
  with_location loc ["type", `String str] 

let source_or_build = function
  | "source" -> `Source
  | "build"  -> `Build
  | _ -> invalid_arguments ()

let list_or_reset = function
  | "list"  -> `List
  | "reset" -> `Reset
  | _ -> invalid_arguments ()

let add_or_remove = function
  | "add"    -> `Add
  | "remove" -> `Rem
  | _ -> invalid_arguments ()

let load_or_find = function
  | "load" -> `File
  | "find" -> `Find
  | _ -> invalid_arguments ()

module Protocol_io = struct
  exception Failure' = Failure
  open Protocol

  let request_of_json = function
    | [`String "tell"; `String "struct"; `String source] ->
      Request (Tell (`Source source))
    | [`String "tell"; `String "end"; `String source] ->
      Request (Tell (`More source))
    | [`String "tell"; `String ("struct"|"end"); `Null] ->
      Request (Tell `End)
    | (`String "type" :: `String "expression" :: `String expr :: opt_pos) ->
      Request (Type_expr (expr, optional_position opt_pos))
    | [`String "type"; `String "at"; jpos] ->
      Request (Type_at (pos_of_json jpos))
    | [`String "type"; `String "enclosing"; jpos] ->
      Request (Type_enclosing (pos_of_json jpos))
    | (`String "complete" :: `String "prefix" :: `String prefix :: opt_pos) ->
      Request (Complete_prefix (prefix, optional_position opt_pos))
    | (`String "locate" :: `String path :: opt_pos) ->
      Request (Locate (path, optional_position opt_pos))
    | [`String "drop"] ->
      Request Drop
    | [`String "seek"; `String "position"] ->
      Request (Seek `Position)
    | [`String "seek"; `String "before"; jpos] ->
      Request (Seek (`Before (pos_of_json jpos)))
    | [`String "seek"; `String "exact"; jpos] ->
      Request (Seek (`Exact (pos_of_json jpos)))
    | [`String "seek"; `String "end"] ->
      Request (Seek `End)
    | [`String "seek"; `String "maximize_scope"] ->
      Request (Seek `Maximize_scope)
    | (`String "boundary" :: `String "next" :: opt_pos) ->
      Request (Boundary (`Next, optional_position opt_pos))
    | (`String "boundary" :: `String "prev" :: opt_pos) ->
      Request (Boundary (`Prev, optional_position opt_pos))
    | (`String "boundary" :: `String "current" :: opt_pos)
    | (`String "boundary" :: opt_pos) ->
      Request (Boundary (`Current, optional_position opt_pos))
    | [`String "reset"] ->
      Request (Reset None)
    | [`String "reset"; `String "name"; `String fname] ->
      Request (Reset (Some fname))
    | [`String "refresh"] ->
      Request (Refresh `Full)
    | [`String "refresh"; `String "quick"] ->
      Request (Refresh `Quick)
    | [`String "cd"; `String cd] ->
      Request (Cd cd)
    | [`String "errors"] ->
      Request Errors
    | (`String "dump" :: `String "env" :: opt_pos) ->
      Request (Dump (`Env (optional_position opt_pos)))
    | [`String "dump"; `String "sig"] ->
      Request (Dump `Sig)
    | [`String "dump"; `String "chunks"] ->
      Request (Dump `Chunks)
    | [`String "dump"; `String "tree"] ->
      Request (Dump `Tree)
    | [`String "dump"; `String "outline"] ->
      Request (Dump `Outline)
    | [`String "dump"; `String "exn"] ->
      Request (Dump `Exn)
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
    | [`String "path"; `String "reset";
                       `String ("source"|"build" as var)] ->
      Request (Path_reset (source_or_build var))
    | [`String "path"; `String "reset"] ->
      Request (Path_reset `Both)
    | (`String "path" :: `String ("add"|"remove" as action) ::
         `String ("source"|"build" as var) :: ((`List pathes :: []) | pathes)) ->
      Request (Path (source_or_build var, `Relative, 
                     add_or_remove action, string_list pathes))
    | (`String "path" :: `String "raw" :: `String ("add"|"remove" as action) ::
         `String ("source"|"build" as var) :: ((`List pathes :: []) | pathes)) ->
      Request (Path (source_or_build var, `Absolute,
                     add_or_remove action, string_list pathes))
    | [`String "project"; `String ("load"|"find" as action); `String path] ->
      Request (Project_load (load_or_find action, path))
    | _ -> invalid_arguments ()
  
  let response_to_json = function
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
        | Tell _, b -> `Bool b
        | Type_expr _, str -> `String str
        | Type_at _, loc_str -> json_of_type_loc loc_str
        | Type_enclosing _, (len,results) ->
          `List [`Int len; `List (List.map json_of_type_loc results)]
        | Complete_prefix _, compl_list -> 
          `List (List.map json_of_completion compl_list)
        | Locate _, None ->
          `String "Not found"
        | Locate _, Some (file,pos) ->
          `Assoc ["file",`String file; "pos",pos_to_json pos]
        | Drop, position -> pos_to_json position
        | Seek _, position -> pos_to_json position
        | Boundary _, Some {Location. loc_start; loc_end} ->
          `List (List.map pos_to_json [loc_start; loc_end])
        | Boundary _, None ->
          `Null
        | Reset _, () -> pos_to_json (make_pos (1,0)) 
        | Refresh _, changed -> `Bool changed
        | Cd _, () -> `Bool true
        | Errors, exns ->
          `List (List.map (fun (_,err) -> error_to_json err)
                          (Error_report.of_exns exns))
        | Dump _, json -> json
        | Which_path _, str -> `String str
        | Which_with_ext _, strs -> json_of_string_list strs
        | Findlib_use _, () -> `Bool true
        | Findlib_list, strs -> json_of_string_list strs
        | Extension_list _, strs -> json_of_string_list strs
        | Extension_set _, () -> `Bool true
        | Path _, changed -> `Bool changed
        | Path_list _, strs -> json_of_string_list strs
        | Path_reset _, () -> `Bool true
        | Project_load _, strs -> json_of_string_list strs
      end]
end

let request_of_json = function
  | `List jsons -> Protocol_io.request_of_json jsons
  | _ -> invalid_arguments ()
let response_to_json = Protocol_io.response_to_json

let lift (i,o : low_io) : io = 
  (stream_map i request_of_json, (fun x -> o (response_to_json x)))
