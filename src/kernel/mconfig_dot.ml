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

open Misc
open Std

(* let {Logger. log} = Logger.for_section "Mconfig_dot" *)

type directive = [
  | `B of string
  | `S of string
  | `CMI of string
  | `CMT of string
  | `EXT of string list
  | `FLG of string
  | `STDLIB of string
  | `SUFFIX of string
  | `READER of string list
  | `EXCLUDE_QUERY_DIR
]

type config = {
  build_path   : string list;
  source_path  : string list;
  cmi_path     : string list;
  cmt_path     : string list;
  flags        : string list with_workdir list;
  extensions   : string list;
  suffixes     : (string * string) list;
  stdlib       : string option;
  reader       : string list;
  exclude_query_dir : bool;
}

let empty_config = {
  build_path   = [];
  source_path  = [];
  cmi_path     = [];
  cmt_path     = [];
  extensions   = [];
  suffixes     = [];
  flags        = [];
  stdlib       = None;
  reader       = [];
  exclude_query_dir = false;
}

let white_regexp = Str.regexp "[ \t]+"

(* Parses suffixes pairs that were supplied as whitespace separated pairs
   designating implementation/interface suffixes. These would be supplied in
   the .merlin file as:

   SUFFIX .sfx .sfxi   *)
let parse_suffix str =
  let trimmed = String.trim str in
  let split_on_white = Str.split white_regexp trimmed in
  if List.length split_on_white != 2 then []
  else
    let (first, second) = (List.nth split_on_white 0, List.nth split_on_white 1) in
    if String.get first 0 != '.' || String.get second 0 != '.' then []
    else [(first, second)]

let prepend_config ~dir:cwd (directives : directive list) config =
  List.fold_left ~init:config ~f:(fun config ->
    function
    | `B path -> {config with build_path = path :: config.build_path}
    | `S path -> {config with source_path = path :: config.source_path}
    | `CMI path -> {config with cmi_path = path :: config.cmi_path}
    | `CMT path -> {config with cmt_path = path :: config.cmt_path}
    | `EXT exts ->
      {config with extensions = exts @ config.extensions}
    | `SUFFIX suffix ->
      {config with suffixes = (parse_suffix suffix) @ config.suffixes}
    | `FLG flags ->
      let flags = {workdir = cwd; workval = Shell.split_command flags} in
      {config with flags = flags :: config.flags}
    | `STDLIB path ->
      {config with stdlib = Some path}
    | `READER reader ->
      {config with reader}
    | `EXCLUDE_QUERY_DIR ->
      {config with exclude_query_dir = true}
  ) directives

module Configurator = struct
  type t =
    | Dot_merlin

  let of_string_opt = function
    | ".merlin" ->
      Some Dot_merlin
    | _ -> None

  module Process = struct
    type t = {
      pid : int;
      stdin: out_channel;
      stdout: in_channel;
      stderr: in_channel;
    }

    let start ~dir cfg =
      let prog, args =
        match cfg with
        | Dot_merlin ->
          let prog = "dot-merlin-reader" in
          prog, [| prog; "--errors-on-stderr" |]
      in
      let cwd = Sys.getcwd () in
      let stdin_r, stdin_w = Unix.pipe () in
      let stdout_r, stdout_w = Unix.pipe () in
      let stderr_r, stderr_w = Unix.pipe () in
      Unix.chdir dir;
      let pid = Unix.create_process prog args stdin_r stdout_w stderr_w in
      Unix.chdir cwd;
      let stdin = Unix.out_channel_of_descr stdin_w in
      let stdout = Unix.in_channel_of_descr stdout_r in
      let stderr = Unix.in_channel_of_descr stderr_r in
      { pid; stdin; stdout; stderr }
  end

  let running_processes : (string * t, Process.t) Hashtbl.t = Hashtbl.create 0

  let get_process ~dir configurator =
    try Hashtbl.find running_processes (dir, configurator)
    with Not_found ->
      let p = Process.start ~dir configurator in
      Hashtbl.add running_processes (dir, configurator) p;
      p
end

let postprocess_config config =
  let clean list = List.rev (List.filter_dup list) in
  {
    build_path   = clean config.build_path;
    source_path  = clean config.source_path;
    cmi_path     = clean config.cmi_path;
    cmt_path     = clean config.cmt_path;
    extensions   = clean config.extensions;
    suffixes     = clean config.suffixes;
    flags        = clean config.flags;
    stdlib      = config.stdlib;
    reader      = config.reader;
    exclude_query_dir = config.exclude_query_dir;
  }

let read_cfg ic =
  let rec aux acc =
    let line = String.trim (input_line ic) in
    if line = "" then
      List.rev acc
    else if String.is_prefixed ~by:"B " line then
      aux @@ (`B (String.drop 2 line)) :: acc
    else if String.is_prefixed ~by:"S " line then
      aux @@ (`S (String.drop 2 line)) :: acc
    else if String.is_prefixed ~by:"SRC " line then
      aux @@ (`S (String.drop 4 line)) :: acc
    else if String.is_prefixed ~by:"CMI " line then
      aux @@ (`CMI (String.drop 4 line)) :: acc
    else if String.is_prefixed ~by:"CMT " line then
      aux @@ (`CMT (String.drop 4 line)) :: acc
    else if String.is_prefixed ~by:"EXT " line then
      aux @@ (`EXT (rev_split_words (String.drop 4 line))) :: acc
    else if String.is_prefixed ~by:"FLG " line then
      aux @@ (`FLG (String.drop 4 line)) :: acc
    else if String.is_prefixed ~by:"STDLIB " line then
      aux @@ (`STDLIB (String.drop 7 line)) :: acc
    else if String.is_prefixed ~by:"SUFFIX " line then
      aux @@ (`SUFFIX (String.drop 7 line)) :: acc
    else if String.is_prefixed ~by:"READER " line then
      aux @@ (`READER (List.rev (rev_split_words (String.drop 7 line)))) :: acc
    else if String.is_prefixed ~by:"EXCLUDE_QUERY_DIR" line then
      aux @@ `EXCLUDE_QUERY_DIR :: acc
    else (
      Logger.notify ~section:"build config" "unexpected directive \"%s\"" line;
      aux @@ acc
    )
  in
  (* TODO: error handling. *)
  aux []

type context = string * Configurator.t

let get_config (dir, cfg) path =
  let p = Configurator.get_process ~dir cfg in
  (* TODO: ensure [path] is absolute, or that it is relative to dir, and not the
     cwd. *)
  output_string p.stdin (path ^ "\n");
  let directives = read_cfg p.stdout in
  postprocess_config (prepend_config ~dir directives empty_config)

let find_project_context start_dir =
  let rec loop dir =
    try
      Some (
        List.find_map [ ".merlin"; (* dune-project, jbuild, ... *)] ~f:(fun f ->
          let fname = Filename.concat dir f in
          if Sys.file_exists fname && not (Sys.is_directory fname)
          then Some (dir, Option.get (Configurator.of_string_opt f))
          else None
        )
    )
    with Not_found ->
      let parent = Filename.dirname dir in
      if parent <> dir
      then loop parent
      else None
  in
  loop start_dir
