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
  dot_merlins  : string list;
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
  dot_merlins  = [];
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

let prepend_config ~stdlib path (directives : directive list) config =
  let cwd = Filename.dirname path in
  let expand config path acc =
    let filter path =
      let name = Filename.basename path in
      name <> "" && name.[0] <> '.' &&
      try Sys.is_directory path
      with _ -> false
    in
    let path =
      expand_directory (Option.value ~default:stdlib config.stdlib) path in
    let path = canonicalize_filename ~cwd path in
    expand_glob ~filter path acc
  in
  List.fold_left ~init:{config with dot_merlins = path :: config.dot_merlins}
  ~f:(fun config ->
    function
    | `B path -> {config with build_path = expand config path config.build_path}
    | `S path -> {config with source_path = expand config path config.source_path}
    | `CMI path -> {config with cmi_path = expand config path config.cmi_path}
    | `CMT path -> {config with cmt_path = expand config path config.cmt_path}
    | `EXT exts ->
      {config with extensions = exts @ config.extensions}
    | `SUFFIX suffix ->
      {config with suffixes = (parse_suffix suffix) @ config.suffixes}
    | `FLG flags ->
      let flags = {workdir = cwd; workval = Shell.split_command flags} in
      {config with flags = flags :: config.flags}
    | `STDLIB path ->
      {config with stdlib = Some (canonicalize_filename ~cwd path)}
    | `READER reader ->
      {config with reader}
    | `EXCLUDE_QUERY_DIR ->
      {config with exclude_query_dir = true}
  ) directives

let postprocess_config config =
  let clean list = List.rev (List.filter_dup list) in
  {
    dot_merlins  = config.dot_merlins;
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

let load ~stdlib = function
  | [] -> failwith "TODO"
  | path :: _ ->
    ignore (postprocess_config (prepend_config ~stdlib path [] empty_config));
    failwith "TODO"

let find_project_config start_dir =
  let rec loop dir =
    let fname = Filename.concat dir ".merlin" in
    if Sys.file_exists fname && not (Sys.is_directory fname)
    then Some fname
    else
      let parent = Filename.dirname dir in
      if parent <> dir
      then loop parent
      else None
  in
  loop start_dir
