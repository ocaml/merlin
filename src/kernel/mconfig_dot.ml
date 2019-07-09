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

type directive = [
  | `B of string
  | `S of string
  | `CMI of string
  | `CMT of string
  | `PKG of string list
  | `EXT of string list
  | `FLG of string
  | `STDLIB of string
  | `SUFFIX of string
  | `READER of string list
  | `EXCLUDE_QUERY_DIR
]

type file = {
  recurse    : bool;
  includes   : string list;
  path       : string;
  directives : directive list;
}

module Cache = File_cache.Make (struct
    type t = file
    let read path =
      let ic = open_in path in
      let acc = ref [] in
      let recurse = ref false in
      let includes = ref [] in
      let tell l = acc := l :: !acc in
      try
        let rec aux () =
          let line = String.trim (input_line ic) in
          if line = "" then ()

          else if String.is_prefixed ~by:"B " line then
            tell (`B (String.drop 2 line))
          else if String.is_prefixed ~by:"S " line then
            tell (`S (String.drop 2 line))
          else if String.is_prefixed ~by:"SRC " line then
            tell (`S (String.drop 4 line))
          else if String.is_prefixed ~by:"CMI " line then
            tell (`CMI (String.drop 4 line))
          else if String.is_prefixed ~by:"CMT " line then
            tell (`CMT (String.drop 4 line))
          else if String.is_prefixed ~by:"PKG " line then
            tell (`PKG (rev_split_words (String.drop 4 line)))
          else if String.is_prefixed ~by:"EXT " line then
            tell (`EXT (rev_split_words (String.drop 4 line)))
          else if String.is_prefixed ~by:"FLG " line then
            tell (`FLG (String.drop 4 line))
          else if String.is_prefixed ~by:"REC" line then
            recurse := true
          else if String.is_prefixed ~by:". " line then
            includes := String.trim (String.drop 2 line) :: !includes
          else if String.is_prefixed ~by:"STDLIB " line then
            tell (`STDLIB (String.drop 7 line))
          else if String.is_prefixed ~by:"SUFFIX " line then
            tell (`SUFFIX (String.drop 7 line))
          else if String.is_prefixed ~by:"READER " line then
            tell (`READER (List.rev (rev_split_words (String.drop 7 line))))
          else if String.is_prefixed ~by:"EXCLUDE_QUERY_DIR" line then
            tell `EXCLUDE_QUERY_DIR
          else if String.is_prefixed ~by:"#" line then
            ()
          else
            Logger.notify ~section:".merlin"
              "%s: unexpected directive \"%s\"" path line;
          aux ()
        in
        aux ()
      with
      | End_of_file ->
        close_in_noerr ic;
        let recurse = !recurse and includes = !includes in
        {recurse; includes; path; directives = List.rev !acc}
      | exn ->
        close_in_noerr ic;
        raise exn

    let cache_name = "Mconfig_dot"
  end)

let find fname =
  if Sys.file_exists fname && not (Sys.is_directory fname) then
    Some fname
  else
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
    loop fname

let directives_of_files filenames =
  let marked = Hashtbl.create 7 in
  let rec process acc = function
    | x :: rest when Hashtbl.mem marked x ->
      process acc rest
    | x :: rest ->
      Hashtbl.add marked x ();
      let file = Cache.read x in
      let dir = Filename.dirname file.path in
      let rest =
        List.map ~f:(canonicalize_filename ~cwd:dir) file.includes @ rest
      in
      let rest =
        if file.recurse then (
          let dir =
            if Filename.basename file.path <> ".merlin"
            then dir else Filename.dirname dir
          in
          if dir <> file.path then
            match find dir with
            | Some fname -> fname :: rest
            | None -> rest
          else rest
        ) else rest
      in
      process (file :: acc) rest
    | [] -> List.rev acc
  in
  process [] filenames

type config = {
  dot_merlins  : string list;
  build_path   : string list;
  source_path  : string list;
  cmi_path     : string list;
  cmt_path     : string list;
  packages     : string list;
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
  packages     = [];
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

let prepend_config ~stdlib {path; directives; _} config =
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
    | `PKG pkgs -> {config with packages = pkgs @ config.packages}
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
    packages     = clean config.packages;
    extensions   = clean config.extensions;
    suffixes     = clean config.suffixes;
    flags        = clean config.flags;
    stdlib      = config.stdlib;
    reader      = config.reader;
    exclude_query_dir = config.exclude_query_dir;
  }


let load ~stdlib filenames =
  let filenames = List.map ~f:canonicalize_filename filenames in
  let filenames = List.filter_map ~f:find filenames in
  let directives = directives_of_files filenames in
  let config =
    List.fold_left directives ~init:empty_config
      ~f:(fun config file -> prepend_config ~stdlib file config)
  in
  postprocess_config config

let standard_library () =
  "%%STDLIB%%"
(*   Findlib.ocaml_stdlib () *)
