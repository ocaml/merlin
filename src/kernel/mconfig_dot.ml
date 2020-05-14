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

let {Logger. log} = Logger.for_section "Mconfig_dot"

type directive = Dot_protocol.directive

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
  List.fold_left ~init:(config, []) ~f:(fun (config, errors) ->
    function
    | `B path -> {config with build_path = path :: config.build_path}, errors
    | `S path -> {config with source_path = path :: config.source_path}, errors
    | `CMI path -> {config with cmi_path = path :: config.cmi_path}, errors
    | `CMT path -> {config with cmt_path = path :: config.cmt_path}, errors
    | `EXT exts ->
      {config with extensions = exts @ config.extensions}, errors
    | `SUFFIX suffix ->
      {config with suffixes = (parse_suffix suffix) @ config.suffixes}, errors
    | `FLG flags ->
      let flags = {workdir = cwd; workval = Shell.split_command flags} in
      {config with flags = flags :: config.flags}, errors
    | `STDLIB path ->
      {config with stdlib = Some path}, errors
    | `READER reader ->
      {config with reader}, errors
    | `EXCLUDE_QUERY_DIR ->
      {config with exclude_query_dir = true}, errors
    | `ERROR_MSG str ->
      config, str :: errors
  ) directives

module Configurator = struct
  type t =
    | Dot_merlin
    | Dune

  let of_string_opt = function
    | ".merlin" ->
      Some Dot_merlin
    | "dune-project" | "dune" ->
      Some Dune
    | _ -> None

  let to_string = function
    | Dot_merlin -> "dot-merlin-reader"
    | Dune -> "dune"

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
          prog, [| prog |]
        | Dune ->
          let prog = "dune" in
          prog, [| prog; "ocaml-merlin" |]
      in
      log ~title:"get_config" "Using %s configuration provider." (to_string cfg);
      let cwd = Sys.getcwd () in
      let stdin_r, stdin_w = Unix.pipe () in
      let stdout_r, stdout_w = Unix.pipe () in
      let stderr_r, stderr_w = Unix.pipe () in
      Unix.chdir dir;
      let pid = Unix.create_process prog args stdin_r stdout_w stderr_w in
      (* let pid = Unix.create_process prog args stdin_r stdout_w (Unix.descr_of_out_channel stderr) in *)
      Unix.chdir cwd;
      Unix.close stdin_r;
      Unix.close stdout_w;
      Unix.close stderr_w;
      let stdin = Unix.out_channel_of_descr stdin_w in
      let stdout = Unix.in_channel_of_descr stdout_r in
      let stderr = Unix.in_channel_of_descr stderr_r in
      { pid; stdin; stdout; stderr }
  end

  let running_processes : (string * t, Process.t) Hashtbl.t = Hashtbl.create 0

  let get_process ~dir configurator =
    try
      let p = Hashtbl.find running_processes (dir, configurator) in
      let i, _ = Unix.waitpid [ WNOHANG ] p.pid in
      if i = 0 then
        p
      else
        let p = Process.start ~dir configurator in
        Hashtbl.replace running_processes (dir, configurator) p;
        p
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

type context = string * Configurator.t

let get_config (dir, cfg) path =
  try
    let p = Configurator.get_process ~dir cfg in
    Dot_protocol.Commands.send_file
      ~out_channel:p.stdin
      path;
    flush p.stdin;
    let directives = Dot_protocol.read ~in_channel:p.stdout in
    let cfg, failures = prepend_config ~dir directives empty_config in
    postprocess_config cfg, failures
  with End_of_file ->
    log ~title:"get_config"
      "%s process died when trying to retrieve the config for %s"
      (Configurator.to_string cfg) dir;
    empty_config, [ Printf.sprintf "couldn't retrieve the config from %s"
                      (Configurator.to_string cfg)]

let find_project_context start_dir =
  let rec loop dir =
    try
      Some (
        List.find_map [
            ".merlin" ; "dune" ; "jbuild"; "dune-project"
          ]
          ~f:(fun f ->
            let fname = Filename.concat dir f in
            if Sys.file_exists fname && not (Sys.is_directory fname)
            then Some ((dir, Option.get (Configurator.of_string_opt f)), fname)
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
