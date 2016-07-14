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
open Misc

type directive = [
  | `B of string
  | `S of string
  | `CMI of string
  | `CMT of string
  | `PKG of string list
  | `EXT of string list
  | `FLG of string
  | `STDLIB of string
  | `FINDLIB of string
  | `SUFFIX of string
  | `READER of string list
  | `FINDLIB_PATH of string
]

type file = {
  recurse    : bool;
  path       : string;
  directives : directive list;
}

module Cache = File_cache.Make (struct
    type t = file
    let read path =
      let ic = open_in path in
      let acc = ref [] in
      let recurse = ref false in
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
          else if String.is_prefixed ~by:"STDLIB " line then
            tell (`STDLIB (String.drop 7 line))
          else if String.is_prefixed ~by:"FINDLIB " line then
            tell (`FINDLIB (String.drop 8 line))
          else if String.is_prefixed ~by:"SUFFIX " line then
            tell (`SUFFIX (String.drop 7 line))
          else if String.is_prefixed ~by:"READER " line then
            tell (`READER (List.rev (rev_split_words (String.drop 7 line))))
          else if String.is_prefixed ~by:"FINDLIB_PATH " line then
            tell (`FINDLIB_PATH (String.drop 13 line))
          else if String.is_prefixed ~by:"#" line then
            ()
          else
            Logger.notify ".merlin" "%s: unexpected directive \"%s\"" path line;
          aux ()
        in
        aux ()
      with
      | End_of_file ->
        close_in_noerr ic;
        {recurse = !recurse; path; directives = List.rev !acc}
      | exn ->
        close_in_noerr ic;
        raise exn
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

let rec directives_of_file fname =
  match find fname with
  | None -> []
  | Some fname ->
    let file = Cache.read fname in
    file ::
    if file.recurse then
      let path = file.path in
      let dir = Filename.dirname @@ Filename.dirname @@ path in
      if dir <> path then
        directives_of_file dir
      else []
    else []

type config = {
  dot_merlins  : string list;
  build_path   : string list;
  source_path  : string list;
  cmi_path     : string list;
  cmt_path     : string list;
  packages     : string list;
  flags        : string list list;
  extensions   : string list;
  suffixes     : (string * string) list;
  stdlib       : string;
  findlib      : string option;
  reader       : string list;
  findlib_path : string list;
}

type t = {
  filenames: string list;
  mutable files: file list list;
  mutable config: config option;
}

let load filenames =
  Logger.logj "dot_merlin" "load filenames"
    (fun () -> `List (List.map (fun x -> `String x) filenames));
  let filenames = List.map ~f:canonicalize_filename filenames in
  {
    filenames;
    files = List.map ~f:directives_of_file filenames;
    config = None;
  }

let update t =
  Logger.logj "dot_merlin" "update filenames"
    (fun () -> `List (List.map (fun x -> `String x) t.filenames));
  let files' = List.map ~f:directives_of_file t.filenames in
  let for_all2 ~f la lb =
    List.length la = List.length lb &&
    List.for_all2 ~f la lb
  in
  let uptodate = for_all2 t.files files'
      ~f:(for_all2 ~f:(fun d d' -> d == d' || d = d'))
  in
  Logger.logj "dot_merlin" "update files"
    (fun () -> `List (List.map (fun x -> `String x.path) (List.flatten files')));
  t.files <- files';
  if not uptodate then
    t.config <- None

let empty_config = {
  build_path   = [];
  source_path  = [];
  cmi_path     = [];
  cmt_path     = [];
  packages     = [];
  dot_merlins  = [];
  extensions   = [];
  suffixes     = [(".ml", ".mli"); (".re", ".rei")];
  flags        = [];
  stdlib       = Config.standard_library;
  findlib      = None;
  reader       = [];
  findlib_path = [];
}

let merge c1 c2 = {
  build_path   = c1.build_path @ c2.build_path;
  source_path  = c1.source_path @ c2.source_path;
  cmi_path     = c1.cmi_path @ c2.cmi_path;
  cmt_path     = c1.cmt_path @ c2.cmt_path;
  packages     = c1.packages @ c2.packages;
  dot_merlins  = c1.dot_merlins @ c2.dot_merlins;
  extensions   = c1.extensions @ c2.extensions;
  suffixes     = c1.suffixes @ c2.suffixes;
  flags        = c1.flags @ c2.flags;
  stdlib       = if c1.stdlib = empty_config.stdlib then c2.stdlib else c1.stdlib;
  findlib      = if c1.findlib = None then c2.findlib else c1.findlib;
  reader       = if c1.reader = [] then c2.reader else c1.reader;
  findlib_path = c1.findlib_path @ c2.findlib_path;
}

let flg_regexp = Str.regexp "\\([^ \t\r\n']+\\|'[^']*'\\)"
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

let rev_split_flags str =
  let rec aux acc str i =
    match try Some (Str.search_forward flg_regexp str i) with Not_found -> None with
    | None -> acc
    | Some first_match ->
      let flag = Str.matched_string str in
      let to_skip = String.length flag in
      let flag =
        if not @@ String.is_prefixed flag ~by:"'" then
          flag
        else
          String.sub flag ~pos:1 ~len:(String.length flag - 2)
      in
      aux (flag :: acc) str (first_match + to_skip)
  in
  aux [] str 0

let prepend_config {path; directives} config =
  let cwd = Filename.dirname path in
  let expand config path acc =
    let filter name =
      try Sys.is_directory name
      with _ -> false
    in
    let path = expand_directory config.stdlib path in
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
      let flags = List.rev (rev_split_flags flags) in
      {config with flags = flags :: config.flags}
    | `STDLIB path ->
      {config with stdlib = canonicalize_filename path}
    | `FINDLIB path ->
      {config with findlib = Some (canonicalize_filename path)}
    | `READER reader ->
      {config with reader}
    | `FINDLIB_PATH path ->
      let canon_path = canonicalize_filename path in
      { config with findlib_path = canon_path :: config.findlib_path }
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
    findlib_path = clean config.findlib_path;
    stdlib      = config.stdlib;
    findlib     = config.findlib;
    reader      = config.reader;
  }

let config t = match t.config with
  | Some config -> config
  | None ->
    let config =
      List.fold_left t.files ~init:empty_config
        ~f:(fun config subfiles ->
            List.fold_left subfiles ~init:config
              ~f:(fun config file -> prepend_config file config))

    in
    let config = postprocess_config config in
    t.config <- Some config;
    config

let same = (==)

(* FIXME: Move elsewhere, processing of findlib packages*)

let ppx_of_package ?(predicates=[]) setup pkg =
  let d = Findlib.package_directory pkg in
  (* Determine the 'ppx' property: *)
  let in_words ~comma s =
    (* splits s in words separated by commas and/or whitespace *)
    let l = String.length s in
    let rec split i j =
      if j < l then
        match s.[j] with
        | (' '|'\t'|'\n'|'\r'|',' as c) when c <> ',' || comma ->
          if i<j then (String.sub s i (j-i)) :: (split (j+1) (j+1))
          else split (j+1) (j+1)
        |	_ ->
          split i (j+1)
      else
      if i<j then [ String.sub s i (j-i) ] else []
    in
    split 0 0
  in
  let resolve_path = Findlib.resolve_path ~base:d ~explicit:true in
  let ppx =
    try
      Some(resolve_path
             (Findlib.package_property predicates pkg "ppx"))
    with Not_found -> None
  and ppxopts =
    try
      List.map
        (fun opt ->
           match in_words ~comma:true opt with
           | pkg :: opts ->
             pkg, List.map resolve_path opts
           | _ -> assert false)
        (in_words ~comma:false
           (Findlib.package_property predicates pkg "ppxopt"))
    with Not_found -> []
  in
  begin match ppx with
    | None -> ()
    | Some ppx ->
      Logger.log ".merlin" "ppx" ppx
  end;
  begin match ppxopts with
    | [] -> ()
    | lst ->
      Logger.logj ".merlin" "ppx options" @@ fun () ->
      let f (ppx,opts) =
        `List [`String ppx; `List (List.map (fun s -> `String s) opts)]
      in
      `List (List.map ~f lst)
  end;
  let setup = match ppx with
    | None -> setup
    | Some ppx -> Ppxsetup.add_ppx ppx setup
  in
  List.fold_left ppxopts ~init:setup
    ~f:(fun setup (ppx,opts) -> Ppxsetup.add_ppxopts ppx opts setup)


let findlib_path = ref []

let path_separator =
  match Sys.os_type with
    | "Cygwin"
    | "Win32"  -> ";"
    | _ -> ":"

let path_of_packages config =
  if config.findlib_path <> !findlib_path then begin
      let env_ocamlpath =
        if config.findlib_path = [] then
          None
        else
          Some(String.concat path_separator config.findlib_path) in
      Findlib.init ?env_ocamlpath ();
      findlib_path := config.findlib_path
  end;
  let packages = config.packages in
  let f pkg =
    try Either.R (Findlib.package_deep_ancestors [] [pkg])
    with exn ->
      Logger.notify "findlib" "%s: %s" pkg (Printexc.to_string exn);
      Either.L (pkg, exn)
  in
  let packages = List.map ~f packages in
  let failures, packages = Either.split packages in
  let packages = List.filter_dup (List.concat packages) in
  let path = List.map ~f:Findlib.package_directory packages in
  let ppxs = List.fold_left ~f:ppx_of_package packages ~init:Ppxsetup.empty in
  `Failures failures, path, ppxs
