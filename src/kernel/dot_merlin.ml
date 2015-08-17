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
open Misc

let section = Logger.section "dot_merlin"

type directive = [
  | `B of string
  | `S of string
  | `CMI of string
  | `CMT of string
  | `PKG of string list
  | `EXT of string list
  | `FLG of string
  | `STDLIB of string
]

type file = {
  project    : string option;
  path       : string;
  directives : directive list;
}

let parse_dot_merlin_file path : bool * file =
  let ic = open_in path in
  let acc = ref [] in
  let recurse = ref false in
  let proj = ref None in
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
      else if String.is_prefixed ~by:"PRJ " line then
        proj := Some (String.trim (String.drop 4 line))
      else if String.is_prefixed ~by:"PRJ" line then
        proj := Some ""
      else if String.is_prefixed ~by:"#" line then
        ()
      else if String.is_prefixed ~by:"STDLIB " line then
        tell (`STDLIB (String.drop 7 line))
      else if String.is_prefixed ~by:"STDLIB " line then
        tell (`STDLIB (String.drop 9 line))
      else
        Logger.info Logger.Section.project_load ~title:".merlin"
          (sprintf "unexpected directive \"%s\"" line) ;
      aux ()
    in
    aux ()
  with
  | End_of_file ->
    close_in_noerr ic;
    !recurse, {project = !proj; path; directives = List.rev !acc}
  | exn ->
    close_in_noerr ic;
    raise exn

let rec find dir =
  let fname = Filename.concat dir ".merlin" in
  if Sys.file_exists fname && not (Sys.is_directory fname)
  then Some fname
  else
    let parent = Filename.dirname dir in
    if parent <> dir
    then find parent
    else None

let find fname =
  if Sys.file_exists fname && not (Sys.is_directory fname) then
    Some fname
  else find fname

let rec read tail path =
  let recurse, dot_merlin = parse_dot_merlin_file path in
  let next = if recurse
    then lazy (find_next tail (Filename.dirname (Filename.dirname path)))
    else tail
  in
  List.Lazy.Cons (dot_merlin, next)

and find_next tail path =
  match find path with
  | Some path -> read tail path
  | None -> Lazy.force tail

let find path = find (canonicalize_filename path)
let read ?(tail=lazy List.Lazy.Nil) path = read tail (canonicalize_filename path)

let rec project_name = function
  | List.Lazy.Cons (({project = Some ""; path = name} | {project = Some name}), _) ->
    Some name
  | List.Lazy.Cons ({path}, lazy List.Lazy.Nil) -> Some path
  | List.Lazy.Cons (_, lazy tail) -> project_name tail
  | List.Lazy.Nil -> None

type config =
  {
    dot_merlins : string list;
    build_path  : string list;
    source_path : string list;
    cmi_path    : string list;
    cmt_path    : string list;
    packages    : string list;
    flags       : string list list;
    extensions  : string list;
    stdlib      : string;
  }

let flg_regexp = Str.regexp "\\([^ \t\r\n']+\\|'[^']*'\\)"
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

let parse_dot_merlin {path; directives} config =
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
    | `FLG flags ->
      let flags = List.rev (rev_split_flags flags) in
      {config with flags = flags :: config.flags}
    | `STDLIB path ->
      {config with stdlib = canonicalize_filename path}
  ) directives

let empty_config = {
  build_path  = [];
  source_path = [];
  cmi_path    = [];
  cmt_path    = [];
  packages    = [];
  dot_merlins = [];
  extensions  = [];
  flags       = [];
  stdlib      = Config.standard_library
}

let rec parse ?(config=empty_config) =
  function
  | List.Lazy.Cons (dot_merlin, lazy tail) ->
    parse ~config:(parse_dot_merlin dot_merlin config) tail
  | List.Lazy.Nil ->
    {
      dot_merlins = config.dot_merlins;
      build_path  = List.rev (List.filter_dup config.build_path);
      source_path = List.rev (List.filter_dup config.source_path);
      cmi_path    = List.rev (List.filter_dup config.cmi_path);
      cmt_path    = List.rev (List.filter_dup config.cmt_path);
      packages    = List.rev (List.filter_dup config.packages);
      extensions  = List.rev (List.filter_dup config.extensions);
      flags       = List.rev (List.filter_dup config.flags);
      stdlib      = config.stdlib
    }

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
    | Some ppx -> Logger.info section ~title:"ppx" ppx
  end;
  begin match ppxopts with
    | [] -> ()
    | lst -> Logger.infojf section ~title:"ppxopts"
               (fun lst -> `List (List.map (fun (ppx,opts) ->
                    `List [`String ppx; `List (List.map (fun s -> `String s)
                                                 opts)]) lst)) lst
  end;
  let setup = match ppx with
    | None -> setup
    | Some ppx -> Ppxsetup.add_ppx ppx setup
  in
  List.fold_left ppxopts ~init:setup
    ~f:(fun setup (ppx,opts) -> Ppxsetup.add_ppxopts ppx opts setup)

let path_of_packages packages =
  let packages =  packages in
  let f pkg =
    try Either.R (Findlib.package_deep_ancestors [] [pkg])
    with exn ->
      Logger.infof Logger.Section.project_load ~title:"findlib"
        (fun fmt (exn, pkg) -> Format.fprintf fmt "%s: %s" pkg (Printexc.to_string exn))
        (exn, pkg) ;
      Either.L (pkg, exn)
  in
  let packages = List.map ~f packages in
  let failures, packages = Either.split packages in
  let packages = List.filter_dup (List.concat packages) in
  let path = List.map ~f:Findlib.package_directory packages in
  let ppxs = List.fold_left ~f:ppx_of_package packages ~init:Ppxsetup.empty in
  let failures = match failures with
    | [] -> `Ok
    | ls -> `Failures ls
  in
  failures, path, ppxs
