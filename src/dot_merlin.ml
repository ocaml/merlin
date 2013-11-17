open Std
open Misc

module Directives = struct
  type t = [
    | `B of string
    | `S of string
    | `PKG of string list
    | `EXT of string list
    | `FLG of string
  ]
end

type t = {
  project: string option;
  path: string;
  entries: Directives.t list;
}

let parse_dot_merlin path : bool * t =
  let ic = open_in path in
  let acc = ref [] in
  let recurse = ref false in
  let proj = ref None in
  let tell l = acc := l :: !acc in
  try
    let rec aux () =
      let line = input_line ic in
      if line = "" then ()
      else if String.is_prefixed ~by:"B " line then
        tell (`B (String.drop 2 line))
      else if String.is_prefixed ~by:"S " line then
        tell (`S (String.drop 2 line))
      else if String.is_prefixed ~by:"SRC " line then
        tell (`S (String.drop 4 line))
      else if String.is_prefixed ~by:"PKG " line then
        tell (`PKG (rev_split_words (String.drop 4 line)))
      else if String.is_prefixed ~by:"EXT " line then
        tell (`EXT (rev_split_words (String.drop 4 line)))
      else if String.is_prefixed ~by:"FLG " line then
        tell (`FLG (String.drop 4 line))
      else if String.is_prefixed ~by:"REC" line then recurse := true
      else if String.is_prefixed ~by:"PRJ " line then
        proj := Some (String.trim (String.drop 4 line))
      else if String.is_prefixed ~by:"PRJ" line then
        proj := Some ""
      else if String.is_prefixed ~by:"#" line then ()
      else ();
      aux ()
    in
    aux ()
  with
  | End_of_file ->
    close_in_noerr ic;
    !recurse, {project = !proj; path; entries = !acc}
  | exn ->
    close_in_noerr ic;
    raise exn

let rec read path =
  let recurse, dot_merlin = parse_dot_merlin path in
  List.Lazy.(Cons (dot_merlin,
                   if recurse
                   then lazy (find (Filename.dirname (Filename.dirname path)))
                   else lazy Nil))

and find path =
  let rec loop dir =
    let fname = Filename.concat dir ".merlin" in
    if Sys.file_exists fname
    then Some fname
    else
      let parent = Filename.dirname dir in
      if parent <> dir
      then loop parent
      else None
  in
  match loop (canonicalize_filename path) with
  | Some fname -> read fname
  | None -> List.Lazy.Nil

let rec project_name = function
  | List.Lazy.Cons (({project = Some ""; path = name} | {project = Some name}), _) ->
    Some name
  | List.Lazy.Cons ({path}, lazy List.Lazy.Nil) -> Some path
  | List.Lazy.Cons (_, lazy tail) -> project_name tail
  | List.Lazy.Nil -> None

type path_config =
  {
    dot_merlins : string list;
    build_path  : string list;
    source_path : string list;
    packages    : string list;
    flags       : string list list;
    extensions  : string list;
  }

let parse_dot_merlin {path; entries} config =
  let cwd = Filename.dirname path in
  let expand path =
    canonicalize_filename ~cwd (expand_directory Config.standard_library path)
  in
  List.fold_left ~init:{config with dot_merlins = path :: config.dot_merlins}
  ~f:(fun config ->
    function
    | `B path -> {config with build_path = expand path :: config.build_path}
    | `S path -> {config with source_path = expand path :: config.source_path}
    | `PKG pkgs -> {config with packages = pkgs @ config.packages}
    | `EXT exts ->
      {config with extensions = exts @ config.extensions}
    | `FLG flags ->
      let lst = rev_split_words flags in
      let flags = List.rev lst in
      {config with flags = flags :: config.flags}
  ) entries

let empty_config = {
  build_path  = [];
  source_path = [];
  packages    = [];
  dot_merlins = [];
  extensions  = [];
  flags       = [];
}

let rec parse ?(config=empty_config) =
  function
  | List.Lazy.Cons (dot_merlin, lazy tail) ->
    parse ~config:(parse_dot_merlin dot_merlin config) tail
  | List.Lazy.Nil ->
    { config with
      build_path  = List.rev (List.filter_dup config.build_path);
      source_path = List.rev (List.filter_dup config.source_path);
      packages    = List.rev (List.filter_dup config.packages);
      extensions  = List.rev (List.filter_dup config.extensions);
      flags       = List.rev (List.filter_dup config.flags);
    }

let packages_path packages =
  let packages = Findlib.package_deep_ancestors [] packages in
  let path = List.map ~f:Findlib.package_directory packages in
  List.filter_dup path
