open Std

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

type dot_merlins =
  | Cons of t * dot_merlins Lazy.t
  | Nil

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
      else if Misc.has_prefix "B " line then
        tell (`B (Misc.string_drop 2 line))
      else if Misc.has_prefix "S " line then
        tell (`S (Misc.string_drop 2 line))
      else if Misc.has_prefix "SRC " line then
        tell (`S (Misc.string_drop 4 line))
      else if Misc.has_prefix "PKG " line then
        tell (`PKG (Misc.rev_split_words (Misc.string_drop 4 line)))
      else if Misc.has_prefix "EXT " line then
        tell (`EXT (Misc.rev_split_words (Misc.string_drop 4 line)))
      else if Misc.has_prefix "FLG " line then
        tell (`FLG (Misc.string_drop 4 line))
      else if Misc.has_prefix "REC" line then recurse := true
      else if Misc.has_prefix "PRJ " line then
        proj := Some (String.trim (Misc.string_drop 4 line))
      else if Misc.has_prefix "PRJ" line then
        proj := Some ""
      else if Misc.has_prefix "#" line then ()
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
  if recurse
  then Cons (dot_merlin, lazy (find (Filename.dirname (Filename.dirname path))))
  else Cons (dot_merlin, lazy Nil)

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
  match loop (Misc.canonicalize_filename path) with
  | Some fname -> read fname
  | None -> Nil 

let rec project_name = function
  | Cons (({project = Some ""; path = name} | {project = Some name}), _) ->
    Some name
  | Cons ({path}, lazy Nil) -> Some path
  | Cons (_, lazy tail) -> project_name tail
  | Nil -> None

let err_log msg = Logger.error `dot_merlin msg

module Flags = Top_options.Make (struct
  let _projectfind _ = err_log "unsupported flag \"-project-find\" (ignored)" ;
end)

let exec_dot_merlin ~path_modify ~load_packages {path; project; entries} =
  let cwd = Filename.dirname path in
  List.iter entries ~f:(
    function
    | `B path   -> path_modify `Add `Build ~cwd path
    | `S path   -> path_modify `Add `Source ~cwd path
    | `PKG pkgs -> load_packages pkgs
    | `EXT exts ->
      List.iter exts ~f:(fun e -> Extensions_utils.set_extension ~enabled:true e)
    | `FLG flags ->
      let lst = Misc.rev_split_words flags in
      let flags = Array.of_list (List.rev lst) in
      begin try
        Arg.parse_argv ~current:(ref (-1)) flags Flags.list
          Top_options.unexpected_argument "error..."
      with
      | Arg.Bad msg -> err_log msg; exit 2
      | Arg.Help msg -> err_log msg; exit 0 (* FIXME *)
      end
  ) ;
  path

let rec exec ~path_modify ~load_packages= function
  | Cons (dot_merlin, tail) ->
    exec_dot_merlin ~path_modify ~load_packages dot_merlin :: 
      exec ~path_modify ~load_packages (Lazy.force tail)
  | Nil -> []

