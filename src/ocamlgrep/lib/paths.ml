(* This file is part of the ocamlgrep package
   See the attached LICENSE file.
   Copyright (C) 2026 LexiFi

   Originally written by Nicolás Ojeda Bär (LexiFi);
   maintained by Martin Jambon (LexiFi). *)

(*
   Path management for ocamlgrep: where to look for the files we need

   We provide utilities that make assumptions about where the build system
   keeps its files.
*)

type dune_project_info = {
  search_root: string option;
    (* The scan root as specified by the user. It can be absolute or relative.

       If unspecified, the current directory (.) is assumed.
       If specified, this path must be a prefix of every file in search
       results. For example, specifying the search root as foo/../bar should
       return results such as foo/../bar/baz.ml, not an absolute path,
       not a normalized path (bar/baz.ml).

       It's also the starting point for locating
       which Dune project we're in and for locating the compiled data
       we need to run a pattern search.
    *)
  project_root: string;
    (* Folder containing source files *)
  project_relative_search_root: string;
    (* search_root made relative to project_root *)
  build_source_root: string;
    (* Path equivalent to project_root but for the copy of source files
       in Dune's build workspace.
       For example, <project root>/src/foo.ml exists as a copy in
       <project_root>/_build/default/src/foo.ml.
       In this case, build_source_root = <project_root>/_build/default
    *)
}

(* TODO: add support for other build systems that Dune? *)

(* TODO: project enumeration via the build system rather than walking
   [_build/].

   The pattern in the merlin tree (see ocaml-index) is to let the build
   system enumerate cmt files and pass them in, instead of discovering
   them ourselves. Dune publishes cmts via aliases like [@ocaml-index];
   ocamlgrep could consume an explicit cmt-file list (or read an
   existing .ocaml-index file) and stop walking [_build/] altogether.
   That would also remove the hard-coded Dune-only assumption below. *)
type t = dune_project_info

let ( / ) = Filename.concat

let search_root x = x.search_root
let project_root x = x.project_root
let project_relative_search_root x = x.project_relative_search_root

(*
   This looks for a folder containing '_build/', starting from the search root
   (typically cwd) and then by recursively checking its parents.

   TODO: use Dune's algorithm instead?
   The Dune docs cover this well:
   https://dune.readthedocs.io/en/latest/usage.html#finding-root
*)
let identify_dune_project
    ?(context = "default")
    ?search_root
    () : (t, string) result =
  let rec loop prefix abs_dir =
    (* abs_dir: search_root or one of its ancestors
       prefix: path from dir to search_root s.t. search_root = dir / prefix *)
    if Sys.file_exists (Filename.concat abs_dir "_build") then
      let project_root = abs_dir in
      let build_source_root = project_root / "_build" / context in
      Ok {
        search_root;
        project_root;
        project_relative_search_root = prefix;
        build_source_root;
      }
    else
      let dir' = Filename.dirname abs_dir in
      if dir' = abs_dir then
        (* reached "/" *)
        failwith "Could not detect _build";
      loop (Filename.concat (Filename.basename abs_dir) prefix) dir'
  in
  let abs_search_root =
    Option.value ~default:Filename.current_dir_name (* . *) search_root
    |> Unix.realpath
  in
  loop "" abs_search_root

(* foo/bar -> <root>/_build/default/foo/bar

   where "default" is the context name.
*)
let in_build_dir paths source_path =
  Filename.concat paths.build_source_root source_path

(*
   Collect folders named 'byte' that contain cmi files:
     shopt -s globstar
     shopt -s dotglob
     dirname **/byte/*.cmi

   Example for this project which is built with dune:
   $ ls _build/default/lib/.ocamlgrep.objs/byte/
   ocamlgrep.cmi  ocamlgrep.cmo  ocamlgrep.cmt  ocamlgrep.cmti

   This function is Dune-specific.
*)
let collect_cmi_dirs paths =
  let res = ref [] in
  let rec walk dir =
    Array.iter (fun entry ->
        let entry = Filename.concat dir entry in
        if Sys.is_directory entry then begin
          if Filename.basename entry = "byte"
          && Array.exists
               (fun name -> Filename.check_suffix name ".cmi")
               (Sys.readdir entry)
          then
            res := entry :: !res;
          walk entry
        end
      ) (Sys.readdir dir)
  in
  walk paths.build_source_root;
  List.rev !res

let init paths =
  let extra_includes = collect_cmi_dirs paths in
  Load_path.init
    ~auto_include:Load_path.no_auto_include
    ~visible:(List.append extra_includes [Standard_library.path])
    ~hidden:[]
