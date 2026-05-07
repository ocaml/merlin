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

open Std (* extends [String] with [chop_prefix] etc. *)

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

(* Compute the path of [abs_search_root] relative to [project_root].
   Both must be absolute and normalized (e.g. via [Unix.realpath]).
   Returns [""] when they are the same directory. *)
let relative_to ~project_root abs_search_root =
  match String.chop_prefix ~prefix:project_root abs_search_root with
  | None ->
    (* find_project_context found the project by walking *up* from
       abs_search_root, so the prefix relation must hold. *)
    ""
  | Some "" -> ""
  | Some s when s.[0] = '/' -> String.sub s ~pos:1 ~len:(String.length s - 1)
  | Some s -> s

(* Project location is delegated to [Mconfig_dot.find_project_context]
   so the rule for "what counts as a project root" stays consistent
   with the rest of merlin: walk up looking for a [.merlin],
   [dune-project], or [dune-workspace] file.

   The [Mconfig_dot.context] is abstract in the .mli, so we read what
   we need from the second return value, which is the path of the
   config file: its directory is the project root, its basename tells
   us whether the configurator is dune or .merlin. *)
let find
    ?(context = "default")
    ?search_root
    () : (t, string) result =
  let abs_search_root =
    Option.value ~default:Filename.current_dir_name (* . *) search_root
    |> Unix.realpath
  in
  match Mconfig_dot.find_project_context abs_search_root with
  | None ->
    Error
      (Printf.sprintf
         "Could not find a project rooted at or above %s. ocamlgrep \
          looks for a dune-project, dune-workspace, or .merlin file."
         abs_search_root)
  | Some (_ctx, config_file) ->
    let project_root = Filename.dirname config_file in
    let basename = Filename.basename config_file in
    if basename = ".merlin" then
      Error
        (Printf.sprintf
           "ocamlgrep currently only supports Dune projects, but the \
            project at %s was located via a .merlin file. Support for \
            other build systems is a TODO."
           project_root)
    else
      let build_source_root = project_root / "_build" / context in
      let project_relative_search_root =
        relative_to ~project_root abs_search_root
      in
      Ok
        { search_root;
          project_root;
          project_relative_search_root;
          build_source_root
        }

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

