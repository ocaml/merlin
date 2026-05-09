(* This file is part of merlin.
   See the attached LICENSE file.
   Copyright (C) 2026 LexiFi

   Originally written by Nicolás Ojeda Bär (LexiFi);
   maintained by Martin Jambon (LexiFi). *)

(*
   Path management for iterating over the cmt files of a project.

   This is currently limited to Dune projects.
*)

open Std (* extends [String] with [chop_prefix] etc. *)

type t = {
  (* see mli for field documentation *)
  project_kind: Mconfig_dot.Configurator.t;
  dune_context: string option;
  search_root: string option;
  project_root: string;
  project_relative_search_root: string;
  build_source_root: string;
}

let ( / ) = Filename.concat

(* Compute the path of [abs_search_root] relative to [project_root].
   Both must be absolute and normalized (e.g. via [Unix.realpath]).
   Returns [""] when they are the same directory.
   TODO: use Fpath.relativize for a more reliable implementation
*)
let relative_to ~project_root abs_search_root =
  match String.chop_prefix ~prefix:project_root abs_search_root with
  | None ->
    (* find_project_context found the project by walking *up* from
       abs_search_root, so the prefix relation must hold. *)
    ""
  | Some "" -> ""
  | Some s when s.[0] = '/' -> String.sub s ~pos:1 ~len:(String.length s - 1)
  | Some s -> s

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

   Example for a project built with dune:
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

(* Project location is delegated to [Mconfig_dot.find_project_context]
   so the rule for "what counts as a project root" stays consistent
   with the rest of merlin: walk up looking for a [.merlin],
   [dune-project], or [dune-workspace] file.

   The [Mconfig_dot.context] is abstract in the .mli, so we read what
   we need from the second return value, which is the path of the
   config file: its directory is the project root, its basename tells
   us whether the configurator is dune or .merlin.

   We also set up [Load_path] with the project's cmi directories.
   These two steps used to be split between [find] and [init], but
   callers always need both, so we expose a single entry point. *)
let init
    ?(dune_context = "default")
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
         "Could not find a project rooted at or above %s. \
          Looks for a dune-project, dune-workspace, or .merlin file."
         abs_search_root)
  | Some (_ctx, config_file) ->
    let project_root = Filename.dirname config_file in
    match Filename.basename config_file with
    | ".merlin" ->
        Error
          (Printf.sprintf
             "cmt scanning currently only supports Dune projects, but the \
              project at %s was located via a .merlin file. Support for \
              other build systems is a TODO."
             project_root)
    | "dune-project"
    | "dune-workspace" ->
        let build_source_root = project_root / "_build" / dune_context in
        let project_relative_search_root =
          relative_to ~project_root abs_search_root
        in
        let paths = {
          project_kind = Dune;
          dune_context = Some dune_context;
          search_root;
          project_root;
          project_relative_search_root;
          build_source_root
        }
        in
        let extra_includes = collect_cmi_dirs paths in
        Load_path.init
          ~auto_include:Load_path.no_auto_include
          ~visible:(List.append extra_includes [ Standard_library.path ])
          ~hidden:[];
        Ok paths
    | _ ->
        failwith ("merlin internal error: unexpected project root file: "
                  ^ config_file)
