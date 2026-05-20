(**
   Path management for iterating over the cmt files of a project.

   For now, we only support Dune, keeping in mind that we might want
   to support other build systems later.
*)

(** An abstract representation of what's needed to locate project files.

    We only expose the fields that are independent from the build system
    such as [search_root].

    This interface is internal to Merlin and subject to change.
*)
type t = private {
  project_kind: Mconfig_dot.Configurator.t;
  dune_context: string option;
    (** Dune's build context. It defaults to Dune's default which is
        named [default]. Relevant to Dune projects only. *)
  search_root: string option;
    (** The scan root as specified by the user. It can be absolute or relative.

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
    (** Folder containing source files *)
  project_relative_search_root: string;
    (** [search_root] made relative to [project_root] *)
  build_source_root: string;
    (** Path equivalent to [project_root] but for the copy of source files
       in Dune's build workspace.
       For example, [<project root>/src/foo.ml] exists as a copy in
       [<project_root>/_build/default/src/foo.ml].
       In this case, build_source_root = <project_root>/_build/default
    *)
}

(** [init ()] locates the OCaml project containing [search_root],
    derives the dune-specific paths needed for cmt scanning, and
    initializes [Load_path] with the project's cmi directories so
    that user queries containing type annotations can be typechecked.
    Returns [Error] if no project is found or if the build system is
    not supported.

    Project location is delegated to {!Mconfig_dot.find_project_context}
    so the heuristic stays consistent with the rest of merlin: walk
    up from [search_root] looking for a [dune-project],
    [dune-workspace], or [.merlin] file. Currently only Dune projects
    are supported; [.merlin]-located projects yield an [Error]
    explaining the limitation.

    @param dune_context a Dune context for which type information for the
    project is available (cmt files). The default is Dune's default
    context named [default].

    @param search_root specifies a search root other than the current
    working directory. It must be within a Dune project. *)
val init :
  ?dune_context:string ->
  ?search_root:string ->
  unit -> (t, string) result

(** Translate a source path relative to the search root into a path
    to the copy of the same source file in the build workspace.

    If the project root is [/project], and the search root is [/project/foo],
    and the Dune context is [default],
    then [in_build_dir paths "bar.ml"] is [/project/_build/default/foo/bar.ml].
*)
val in_build_dir : t -> string -> string
