(* This file is part of the ocamlgrep package
   See the attached LICENSE file.
   Copyright (C) 2026 LexiFi

   Originally written by Nicolás Ojeda Bär (LexiFi);
   maintained by Martin Jambon (LexiFi). *)

(**
   Path management for ocamlgrep: where to look for the files we need

   For now, we only support Dune, keeping in mind that we might want
   to support other build systems later.
*)

(** An abstract representation of what's needed to locate project files.

    We only expose the fields that are independent from the build system
    such as [search_root]. *)
type t

(** Return the search root path optionally specified by the user *)
val search_root : t -> string option

(** Return the project root path (for debugging purposes) *)
val project_root : t -> string

(** Return the search root path expressed as a path that is relative to
    the project root. *)
val project_relative_search_root : t -> string

(** [init ()] locates the OCaml project containing [search_root],
    derives the dune-specific paths ocamlgrep needs, and initializes
    [Load_path] with the project's cmi directories so that user
    queries containing type annotations can be typechecked. Returns
    [Error] if no project is found or if the build system is not
    supported.

    Project location is delegated to {!Mconfig_dot.find_project_context}
    so the heuristic stays consistent with the rest of merlin: walk
    up from [search_root] looking for a [dune-project],
    [dune-workspace], or [.merlin] file. Currently only Dune projects
    are supported; [.merlin]-located projects yield an [Error]
    explaining the limitation.

    @param context a Dune context for which type information for the
    project is available (cmt files). The default is Dune's default
    context named [default].

    @param search_root specifies a search root other than the current
    working directory. It must be within a Dune project. *)
val init :
  ?context:string ->
  ?search_root:string ->
  unit -> (t, string) result

(** Translate a source path relative to the search root into a path
    to the copy of the same source file in the build workspace.

    If the project root is [/project], and the search root is [/project/foo],
    and the Dune context is [default],
    then [in_build_dir paths "bar.ml"] is [/project/_build/default/foo/bar.ml].
*)
val in_build_dir : t -> string -> string

