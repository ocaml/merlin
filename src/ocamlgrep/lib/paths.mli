(* This file is part of the ocamlgrep package,
   See the attached LICENSE file.
   Copyright (C) 2026 LexiFi *)

(**
   Path management for ocamlgrep: where to look for the files we need

   For now, we only support Dune, keeping in mind that we might want
   to support other build systems later.
*)

(** An abstract representation of what's needed to locate project files.

    We only expose the fields that are independent from the build system
    such as [search_root]. *)
type t

(** Initialize compiler-libs so as to search the correct list of include dirs
    (details?)
*)
val init : t -> unit

(** Return the search root path optionally specified by the user *)
val search_root : t -> string option

(** Return the project root path (for debugging purposes) *)
val project_root : t -> string

(** Return the search root path expressed as a path that is relative to
    the project root. *)
val project_relative_search_root : t -> string

(** [identify_dune_project ()] identifies
    where the source files and compiled files we need are located.

    @param context a Dune context for which type information for the project
    is available (cmt files).
    The default is Dune's default context named [default].

    @param search_root specifies a search root other than the current working
    directory. It must be within a Dune project.
*)
val identify_dune_project :
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
