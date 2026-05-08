(* This file is part of the merlin-lib.project library
   See the attached LICENSE file. *)

(** Structured access to the output of [dune describe workspace].

    Requires the [dune] command.

    This provides locations of build assets (ml, cmi, cmt, ...)
    in the build folder so that they can could scanned after a build.
    Files are listed based on the source files and the build rules
    identified by Dune. Listed files may or may not exist depending
    on the build state.

    Building all the cmt files of a project in the default workspace
    can be done with [dune build @check] which may be slightly faster
    than a full [dune build].
*)

(** A module described by dune. dune encodes each of [impl], [intf],
    [cmt], [cmti] as a 0-or-1-element list ([(intf ())] or
    [(intf (lib/foo.mli))]); the sexp deriver maps that directly to
    [string option]. *)
type module_ =
  { name : string;
    impl : string option;
    intf : string option;
    cmt : string option;
    cmti : string option
  }

(** A [(library ...)] entry from the workspace description. *)
type library =
  { name : string;
    uid : string;
    local : bool;
        (** [true] for libraries defined in the project, [false] for
            external dependencies (e.g. compiler-libs, unix). *)
    requires : string list;
    source_dir : string;
    modules : module_ list;
    include_dirs : string list
  }

(** An [(executables ...)] entry: one per [(executable)] /
    [(executables)] stanza in the project's [dune] files. *)
type executables =
  { names : string list;
    requires : string list;
    modules : module_ list;
    include_dirs : string list
  }

(** A digested view of the workspace. *)
type t =
  { root : string;
    build_context : string;
    libraries : library list;
    executables : executables list
  }

(** [describe ?context ?root ()] runs
    {[
      dune describe workspace --format=csexp --lang 0.1
    ]}
    in the given directory and parses its output.

    The [--lang 0.1] pin is dune's stability contract for this output
    format: dune commits to keeping the format unchanged across
    versions. New top-level entry kinds added in a future [--lang] are
    silently ignored, so old code keeps working against newer dunes.

    @param context specify which build context to describe. The default
    if Dune's default which is named [default].

    @param root force the project root instead of inferring it by scanning
    the file system starting from the current directory.
*)
val describe :
  ?context:string ->
  ?root:string ->
  unit -> (t, string) result

(** Every cmt path declared by dune for project-local modules across
    all libraries (excluding [(local false)]) and all executables.
    Paths are returned as dune emitted them -- typically relative to
    the project root, under [_build/<context>/...]. *)
val local_cmt_files : t -> string list
