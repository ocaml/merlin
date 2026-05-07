(* This file is part of the merlin-lib.project library
   See the attached LICENSE file. *)

(** Structured access to the output of [dune describe workspace]. *)

(** A module described by dune. Each of [impl], [intf], [cmt], [cmti]
    is encoded by dune as a 0-or-1-element list -- e.g. [(intf ())] or
    [(intf (lib/foo.mli))]. We mirror that shape with [string list];
    use {!impl_path}, {!cmt_path} etc. for the convenient
    [string option] view. *)
type module_ =
  { name : string;
    impl : string list;
    intf : string list;
    cmt : string list;
    cmti : string list
  }

val impl_path : module_ -> string option
val intf_path : module_ -> string option
val cmt_path : module_ -> string option
val cmti_path : module_ -> string option

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

(** [describe ?root ()] runs
    {[
      dune describe workspace --format=csexp --lang 0.1
    ]}
    in the given directory (defaulting to the current working
    directory; passed via dune's [--root] flag) and parses its output.

    The [--lang 0.1] pin is dune's stability contract for this output
    format: dune commits to keeping the format unchanged across
    versions. New top-level entry kinds added in a future [--lang] are
    silently ignored, so old code keeps working against newer dunes. *)
val describe : ?root:string -> unit -> (t, string) result

(** [parse_csexp sexp] interprets a [Csexp.t] produced by [dune
    describe workspace --format=csexp --lang 0.1]. Useful when you
    have the output already in hand (e.g. from a test fixture). *)
val parse_csexp : Csexp.t -> (t, string) result

(** Every cmt path declared by dune for project-local modules across
    all libraries (excluding [(local false)]) and all executables.
    Paths are returned as dune emitted them -- typically relative to
    the project root, under [_build/<context>/...]. *)
val local_cmt_files : t -> string list
