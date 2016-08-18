open Std

(** {1 OCaml commandline parsing} *)

type ocaml = {
  include_dirs         : string list;
  no_std_include       : bool;
  unsafe               : bool;
  classic              : bool;
  principal            : bool;
  real_paths           : [ `Real | `Short | `Opened ];
  threads              : [ `None | `Threads | `Vmthreads ];
  recursive_types      : bool;
  strict_sequence      : bool;
  applicative_functors : bool;
  unsafe_string        : bool;
  nopervasives         : bool;
  strict_formats       : bool;
  open_modules         : string list;
  ppx                  : string list;
  pp                   : string;
}

val dump_ocaml : ocaml -> json

(** {1 Findlib configuration} *)

type findlib = {
  conf : string option;
  path : string list;
}

val dump_findlib : findlib -> json

(** {1 Merlin high-level settings} *)

type merlin = {
  build_path  : string list;
  source_path : string list;
  cmi_path    : string list;
  cmt_path    : string list;
  extensions  : string list;
  suffixes    : (string * string) list;
  stdlib      : string option;
  reader      : string list;

  flags_to_apply    : string list list;
  dotmerlin_to_load : string list;
  packages_to_load  : string list;
  packages_path     : string list;

  flags_applied    : string list list;
  dotmerlin_loaded : string list;
  packages_loaded  : string list;
}

val dump_merlin : merlin -> json

(** {1 Some flags affecting queries} *)

type query = {
  filename  : string;
  directory : string;
  terminal_width : int;
  verbosity : int;
}

(** {1 Main configuration} *)

type t = {
  ocaml   : ocaml;
  findlib : findlib;
  merlin  : merlin;
  query   : query;
}

val initial : t

val dump : t -> json

val normalize : Trace.t -> t -> t

val is_normalized : t -> bool

val arguments_table : t Marg.table

val document_arguments : out_channel -> unit

(** {1 Computing project paths} *)

val source_path : t -> string list

val build_path : t -> string list

val cmt_path : t -> string list

val global_modules : ?include_current:bool -> t -> string list

