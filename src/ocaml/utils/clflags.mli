(** {0 OCaml compiler compatible command-line parameters}

    For compatibility with typechecker.
    Argument parsing / build environment construction happens elsewhere.
*)

(** {1 Relevant settings}
    Parameters from OCaml compiler which affect Merlin behavior. *)
val cmi_file             : string option ref
val include_dirs         : string list ref
val hidden_include_dirs  : string list ref
val fast                 : bool ref
val classic              : bool ref
val all_ppx              : string list ref
val principal            : bool ref
val real_paths           : bool ref
val recursive_types      : bool ref
val strict_sequence      : bool ref
val applicative_functors : bool ref
val nopervasives         : bool ref
val strict_formats       : bool ref
val open_modules         : string list ref

(** {1 Dummy values}
    Ignored by merlin but kept for compatibility with upstream code. *)
val annotations          : bool ref
val binary_annotations   : bool ref
val store_occurrences    : bool ref
val print_types          : bool ref
val native_code          : bool ref
val dont_write_files     : bool ref
val error_size           : int ref (* max size of module related errors *)
val keep_locs            : bool ref
val keep_docs            : bool ref
val transparent_modules  : bool ref
val for_package          : string option ref
val debug                : bool ref
val unsafe               : bool ref
val opaque               : bool ref
val unboxed_types        : bool ref

val locations            : bool ref

module Compiler_pass : sig
  type t = Parsing | Typing | Lambda | Scheduling | Emit
  val of_string : string -> t option
  val to_string : t -> string
  val is_compilation_pass : t -> bool
  val available_pass_names : filter:(t -> bool) -> native:bool -> string list
  val can_save_ir_after : t -> bool
  val compare : t -> t -> int
  val to_output_filename: t -> prefix:string -> string
  val of_input_filename: string -> t option
end

val stop_after : Compiler_pass.t option ref
