(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Command-line parameters *)

type path_printing_mode = [`Real | `Short | `Opened ]

module StringSet : Set.S with type elt = string
module StringMap : Map.S with type key = string

type set = {
  include_dirs                 : string list ref;
  mutable std_include          : bool;
  mutable fast                 : bool;
  mutable classic              : bool;
  mutable principal            : bool;
  mutable real_paths           : path_printing_mode;
  mutable timed_logs           : bool;
  mutable recursive_types      : bool;
  mutable strict_sequence      : bool;
  mutable applicative_functors : bool;
  mutable unsafe_string        : bool;
  mutable nopervasives         : bool;
  mutable strict_formats       : bool;
  mutable open_modules         : string list;
  mutable ppx                  : Ppxsetup.t;
}

(* Manage set of flag *)
val initial : set

val fresh : unit -> set
val copy : set -> set

(* Current state *)
val set : set ref

val include_dirs         : unit -> string list
val no_std_include       : unit -> bool
val fast                 : unit -> bool
val classic              : unit -> bool
val principal            : unit -> bool
val real_paths           : unit -> path_printing_mode
val timed_logs           : unit -> bool
val recursive_types      : unit -> bool
val strict_sequence      : unit -> bool
val applicative_functors : unit -> bool
val unsafe_string        : unit -> bool
val nopervasives         : unit -> bool
val strict_formats       : unit -> bool
val open_modules         : unit -> string list
val ppx                  : unit -> string list

(* Dummy values *)
val annotations          : unit -> bool
val binary_annotations   : unit -> bool
val print_types          : unit -> bool
val native_code          : unit -> bool
val dont_write_files     : unit -> bool
val error_size           : unit -> int (* max size of module related errors *)
val keep_locs            : unit -> bool
val keep_docs            : unit -> bool
val transparent_modules  : unit -> bool
val for_package          : unit -> string option
val debug                : unit -> bool

(* Compute arguments specification *)
val arg_spec : set -> (string * Arg.spec * string) list
