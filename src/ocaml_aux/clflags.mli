(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2005 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Command-line parameters *)

type path_printing_mode = [`Real | `Short | `Opened ]

type config

(* Manage set of flag *)
val save : unit -> config
val load : config -> unit

val include_dirs         : string list ref
val no_std_include       : bool ref
val fast                 : bool ref
val classic              : bool ref
val principal            : bool ref
val real_paths           : path_printing_mode ref
val timed_logs           : bool ref
val recursive_types      : bool ref
val strict_sequence      : bool ref
val applicative_functors : bool ref
val unsafe_string        : bool ref
val nopervasives         : bool ref
val strict_formats       : bool ref
val open_modules         : string list ref
val ppx                  : Ppxsetup.t ref
val pp                   : string ref

(* Dummy values *)
val annotations          : bool ref
val binary_annotations   : bool ref
val print_types          : bool ref
val native_code          : bool ref
val dont_write_files     : bool ref
val error_size           : int ref (* max size of module related errors *)
val keep_locs            : bool ref
val keep_docs            : bool ref
val transparent_modules  : bool ref
val for_package          : string option ref
val debug                : bool ref
val opaque               : bool ref

(* Argument specification *)
val arg_spec : (string * Arg.spec * string) list
