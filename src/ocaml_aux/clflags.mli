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

(* $Id: clflags.mli 12800 2012-07-30 18:59:07Z doligez $ *)

type t = {
  mutable objfiles             : string list;
  mutable ccobjs               : string list;
  mutable dllibs               : string list;
  mutable compile_only         : bool;
  mutable output_name          : string option;
  mutable include_dirs         : string list;
  mutable no_std_include       : bool;
  mutable print_types          : bool;
  mutable make_archive         : bool;
  mutable debug                : bool;
  mutable fast                 : bool;
  mutable link_everything      : bool;
  mutable custom_runtime       : bool;
  mutable output_c_object      : bool;
  mutable ccopts               : string list;
  mutable classic              : bool;
  mutable nopervasives         : bool;
  mutable preprocessor         : string option;
  mutable annotations          : bool;
  mutable binary_annotations   : bool;
  mutable use_threads          : bool;
  mutable use_vmthreads        : bool;
  mutable noassert             : bool;
  mutable verbose              : bool;
  mutable noprompt             : bool;
  mutable nopromptcont         : bool;
  mutable init_file            : string option;
  mutable use_prims            : string;
  mutable use_runtime          : string;
  mutable principal            : bool;
  mutable real_paths           : bool;
  mutable recursive_types      : bool;
  mutable strict_sequence      : bool;
  mutable applicative_functors : bool;
  mutable make_runtime         : bool;
  mutable gprofile             : bool;
  mutable c_compiler           : string option;
  mutable no_auto_link         : bool;
  mutable dllpaths             : string list;
  mutable make_package         : bool;
  mutable for_package          : string option;
  mutable error_size           : int;
  mutable dump_parsetree       : bool;
  mutable dump_rawlambda       : bool;
  mutable dump_lambda          : bool;
  mutable dump_clambda         : bool;
  mutable dump_instr           : bool;
  mutable keep_asm_file        : bool;
  mutable optimize_for_speed   : bool;
  mutable dump_cmm             : bool;
  mutable dump_selection       : bool;
  mutable dump_live            : bool;
  mutable dump_spill           : bool;
  mutable dump_split           : bool;
  mutable dump_interf          : bool;
  mutable dump_prefer          : bool;
  mutable dump_regalloc        : bool;
  mutable dump_reload          : bool;
  mutable dump_scheduling      : bool;
  mutable dump_linear          : bool;
  mutable keep_startup_file    : bool;
  mutable dump_combine         : bool;
  mutable native_code          : bool;
  mutable inline_threshold     : int;
  mutable dont_write_files     : bool;
  mutable shared               : bool;
  mutable dlcode               : bool;
  mutable runtime_variant      : string;
}

val t : t ref

val objfiles             : unit -> string list
val ccobjs               : unit -> string list
val dllibs               : unit -> string list
val compile_only         : unit -> bool
val output_name          : unit -> string option
val include_dirs         : unit -> string list
val no_std_include       : unit -> bool
val print_types          : unit -> bool
val make_archive         : unit -> bool
val debug                : unit -> bool
val fast                 : unit -> bool
val link_everything      : unit -> bool
val custom_runtime       : unit -> bool
val output_c_object      : unit -> bool
val ccopts               : unit -> string list
val classic              : unit -> bool
val nopervasives         : unit -> bool
val preprocessor         : unit -> string option
val annotations          : unit -> bool
val binary_annotations   : unit -> bool
val use_threads          : unit -> bool
val use_vmthreads        : unit -> bool
val noassert             : unit -> bool
val verbose              : unit -> bool
val noprompt             : unit -> bool
val nopromptcont         : unit -> bool
val init_file            : unit -> string option
val use_prims            : unit -> string
val use_runtime          : unit -> string
val principal            : unit -> bool
val real_paths           : unit -> bool
val recursive_types      : unit -> bool
val strict_sequence      : unit -> bool
val applicative_functors : unit -> bool
val make_runtime         : unit -> bool
val gprofile             : unit -> bool
val c_compiler           : unit -> string option
val no_auto_link         : unit -> bool
val dllpaths             : unit -> string list
val make_package         : unit -> bool
val for_package          : unit -> string option
val error_size           : unit -> int
val dump_parsetree       : unit -> bool
val dump_rawlambda       : unit -> bool
val dump_lambda          : unit -> bool
val dump_clambda         : unit -> bool
val dump_instr           : unit -> bool
val keep_asm_file        : unit -> bool
val optimize_for_speed   : unit -> bool
val dump_cmm             : unit -> bool
val dump_selection       : unit -> bool
val dump_live            : unit -> bool
val dump_spill           : unit -> bool
val dump_split           : unit -> bool
val dump_interf          : unit -> bool
val dump_prefer          : unit -> bool
val dump_regalloc        : unit -> bool
val dump_reload          : unit -> bool
val dump_scheduling      : unit -> bool
val dump_linear          : unit -> bool
val keep_startup_file    : unit -> bool
val dump_combine         : unit -> bool
val native_code          : unit -> bool
val inline_threshold     : unit -> int
val dont_write_files     : unit -> bool
val shared               : unit -> bool
val dlcode               : unit -> bool
val runtime_variant      : unit -> string
val std_include_dir      : unit -> string list
