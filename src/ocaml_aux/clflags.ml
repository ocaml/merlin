(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: clflags.ml 12511 2012-05-30 13:29:48Z lefessan $ *)

(* Command-line parameters *)

type set = {
  mutable include_dirs         : string list;
  mutable no_std_include       : bool;
  mutable fast                 : bool;
  mutable classic              : bool;
  mutable principal            : bool;
  mutable real_paths           : bool;
  mutable recursive_types      : bool;
  mutable strict_sequence      : bool;
  mutable applicative_functors : bool;
}

let fresh () =
  {
    include_dirs         = [];    (* -I *)
    no_std_include       = false; (* -nostdlib *)
    fast                 = false; (* -unsafe *)
    classic              = false; (* -nolabels *)
    principal            = false; (* -principal *)
    real_paths           = false; (* -real-paths *)
    recursive_types      = false; (* -rectypes *)
    strict_sequence      = false; (* -strict-sequence *)
    applicative_functors = true;  (* -no-app-funct *)
  }

let copy t = {t with fast = t.fast}

let initial = fresh ()
let set = ref initial

let include_dirs () = !set.include_dirs
let include_dirs_spec t =
  "-I",
  Arg.String (fun s -> t.include_dirs <- s :: t.include_dirs),
  "<dir> Add <dir> to the list of include directories"

let no_std_include () = !set.no_std_include
let no_std_include_spec t =
  "-nostdlib",
  Arg.Unit (fun () -> t.no_std_include <- true),
  " Do not add default directory to the list of include directories"

let fast () = !set.fast
let fast_spec t =
  "-unsafe",
  Arg.Unit (fun () -> t.fast <- true),
  " Do not compile bounds checking on array and string access"

let classic () = !set.classic
let labels_spec t =
  "-labels",
  Arg.Unit (fun () -> t.classic <- false),
  " Use commuting label mode"
let nolabels_spec t =
  "-nolabels",
  Arg.Unit (fun () -> t.classic <- true),
  " Ignore non-optional labels in types"

let principal () = !set.principal
let principal_spec t =
  "-principal",
  Arg.Unit (fun () -> t.principal <- true),
  " Check principality of type inference"

let real_paths () = !set.real_paths
let real_paths_spec t =
  "-real-paths",
  Arg.Unit (fun () -> t.real_paths <- true),
  " Display real paths in types rather than short ones"

let recursive_types () = !set.recursive_types
let recursive_types_spec t =
  "-rectypes",
  Arg.Unit (fun () -> t.recursive_types <- true),
  " Allow arbitrary recursive types"

let strict_sequence () = !set.strict_sequence
let strict_sequence_spec t =
  "-strict-sequence",
  Arg.Unit (fun () -> t.strict_sequence <- true),
  " Left-hand part of a sequence must have type unit"

let applicative_functors () = !set.applicative_functors
let applicative_functors_spec t =
  "-no-app-funct",
  Arg.Unit (fun () -> t.applicative_functors <- false),
  " Deactivate applicative functors"

(* Dummy values *)
let annotations        () = false
let binary_annotations () = false
let print_types        () = false
let native_code        () = false
let error_size         () = 500
let dont_write_files   () = true

let std_include_dir () =
  if !set.no_std_include
  then []
  else [Config.standard_library]

let arg_spec t =
  [
    applicative_functors_spec t;
    fast_spec t;
    include_dirs_spec t;
    labels_spec t;
    nolabels_spec t;
    no_std_include_spec t;
    principal_spec t;
    real_paths_spec t;
    recursive_types_spec t;
    strict_sequence_spec t;
  ]

