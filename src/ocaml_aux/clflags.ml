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
  include_dirs                 : string list ref;
  std_include                  : string list ref;
  mutable fast                 : bool;
  mutable classic              : bool;
  mutable principal            : bool;
  mutable real_paths           : bool;
  mutable timed_logs           : bool;
  mutable recursive_types      : bool;
  mutable strict_sequence      : bool;
  mutable applicative_functors : bool;
}

let fresh () =
  {
    include_dirs         = ref [];    (* -I *)
    std_include          = ref [Config.standard_library]; (* -nostdlib *)
    fast                 = false; (* -unsafe *)
    classic              = false; (* -nolabels *)
    principal            = false; (* -principal *)
    real_paths           = false; (* -real-paths *)
    timed_logs           = false; (* -timed-logs *)
    recursive_types      = false; (* -rectypes *)
    strict_sequence      = false; (* -strict-sequence *)
    applicative_functors = true;  (* -no-app-funct *)
  }

let copy t = {t with
              include_dirs = ref !(t.include_dirs);
              std_include = ref !(t.std_include);
             }

let initial = fresh ()
let set = ref initial

let debug_spec =
  let f section =
    match Misc.rev_string_split section ~on:',' with
    | [ section ] ->
      begin try Logger.(monitor (Section.of_string section))
        with Invalid_argument _ -> () end
    | [ log_path ; section ] ->
      begin try
          let section = Logger.Section.of_string section in
          Logger.monitor ~dest:log_path section
        with Invalid_argument _ ->
          ()
      end
    | _ -> assert false
  in
  "-debug",
  Arg.String f,
  "<section>[,<log file path>] Activate logging for given sections.\n\
  \                            Available sections are :\n\
  \                              - protocol\n\
  \                              - locate\n\
  \                              - completion"

let timed_logs () = !(!set.timed_logs)
let timed_logs_spec t =
  "-timed-logs",
  Arg.Unit (fun () -> t.timed_logs := true),
  " Add time information in the log file when enabled"

let include_dirs () = !(!set.include_dirs)
let include_dirs_spec t =
  "-I",
  Arg.String (fun s -> t.include_dirs := s :: !(t.include_dirs)),
  "<dir> Add <dir> to the list of include directories"

let no_std_include () = !(!set.std_include) = []
let no_std_include_spec t =
  "-nostdlib",
  Arg.Unit (fun () -> t.std_include := []),
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

let threads_spec t =
  "-thread",
  Arg.Unit (fun () -> t.include_dirs := "+threads" :: !(t.include_dirs)),
  " Add support for system threads library"

let vmthreads_spec t =
  "-vmthread",
  Arg.Unit (fun () -> t.include_dirs := "+vmthreads" :: !(t.include_dirs)),
  " Add support for VM-scheduled threads library"

(* Dummy values *)
let annotations         () = false
let binary_annotations  () = false
let print_types         () = false
let native_code         () = false
let error_size          () = 500
let dont_write_files    () = true
let keep_locs           () = true
let transparent_modules () = true

let arg_spec t =
  [
    debug_spec;
    applicative_functors_spec t;
    fast_spec t;
    include_dirs_spec t;
    labels_spec t;
    nolabels_spec t;
    no_std_include_spec t;
    principal_spec t;
    real_paths_spec t;
    timed_logs_spec t;
    recursive_types_spec t;
    strict_sequence_spec t;
    threads_spec t;
    vmthreads_spec t;
  ]

