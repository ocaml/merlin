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

(* Command-line parameters *)

type path_printing_mode = [`Real | `Short | `Opened ]

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

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

let fresh () =
  {
    include_dirs         = ref [];    (* -I *)
    std_include          = true; (* -nostdlib *)
    fast                 = false; (* -unsafe *)
    classic              = false; (* -nolabels *)
    principal            = false; (* -principal *)
    real_paths           = `Real;  (* -real-paths / ! -short-paths *)
    timed_logs           = false; (* -timed-logs *)
    recursive_types      = false; (* -rectypes *)
    strict_sequence      = false; (* -strict-sequence *)
    applicative_functors = true;  (* -no-app-funct *)
    unsafe_string        = true;  (* -safe-string / -unsafe-string *)
    nopervasives         = false; (* -nopervasives *)
    strict_formats       = false; (* -strict-formats *)
    open_modules         = [];
    ppx                  = Ppxsetup.empty;    (* -ppx *)
  }

let copy t = {t with include_dirs = ref !(t.include_dirs)}

let initial = fresh ()
let set = ref initial

let debug_spec () =
  let f section =
    let split = Misc.rev_string_split in
    let section, dest =
      match split section ~on:',' with
      | [ section ] -> section, None
      | [ log_path ; section ] -> section, Some log_path
      | _ -> invalid_arg "-debug"
    in
    let section, level =
      match Misc.rev_string_split section ~on:':' with
      | [ section ] -> section, `info
      | [ "error" ; section  ] -> section, `error
      | [ "info"  ; section  ] -> section, `info
      | [ "debug" ; section  ] -> section, `debug
      | _ -> invalid_arg "-debug"
    in
    begin try
      let section = Logger.section section in
      Logger.monitor ?dest section level
    with Invalid_argument _ -> ()
    end
  in
  let sections =
    String.concat ", " (List.map Logger.Section.to_string (Logger.Section.list ()))
  in
  "-debug",
  Arg.String f,
  "<section>[:level][,<log file path>] Activate logging for given section.\n\
  \                            Available sections are : " ^ sections ^ "\n\
  \                            Available levels are :\n\
  \                              - error\n\
  \                              - info (default)\n\
  \                              - debug"

let timed_logs () = !set.timed_logs
let timed_logs_spec t =
  "-timed-logs",
  Arg.Unit (fun () -> t.timed_logs <- true),
  " Add time information in the log file when enabled"

let include_dirs () = !(!set.include_dirs)
let include_dirs_spec t =
  "-I",
  Arg.String (fun s -> t.include_dirs := s :: !(t.include_dirs)),
  "<dir> Add <dir> to the list of include directories"

let no_std_include () = not !set.std_include
let no_std_include_spec t =
  "-nostdlib",
  Arg.Unit (fun () -> t.std_include <- false),
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
  Arg.Unit (fun () -> t.real_paths <- `Real),
  " Display real paths in types rather than short ones"
let short_paths_spec t =
  "-short-paths",
  Arg.Unit (fun () -> t.real_paths <- `Short),
  " Shorten paths in types"
let opened_paths_spec t =
  "-opened-paths",
  Arg.Unit (fun () -> t.real_paths <- `Opened),
  " Remove opened prefix from displayed types"

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

let unsafe_string () = !set.unsafe_string
let unsafe_string_spec t =
  "-unsafe-string",
  Arg.Unit (fun () -> t.unsafe_string <- true),
  " Make strings mutable (default)"
let safe_string_spec t =
  "-safe-string",
  Arg.Unit (fun () -> t.unsafe_string <- false),
  " Make strings immutable"

let nopervasives () = !set.nopervasives
let nopervasives_spec t =
  "-nopervasives",
  Arg.Unit (fun () -> t.nopervasives <- true),
  " Don't open Pervasives module (advanced)"

let strict_formats () = !set.strict_formats
let strict_formats_spec t =
  "-strict-formats",
  Arg.Unit (fun () -> t.strict_formats <- true),
  " Reject invalid formats accepted by legacy implementations"

let open_modules () = !set.open_modules
let open_modules_spec t =
  "-open",
  Arg.String (fun md -> t.open_modules <- md :: t.open_modules),
  "<module>  Opens the module <module> before typing"

let open_modules () = !set.open_modules
let open_modules_spec t =
  "-open",
  Arg.String (fun md -> t.open_modules <- md :: t.open_modules),
  "<module>  Opens the module <module> before typing"

let ppx () = Ppxsetup.command_line !set.ppx

let ppx_spec t =
  "-ppx",
  Arg.String (fun s -> t.ppx <- Ppxsetup.add_ppx s t.ppx),
  "<command> Pipe abstract syntax trees through preprocessor <command>"

(* Dummy values *)
let annotations         () = false
let binary_annotations  () = false
let print_types         () = false
let native_code         () = false
let error_size          () = 500
let dont_write_files    () = true
let keep_locs           () = true
let keep_docs           () = false
let transparent_modules () = true
let for_package         () = None
let debug               () = false

let arg_spec t =
  [
    debug_spec ();
    applicative_functors_spec t;
    fast_spec t;
    include_dirs_spec t;
    labels_spec t;
    nolabels_spec t;
    no_std_include_spec t;
    principal_spec t;
    real_paths_spec t;
    short_paths_spec t;
    opened_paths_spec t;
    timed_logs_spec t;
    recursive_types_spec t;
    strict_sequence_spec t;
    threads_spec t;
    vmthreads_spec t;
    safe_string_spec t;
    unsafe_string_spec t;
    nopervasives_spec t;
    strict_formats_spec t;
    open_modules_spec t;
    ppx_spec t;
  ]

