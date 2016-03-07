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

(* FIXME
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
*)

let timed_logs = ref false
let timed_logs_spec =
  "-timed-logs",
  Arg.Set timed_logs,
  " Add time information in the log file when enabled"

let include_dirs = ref []
let include_dirs_spec =
  "-I",
  Arg.String (fun s -> include_dirs := s :: !include_dirs),
  "<dir> Add <dir> to the list of include directories"

let no_std_include = ref false
let no_std_include_spec =
  "-nostdlib",
  Arg.Set no_std_include,
  " Do not add default directory to the list of include directories"

let fast = ref false
let fast_spec =
  "-unsafe",
  Arg.Set fast,
  " Do not compile bounds checking on array and string access"

let classic = ref false
let labels_spec =
  "-labels",
  Arg.Clear classic,
  " Use commuting label mode"
let nolabels_spec =
  "-nolabels",
  Arg.Set classic,
  " Ignore non-optional labels in types"

let principal = ref false
let principal_spec =
  "-principal",
  Arg.Set principal,
  " Check principality of type inference"

let real_paths : path_printing_mode ref = ref `Real
let real_paths_spec =
  "-real-paths",
  Arg.Unit (fun () -> real_paths := `Real),
  " Display real paths in types rather than short ones"

let short_paths_spec =
  "-short-paths",
  Arg.Unit (fun () -> real_paths := `Short),
  " Shorten paths in types"

let opened_paths_spec =
  "-opened-paths",
  Arg.Unit (fun () -> real_paths := `Opened),
  " Remove opened prefix from displayed types"

let recursive_types = ref false
let recursive_types_spec =
  "-rectypes",
  Arg.Set recursive_types,
  " Allow arbitrary recursive types"

let strict_sequence = ref false
let strict_sequence_spec =
  "-strict-sequence",
  Arg.Set strict_sequence,
  " Left-hand part of a sequence must have type unit"

let applicative_functors = ref true
let applicative_functors_spec =
  "-no-app-funct",
  Arg.Clear applicative_functors,
  " Deactivate applicative functors"

let threads_spec =
  "-thread",
  Arg.Unit (fun () -> include_dirs := "+threads" :: !include_dirs),
  " Add support for system threads library"

let vmthreads_spec =
  "-vmthread",
  Arg.Unit (fun () -> include_dirs := "+vmthreads" :: !include_dirs),
  " Add support for VM-scheduled threads library"

let unsafe_string = ref true
let unsafe_string_spec =
  "-unsafe-string",
  Arg.Set unsafe_string,
  " Make strings mutable (default)"
let safe_string_spec =
  "-safe-string",
  Arg.Clear unsafe_string,
  " Make strings immutable"

let nopervasives = ref false
let nopervasives_spec =
  "-nopervasives",
  Arg.Set nopervasives,
  " Don't open Pervasives module (advanced)"

let strict_formats = ref false
let strict_formats_spec =
  "-strict-formats",
  Arg.Set strict_formats,
  " Reject invalid formats accepted by legacy implementations"

let open_modules = ref []
let open_modules_spec =
  "-open",
  Arg.String (fun md -> open_modules := md :: !open_modules),
  "<module>  Opens the module <module> before typing"

let ppx = ref Ppxsetup.empty

let ppx_spec =
  "-ppx",
  Arg.String (fun s -> ppx := Ppxsetup.add_ppx s !ppx),
  "<command> Pipe abstract syntax trees through preprocessor <command>"

let pp = ref ""

let pp_spec =
  "-pp",
  Arg.Set_string pp,
  "<command> Pipe sources through preprocessor <command>"

(* Dummy values *)
let annotations         = ref false
let binary_annotations  = ref true
let print_types         = ref false
let native_code         = ref false
let error_size          = ref 500
let dont_write_files    = ref true
let keep_locs           = ref true
let keep_docs           = ref false
let transparent_modules = ref true
let for_package         = ref None
let debug               = ref false
let opaque              = ref false

let arg_spec =
  [
    (*debug_spec ();*)
    applicative_functors_spec;
    fast_spec;
    include_dirs_spec;
    labels_spec;
    nolabels_spec;
    no_std_include_spec;
    principal_spec;
    real_paths_spec;
    short_paths_spec;
    opened_paths_spec;
    timed_logs_spec;
    recursive_types_spec;
    strict_sequence_spec;
    threads_spec;
    vmthreads_spec;
    safe_string_spec;
    unsafe_string_spec;
    nopervasives_spec;
    strict_formats_spec;
    open_modules_spec;
    ppx_spec;
    pp_spec;
  ] @
  Warnings.arg_spec

type config =
  | Nil
  | Set : 'a ref * 'a * config -> config

let save_one r c =
  Set (r, !r, c)

let save () =
  save_one include_dirs         @@
  save_one no_std_include       @@
  save_one fast                 @@
  save_one classic              @@
  save_one principal            @@
  save_one real_paths           @@
  save_one timed_logs           @@
  save_one recursive_types      @@
  save_one strict_sequence      @@
  save_one applicative_functors @@
  save_one unsafe_string        @@
  save_one nopervasives         @@
  save_one strict_formats       @@
  save_one open_modules         @@
  save_one ppx                  @@
  save_one pp                   @@
  Nil

let rec load = function
  | Nil -> ()
  | Set (r, x, c) ->
    r := x;
    load c
