(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

let ignore_sigint_spec =
  let f () =
    try ignore (Sys.(signal sigint Signal_ignore))
    with Invalid_argument _ -> ()
  in
  "-ignore-sigint",
  Arg.Unit f,
  " Ignore SIGINT, useful when invoked from editor"

let version_spec =
  Printf.sprintf "The Merlin toolkit version %s, for Ocaml %s"
    My_config.version Sys.ocaml_version

let print_version_spec =
  let f () =
    print_endline version_spec;
    exit 0
  in
  "-version",
  Arg.Unit f,
  " Print version and exit"

let print_version_num_spec =
  let f () =
    Printf.printf "%s\n" My_config.version;
    exit 0
  in
  "-vnum",
  Arg.Unit f,
  " Print version number and exit"

let warn_help_spec =
  "-warn-help",
  Arg.Unit Warnings.help_warnings,
  " Show description of warning numbers"

let chosen_protocol = ref None
let protocol_spec =
  "-protocol",
  Arg.String (fun p -> chosen_protocol := Some p),
  " Select frontend protocol (or 'help' to list)"

let unexpected_argument s =
  failwith ("Unexpected argument: " ^ s)

let flags =
  [
    ignore_sigint_spec;
    print_version_spec;
    print_version_num_spec;
    warn_help_spec;
    protocol_spec;
  ]

let () =
  (* Parse arguments on commandline *)
  Arg.parse Clflags.arg_spec
    unexpected_argument
    "Usage: ocamlmerlin [options]\noptions are:"

let chosen_protocol = !chosen_protocol
