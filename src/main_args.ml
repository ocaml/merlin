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

let mk_debug f =
  "-debug", Arg.String f,
         "<section>[,<log file path>] Activate logging for given sections.\n\
\                                     Available sections are :\n\
\                                       - protocol\n\
\                                       - locate\n\
\                                       - completion"
;;

let mk_projectfind f =
  "-project-find", Arg.String f, "<path> Print name of merlin project near <path>, if any"
;;

let mk_real_paths f =
    "-real-paths", Arg.Unit f, " Display real paths in types rather than short ones"
;;

let mk_ignore_sigint f =
  "-ignore-sigint", Arg.Unit f, "  Ignore SIGINT, useful when invoked from editor"
;;

let mk_absname f =
  "-absname", Arg.Unit f, "  Show absolute filenames in error message"
;;

let mk_I f =
  "-I", Arg.String f, "<dir>  Add <dir> to the list of include directories"
;;

let mk_init f =
  "-init", Arg.String f, "<file>  Load <file> instead of default init file"
;;

let mk_labels f =
  "-labels", Arg.Unit f, " Use commuting label mode"
;;

let mk_no_app_funct f =
  "-no-app-funct", Arg.Unit f, " Deactivate applicative functors"
;;

let mk_nolabels f =
  "-nolabels", Arg.Unit f, " Ignore non-optional labels in types"
;;

let mk_nostdlib f =
  "-nostdlib", Arg.Unit f,
  " Do not add default directory to the list of include directories"
;;

let mk_principal f =
  "-principal", Arg.Unit f, " Check principality of type inference"
;;

let mk_rectypes f =
  "-rectypes", Arg.Unit f, " Allow arbitrary recursive types"
;;

let mk_strict_sequence f =
  "-strict-sequence", Arg.Unit f,
  " Left-hand part of a sequence must have type unit"
;;

let mk_version f =
  "-version", Arg.Unit f, " Print version and exit"
;;

let mk_vnum f =
  "-vnum", Arg.Unit f, " Print version number and exit"
;;

let mk_w f =
  "-w", Arg.String f,
  Printf.sprintf
  "<list>  Enable or disable warnings according to <list>:\n\
  \        +<spec>   enable warnings in <spec>\n\
  \        -<spec>   disable warnings in <spec>\n\
  \        @<spec>   enable warnings in <spec> and treat them as errors\n\
  \     <spec> can be:\n\
  \        <num>             a single warning number\n\
  \        <num1>..<num2>    a range of consecutive warning numbers\n\
  \        <letter>          a predefined set\n\
  \     default setting is %S" Warnings.defaults_w
;;

let mk_warn_error f =
  "-warn-error", Arg.String f,
  Printf.sprintf
  "<list>  Enable or disable error status for warnings according\n\
  \     to <list>.  See option -w for the syntax of <list>.\n\
  \     Default setting is %S" Warnings.defaults_warn_error
;;

let mk_warn_help f =
  "-warn-help", Arg.Unit f, "  Show description of warning numbers"
;;

let mk_protocol f =
  "-protocol", Arg.String f, "  Select frontend protocol (or 'help' to list)"
;;

let mk__ f =
  "-", Arg.String f,
  "<file>  Treat <file> as a file name (even if it starts with `-')"
;;

module type Top_options = sig
  val _debug : string -> unit
  val _projectfind : string -> unit
  val _real_paths : unit -> unit
  val _absname : unit -> unit
  val _ignore_sigint : unit -> unit
  val _I : string -> unit
  val _init : string -> unit
  val _labels : unit -> unit
  val _no_app_funct : unit -> unit
  val _nolabels : unit -> unit
  val _nostdlib : unit -> unit
  val _principal : unit -> unit
  val _rectypes : unit -> unit
  val _strict_sequence : unit -> unit
  val _unsafe : unit -> unit
  val _version : unit -> unit
  val _vnum : unit -> unit
  val _w : string -> unit
  val _warn_error : string -> unit
  val _warn_help : unit -> unit
  val _protocol : string -> unit

  val anonymous : string -> unit
end;;

module type Arg_list = sig
    val list : (string * Arg.spec * string) list
end;;

module Make_top_options (F : Top_options) =
struct
  let list = [
    mk_debug F._debug;
    mk_projectfind F._projectfind;
    mk_real_paths F._real_paths;
    mk_absname F._absname;
    mk_ignore_sigint F._ignore_sigint;
    mk_I F._I;
    mk_init F._init;
    mk_labels F._labels;
    mk_no_app_funct F._no_app_funct;
    mk_nolabels F._nolabels;
    mk_nostdlib F._nostdlib;
    mk_principal F._principal;
    mk_rectypes F._rectypes;
    mk_strict_sequence F._strict_sequence;
    mk_version F._version;
    mk_vnum F._vnum;
    mk_w F._w;
    mk_warn_error F._warn_error;
    mk_warn_help F._warn_help;
    mk_protocol F._protocol;

    mk__ F.anonymous;
  ]
end;;

