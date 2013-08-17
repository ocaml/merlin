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

(* $Id: main_args.ml 12511 2012-05-30 13:29:48Z lefessan $ *)

let mk_projectfind f =
  "-project-find", Arg.String f, "<path> Print name of merlin project near <path>, if any"
;;

let mk_a f =
  "-a", Arg.Unit f, " Build a library"
;;

let mk_absname f =
  "-absname", Arg.Unit f, "  Show absolute filenames in error message"
;;

let mk_annot f =
  "-annot", Arg.Unit f, " Save information in <filename>.annot"
;;

let mk_binannot f =
  "-bin-annot", Arg.Unit f, " Save typedtree in <filename>.cmt"
;;

let mk_c f =
  "-c", Arg.Unit f, " Compile only (do not link)"
;;

let mk_cc f =
  "-cc", Arg.String f, "<command>  Use <command> as the C compiler and linker"
;;

let mk_cclib f =
  "-cclib", Arg.String f, "<opt>  Pass option <opt> to the C linker"
;;

let mk_ccopt f =
  "-ccopt", Arg.String f, "<opt>  Pass option <opt> to the C compiler and linker"
;;

let mk_compact f =
  "-compact", Arg.Unit f, " Optimize code size rather than speed"
;;

let mk_config f =
  "-config", Arg.Unit f, " Print configuration values and exit"
;;

let mk_custom f =
  "-custom", Arg.Unit f, " Link in custom mode"
;;

let mk_dllib f =
  "-dllib", Arg.String f, "<lib>  Use the dynamically-loaded library <lib>"
;;

let mk_dllpath f =
  "-dllpath", Arg.String f,
  "<dir>  Add <dir> to the run-time search path for shared libraries"
;;

let mk_dtypes f =
  "-dtypes", Arg.Unit f, " (deprecated) same as -annot"
;;

let mk_for_pack_byt () =
  "-for-pack", Arg.String ignore,
  "<ident>  Ignored (for compatibility with ocamlopt)"
;;

let mk_for_pack_opt f =
  "-for-pack", Arg.String f,
  "<ident>  Generate code that can later be `packed' with\n\
  \     ocamlopt -pack -o <ident>.cmx"
;;

let mk_g_byt f =
  "-g", Arg.Unit f, " Save debugging information"
;;

let mk_g_opt f =
  "-g", Arg.Unit f, " Record debugging information for exception backtrace"
;;

let mk_i f =
  "-i", Arg.Unit f, " Print inferred interface"
;;

let mk_I f =
  "-I", Arg.String f, "<dir>  Add <dir> to the list of include directories"
;;

let mk_impl f =
  "-impl", Arg.String f, "<file>  Compile <file> as a .ml file"
;;

let mk_init f =
  "-init", Arg.String f, "<file>  Load <file> instead of default init file"
;;

let mk_inline f =
  "-inline", Arg.Int f, "<n>  Set aggressiveness of inlining to <n>"
;;

let mk_intf f =
  "-intf", Arg.String f, "<file>  Compile <file> as a .mli file"
;;

let mk_intf_suffix f =
  "-intf-suffix", Arg.String f,
  "<string>  Suffix for interface files (default: .mli)"
;;

let mk_intf_suffix_2 f =
  "-intf_suffix", Arg.String f, "<string>  (deprecated) same as -intf-suffix"
;;

let mk_labels f =
  "-labels", Arg.Unit f, " Use commuting label mode"
;;

let mk_linkall f =
  "-linkall", Arg.Unit f, " Link all modules, even unused ones"
;;

let mk_make_runtime f =
  "-make-runtime", Arg.Unit f,
  " Build a runtime system with given C objects and libraries"
;;

let mk_make_runtime_2 f =
  "-make_runtime", Arg.Unit f, " (deprecated) same as -make-runtime"
;;

let mk_modern f =
  "-modern", Arg.Unit f, " (deprecated) same as -labels"
;;

let mk_no_app_funct f =
  "-no-app-funct", Arg.Unit f, " Deactivate applicative functors"
;;

let mk_noassert f =
  "-noassert", Arg.Unit f, " Do not compile assertion checks"
;;

let mk_noautolink_byt f =
  "-noautolink", Arg.Unit f,
  " Do not automatically link C libraries specified in .cma files"
;;

let mk_noautolink_opt f =
  "-noautolink", Arg.Unit f,
  " Do not automatically link C libraries specified in .cmxa files"
;;

let mk_nodynlink f =
  "-nodynlink", Arg.Unit f,
  " Enable optimizations for code that will not be dynlinked"
;;

let mk_nolabels f =
  "-nolabels", Arg.Unit f, " Ignore non-optional labels in types"
;;

let mk_noprompt f =
  "-noprompt", Arg.Unit f, " Suppress all prompts"
;;

let mk_nopromptcont f =
  "-nopromptcont", Arg.Unit f,
  " Suppress prompts for continuation lines of multi-line inputs"
;;

let mk_nostdlib f =
  "-nostdlib", Arg.Unit f,
  " Do not add default directory to the list of include directories"
;;

let mk_o f =
  "-o", Arg.String f, "<file>  Set output file name to <file>"
;;

let mk_output_obj f =
  "-output-obj", Arg.Unit f, " Output a C object file instead of an executable"
;;

let mk_p f =
  "-p", Arg.Unit f,
  " Compile and link with profiling support for \"gprof\"\n\
  \     (not supported on all platforms)"
;;

let mk_pack_byt f =
  "-pack", Arg.Unit f, " Package the given .cmo files into one .cmo"
;;

let mk_pack_opt f =
  "-pack", Arg.Unit f, " Package the given .cmx files into one .cmx"
;;

let mk_pp f =
  "-pp", Arg.String f, "<command>  Pipe sources through preprocessor <command>"
;;

let mk_principal f =
  "-principal", Arg.Unit f, " Check principality of type inference"
;;

let mk_rectypes f =
  "-rectypes", Arg.Unit f, " Allow arbitrary recursive types"
;;

let mk_runtime_variant f =
  "-runtime-variant", Arg.String f,
  "<str>  Use the <str> variant of the run-time system"
;;

let mk_S f =
  "-S", Arg.Unit f, " Keep intermediate assembly file"
;;

let mk_stdin f =
  "-stdin", Arg.Unit f, " Read script from standard input"
;;

let mk_strict_sequence f =
  "-strict-sequence", Arg.Unit f,
  " Left-hand part of a sequence must have type unit"
;;

let mk_shared f =
  "-shared", Arg.Unit f, " Produce a dynlinkable plugin"
;;

let mk_thread f =
  "-thread", Arg.Unit f,
  " Generate code that supports the system threads library"
;;

let mk_unsafe f =
  "-unsafe", Arg.Unit f,
  " Do not compile bounds checking on array and string access"
;;

let mk_use_runtime f =
  "-use-runtime", Arg.String f,
  "<file>  Generate bytecode for the given runtime system"
;;

let mk_use_runtime_2 f =
  "-use_runtime", Arg.String f,
  "<file>  (deprecated) same as -use-runtime"
;;

let mk_v f =
  "-v", Arg.Unit f,
  " Print compiler version and location of standard library and exit"
;;

let mk_version f =
  "-version", Arg.Unit f, " Print version and exit"
;;

let mk_vnum f =
  "-vnum", Arg.Unit f, " Print version number and exit"
;;

let mk_verbose f =
  "-verbose", Arg.Unit f, " Print calls to external commands"
;;

let mk_vmthread f =
  "-vmthread", Arg.Unit f,
  " Generate code that supports the threads library with VM-level\n\
  \     scheduling"
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

let mk_where f =
  "-where", Arg.Unit f, " Print location of standard library and exit"
;;

let mk_nopervasives f =
  "-nopervasives", Arg.Unit f, " (undocumented)"
;;

let mk_use_prims f =
  "-use-prims", Arg.String f, "<file>  (undocumented)"
;;

let mk_dparsetree f =
  "-dparsetree", Arg.Unit f, " (undocumented)"
;;

let mk_drawlambda f =
  "-drawlambda", Arg.Unit f, " (undocumented)"
;;

let mk_dlambda f =
  "-dlambda", Arg.Unit f, " (undocumented)"
;;

let mk_dclambda f =
  "-dclambda", Arg.Unit f, " (undocumented)"
;;

let mk_dinstr f =
  "-dinstr", Arg.Unit f, " (undocumented)"
;;

let mk_dcmm f =
  "-dcmm", Arg.Unit f, " (undocumented)"
;;

let mk_dsel f =
  "-dsel", Arg.Unit f, " (undocumented)"
;;

let mk_dcombine f =
  "-dcombine", Arg.Unit f, " (undocumented)"
;;

let mk_dlive f =
  "-dlive", Arg.Unit f, " (undocumented)"
;;

let mk_dspill f =
  "-dspill", Arg.Unit f, " (undocumented)"
;;

let mk_dsplit f =
  "-dsplit", Arg.Unit f, " (undocumented)"
;;

let mk_dinterf f =
  "-dinterf", Arg.Unit f, " (undocumented)"
;;

let mk_dprefer f =
  "-dprefer", Arg.Unit f, " (undocumented)"
;;

let mk_dalloc f =
  "-dalloc", Arg.Unit f, " (undocumented)"
;;

let mk_dreload f =
  "-dreload", Arg.Unit f, " (undocumented)"
;;

let mk_dscheduling f =
  "-dscheduling", Arg.Unit f, " (undocumented)"
;;

let mk_dlinear f =
  "-dlinear", Arg.Unit f, " (undocumented)"
;;

let mk_dstartup f =
  "-dstartup", Arg.Unit f, " (undocumented)"
;;

let mk__ f =
  "-", Arg.String f,
  "<file>  Treat <file> as a file name (even if it starts with `-')"
;;

module type Top_options = sig
  val _projectfind : string -> unit
  val _absname : unit -> unit
  val _I : string -> unit
  val _init : string -> unit
  val _labels : unit -> unit
  val _no_app_funct : unit -> unit
  val _noassert : unit -> unit
  val _nolabels : unit -> unit
  val _noprompt : unit -> unit
  val _nopromptcont : unit -> unit
  val _nostdlib : unit -> unit
  val _principal : unit -> unit
  val _rectypes : unit -> unit
  val _stdin: unit -> unit
  val _strict_sequence : unit -> unit
  val _unsafe : unit -> unit
  val _version : unit -> unit
  val _vnum : unit -> unit
  val _w : string -> unit
  val _warn_error : string -> unit
  val _warn_help : unit -> unit
  val _protocol : string -> unit

  val _dparsetree : unit -> unit
  val _drawlambda : unit -> unit
  val _dlambda : unit -> unit
  val _dinstr : unit -> unit

  val anonymous : string -> unit
end;;

module type Arg_list = sig
    val list : (string * Arg.spec * string) list
end;;

module Make_top_options (F : Top_options) =
struct
  let list = [
    mk_projectfind F._projectfind;
    mk_absname F._absname;
    mk_I F._I;
    mk_init F._init;
    mk_labels F._labels;
    mk_no_app_funct F._no_app_funct;
    mk_noassert F._noassert;
    mk_nolabels F._nolabels;
    mk_noprompt F._noprompt;
    mk_nopromptcont F._nopromptcont;
    mk_nostdlib F._nostdlib;
    mk_principal F._principal;
    mk_rectypes F._rectypes;
    mk_stdin F._stdin;
    mk_strict_sequence F._strict_sequence;
    mk_unsafe F._unsafe;
    mk_version F._version;
    mk_vnum F._vnum;
    mk_w F._w;
    mk_warn_error F._warn_error;
    mk_warn_help F._warn_help;
    mk_protocol F._protocol;

    mk_dparsetree F._dparsetree;
    mk_drawlambda F._drawlambda;
    mk_dlambda F._dlambda;
    mk_dinstr F._dinstr;

    mk__ F.anonymous;
  ]
end;;

