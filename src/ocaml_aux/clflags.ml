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

let make () =
  {
    objfiles             = [];    (* .cmo and .cma files *)
    ccobjs               = [];    (* .o, .a, .so and -cclib -lxxx *)
    dllibs               = [];    (* .so and -dllib -lxxx *)

    compile_only         = false; (* -c *)
    output_name          = None;  (* -o *)
    include_dirs         = [];    (* -I *)
    no_std_include       = false; (* -nostdlib *)
    print_types          = false; (* -i *)
    make_archive         = false; (* -a *)
    debug                = false; (* -g *)
    fast                 = false; (* -unsafe *)

    link_everything      = false; (* -linkall *)
    custom_runtime       = false; (* -custom *)
    output_c_object      = false; (* -output-obj *)
    ccopts               = [];    (* -ccopt *)
    classic              = false; (* -nolabels *)
    nopervasives         = false; (* -nopervasives *)
    preprocessor         = None;  (* -pp *)
    annotations          = false; (* -annot *)
    binary_annotations   = false; (* -annot *)

    use_threads          = false; (* -thread *)
    use_vmthreads        = false; (* -vmthread *)

    noassert             = false; (* -noassert *)
    verbose              = false; (* -verbose *)
    noprompt             = false; (* -noprompt *)
    nopromptcont         = false; (* -nopromptcont *)
    init_file            = None;  (* -init *)
    use_prims            = "";    (* -use-prims ... *)
    use_runtime          = "";    (* -use-runtime ... *)
    principal            = false; (* -principal *)
    real_paths           = false; (* -real-paths *)
    recursive_types      = false; (* -rectypes *)
    strict_sequence      = false; (* -strict-sequence *)
    applicative_functors = true;  (* -no-app-funct *)
    make_runtime         = false; (* -make-runtime *)
    gprofile             = false; (* -p *)

    c_compiler           = None;  (* -cc *)
    no_auto_link         = false; (* -noautolink *)
    dllpaths             = [];    (* -dllpath *)
    make_package         = false; (* -pack *)
    for_package          = None;  (* -for-pack *)
    error_size           = 500;   (* -error-size *)
    dump_parsetree       = false; (* -dparsetree *)
    dump_rawlambda       = false; (* -drawlambda *)
    dump_lambda          = false; (* -dlambda *)
    dump_clambda         = false; (* -dclambda *)
    dump_instr           = false; (* -dinstr *)

    keep_asm_file        = false; (* -S *)
    optimize_for_speed   = true;  (* -compact *)

    dump_cmm             = false; (* -dcmm *)
    dump_selection       = false; (* -dsel *)
    dump_live            = false; (* -dlive *)
    dump_spill           = false; (* -dspill *)
    dump_split           = false; (* -dsplit *)
    dump_interf          = false; (* -dinterf *)
    dump_prefer          = false; (* -dprefer *)
    dump_regalloc        = false; (* -dalloc *)
    dump_reload          = false; (* -dreload *)
    dump_scheduling      = false; (* -dscheduling *)
    dump_linear          = false; (* -dlinear *)
    keep_startup_file    = false; (* -dstartup *)
    dump_combine         = false; (* -dcombine *)

    native_code          = false; (* set to true under ocamlopt *)
    inline_threshold     = 10;

    dont_write_files     = false; (* set to true under ocamldoc *)

    shared               = false; (* -shared *)
    dlcode               = true; (* not -nodynlink *)
    runtime_variant      = "";
  }

let t = ref (make ())

let objfiles             () = !t.objfiles
let ccobjs               () = !t.ccobjs
let dllibs               () = !t.dllibs
let compile_only         () = !t.compile_only
let output_name          () = !t.output_name
let include_dirs         () = !t.include_dirs
let no_std_include       () = !t.no_std_include
let print_types          () = !t.print_types
let make_archive         () = !t.make_archive
let debug                () = !t.debug
let fast                 () = !t.fast
let link_everything      () = !t.link_everything
let custom_runtime       () = !t.custom_runtime
let output_c_object      () = !t.output_c_object
let ccopts               () = !t.ccopts
let classic              () = !t.classic
let nopervasives         () = !t.nopervasives
let preprocessor         () = !t.preprocessor
let annotations          () = !t.annotations
let binary_annotations   () = !t.binary_annotations
let use_threads          () = !t.use_threads
let use_vmthreads        () = !t.use_vmthreads
let noassert             () = !t.noassert
let verbose              () = !t.verbose
let noprompt             () = !t.noprompt
let nopromptcont         () = !t.nopromptcont
let init_file            () = !t.init_file
let use_prims            () = !t.use_prims
let use_runtime          () = !t.use_runtime
let principal            () = !t.principal
let real_paths           () = !t.real_paths
let recursive_types      () = !t.recursive_types
let strict_sequence      () = !t.strict_sequence
let applicative_functors () = !t.applicative_functors
let make_runtime         () = !t.make_runtime
let gprofile             () = !t.gprofile
let c_compiler           () = !t.c_compiler
let no_auto_link         () = !t.no_auto_link
let dllpaths             () = !t.dllpaths
let make_package         () = !t.make_package
let for_package          () = !t.for_package
let error_size           () = !t.error_size
let dump_parsetree       () = !t.dump_parsetree
let dump_rawlambda       () = !t.dump_rawlambda
let dump_lambda          () = !t.dump_lambda
let dump_clambda         () = !t.dump_clambda
let dump_instr           () = !t.dump_instr
let keep_asm_file        () = !t.keep_asm_file
let optimize_for_speed   () = !t.optimize_for_speed
let dump_cmm             () = !t.dump_cmm
let dump_selection       () = !t.dump_selection
let dump_live            () = !t.dump_live
let dump_spill           () = !t.dump_spill
let dump_split           () = !t.dump_split
let dump_interf          () = !t.dump_interf
let dump_prefer          () = !t.dump_prefer
let dump_regalloc        () = !t.dump_regalloc
let dump_reload          () = !t.dump_reload
let dump_scheduling      () = !t.dump_scheduling
let dump_linear          () = !t.dump_linear
let keep_startup_file    () = !t.keep_startup_file
let dump_combine         () = !t.dump_combine
let native_code          () = !t.native_code
let inline_threshold     () = !t.inline_threshold
let dont_write_files     () = !t.dont_write_files
let shared               () = !t.shared
let dlcode               () = !t.dlcode
let runtime_variant      () = !t.runtime_variant

let std_include_dir () =
  if !t.no_std_include
  then []
  else [Config.standard_library]

