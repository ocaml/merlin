(* utils/config.generated.ml.  Generated from config.generated.ml.in by configure. *)
#2 "utils/config.generated.ml.in"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* This file is included in config_main.ml during the build rather
   than compiled on its own *)

let bindir = {|/usr/local/bin|}

let standard_library_default = {|/usr/local/lib/ocaml|}

let ccomp_type = {|cc|}
let c_compiler = {|gcc|}
let c_output_obj = {|-o |}
let c_has_debug_prefix_map = true
let as_has_debug_prefix_map = false
let ocamlc_cflags = {| -O2 -fno-strict-aliasing -fwrapv -pthread   -pthread|}
let ocamlc_cppflags = {| -D_FILE_OFFSET_BITS=64 |}
(* #7678: ocamlopt uses these only to compile .c files, and the behaviour for
          the two drivers should be identical. *)
let ocamlopt_cflags = {| -O2 -fno-strict-aliasing -fwrapv -pthread   -pthread|}
let ocamlopt_cppflags = {| -D_FILE_OFFSET_BITS=64 |}
let bytecomp_c_libraries = {| -L/opt/homebrew/opt/zstd/lib -lzstd   -lpthread|}
(* bytecomp_c_compiler and native_c_compiler have been supported for a
   long time and are retained for backwards compatibility.
   For programs that don't need compatibility with older OCaml releases
   the recommended approach is to use the constituent variables
   c_compiler, ocamlc_cflags, ocamlc_cppflags etc., directly.
*)
let bytecomp_c_compiler =
  c_compiler ^ " " ^ ocamlc_cflags ^ " " ^ ocamlc_cppflags
let native_c_compiler =
  c_compiler ^ " " ^ ocamlopt_cflags ^ " " ^ ocamlopt_cppflags
let native_c_libraries = {| -L/opt/homebrew/opt/zstd/lib -lzstd   -lpthread|}
let native_ldflags = {||}
let native_pack_linker = {|ld -r -o |}
let default_rpath = {||}
let mksharedlibrpath = {||}
let ar = {|ar|}
let supports_shared_libraries = true
let native_dynlink = true
let mkdll = {|gcc -shared -undefined dynamic_lookup -Wl,-w  |}
let mkexe = {|gcc -O2 -fno-strict-aliasing -fwrapv -pthread  -pthread  |}
let mkmaindll = {|gcc -shared -undefined dynamic_lookup -Wl,-w  |}

let flambda = false
let with_flambda_invariants = false
let with_cmm_invariants = false
let windows_unicode = 0 != 0

let flat_float_array = true

let function_sections = false
let afl_instrument = false

let native_compiler = true

let architecture = {|arm64|}
let model = {|default|}
let system = {|macosx|}

let asm = {|gcc -c -Wno-trigraphs|}
let asm_cfi_supported = true
let with_frame_pointers = false
let reserved_header_bits = 0

let ext_exe = {||}
let ext_obj = "." ^ {|o|}
let ext_asm = "." ^ {|s|}
let ext_lib = "." ^ {|a|}
let ext_dll = "." ^ {|so|}

let host = {|aarch64-apple-darwin23.0.0|}
let target = {|aarch64-apple-darwin23.0.0|}

let systhread_supported = true

let flexdll_dirs = []

let ar_supports_response_files = true

let tsan = false
(* utils/config.common.ml.  Generated from config.common.ml.in by configure. *)
#3 "utils/config.common.ml.in"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Portions of the Config module common to both the boot and main compiler. *)

(* The main OCaml version string has moved to ../build-aux/ocaml_version.m4 *)
let version = Sys.ocaml_version

let standard_library =
  try
    Sys.getenv "OCAMLLIB"
  with Not_found ->
  try
    Sys.getenv "CAMLLIB"
  with Not_found ->
    standard_library_default

let exec_magic_number = {magic|Caml1999X033|magic}
    (* exec_magic_number is duplicated in runtime/caml/exec.h *)
and cmi_magic_number = {magic|Caml1999I033|magic}
and cmo_magic_number = {magic|Caml1999O033|magic}
and cma_magic_number = {magic|Caml1999A033|magic}
and cmx_magic_number = {magic|Caml1999Y033|magic}
and cmxa_magic_number = {magic|Caml1999Z033|magic}
and ast_impl_magic_number = {magic|Caml1999M033|magic}
and ast_intf_magic_number = {magic|Caml1999N033|magic}
and cmxs_magic_number = {magic|Caml1999D033|magic}
and cmt_magic_number = {magic|Caml1999T033|magic}
and linear_magic_number = {magic|Caml1999L033|magic}

let safe_string = true
let default_safe_string = true
let naked_pointers = false

let interface_suffix = ref ".mli"

let max_tag = 243
(* This is normally the same as in obj.ml, but we have to define it
   separately because it can differ when we're in the middle of a
   bootstrapping phase. *)
let lazy_tag = 246

let max_young_wosize = 256
let stack_threshold = 32 (* see runtime/caml/config.h *)
let stack_safety_margin = 6
let default_executable_name =
  match Sys.os_type with
    "Unix" -> "a.out"
  | "Win32" | "Cygwin" -> "camlprog.exe"
  | _ -> "camlprog"
type configuration_value =
  | String of string
  | Int of int
  | Bool of bool

let configuration_variables () =
  let p x v = (x, String v) in
  let p_int x v = (x, Int v) in
  let p_bool x v = (x, Bool v) in
[
  p "version" version;
  p "standard_library_default" standard_library_default;
  p "standard_library" standard_library;
  p "ccomp_type" ccomp_type;
  p "c_compiler" c_compiler;
  p "ocamlc_cflags" ocamlc_cflags;
  p "ocamlc_cppflags" ocamlc_cppflags;
  p "ocamlopt_cflags" ocamlopt_cflags;
  p "ocamlopt_cppflags" ocamlopt_cppflags;
  p "bytecomp_c_compiler" bytecomp_c_compiler;
  p "native_c_compiler" native_c_compiler;
  p "bytecomp_c_libraries" bytecomp_c_libraries;
  p "native_c_libraries" native_c_libraries;
  p "native_ldflags" native_ldflags;
  p "native_pack_linker" native_pack_linker;
  p_bool "native_compiler" native_compiler;
  p "architecture" architecture;
  p "model" model;
  p_int "int_size" Sys.int_size;
  p_int "word_size" Sys.word_size;
  p "system" system;
  p "asm" asm;
  p_bool "asm_cfi_supported" asm_cfi_supported;
  p_bool "with_frame_pointers" with_frame_pointers;
  p "ext_exe" ext_exe;
  p "ext_obj" ext_obj;
  p "ext_asm" ext_asm;
  p "ext_lib" ext_lib;
  p "ext_dll" ext_dll;
  p "os_type" Sys.os_type;
  p "default_executable_name" default_executable_name;
  p_bool "systhread_supported" systhread_supported;
  p "host" host;
  p "target" target;
  p_bool "flambda" flambda;
  p_bool "safe_string" safe_string;
  p_bool "default_safe_string" default_safe_string;
  p_bool "flat_float_array" flat_float_array;
  p_bool "function_sections" function_sections;
  p_bool "afl_instrument" afl_instrument;
  p_bool "tsan" tsan;
  p_bool "windows_unicode" windows_unicode;
  p_bool "supports_shared_libraries" supports_shared_libraries;
  p_bool "native_dynlink" native_dynlink;
  p_bool "naked_pointers" naked_pointers;
  p_bool "compression_supported" (Marshal.compression_supported());

  p "exec_magic_number" exec_magic_number;
  p "cmi_magic_number" cmi_magic_number;
  p "cmo_magic_number" cmo_magic_number;
  p "cma_magic_number" cma_magic_number;
  p "cmx_magic_number" cmx_magic_number;
  p "cmxa_magic_number" cmxa_magic_number;
  p "ast_impl_magic_number" ast_impl_magic_number;
  p "ast_intf_magic_number" ast_intf_magic_number;
  p "cmxs_magic_number" cmxs_magic_number;
  p "cmt_magic_number" cmt_magic_number;
  p "linear_magic_number" linear_magic_number;
]

let print_config_value oc = function
  | String s ->
      Printf.fprintf oc "%s" s
  | Int n ->
      Printf.fprintf oc "%d" n
  | Bool p ->
      Printf.fprintf oc "%B" p

let print_config oc =
  let print (x, v) =
    Printf.fprintf oc "%s: %a\n" x print_config_value v in
  List.iter print (configuration_variables ());
  flush oc

let config_var x =
  match List.assoc_opt x (configuration_variables()) with
  | None -> None
  | Some v ->
      let s = match v with
        | String s -> s
        | Int n -> Int.to_string n
        | Bool b -> string_of_bool b
      in
      Some s

let merlin = false
