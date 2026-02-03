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
let target_bindir = {|/usr/local/bin|}

let ccomp_type = {|cc|}
let c_compiler = {|gcc|}
let c_compiler_vendor = {|clang-17-0|}
let c_output_obj = {|-o |}
let c_has_debug_prefix_map = true
let as_has_debug_prefix_map = false
let as_is_cc = true
let bytecode_cflags = {|-O2 -fno-strict-aliasing -fwrapv   -pthread |}
let bytecode_cppflags = {| -D_FILE_OFFSET_BITS=64 |}
let native_cflags = {|-O2 -fno-strict-aliasing -fwrapv   -pthread |}
let native_cppflags = {| -D_FILE_OFFSET_BITS=64 |}

let bytecomp_c_libraries = {|-L/opt/homebrew/opt/zstd/lib -lzstd    -lpthread|}
(* bytecomp_c_compiler and native_c_compiler have been supported for a
   long time and are retained for backwards compatibility.
   For programs that don't need compatibility with older OCaml releases
   the recommended approach is to use the constituent variables
   c_compiler, {bytecode,native}_c[pp]flags etc. directly.
*)
let bytecomp_c_compiler =
  c_compiler ^ " " ^ bytecode_cflags ^ " " ^ bytecode_cppflags
let native_c_compiler =
  c_compiler ^ " " ^ native_cflags ^ " " ^ native_cppflags
let native_c_libraries = {|   -lpthread|}
let compression_c_libraries = {|-L/opt/homebrew/opt/zstd/lib -lzstd|}
let native_ldflags = {||}
let with_nonexecstack_note = false
let native_pack_linker = {|ld -r -o |}
let default_rpath = {||}
let mksharedlibrpath = {||}
let ar = {|ar|}
let supports_shared_libraries = true
let native_dynlink = true
let mkdll = {|gcc -shared -undefined dynamic_lookup -Wl,-w |}
let mkexe = {|gcc  |}
let mkmaindll = {|gcc -shared -undefined dynamic_lookup -Wl,-w |}

let flambda = false
let with_flambda_invariants = false
let with_cmm_invariants = false
let with_codegen_invariants = true
let windows_unicode = 0 != 0

let flat_float_array = true
let align_double = false
let align_int64 = false

let function_sections = false
let afl_instrument = false

let bytecode_runtime_id = {|b100|}
let native_runtime_id = {|b100|}

let native_compiler = true

let architecture = {|arm64|}
let model = {|default|}
let system = {|macosx|}
let target_os_type = {|Unix|}

let asm = {|gcc -c -Wno-trigraphs|}
let asm_cfi_supported = true
let asm_size_type_directives = false
let asm_dwarf_version = Some 5
let with_frame_pointers = false
let reserved_header_bits = 0

let ext_exe = {||}
let ext_obj = "." ^ {|o|}
let ext_asm = "." ^ {|s|}
let ext_lib = "." ^ {|a|}
let ext_dll = "." ^ {|so|}

let host = {|aarch64-apple-darwin24.5.0|}
let target = {|aarch64-apple-darwin24.5.0|}

let systhread_supported = true

let flexdll_dirs = []

let ar_supports_response_files = false

let tsan = false

let shebangscripts = true

let suffixing = true

let launch_method = {|sh|}

let search_method = {||}
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

(* is_official_release and release_number are automatically updated autoconf
   from values in ../build-aux/ocaml_version.m4 - do not edit these lines
   directly. *)
let is_official_release = false
let release_number = 21

external standard_library_default : unit -> string = "%standard_library_default"

let standard_library_default_raw = standard_library_default ()

external stdlib_dirs : string -> string * string option
   = "caml_sys_get_stdlib_dirs"

let standard_library_default, relative_root_dir =
  stdlib_dirs standard_library_default_raw

let standard_library_relative =
  if relative_root_dir = None then
    None
  else
    Some standard_library_default_raw

let standard_library =
  try
    Sys.getenv "OCAMLLIB"
  with Not_found ->
  try
    Sys.getenv "CAMLLIB"
  with Not_found ->
    standard_library_default

let bindir = Option.value ~default:bindir relative_root_dir
let target_bindir =
  if target_bindir = Filename.current_dir_name then
    Filename.dirname Sys.executable_name
  else
    target_bindir

let exec_magic_number = {magic|Caml1999X037|magic}
    (* exec_magic_number is duplicated in runtime/caml/exec.h *)
and cmi_magic_number = {magic|Caml1999I037|magic}
and cmo_magic_number = {magic|Caml1999O037|magic}
and cma_magic_number = {magic|Caml1999A037|magic}
and cmx_magic_number = {magic|Caml1999Y037|magic}
and cmxa_magic_number = {magic|Caml1999Z037|magic}
and ast_impl_magic_number = {magic|Caml1999M037|magic}
and ast_intf_magic_number = {magic|Caml1999N037|magic}
and cmxs_magic_number = {magic|Caml1999D037|magic}
and cmt_magic_number = {magic|Caml1999T037|magic}
and linear_magic_number = {magic|Caml1999L037|magic}

let safe_string = true
let default_safe_string = true
let naked_pointers = false

type launch_method = Executable | Shebang of string option
type search_method = Disable | Fallback | Enable

let launch_method =
  match launch_method with
  | "exe" -> Executable
  | "sh" -> Shebang None
  | _ -> Shebang (Some launch_method)

let search_method =
  match search_method with
  | "enable" -> Enable
  | "fallback" -> Fallback
  | _ -> Disable

let interface_suffix = ref ".mli"

let max_tag = 243
(* This is normally the same as in obj.ml, but we have to define it
   separately because it can differ when we're in the middle of a
   bootstrapping phase. *)
let lazy_tag = 246

let max_young_wosize = 256
let stack_threshold = 32 (* see runtime/caml/config.h *)
let stack_safety_margin = 6
let target_unix = (target_os_type = "Unix")
let target_win32 = (target_os_type = "Win32")
let target_cygwin = (target_os_type = "Cygwin")
let default_executable_name =
  if target_unix then
    "a.out"
  else if target_win32 || target_cygwin then
    "camlprog.exe"
  else
    "camlprog"
type configuration_value =
  | String of string
  | Int of int
  | Bool of bool

let configuration_variables () =
  let p x v = (x, String v) in
  let p_int x v = (x, Int v) in
  let p_bool x v = (x, Bool v) in
  let standard_library_relative =
    Option.value ~default:"" standard_library_relative
  in
[
  p "version" version;
  p "standard_library_default" standard_library_default;
  p "standard_library_relative" standard_library_relative;
  p "standard_library" standard_library;
  p "ccomp_type" ccomp_type;
  p "c_compiler" c_compiler;
  p "bytecode_cflags" bytecode_cflags;
  p "ocamlc_cflags" bytecode_cflags;
  p "bytecode_cppflags" bytecode_cppflags;
  p "ocamlc_cppflags" bytecode_cppflags;
  p "native_cflags" native_cflags;
  p "ocamlopt_cflags" native_cflags;
  p "native_cppflags" native_cppflags;
  p "ocamlopt_cppflags" native_cppflags;
  p "bytecomp_c_compiler" bytecomp_c_compiler;
  p "native_c_compiler" native_c_compiler;
  p "bytecomp_c_libraries" bytecomp_c_libraries;
  p "native_c_libraries" native_c_libraries;
  p "compression_c_libraries" compression_c_libraries;
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
  p_bool "asm_size_type_directives" asm_size_type_directives;
  p_bool "with_frame_pointers" with_frame_pointers;
  p_bool "with_nonexecstack_note" with_nonexecstack_note;
  p "ext_exe" ext_exe;
  p "ext_obj" ext_obj;
  p "ext_asm" ext_asm;
  p "ext_lib" ext_lib;
  p "ext_dll" ext_dll;
  p "os_type" target_os_type;
  p "default_executable_name" default_executable_name;
  p_bool "systhread_supported" systhread_supported;
  p "host" host;
  p "target" target;
  p "bytecode_runtime_id" bytecode_runtime_id;
  p "native_runtime_id" native_runtime_id;
  p_bool "flambda" flambda;
  p_bool "safe_string" safe_string;
  p_bool "default_safe_string" default_safe_string;
  p_bool "flat_float_array" flat_float_array;
  p_bool "align_double" align_double;
  p_bool "align_int64" align_int64;
  p_bool "function_sections" function_sections;
  p_bool "afl_instrument" afl_instrument;
  p_bool "tsan" tsan;
  p_bool "windows_unicode" windows_unicode;
  p_bool "supports_shared_libraries" supports_shared_libraries;
  p_bool "native_dynlink" native_dynlink;
  p_bool "naked_pointers" naked_pointers;
  p_bool "with_codegen_invariants" with_codegen_invariants;

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

(***********************************************************************)
(**                                                                   **)
(**               WARNING WARNING WARNING                             **)
(**                                                                   **)
(** When you change this file, you must make the parallel change      **)
(** in config.mlbuild                                                 **)
(**                                                                   **)
(***********************************************************************)


(* The main OCaml version string has moved to ../VERSION *)
let version = Sys.ocaml_version

let flambda = false

let ext_obj = ".o_The boot compiler cannot process C objects"

let exec_magic_number = "Caml1999X036"
    (* exec_magic_number is duplicated in runtime/caml/exec.h *)
and cmi_magic_number = "Caml1999I036"
and cmo_magic_number = "Caml1999O036"
and cma_magic_number = "Caml1999A036"
and cmx_magic_number =
  if flambda then
    "Caml1999y036"
  else
    "Caml1999Y036"
and cmxa_magic_number =
  if flambda then
    "Caml1999z036"
  else
    "Caml1999Z036"
and ast_impl_magic_number = "Caml1999M036"
and ast_intf_magic_number = "Caml1999N036"
and cmxs_magic_number = "Caml1999D036"
and cmt_magic_number = "Caml1999T036"
and index_magic_number = "Merl2023I004"

let interface_suffix = ref ".mli"
let flat_float_array = true

let max_tag = 245

let merlin = true
