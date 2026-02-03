#2 "utils/config.fixed.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       David Allsopp, Tarides UK.                       *)
(*                                                                        *)
(*   Copyright 2022 David Allsopp Ltd.                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Configuration for the boot compiler. The compiler should refuse to bootstrap
   if configured with values which would contradict the configuration below.
   The values below are picked to trigger errors if accidentally used in the
   compiler (e.g. for the C compiler). *)

let boot_cannot_call s = "/ The boot compiler should not call " ^ s

let bindir = "/tmp"
let target_bindir = bindir
let ccomp_type = "n/a"
let c_compiler = boot_cannot_call "the C compiler"
let c_compiler_vendor = ""
let c_output_obj = ""
let c_has_debug_prefix_map = false
let as_has_debug_prefix_map = false
let as_is_cc = false
let bytecode_cflags = ""
let bytecode_cppflags = ""
let native_cflags = ""
let native_cppflags = ""
let bytecomp_c_libraries = ""
let bytecomp_c_compiler = ""
let native_c_compiler = c_compiler
let native_c_libraries = ""
let compression_c_libraries = ""
let native_ldflags = ""
let with_nonexecstack_note = false
let native_pack_linker = boot_cannot_call "the linker"
let default_rpath = ""
let mksharedlibrpath = ""
let ar = boot_cannot_call "ar"
let supports_shared_libraries = false
let native_dynlink = false
let mkdll = native_pack_linker
let mkexe = native_pack_linker
let mkmaindll = native_pack_linker
let flambda = false
let with_flambda_invariants = false
let with_cmm_invariants = false
let with_codegen_invariants = false
let windows_unicode = false
let flat_float_array = true
let align_double = true
let align_int64 = true
let function_sections = false
let afl_instrument = false
let bytecode_runtime_id = ""
let native_runtime_id = ""
let native_compiler = false
let tsan = false
let architecture = "none"
let model = "default"
let system = "unknown"
let target_os_type =
  "The boot compiler should not be using Config.target_os_type"
let asm = boot_cannot_call "the assembler"
let asm_cfi_supported = false
let asm_size_type_directives = false
let asm_dwarf_version = None
let with_frame_pointers = false
let reserved_header_bits = 0
let ext_exe = ".ex_The boot compiler should not be using Config.ext_exe"
let ext_obj = ".o_The boot compiler cannot process C objects"
let ext_asm = ".s_The boot compiler should not be using Config.ext_asm"
let ext_lib = ".a_The boot compiler cannot process C libraries"
let ext_dll = ".so_The boot compiler cannot load DLLs"
let host = "zinc-boot-ocaml"
let target = host
let systhread_supported = false
let flexdll_dirs = []
let ar_supports_response_files = true
let shebangscripts = false
let suffixing = false
let launch_method = "sh"
let search_method = "always"
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
