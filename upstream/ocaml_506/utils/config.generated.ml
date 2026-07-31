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
let c_compiler_vendor = {|gcc-15-2|}
let c_output_obj = {|-o |}
let c_has_debug_prefix_map = true
let as_has_debug_prefix_map = true
let as_is_cc = false
let bytecode_cflags = {|-O2 -fno-strict-aliasing -fwrapv -fPIC  -pthread |}
let bytecode_cppflags = {| -D_FILE_OFFSET_BITS=64 |}
let native_cflags = {|-O2 -fno-strict-aliasing -fwrapv -fPIC  -pthread |}
let native_cppflags = {| -D_FILE_OFFSET_BITS=64 |}

let bytecomp_c_libraries = {|-lzstd  -lm  -lpthread|}
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
let native_c_libraries = {| -lm  -lpthread|}
let compression_c_libraries = {|-lzstd|}
let native_ldflags = {||}
let with_nonexecstack_note = true
let native_pack_linker = {|ld -r -o |}
let default_rpath = {|-Wl,-rpath,|}
let mksharedlibrpath = {|-Wl,-rpath,|}
let ar = {|ar|}
let supports_shared_libraries = true
let native_dynlink = true
let mkdll = {|gcc -shared |}
let mkexe = {|gcc  -Wl,-E |}
let mkmaindll = {|gcc -shared |}

let flambda = false
let with_flambda_invariants = false
let with_cmm_invariants = false
let with_codegen_invariants = true
let windows_unicode = 0 != 0

let flat_float_array = true
let align_double = false
let align_int64 = false

let function_sections = true
let afl_instrument = false

let bytecode_runtime_id = {|d100|}
let native_runtime_id = {|d100|}

let native_compiler = true

let architecture = {|amd64|}
let model = {|default|}
let system = {|linux|}
let target_os_type = {|Unix|}

let asm = {|as|}
let asm_cfi_supported = true
let asm_size_type_directives = true
let asm_dwarf_version = None
let with_frame_pointers = false
let reserved_header_bits = 0

let ext_exe = {||}
let ext_obj = "." ^ {|o|}
let ext_asm = "." ^ {|s|}
let ext_lib = "." ^ {|a|}
let ext_dll = "." ^ {|so|}

let host = {|x86_64-pc-linux-gnu|}
let target = {|x86_64-pc-linux-gnu|}

let systhread_supported = true

let flexdll_dirs = []

let ar_supports_response_files = true

let tsan = false

let shebangscripts = true

let suffixing = true

let launch_method = {|sh|}

let search_method = {||}
