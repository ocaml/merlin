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
