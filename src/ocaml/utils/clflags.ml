(** {0 OCaml compiler compatible command-line parameters} *)
let cmi_file = ref None
let include_dirs        = ref []
let hidden_include_dirs = ref []
let fast                = ref false
let classic             = ref false
let all_ppx             = ref []
let principal           = ref false
let real_paths          = ref true
let recursive_types     = ref false
let strict_sequence     = ref false
let applicative_functors = ref true

let nopervasives        = ref false
let strict_formats      = ref true
let open_modules        = ref []

let annotations         = ref false
let binary_annotations  = ref true
let store_occurrences   = ref true
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
let unboxed_types       = ref false

let locations = ref true

(* This is used by the -save-ir-after option. *)
module Compiler_ir = struct
  type t = Linear

  let all = [
    Linear;
  ]

  let extension t =
    let ext =
    match t with
      | Linear -> "linear"
    in
    ".cmir-" ^ ext

  (** [extract_extension_with_pass filename] returns the IR whose extension
      is a prefix of the extension of [filename], and the suffix,
      which can be used to distinguish different passes on the same IR.
      For example, [extract_extension_with_pass "foo.cmir-linear123"]
      returns [Some (Linear, "123")]. *)
  let extract_extension_with_pass filename =
    let ext = Filename.extension filename in
    let ext_len = String.length ext in
    if ext_len <= 0 then None
    else begin
      let is_prefix ir =
        let s = extension ir in
        let s_len = String.length s in
        s_len <= ext_len && s = String.sub ext 0 s_len
      in
      let drop_prefix ir =
        let s = extension ir in
        let s_len = String.length s in
        String.sub ext s_len (ext_len - s_len)
      in
      let ir = List.find_opt is_prefix all in
      match ir with
      | None -> None
      | Some ir -> Some (ir, drop_prefix ir)
    end
end


(* This is used by the -stop-after option. *)
module Compiler_pass = struct
  (* If you add a new pass, the following must be updated:
     - the variable `passes` below
     - the manpages in man/ocaml{c,opt}.m
     - the manual manual/src/cmds/unified-options.etex
  *)
  type t = Parsing | Typing | Lambda | Scheduling | Emit

  let to_string = function
    | Parsing -> "parsing"
    | Typing -> "typing"
    | Lambda -> "lambda"
    | Scheduling -> "scheduling"
    | Emit -> "emit"

  let of_string = function
    | "parsing" -> Some Parsing
    | "typing" -> Some Typing
    | "lambda" -> Some Lambda
    | "scheduling" -> Some Scheduling
    | "emit" -> Some Emit
    | _ -> None

  let rank = function
    | Parsing -> 0
    | Typing -> 1
    | Lambda -> 2
    | Scheduling -> 50
    | Emit -> 60

  let passes = [
    Parsing;
    Typing;
    Lambda;
    Scheduling;
    Emit;
  ]
  let is_compilation_pass _ = true
  let is_native_only = function
    | Scheduling -> true
    | Emit -> true
    | _ -> false

  let enabled is_native t = not (is_native_only t) || is_native
  let can_save_ir_after = function
    | Scheduling -> true
    | _ -> false

  let available_pass_names ~filter ~native =
    passes
    |> List.filter (enabled native)
    |> List.filter filter
    |> List.map to_string

  let compare a b =
    compare (rank a) (rank b)

  let to_output_filename t ~prefix =
    match t with
    | Scheduling -> prefix ^ Compiler_ir.(extension Linear)
    | _ -> Misc.fatal_error "Not supported"

  let of_input_filename name =
    match Compiler_ir.extract_extension_with_pass name with
    | Some (Linear, _) -> Some Emit
    | None -> None
end

let stop_after = ref None
