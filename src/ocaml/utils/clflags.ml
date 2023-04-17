(** {0 OCaml compiler compatible command-line parameters} *)
let cmi_file = ref None
let include_dirs        = ref []
let fast                = ref false
let classic             = ref false
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
