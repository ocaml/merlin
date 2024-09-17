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
<<<<<<<
=======
let dump_parsetree = ref false          (* -dparsetree *)
and dump_typedtree = ref false          (* -dtypedtree *)
and dump_shape = ref false              (* -dshape *)
and dump_matchcomp = ref false          (* -dmatchcomp *)
and dump_rawlambda = ref false          (* -drawlambda *)
and dump_lambda = ref false             (* -dlambda *)
and dump_rawclambda = ref false         (* -drawclambda *)
>>>>>>>
<<<<<<<
=======

and dump_cmm = ref false                (* -dcmm *)
let dump_selection = ref false          (* -dsel *)
let dump_combine = ref false            (* -dcombine *)
let dump_cse = ref false                (* -dcse *)
let dump_live = ref false               (* -dlive *)
let dump_spill = ref false              (* -dspill *)
let dump_split = ref false              (* -dsplit *)
let dump_interf = ref false             (* -dinterf *)
let dump_prefer = ref false             (* -dprefer *)
let dump_interval = ref false           (* -dinterval *)
let dump_regalloc = ref false           (* -dalloc *)
let dump_reload = ref false             (* -dreload *)
let dump_scheduling = ref false         (* -dscheduling *)
let dump_linear = ref false             (* -dlinear *)
let keep_startup_file = ref false       (* -dstartup *)
let profile_columns : Profile.column list ref = ref [] (* -dprofile/-dtimings *)

let native_code = ref false             (* set to true under ocamlopt *)
>>>>>>>
<<<<<<<
=======
let dlcode = ref true (* not -nodynlink *)

let pic_code = ref (match Config.architecture with (* -fPIC *)
                     | "amd64" | "s390x" -> true
                     | _                 -> false)

let runtime_variant = ref ""

>>>>>>>
<<<<<<<
=======
  in
  save_ir_after := new_passes

module Dump_option = struct
  type t =
    | Source
    | Parsetree
    | Typedtree
    | Shape
    | Match_comp
    | Raw_lambda
    | Lambda
    | Instr
    | Raw_clambda
    | Clambda
    | Raw_flambda
    | Flambda
    | Cmm
    | Selection
    | Combine
    | CSE
    | Live
    | Spill
    | Split
    | Interf
    | Prefer
    | Regalloc
    | Scheduling
    | Linear
    | Interval

  let compare (op1 : t) op2 =
    Stdlib.compare op1 op2

  let to_string = function
    | Source -> "source"
    | Parsetree -> "parsetree"
    | Typedtree -> "typedtree"
    | Shape -> "shape"
    | Match_comp -> "matchcomp"
    | Raw_lambda -> "rawlambda"
    | Lambda -> "lambda"
    | Instr -> "instr"
    | Raw_clambda -> "rawclambda"
    | Clambda -> "clambda"
    | Raw_flambda -> "rawflambda"
    | Flambda -> "flambda"
    | Cmm -> "cmm"
    | Selection -> "selection"
    | Combine -> "combine"
    | CSE -> "cse"
    | Live -> "live"
    | Spill -> "spill"
    | Split -> "split"
    | Interf -> "interf"
    | Prefer -> "prefer"
    | Regalloc -> "regalloc"
    | Scheduling -> "scheduling"
    | Linear -> "linear"
    | Interval -> "interval"

  let of_string = function
    | "source" -> Some Source
    | "parsetree" -> Some Parsetree
    | "typedtree" -> Some Typedtree
    | "shape" -> Some Shape
    | "matchcomp" -> Some Match_comp
    | "rawlambda" -> Some Raw_lambda
    | "lambda" -> Some Lambda
    | "instr" -> Some Instr
    | "rawclambda" -> Some Raw_clambda
    | "clambda" -> Some Clambda
    | "rawflambda" -> Some Raw_flambda
    | "flambda" -> Some Flambda
    | "cmm" -> Some Cmm
    | "selection" -> Some Selection
    | "combine" -> Some Combine
    | "cse" -> Some CSE
    | "live" -> Some Live
    | "spill" -> Some Spill
    | "split" -> Some Split
    | "interf" -> Some Interf
    | "prefer" -> Some Prefer
    | "regalloc" -> Some Regalloc
    | "scheduling" -> Some Scheduling
    | "linear" -> Some Linear
    | "interval" -> Some Interval
    | _ -> None

  let flag = function
    | Source -> dump_source
    | Parsetree -> dump_parsetree
    | Typedtree -> dump_typedtree
    | Shape -> dump_shape
    | Match_comp -> dump_matchcomp
    | Raw_lambda -> dump_rawlambda
    | Lambda -> dump_lambda
    | Instr -> dump_instr
    | Raw_clambda -> dump_rawclambda
    | Clambda -> dump_clambda
    | Raw_flambda -> dump_rawflambda
    | Flambda -> dump_flambda
    | Cmm -> dump_cmm
    | Selection -> dump_selection
    | Combine -> dump_combine
    | CSE -> dump_cse
    | Live -> dump_live
    | Spill -> dump_spill
    | Split -> dump_split
    | Interf -> dump_interf
    | Prefer -> dump_prefer
    | Regalloc -> dump_regalloc
    | Scheduling -> dump_scheduling
    | Linear -> dump_linear
    | Interval -> dump_interval

  type middle_end =
    | Flambda
    | Any
    | Closure

  type class_ =
    | Frontend
    | Bytecode
    | Middle of middle_end
    | Backend

  let _ =
    (* no Closure-specific dump option for now, silence a warning *)
    Closure

  let classify : t -> class_ = function
    | Source
    | Parsetree
    | Typedtree
    | Shape
    | Match_comp
    | Raw_lambda
    | Lambda
      -> Frontend
    | Instr
      -> Bytecode
    | Raw_clambda
    | Clambda
      -> Middle Any
    | Raw_flambda
    | Flambda
      -> Middle Flambda
    | Cmm
    | Selection
    | Combine
    | CSE
    | Live
    | Spill
    | Split
    | Interf
    | Prefer
    | Regalloc
    | Scheduling
    | Linear
    | Interval
      -> Backend

  let available (option : t) : (unit, string) result =
    let pass = Result.ok () in
    let ( let* ) = Result.bind in
    let fail descr =
      Error (
        Printf.sprintf
          "this compiler does not support %s-specific options"
          descr
      ) in
    let guard descr cond =
      if cond then pass
      else fail descr in
    let check_bytecode = guard "bytecode" (not !native_code) in
    let check_native = guard "native" !native_code in
    let check_middle_end = function
      | Flambda -> guard "flambda" Config.flambda
      | Closure -> guard "closure" (not Config.flambda)
      | Any -> pass
    in
    match classify option with
    | Frontend ->
        pass
    | Bytecode ->
        check_bytecode
    | Middle middle_end ->
        let* () = check_native in
        check_middle_end middle_end
    | Backend ->
        check_native
end

module String = Misc.Stdlib.String

let arg_spec = ref []
>>>>>>>
