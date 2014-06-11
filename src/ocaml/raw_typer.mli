open Location
open Parsetree

type item =
  | Structure of structure
  | Signature of signature
  | Pattern of (Asttypes.label * expression option * pattern)
  | Eval of expression
  | Bindings of Asttypes.rec_flag * value_binding list
  | Newtype of string
  | Functor_argument of string loc * module_type option
  | Open of Asttypes.override_flag * Longident.t loc

type t
val empty : t
val step : Raw_parser.symbol -> t -> t

val observe : t -> item list

val dump : Format.formatter -> t -> unit
