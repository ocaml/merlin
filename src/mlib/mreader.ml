type t = Mconfig.t

let make trace config source = config

let make_for_completion trace config source pos =
  (`No_labels false, config)

let release _ = ()

let dump _ = `Null

(* Accessors *)

type parsetree = [
  | `Interface of Parsetree.signature
  | `Implementation of Parsetree.structure
]

let get_parsetree _ =
  `Implementation []

let get_errors _ = []

let get_config config = config

(* Pretty-printing *)

type pretty_parsetree = Extend_protocol.Reader.pretty_parsetree
type outcometree = Extend_protocol.Reader.outcometree

let print_pretty _ _ = ""
let print_outcome _ _ = ""
let print_batch_outcome _ _ = []
