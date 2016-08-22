open Std

type parsetree = [
  | `Interface of Parsetree.signature
  | `Implementation of Parsetree.structure
]

type comment = (string * Location.t)

type result = {
  config        : Mconfig.t;
  lexer_errors  : exn list;
  parser_errors : exn list;
  comments      : comment list;
  parsetree     : parsetree;
  no_labels_for_completion : bool;
}

(* Entry points *)

let run ?for_completion _trace config source =
  let kind =
    let filename = Msource.filename source in
    let extension =
      match String.rindex filename '.' with
      | exception Not_found -> ""
      | pos -> String.sub ~pos ~len:(String.length filename - pos) filename
    in
    Logger.logf "reader" "run" "extension(%S) = %S" filename extension;
    match extension with
    | ".mli" -> Mreader_parser.MLI
    | _ -> Mreader_parser.ML
  in
  Mocaml.setup_config config;
  let lexer =
    let keywords = Extension.keywords Mconfig.(config.merlin.extensions) in
    Mreader_lexer.make Mconfig.(config.ocaml.warnings) keywords source
  in
  let no_labels_for_completion, lexer = match for_completion with
    | None -> false, lexer
    | Some pos ->
      let pos = Msource.get_lexing_pos source pos in
      Mreader_lexer.for_completion lexer pos
  in
  let parser = Mreader_parser.make Mconfig.(config.ocaml.warnings) lexer kind in
  let lexer_errors = Mreader_lexer.errors lexer
  and parser_errors = Mreader_parser.errors parser
  and parsetree = Mreader_parser.result parser
  and comments = Mreader_lexer.comments lexer
  in
  { config; lexer_errors; parser_errors; comments; parsetree;
    no_labels_for_completion; }

(* Pretty-printing *)

type pretty_parsetree = Extend_protocol.Reader.pretty_parsetree
type outcometree = Extend_protocol.Reader.outcometree

let print_pretty _ _ = ""
let print_outcome mconfig tree =
  Mocaml.default_printer Format.str_formatter tree;
  Format.flush_str_formatter ()

let print_batch_outcome mconfig tree =
  List.map (print_outcome mconfig) tree
