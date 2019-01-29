(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2019  Merlin contributors

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

open MenhirSdk
open Cmly_format

include Cmly_read.Read(struct let filename = Sys.argv.(1) end)

let is_attribute names attr =
  List.exists (fun l -> Attribute.has_label l attr) names

let printf = Printf.printf
let sprintf = Printf.sprintf

let menhir = "MenhirInterpreter"

(** Print header, if any *)

let print_header () =
  let name = Filename.chop_extension (Filename.basename Sys.argv.(1)) in
  printf "open %s\n" (String.capitalize name);
  List.iter
    (fun attr ->
       if is_attribute ["header"; "printer.header"] attr then
         printf "%s\n" (Attribute.payload attr))
    Grammar.attributes

(** Printer from attributes *)

let symbol_printer default attribs =
  match List.find (is_attribute ["symbol"]) attribs with
  | attr -> Attribute.payload attr
  | exception Not_found ->
    sprintf "%S" default

let print_symbol () =
  let case_t t =
    match Terminal.kind t with
    | `REGULAR | `ERROR | `EOF ->
      printf "  | %s.X (%s.T %s.T_%s) -> %s\n"
        menhir menhir menhir
        (Terminal.name t)
        (symbol_printer (Terminal.name t) (Terminal.attributes t))
    | `PSEUDO -> ()
  and case_n n =
    match Nonterminal.kind n with
    | `REGULAR ->
      printf "  | %s.X (%s.N %s.N_%s) -> %s\n"
        menhir menhir menhir
        (Nonterminal.mangled_name n)
        (symbol_printer (Nonterminal.mangled_name n) (Nonterminal.attributes n))
    | `START -> ()
  in
  printf "let print_symbol = function\n";
  Terminal.iter case_t;
  Nonterminal.iter case_n

let value_printer default attribs =
  match List.find (is_attribute ["printer"]) attribs with
  | attr -> sprintf "(%s)" (Attribute.payload attr)
  | exception Not_found ->
    sprintf "(fun _ -> %s)" (symbol_printer default attribs)

let print_value () =
  let case_t t =
    match Terminal.kind t with
    | `REGULAR | `ERROR | `EOF->
      printf "  | %s.T %s.T_%s -> %s\n"
        menhir menhir
        (Terminal.name t)
        (value_printer (Terminal.name t) (Terminal.attributes t))
    | `PSEUDO -> ()
  and case_n n =
    match Nonterminal.kind n with
    | `REGULAR ->
      printf "  | %s.N %s.N_%s -> %s\n"
        menhir menhir
        (Nonterminal.mangled_name n)
        (value_printer (Nonterminal.mangled_name n) (Nonterminal.attributes n))
    | `START -> ()
  in
  printf "let print_value (type a) : a %s.symbol -> a -> string = function\n"
    menhir;
  Terminal.iter case_t;
  Nonterminal.iter case_n

let print_token () =
  let case t =
    match Terminal.kind t with
    | `REGULAR | `EOF ->
      printf "  | %s%s -> print_value (%s.T %s.T_%s) %s\n"
        (Terminal.name t)
        (match Terminal.typ t with | None -> "" | Some typ -> " v")
        menhir menhir
        (Terminal.name t)
        (match Terminal.typ t with | None -> "()" | Some typ -> "v")
    | `PSEUDO | `ERROR -> ()
  in
  printf "let print_token = function\n";
  Terminal.iter case

let print_token_of_terminal () =
  let case t =
    match Terminal.kind t with
    | `REGULAR | `EOF ->
      printf "  | %s.T_%s -> %s%s\n"
        menhir (Terminal.name t)
        (Terminal.name t) (if Terminal.typ t <> None then " v" else "")
    | `ERROR ->
      printf "  | %s.T_%s -> assert false\n"
        menhir (Terminal.name t)
    | `PSEUDO -> ()
  in
  printf
    "let token_of_terminal (type a) (t : a %s.terminal) (v : a) : token =\n\
    \  match t with\n"
    menhir;
  Terminal.iter case

let () =
  print_header ();
  print_newline ();
  print_symbol ();
  print_newline ();
  print_value ();
  print_newline ();
  print_token ();
  print_newline ();
  print_token_of_terminal ()
