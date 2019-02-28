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

type pretty_parsetree = Extend_protocol.Reader.pretty_parsetree
type outcometree = Extend_protocol.Reader.outcometree

(* Ambient reader.

   Some actions need to interact with an external process.
   `with_ambient_reader' will setup this process to speed up later calls.
*)

val with_ambient_reader : Mconfig.t -> Msource.t -> (unit -> 'a) -> 'a

(* Main functions *)

val parse :
  ?for_completion:Msource.position -> Mconfig.t -> Msource.t -> result

val print_pretty :
  Mconfig.t -> Msource.t -> pretty_parsetree -> string

val print_outcome :
  Mconfig.t -> Msource.t -> outcometree -> string

val print_batch_outcome :
  Mconfig.t -> Msource.t -> outcometree list -> string list

val reconstruct_identifier:
  Mconfig.t -> Msource.t -> Lexing.position -> string Location.loc list

(* Update config after parse *)

val apply_directives: Mconfig.t -> parsetree -> Mconfig.t
