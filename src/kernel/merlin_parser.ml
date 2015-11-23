(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>

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

open Std

type kind =
  | ML
  | MLI
  (*| MLL | MLY*)


type tree = [
  | `Signature of Parsetree.signature
  | `Structure of Parsetree.structure
]

type t = {
  kind: kind;
  tree: tree;
  errors: (exn * Location.t) list;
}

let default = function
  | ML  -> `Structure []
  | MLI -> `Signature []

let run_parser lexer lexbuf = function
  | ML  -> `Structure (Parser_raw.implementation lexer lexbuf)
  | MLI -> `Signature (Parser_raw.interface lexer lexbuf)

let run_parser lexer kind =
  let tokens = ref (Merlin_lexer.tokens lexer) in
  let lexer lexbuf =
    match !tokens with
    | [] -> Parser_raw.EOF
    | (startp, tok, endp) :: xs ->
      tokens := xs;
      lexbuf.Lexing.lex_start_p <- startp;
      lexbuf.Lexing.lex_curr_p <- endp;
      tok
  in
  let lexbuf = Lexing.from_string "" in
  try (run_parser lexer lexbuf kind), []
  with exn ->
    (default kind), [exn, Location.curr lexbuf]

let make lexer kind =
  let tree, errors = run_parser lexer kind in
  {kind; tree; errors}

let update lexer t =
  let tree, errors = run_parser lexer t.kind in
  {t with tree; errors}

let result t = t.tree

let errors t = t.errors
