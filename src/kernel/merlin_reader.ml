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

type spec = Normal of Extension.set * Merlin_parser.kind

type t =
  | Is_normal of Merlin_parser.t

let make spec src = match spec with
  | Normal (ext, kind) ->
    let lexer = Merlin_lexer.make (Extension.keywords ext) src in
    let parser = Merlin_parser.make lexer kind in
    Is_normal parser

let update src = function
  | Is_normal parser as t ->
    let lexer = Merlin_parser.lexer parser in
    let lexer = Merlin_lexer.update src lexer in
    let parser' = Merlin_parser.update lexer parser in
    if parser == parser' then t
    else Is_normal parser'

let result = function
  | Is_normal parser -> Merlin_parser.result parser

let source = function
  | Is_normal parser -> Merlin_lexer.source (Merlin_parser.lexer parser)

let compare a b = match a, b with
  | Is_normal a, Is_normal b ->
    Merlin_parser.compare a b

let is_normal = function
  | Is_normal p -> Some p

let find_lexer = function
  | Is_normal p -> Some (Merlin_parser.lexer p)

let errors = function
  | Is_normal p ->
    Merlin_lexer.errors (Merlin_parser.lexer p) @
    Merlin_parser.errors p

let comments = function
  | Is_normal p ->
    Merlin_lexer.comments (Merlin_parser.lexer p)

let trace t nav = match t with
  | Is_normal p ->
    Merlin_parser.trace p nav
