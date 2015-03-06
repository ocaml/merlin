(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2014  Frédéric Bour  <frederic.bour(_)lakaban.net>
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

(** Helpers around Menhir generated definitions *)

type token = Raw_parser.token

type annotation =
  [ `Shift of int
  | `Shift_token of int * token
  | `Cost of int
  | `Indent of int
  | `Unclosed of string
  | `Close
  | `Item of string
  ]

type 'a token_class = 'a Raw_parser.token_class
type 'a nonterminal_class = 'a Raw_parser.nonterminal_class

type symbol_class = Raw_parser.symbol_class =
  | CT_ : 'a token_class * annotation list -> symbol_class
  | CN_ : 'a nonterminal_class  * annotation list -> symbol_class

type symbol = Raw_parser.symbol =
  | T_ : 'a token_class * 'a -> symbol
  | N_ : 'a nonterminal_class * 'a -> symbol
  | Bottom

val class_of_symbol: symbol -> symbol_class

val string_of_class: symbol_class -> string

val symbol_of_token: token -> symbol

val default_symbol: symbol_class -> int * symbol

val selection_priority: symbol_class -> int

val token_of_symbol: 'a token_class -> 'a -> token

val is_lparen : token -> string option
val is_ident : token -> string option
val is_uident : token -> string option
val is_operator : token -> string option

val friendly_name : symbol_class -> string option
