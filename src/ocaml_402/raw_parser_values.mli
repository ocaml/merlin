(** Helpers around Menhir generated definitions *)

type token = Raw_parser.token

type annotation =
  [ `Shift of int
  | `Shift_token of int * token
  | `Cost of int
  | `Indent of int
  | `Unclosed of string | `Close
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
