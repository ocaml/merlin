
(* The type of tokens. *)

type token = 
  | WORD of (string)
  | WILDCARD
  | STAR
  | POLY of (string)
  | PARENS_OPEN
  | PARENS_CLOSE
  | EOF
  | COMMA
  | ARROW

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Type_parsed.t)
