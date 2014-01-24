type keywords = Raw_lexer.keywords

(* Lexing is split in two steps.

   First the list of tokens is represented by a [item History.t].
   It's a pure value, independent of the context.

   Second the process of lexing is represented by values of type [t].  You
   resume the process from an arbitrary list of tokens, feeding it with one
   or more string, and you can extract the current list of tokens and cursor
   position at any time.
   Beware, the cursor may be in the middle of a not yet determined token.

   The process ultimately ends when fed with the empty string, representing
   EOF.
*)

(* Lexing step *)
type item =
  | Valid of Lexing.position * Raw_parser.token * Lexing.position
  | Error of Raw_lexer.error * Location.t
val item_start: item -> Lexing.position
val item_end: item -> Lexing.position

(** Create an empty list new lexer *)
val empty: filename:string -> item History.t

(** Prepare for lexing.
    Returns the start position (end position of last valid token), and a
    lexing function that will append at most one token to the history at each
    call. *)
type t
val history: t -> item History.t
val start: keywords -> item History.t -> t
val position: t -> Lexing.position
val feed: t -> string -> bool
val eof: t -> bool
