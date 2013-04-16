type token = Chunk_parser.token
(* String representation of [token]s, for debug purpose *)
val token_to_string : token -> string
(* Wrap lexing function to optionally dump [token] stream, for debug purpose *)
val dump_lexer : ?who:string -> ('a -> token) -> 'a -> token

(* Heuristic applied before submitting [token] stream containing syntax errors
 * to [Chunk_parser].
 * When the [Outline_parser] encounters an error, parsing is interrupted and
 * [re_sync] is called. [re_sync] consumes tokens until it find a potential
 * point for resuming parser (generally at the beginning of a definition).
 *
 * [re_sync] returns the number of phrases the [Outline_parser] must succeed
 * to parse for the resynchronization to be considered successful.
 * (0 for EOF or explicit synchronization, 1 or more when it's only
 * approximative) *) 
val re_sync : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> int
