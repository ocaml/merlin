type position = Lexing.position
type 'a t

val empty : 'a t
val wrap : 'a t ref -> (Lexing.lexbuf -> 'a) -> (Lexing.lexbuf -> 'a)

val seek_start : position -> 'a t -> 'a t
val seek_curr  : position -> 'a t -> 'a t

val drop_next : 'a t -> 'a t * 'a list
 
val fold : 
