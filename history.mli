type position = Lexing.position
type position_compare = position -> int
type 'a t

val empty : 'a t
val wrap : 'a t ref -> (Lexing.lexbuf -> 'a) -> (Lexing.lexbuf -> 'a)

(*val seek_start : (position -> int) -> 'a t -> 'a t*)
val first_pos : 'a t -> position
val last_pos  : 'a t -> position
val current_pos : 'a t -> position

val this_position : position -> position_compare
val this_offset   : int -> position_compare
val seek : position_compare -> 'a t -> 'a t

val split : 'a t -> 'a t * 'a t

(*val drop_next : 'a t -> 'a t * 'a list*)
 
val forward  : 'a t -> ('a * position * position) option * 'a t
val backward : 'a t -> ('a * position * position) option * 'a t

val insert : ('a * position * position) -> 'a t -> 'a t
