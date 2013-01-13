type 'a t

val empty : 'a t

val of_list : 'a list -> 'a t

val move : int -> 'a t -> 'a t
val seek : ('a -> int) -> 'a t -> 'a t
val seek_offset : int -> 'a t -> 'a t


val split : 'a t -> 'a t * 'a t

val prev : 'a t -> 'a option
val prevs : 'a t -> 'a list
val next : 'a t -> 'a option
val nexts : 'a t -> 'a list

val offset : 'a t -> int

val forward  : 'a t -> ('a * 'a t) option 
val backward : 'a t -> ('a * 'a t) option 

val insert : 'a -> 'a t -> 'a t
val remove_current : 'a t -> 'a option * 'a t
val modify_current : ('a -> 'a) -> 'a t -> 'a t

type pos = Lexing.position
type 'a loc = 'a * pos * pos

val wrap_lexer : ?filter:('a -> bool) -> ?bufpos:Lexing.position ref ->
  'a loc t ref -> (Lexing.lexbuf -> 'a) -> (Lexing.lexbuf -> 'a)

val current_pos : ?default:Lexing.position -> 'a loc t -> pos
val seek_pos : pos -> 'a loc t -> 'a loc t

type 'a sync

val sync_origin : 'a sync
val sync_point : 'a t -> 'a sync
val sync_item : 'a sync -> 'a option
  
val sync : ('b -> 'a sync) -> 'a t -> 'b t -> 'a t * 'b t
val sync_backward : ('b -> 'a sync) -> 'a t -> 'b t -> 'a t * 'b t
val sync_right_forward : ('b -> 'a sync) -> 'a t -> 'b t -> 'b t
val sync_left_forward : ('b -> 'a sync) -> 'a t -> 'b t -> 'a t
