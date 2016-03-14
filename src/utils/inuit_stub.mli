(* Minimal text API *)

type flags = [ `Editable | `Clickable | `Raw ]

class type ['flags] cursor =
  object ('self)
    method text : ?flags:'flags list -> string -> unit
    method clear : unit

    method sub : 'self
    method sub_action : ('self -> unit) option -> 'self

    method is_closed : bool

    constraint 'flags = [> flags]
  end

type 'a action = 'a -> unit
  constraint 'a = _ #cursor

val text   : ?flags:'flags list -> 'flags #cursor -> string -> unit
val clear  : _ #cursor -> unit
val sub    : ?action:'cursor action option -> 'cursor -> 'cursor

val link   : 'cursor -> string -> 'cursor action -> unit

val printf : _ #cursor -> ('a, unit, string, unit) format4 -> 'a

val null_cursor : _ cursor
val is_closed   : _ #cursor -> bool

(* Basic widgets *)

module Nav : sig
  type 'cursor t
    constraint 'cursor = _ #cursor

  type 'cursor frame = {
    title: 'cursor;
    body: 'cursor;
    nav: 'cursor t;
  } constraint 'cursor = _ #cursor

  val make : string -> ('cursor frame -> unit) -> 'cursor t

  val push : 'cursor t -> string -> ('cursor frame -> unit) -> unit
  val goto : 'cursor t -> string -> ('cursor frame -> unit) -> unit

  val render : 'cursor t -> 'cursor -> unit
end

module Tree : sig
  type 'cursor t
    constraint 'cursor = _ #cursor
  val make  : 'cursor -> 'cursor t
  val add   : ?children:('cursor t -> unit) -> ?action:'cursor action option ->
              ?opened:bool ref -> 'cursor t -> 'cursor
  val clear : 'cursor t -> unit
end
