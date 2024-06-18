(* A stamped hashtable is a hashtable that can associate an optional integer
   stamp to its bindings.
   The user can then efficiently remove all bindings with stamps greater than a
   bound.

   This datastructure is used to flush of the compiler caches: stamps come from
   [Ident.stamp] which are monotonically increasing unique identifiers.

   Merlin keeps regular snapshots of the compiler state to minimize the amount
   of work that needs to be redone. Flushing the cache is necessary to avoid
   state (and memory) leaking when backtracking.
*)

type ('a, 'b) t
(** An instance of a stamped hashtable *)

type changelog
(** The [changelog] datastructure logs stamped bindings added to tables.
    By separating the log from the table, it is possible to efficiently remove
    stamped bindings spread accross multiple tables. *)

val create : changelog -> int -> ('a, 'b) t
(** [create changelog n] creates a new table with an initial size of [n]
    (see [Hashtbl.create]) that logs its changes to [changelog]. *)

val add : ('a, 'b) t -> ?stamp:int -> 'a -> 'b -> unit
(** Add a binding, like [Hashtbl.add], with an optional [stamp].
    Unlike [Hashtbl.add], having multiple bindings with the same key is
    undefined. (It's ok, this feature is not used by the caches!) *)

val mem : ('a, 'b) t -> 'a -> bool
(** See [Hashtbl.mem]. *)

val find : ('a, 'b) t -> 'a -> 'b
(** See [Hashtbl.find]. *)

val fold : ('a -> 'b -> 'acc -> 'acc) -> ('a, 'b) t -> 'acc -> 'acc
(** See [Hashtbl.fold]. *)

val clear : ('a, 'b) t -> unit
(** Clear the table and empty the changelog. See [Hashtbl.clear]. *)

val create_changelog : unit -> changelog
(** Create a new change log. *)

(* [backtrack changelog ~stamp] remove all items added to tables logging to
   [changelog] with a stamp strictly greater than [stamp] *)
val backtrack : changelog -> stamp:int -> unit

val replace : ('a, 'b) t -> 'a -> 'b -> unit
(** This operation is unsafe in general. Only replacements that does not imply
    re-stamping are safe. *)
