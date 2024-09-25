(** An instance of [t] represents the identity of the contents of a file path.
    Use this to quickly detect if a file has changed.
    (Detection is done by checking some fields from stat syscall,
    it can be tricked but should behave well in regular cases).
    FIXME: precision of mtime is still the second?!
*)
type t

(** Returns true iff the heuristic determines that the file contents has not
    changed. *)
val check : t -> t -> bool

(** [file_id filename] computes an id for the current contents of [filename].
    Returns a generic id, if the id can't be computed. *)
val get : string -> t

(** Same as [get], but returns an error, if the id can't be computed. *)
val get_res : string -> (t, string) Result.t

val with_cache : (unit -> 'a) -> 'a
