(** An instance of [t] represents the identity of the contents of a file path.
    Use this to quickly detect if a file has changed. (Detection is done by
    checking some fields from stat syscall, it can be tricked but should behave
    well in regular cases). FIXME: precision of mtime is still the second?! *)
type t

(** Returns true iff the heuristic determines that the file contents has not
    changed. *)
val check : t -> t -> bool

(** [file_id filename] computes an id for the current contents of [filename] *)
val get : string -> t

val with_cache : (unit -> 'a) -> 'a
