type file_id = Unix.stats option
(** An instance of file_id represents the identity of a file contents.
    Use this to quickly detect if a file has changed.
    (Detection is done by checking some fields from stat syscall,
    it an be tricked but should behave well in regular cases.
    FIXME: precision of mtime is still the second?!
*)

val with_cache : (unit -> 'a) -> 'a

val cached_file_id : string -> file_id

val file_id_check: file_id -> file_id -> bool
(** Returns true iff the heuristic determines that the file contents has not
    changed. *)

val file_id: string -> file_id
(** [file_id filename] computes an id for the current contents of [filename] *)

