(* {{{ COPYING *(

     This file is part of Merlin, an helper for ocaml editors

     Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                                Thomas Refis  <refis.thomas(_)gmail.com>
                                Simon Castellan  <simon.castellan(_)iuwt.fr>

     Permission is hereby granted, free of charge, to any person obtaining a
     copy of this software and associated documentation files (the "Software"),
     to deal in the Software without restriction, including without limitation the
     rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
     sell copies of the Software, and to permit persons to whom the Software is
     furnished to do so, subject to the following conditions:

     The above copyright notice and this permission notice shall be included in
     all copies or substantial portions of the Software.

     The Software is provided "as is", without warranty of any kind, express or
     implied, including but not limited to the warranties of merchantability,
     fitness for a particular purpose and noninfringement. In no event shall
     the authors or copyright holders be liable for any claim, damages or other
     liability, whether in an action of contract, tort or otherwise, arising
     from, out of or in connection with the software or the use or other dealings
     in the Software.

   )* }}} *)

module Make (Input : sig
  (** The type of cached values computed from a file.

      The cache can grow indefinitely if only [read] or [read_only]
      are called.
  *)
  type t

  (** Compute/obtain a value from a file - an expensive operation *)
  val read : string -> t

  val cache_name : string
end) : sig
  (** [read file_path] checks the cache for a valid entry mapping
      the file path [file_path] to a value. If the entry doesn't exist
      of if it is outdated based on the file's attributes (stat record),
      a fresh value is obtained from the user-specified [read] function.

      Exceptions go through and invalidate the cache entry.

      @param read_only if true, don't update the cache
  *)
  val read : ?read_only:bool -> string -> Input.t

  (** Remove this entry if it is outdated based on inspecting file metadata
      (stat record). *)
  val check : string -> bool

  (** Scan the cache and remove entries that are outdated
      or older than [older_than] (age in seconds). *)
  val flush : ?older_than:float -> unit -> unit

  (** Remove all entries *)
  val clear : unit -> unit

  (** @raises Not_found if the file is not in cache. *)
  val get_cached_entry : string -> Input.t

  type cache_stats = { hit : int; miss : int }
  val get_cache_stats : unit -> cache_stats
  val clear_cache_stats : unit -> unit
end
