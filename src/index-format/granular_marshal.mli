(** A pointer to an ['a] value, either residing in memory or on disk. *)
type 'a link

(** [link v] returns a new link to the in-memory value [v]. *)
val link : 'a -> 'a link

(** [fetch lnk] returns the value pointed by the link [lnk].

    We of course have [fetch (link v) = v] and [link (fetch lnk) = lnk]. *)
val fetch : 'a link -> 'a

(** For this demo we can't depend on a PPX or external dependencies,
    so we require a user-defined {!schema} to describe where the links can be
    found.  This is just an iter traversal over the values, recursively
    yielding on any reachable link. Since links can point to values themselves
    containing links, recursion is delayed by asking for the schema of each
    child.

    For example, the following type has the following schema:

    {[
      type t = { first : string link ; second : int link list link }

      let schema : t schema = fun iter t ->
        iter.yield t.first schema_no_sublinks ;
        iter.yield t.second @@ fun iter lst ->
          List.iter (fun v -> iter.yield v schema_no_sublinks) lst
    ]}

    where {!schema_no_sublinks} indicates that the yielded value contains
    no reachable links. *)

(** A function to iter on every {!link} reachable in the value ['a]. *)
type 'a schema = iter -> 'a -> unit

(** A callback to signal the reachable links and the schema of their pointed
    sub-value.  Since a value can contain multiple links each pointing to
    different types of values, the callback is polymorphic. *)
and iter = { yield : 'a. 'a link -> 'a schema -> unit }

(** A schema usable when the ['a] value does not contain any links. *)
val schema_no_sublinks : 'a schema

(** [write oc schema value] writes the [value] in the output channel [oc],
    creating unmarshalling boundaries on every link in [value] specified
    by the [schema]. *)
val write :
  ?flags:Marshal.extern_flags list -> out_channel -> 'a schema -> 'a -> unit

(** The type to represent an open disk connection. *)
type store

(** [read ic schema] reads the value marshalled in the input channel [ic],
    stopping the unmarshalling on every link boundary indicated by the [schema].
    It returns the root [value] read.  *)
val read : in_channel -> 'a schema -> 'a

(** [close store] closes the connection to the disk. Any further {!fetch} requiring
    to read from the disk will fail. *)
val close : store -> unit
