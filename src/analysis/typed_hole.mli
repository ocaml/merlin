(** This module should be used to work with typed holes. The main goal is to
    hide syntactic representation of a typed hole, which may change in future *)

(** checks whether the current string matches the syntax representation of a
    typed hole *)
val can_be_hole : string -> bool

(** [is_a_hole nodes] checks whether the leaf node [1] is a typed hole

    Note: this function is extracted from merlin sources handling [Construct]
    command in [merlin/src/frontend/query_commands.ml]

    [1] leaf node is the head of the list, as
    [Mbrowse.t = (Env.t * Browse_raw.node) list]*)
val is_a_hole : Mbrowse.t -> bool
