open Std

type node = Browse_raw.node
type t = (Env.t * node) List.Non_empty.t

val fold_node : (Env.t -> Browse_raw.node -> 'a -> 'a) ->
                 Env.t -> Browse_raw.node -> 'a -> 'a
val node_loc : Browse_raw.node -> Location.t
val leaf_node : t -> Env.t * node

(* Navigate through tree *)

(** The deepest context inside or before the node, for instance, navigating
 * through:
 *    foo bar (baz :: tail) <cursor>
 * asking for node from cursor position will return context of "tail".
 * Returns the matching node and all its ancestors or the empty list. *)
val deepest_before : Lexing.position -> t list -> t option

(** The nearest context inside or before the node, though stopping after
 * leaving enclosing subtree. For instance, navigating
 * through:
 *    foo bar (baz :: tail) <cursor>
 * asking for node from cursor position will return context of the complete,
 * application, since none of the arguments or the function expression will
 * get us closer to cursor.
 * Returns the matching node and all its ancestors or the empty list. *)
val nearest_before : Lexing.position -> t list -> t option
val enclosing : Lexing.position -> t list -> t option

val of_structure : Typedtree.structure -> t
val of_signature : Typedtree.signature -> t

val of_typedtree :
  [ `Implementation of Typedtree.structure
  | `Interface of Typedtree.signature ] -> t

(** Identify nodes introduced by recovery *)
val is_recovered_expression : Typedtree.expression -> bool
val is_recovered : Browse_raw.node -> bool

