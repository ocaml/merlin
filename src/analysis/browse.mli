(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2014  Frédéric Bour  <frederic.bour(_)lakaban.net>
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

open Std

type node = Env.t * Browse_node.t
type t = node List.Non_empty.t

val node_loc : Browse_node.t -> Location.t
val leaf_node : t -> node

(* Navigate through tree *)

(** The deepest context inside or before the node, for instance, navigating
 * through:
 *    foo bar (baz :: tail) <cursor>
 * asking for node from cursor position will return context of "tail".
 * Returns the matching node and all its ancestors or the empty list. *)
val deepest_before : Lexing.position -> t list -> t option

(** Heuristic to find suitable environment to complete / type at given position.
 *  1. Try to find environment near given cursor.
 *  2. Check if there is an invalid construct between found env and cursor :
 *    Case a.
 *      > let x = valid_expr ||
 *      The env found is the right most env from valid_expr, it's a correct
 *      answer.
 *    Case b.
 *      > let x = valid_expr
 *      > let y = invalid_construction||
 *      In this case, the env found is the same as in case a, however it is
 *      preferable to use env from enclosing module rather than an env from
 *      inside x definition.
 *)
val node_at : ?skip_recovered:bool -> Merlin_lib.Typer.t -> Lexing.position ->
  t

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
val of_typer_contents : (Merlin_typer.content * _) list -> t list

(** Identify nodes introduced by recovery *)
val is_recovered_expression : Typedtree.expression -> bool
val is_recovered : Browse_node.t -> bool
