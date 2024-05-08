(* {{{ COPYING *(
  Same as Mbrowse module but for the Parstree.
  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 -  Pizie Dust  <playersrebirth(_)gmail.com>

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

type node = Browse_raw_p.node
type t = node list

val fold_node : (Browse_raw_p.node -> 'a -> 'a) ->
                 Browse_raw_p.node -> 'a -> 'a
val node_loc : Browse_raw_p.node -> Location.t
val leaf_node : t -> node
val drop_leaf : t -> t option

(* Navigate through tree *)

(** The deepest context inside or before the node, for instance, navigating
 * through:
 *    foo bar (baz :: tail) <cursor>
 * asking for node from cursor position will return context of "tail".
 * Returns the matching node and all its ancestors or the empty list. *)
val deepest_before : Lexing.position -> t list -> t


val select_open_node : t -> (Longident.t * t) option

val enclosing : Lexing.position -> t list -> t

val of_structure : Parsetree.structure -> t
val of_structure_items : Parsetree.structure_item list -> node list
val of_signature_items : Parsetree.signature_item list -> node list
val of_signature : Parsetree.signature -> t

val of_parsetree :
  [ `Implementation of Parsetree.structure
  | `Interface of Parsetree.signature ] -> t

val to_parsetree : t ->
  [ `Implementation of Parsetree.structure
  | `Interface of Parsetree.signature ]

(** Identify nodes introduced by recovery *)
val is_recovered_expression : Parsetree.expression -> bool
val is_recovered : Browse_raw_p.node -> bool

(** When an optional argument is applied with labelled syntax
    sugar (~a:v instead of ?a:(Some v)), the frontend will have
    wrapped it in [Some _].
    [optional_label_sugar exp] returns [Some exp'] with the sugar
    removed in that case. *)
val optional_label_sugar :
  Parsetree.expression_desc -> Parsetree.expression option

(** {1 Dump} *)

val print_node : unit -> node -> string
val print : unit -> t -> string
val pprint_deriver_node : unit -> node -> string
val pprint_deriver_nodes : unit -> node list -> string
val get_children : Lexing.position -> node list ->  node

