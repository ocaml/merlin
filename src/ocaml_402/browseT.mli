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

(** [BrowseT] offers a uniform interface to traverse constructions from
  * [TypedTree].
  *
  * Mutually recursive types from [TypedTree] are wrapped into different
  * constructors of the type [node].
  * Then type [t] allows to build a tree of [node]s.
  *
  * Finally the function [of_node] turns a [node] into a [t] tree, which
  * structure mimics the recursive structure of the [TypedTree] node.
  *
  * [t] also tries to capture the location and environment of node and defaults
  * to [default_loc] and [default_env] otherwise.
  *
  *)

(* Compatibility with previous versions of OCaml *)
type constructor_declaration = Typedtree.constructor_declaration

open Typedtree

type node =
  | Dummy
  | Pattern                  of pattern
  | Expression               of expression
  | Case                     of case
  | Class_expr               of class_expr
  | Class_structure          of class_structure
  | Class_field              of class_field
  | Class_field_kind         of class_field_kind
  | Module_expr              of module_expr
  | Module_type_constraint   of module_type_constraint
  | Structure                of structure
  | Structure_item           of structure_item
  | Module_binding           of module_binding
  | Value_binding            of value_binding
  | Module_type              of module_type
  | Signature                of signature
  | Signature_item           of signature_item
  | Module_declaration       of module_declaration
  | Module_type_declaration  of module_type_declaration
  | With_constraint          of with_constraint
  | Core_type                of core_type
  | Package_type             of package_type
  | Row_field                of row_field
  | Value_description        of value_description
  | Type_declaration         of type_declaration
  | Type_kind                of type_kind
  | Type_extension           of type_extension
  | Extension_constructor    of extension_constructor
  | Label_declaration        of label_declaration
  | Constructor_declaration  of constructor_declaration
  | Class_type               of class_type
  | Class_signature          of class_signature
  | Class_type_field         of class_type_field
  | Class_declaration        of class_declaration
  | Class_description        of class_description
  | Class_type_declaration   of class_type_declaration

  | Method_call              of expression * meth
  | Module_binding_name      of module_binding
  | Module_declaration_name  of module_declaration
  | Module_type_declaration_name of module_type_declaration

type t = {
  t_node     : node;
  t_loc      : Location.t;
  t_env      : Env.t;
  t_children : t list lazy_t;
}

val default_loc : Location.t
val default_env : Env.t

(** Dummy value, used as fallback when there is nothing to analyze (e.g
   incorrect input) *)
val dummy : t

(** [of_node ?loc ?env node] produces a tree from [node], using [loc] and [env]
  * as default annotation when nothing can be inferred from the [node].
  * [loc] and [env] default to [default_loc] and [default_env].
  *)
val of_node : ?loc:Location.t -> ?env:Env.t -> node -> t

(** [annot loc env t] replace [default_loc] and [default_env] in [t] by [loc]
  * and [env]. *)
val annot : Location.t -> Env.t -> t -> t

(** Accessors for information specific to a node *)

val string_of_node : node -> string

val pattern_paths : pattern -> Path.t Location.loc list
val expression_paths : expression -> Path.t Location.loc list

val is_constructor : t ->
  [ `Description of Types.constructor_description
  | `Declaration of Typedtree.constructor_declaration ] Location.loc option
