(* The browse module transforms Typedtree into an uniform tree 
 * suited for easy navigation, allowing to locate the typing environment
 * and the kind of construction at a given point.
 * This is used both by the type-at-point and completion features.
 *)

(* Typedtree constructions recognized *)
type context =
  | Type      of Types.type_declaration
  | Expr      of Types.type_expr
  | Module    of Types.module_type
  | Modtype   of Types.modtype_declaration
  | Class     of Ident.t * Types.class_declaration
  | ClassType of Ident.t * Types.class_type_declaration
  | Other

(* The browse tree; lazyness is added to prevent building the
 * complete tree, especially useful considering the tree is not kept
 * in memory but rebuilt every time needed.
 *)
type t = {
  loc : Location.t;
  env : Env.t;
  context : context;
  nodes : t list Lazy.t
}

val dummy : t

(* Build tree out of Typedtree fragment *)
val structure   : Typedtree.structure   -> t list
val expression  : Typedtree.expression  -> t
val module_expr : Typedtree.module_expr -> t

(* Navigate through tree *)

(* The deepest context inside or before the node, for instance, navigating
 * through:
 *    foo bar (baz :: tail) <cursor> 
 * asking for node from cursor position will return context of "tail". *)
val deepest_before : Lexing.position -> t list -> t option
(* The nearest context inside or before the node, though stopping after
 * leaving enclosing subtree. For instance, navigating
 * through:
 *    foo bar (baz :: tail) <cursor> 
 * asking for node from cursor position will return context of the complete,
 * application, since none of the arguments or the function expression will
 * get us closer to cursor. *)
val nearest_before : Lexing.position -> t list -> t option
(* Return the path of nodes enclosing expression at cursor.
 * For instance, navigating through:
 *    f (g x<cursor>))
 * will return the contexts of "x", "g x" then "f (g x)". *)
val enclosing : Lexing.position -> t list -> t list
