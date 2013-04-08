(* The browse module transforms Typedtree into an uniform tree 
 * suited for easy navigation, allowing to locate the typing environment
 * and the kind of construction at a given point.
 * This is used both by the type-at-point and completion features.
 *)

(* Typedtree constructions recognized *)
type kind =
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
  kind : kind;
  nodes : t list Lazy.t
}


(* Build tree out of Typedtree fragment *)
val structure   : Typedtree.structure   -> t list
val expression  : Typedtree.expression  -> t
val module_expr : Typedtree.module_expr -> t

(* Navigate through tree *)
val near : Lexing.position -> t list -> t option
val enclosing : Lexing.position -> t list -> t list
