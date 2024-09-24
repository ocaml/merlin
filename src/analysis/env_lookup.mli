(** Provides tools to lookup items in the typing environment.

  Establishing the namespace of an item before looking it up in the environement
  is necessary to prevent mixing items which have the same name but are not of
  the same namespace. (For example the environment can contain both type named
  `t` and a value named `t`.) *)

(** Namespaces describe in which section of the environment an item should be
  looked for. *)
module Namespace : sig
  type t = Shape.Sig_component_kind.t

  val to_string : t -> string

  type under_type = [ `Constr | `Labels ]
  type inferred_basic = [ `Constr | `Labels | `Mod | `Modtype | `Type | `Vals ]
  type inferred =
    [ `Constr
    | `Labels
    | `Mod
    | `Modtype
    | `This_cstr of Types.constructor_description
    | `This_label of Types.label_description
    | `Type
    | `Vals ]

  (** Returns potential namespaces given the context of an expression *)
  val from_context : Context.t -> inferred list
end

type item = { uid : Shape.Uid.t; loc : Location.t; namespace : Namespace.t }

val by_path : Path.t -> Namespace.t -> Env.t -> item option

val by_longident :
  Namespace.inferred list -> Longident.t -> Env.t -> (Path.t * item) option
