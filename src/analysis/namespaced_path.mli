module Namespace : sig
  type t = [
    | `Vals
    | `Type
    | `Constr
    | `Mod
    | `Modtype
    | `Functor
    | `Labels
    | `Unknown
    | `Apply
  ]

  val to_string : t -> string
end

module Ident : sig
  type t = private
    | Id of Ident.t
    | String of string

  val name : t -> string
end

type t = private
  | TPident of Ident.t * Namespace.t
  | TPdot of t * string * Namespace.t
  | TPapply of t * t

val to_string : t -> string

val head : t -> Ident.t * Namespace.t

val peal_head : t -> t option

val peal_head_exn : t -> t

val equal : t -> t -> bool

val rewrite_path : new_prefix:t -> t -> t

val of_path : namespace:Namespace.t -> Path.t -> t
