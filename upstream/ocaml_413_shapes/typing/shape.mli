module Uid : sig
  type t

  val reinit : unit -> unit

  val mk : current_unit:string -> t
  val of_compilation_unit_id : Ident.t -> t
  val of_predef_id : Ident.t -> t
  val internal_not_actually_unique : t

  val for_actual_declaration : t -> bool

  include Identifiable.S with type t := t
end

module Sig_component_kind : sig
  type t =
    | Value
    | Type
    | Module
    | Module_type
    | Extension_constructor
    | Class
    | Class_type

  val to_string : t -> string

  (** Whether the name of a component of that kind can appear in a type. *)
  val can_appear_in_types : t -> bool
end

module Item : sig
  type t

  module Map : Map.S with type key = t
end

type var = Ident.t
type t =
  | Var of var
  | Abs of var * t
  | App of t * t
  | Struct of Uid.t option * t Item.Map.t
  | Leaf of Uid.t
  | Proj of t * Item.t
  | Comp_unit of string

val print : Format.formatter -> t -> unit

module Map : sig
  type shape = t
  type nonrec t = t Item.Map.t

  val empty : t

  val add_value : t -> Ident.t -> Uid.t -> t
  val add_value_proj : t -> Ident.t -> shape -> t

  val add_type : t -> Ident.t -> Uid.t -> t
  val add_type_proj : t -> Ident.t -> shape -> t

  val add_module : t -> Ident.t -> shape -> t
  val add_module_proj : t -> Ident.t -> shape -> t

  val add_module_type : t -> Ident.t -> Uid.t -> t
  val add_module_type_proj : t -> Ident.t -> shape -> t

  val add_extcons : t -> Ident.t -> Uid.t -> t
  val add_extcons_proj : t -> Ident.t -> shape -> t

  val add_class : t -> Ident.t -> Uid.t -> t
  val add_class_proj : t -> Ident.t -> shape -> t

  val add_class_type : t -> Ident.t -> Uid.t -> t
  val add_class_type_proj : t -> Ident.t -> shape -> t
end

(* Forward declaration -- to be filled in by Cms_format *)
val load_shape : (string -> t) ref

val fresh_var : ?name:string -> unit -> var * t

val dummy_mod : t

val of_path :
  find_shape:(Sig_component_kind.t -> Ident.t -> t) ->
  ?ns:Sig_component_kind.t -> Path.t -> t

val make_var : var -> t
val make_abs : var -> t -> t
val make_app : arg:t -> t -> t
val proj : t -> (string * Sig_component_kind.t) -> t
val make_const_fun : t -> t
val make_persistent : string -> t
val make_functor : param:(Ident.t option) -> t -> t
val make_structure : Uid.t option -> Map.t -> t

val add_struct_uid : t -> Uid.t -> t

(** [env_lookup] is used for recursive modules.

    FIXME: doc *)
val reduce : env_lookup:(Ident.t -> t) -> t -> t
