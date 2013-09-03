open Location
type 'a binding = 'a * t * string loc
type position = int

module type CONTEXT = sig
  type state

  type signature_item
  type structure_item
end

module type STEP = sig
  module Context : CONTEXT

  type ('a,'b) step
  val value    : ('a, 'b) step -> 'a
  val state    : ('a, 'b) step -> Context.state
  val parent   : ('a, 'b) step -> 'b
  val position : ('a, 'b) step -> position
end

module type S = sig
  include STEP

  type t_sig =
    | Sig_root of (unit, unit) step
    | Sig_item of (Context.signature_item, t_sig) step
    | Sig_in_sig_modtype of (Parsetree.modtype_declaration binding, t_sig) step
    | Sig_in_sig_module  of (Parsetree.module_type binding, t_sig) step
    | Sig_in_str_modtype of (Parsetree.module_type binding, t_str) step

  and t_str =
    | Str_root of (unit, unit) step
    | Str_item of (Context.structure_item, t_str) step
    | Str_in_module of (Parsetree.module_expr binding, t_str) step

  type t = 
    | Str of t_str
    | Sig of t_sig

  val sig_position : t_sig -> int
  val str_position : t_str -> int
  val position : t -> int

  val str_previous : t_str -> t option
  val sig_previous : t_sig -> t option
  val previous : t -> t option

  val str_state : t_str -> Context.state
  val sig_state : t_sig -> Context.state
  val get_state : t -> Context.state
end

module Make (Context : CONTEXT) : sig
  include S 
  val sig_step : t_sig -> Context.state -> 'a -> ('a,t_sig) step
  val str_step : t_str -> Context.state -> 'a -> ('a,t_str) step

  val initial : Context.state -> (unit, unit) step
end with module Context = Context

module Transform (Context : CONTEXT) (Dom : S)
  (Fold : sig
    (* Initial state *)
    val sig_root : (unit, unit) Dom.step -> Context.state
    val str_root : (unit, unit) Dom.step -> Context.state

    (* Fold items *)
    val sig_item 
      :  (Dom.Context.signature_item, Dom.t_sig) Dom.step
      -> Context.state -> Context.state * Context.signature_item
    val str_item 
      :  (Dom.Context.structure_item, Dom.t_str) Dom.step
      -> Context.state -> Context.state * Context.structure_item

    (* Fold signature shape *)
    val sig_in_sig_modtype
      :  (Parsetree.modtype_declaration binding, Dom.t_sig) Dom.step
      -> Context.state -> Context.state
    val sig_in_sig_module 
      :  (Parsetree.module_type binding, Dom.t_sig) Dom.step
      -> Context.state -> Context.state
    val sig_in_str_modtype
      :  (Parsetree.module_type binding, Dom.t_str) Dom.step
      -> Context.state -> Context.state

    (* Fold structure shape *)
    val str_in_module 
      :  (Parsetree.module_expr binding, Dom.t_str) Dom.step
      -> Context.state -> Context.state
   end) :
sig 
  module Dom : S
  include S
  val rewind : Dom.t -> t -> Dom.t * t
  val update : Dom.t -> t option -> t
end with module Dom = Dom and module Context = Context
