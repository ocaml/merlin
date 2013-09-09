open Location
type position = int

module type CONTEXT = sig
  type state

  type sig_item
  type str_item
  type sig_in_sig_modtype
  type sig_in_sig_module
  type sig_in_str_modtype
  type str_in_module
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
    | Sig_item of (Context.sig_item, t_sig) step
    | Sig_in_sig_modtype of (Context.sig_in_sig_modtype, t_sig) step
    | Sig_in_sig_module  of (Context.sig_in_sig_module,  t_sig) step
    | Sig_in_str_modtype of (Context.sig_in_str_modtype, t_str) step

  and t_str =
    | Str_root of (unit, unit) step
    | Str_item of (Context.str_item, t_str) step
    | Str_in_module of (Context.str_in_module, t_str) step

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

  val dump :  ?sig_item:(string -> Context.state -> Context.sig_item -> string) 
           -> ?str_item:(string -> Context.state -> Context.str_item -> string) 
           -> ?state:(string -> Context.state -> string) 
           -> t -> string list
end

module Initial (Context : CONTEXT) : sig
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
      :  (Dom.Context.sig_item, Dom.t_sig) Dom.step
      -> Context.state -> Context.state * Context.sig_item
    val str_item 
      :  (Dom.Context.str_item, Dom.t_str) Dom.step
      -> Context.state -> Context.state * Context.str_item

    (* Fold signature shape *)
    val sig_in_sig_modtype
      :  (Dom.Context.sig_in_sig_modtype, Dom.t_sig) Dom.step
      -> Context.state -> Context.state * Context.sig_in_sig_modtype
    val sig_in_sig_module 
      :  (Dom.Context.sig_in_sig_module, Dom.t_sig) Dom.step
      -> Context.state -> Context.state * Context.sig_in_sig_module
    val sig_in_str_modtype
      :  (Dom.Context.sig_in_str_modtype, Dom.t_str) Dom.step
      -> Context.state -> Context.state * Context.sig_in_str_modtype

    (* Fold structure shape *)
    val str_in_module 
      :  (Dom.Context.str_in_module, Dom.t_str) Dom.step
      -> Context.state -> Context.state * Context.str_in_module
   end) :
sig 
  module Dom : S
  include S
  val rewind : Dom.t -> t -> Dom.t * t
  val update : Dom.t -> t option -> t
end with module Dom = Dom and module Context = Context
