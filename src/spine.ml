open Location
type 'a binding = 'a * t * string loc
type position = int

module Sync : sig
  type t
  val none : t
  val make : 'a -> t
  val same : 'a -> t -> bool
end = struct
  type t = Obj.t Weak.t
  let make x = 
    let t = Weak.create 1 in
    Weak.set t 0 (Some (Obj.repr x));
    t
  let same x t =
    match Weak.get t 0 with
    | None -> false
    | Some x' -> (Obj.repr x) == x'
  let none = make Not_found
end

let rec try_ntimes n f s =
  if n > 0 then 
    match f s with
      | None -> None
      | Some s' -> try_ntimes (pred n) f s'
  else Some s

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

module Make_S (Step : STEP) :
  S with module Context = Step.Context and type ('a,'b) step = ('a,'b) Step.step =
struct
  include Step
  open Context

  type t_sig =
    | Sig_root of (unit, unit) step
    | Sig_item of (signature_item, t_sig) step
    | Sig_in_sig_modtype of (Parsetree.modtype_declaration binding, t_sig) step
    | Sig_in_sig_module  of (Parsetree.module_type binding, t_sig) step
    | Sig_in_str_modtype of (Parsetree.module_type binding, t_str) step

  and t_str =
    | Str_root of (unit, unit) step
    | Str_item of (structure_item, t_str) step
    | Str_in_module of (Parsetree.module_expr binding, t_str) step
  
  type t = 
    | Str of t_str
    | Sig of t_sig

  let str_position = function
    | Str_root _ -> 0
    | Str_item step -> position step
    | Str_in_module step -> position step
  
  let sig_position = function
    | Sig_root _ -> 0
    | Sig_item step -> position step
    | Sig_in_str_modtype step -> position step
    | Sig_in_sig_module step  -> position step
    | Sig_in_sig_modtype step -> position step

  let position = function
    | Sig sg  -> sig_position sg
    | Str str -> str_position str

  let str_previous = function
    | Str_root _ -> None
    | Str_item step -> Some (Str (parent step))
    | Str_in_module step -> Some (Str (parent step))
  
  let sig_previous = function
    | Sig_root _ -> None
    | Sig_item step -> Some (Sig (parent step))
    | Sig_in_sig_module step  -> Some (Sig (parent step))
    | Sig_in_sig_modtype step -> Some (Sig (parent step))
    | Sig_in_str_modtype step -> Some (Str (parent step))

  let previous = function
    | Sig sg  -> sig_previous sg
    | Str str -> str_previous str

  let str_state = function
    | Str_root step -> state step
    | Str_item step -> state step
    | Str_in_module step -> state step
  
  let sig_state = function
    | Sig_root step -> state step
    | Sig_item step -> state step
    | Sig_in_sig_module step  -> state step
    | Sig_in_sig_modtype step -> state step
    | Sig_in_str_modtype step -> state step

  let get_state = function
    | Sig sg  -> sig_state sg
    | Str str -> str_state str
end

module Make (Context : CONTEXT) :
sig
  include S 
  val sig_step : t_sig -> Context.state -> 'a -> ('a,t_sig) step
  val str_step : t_str -> Context.state -> 'a -> ('a,t_str) step

  val initial : Context.state -> (unit, unit) step
end with module Context = Context =
struct
  module Step = struct
    module Context = Context
    open Context
    type ('a,'b) step = { 
      value: 'a;
      state: state;
      position: position;
      parent: 'b;
    }
    let value t = t.value
    let state t = t.state
    let position t = t.position
    let parent t = t.parent
  end
  include Make_S (Step)

  let str_root state = Str_root state
  let sig_root state = Sig_root state

  let str_step str state value =
    let position = succ (str_position str) in
    {Step. value; state; position; parent = str}

  let sig_step sg state value =
    let position = succ (sig_position sg) in
    {Step. value; state; position; parent = sg}

  let initial state =
    {Step. value = (); state; position = 0; parent = ()}
end

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
end with module Dom = Dom and module Context = Context =
struct
  module Dom = Dom

  module Step = struct
    module Context = Context
    open Context
    type ('a,'b) step = { 
      value: 'a;
      state: state;
      position: position;
      parent: 'b;
      sync: Sync.t;
    }
    let value t = t.value
    let state t = t.state
    let position t = t.position
    let parent t = t.parent
  end

  include Make_S (Step)

  let sig_step dom parent state value =
    let position = succ (sig_position parent) in
    let sync = Sync.make dom in
    {Step. value; state; position; parent; sync}

  let str_step dom parent state value =
    let position = succ (str_position parent) in
    let sync = Sync.make dom in
    {Step. value; state; position; parent; sync}

  let initial dom state =
    let sync = Sync.make dom in
    {Step. value = (); state; position = 0; parent = (); sync}

  let str_sync = function
    | Str_root _ -> Sync.none
    | Str_item {Step.sync} | Str_in_module {Step.sync} -> 
      sync
  
  let sig_sync = function
    | Sig_root _ -> Sync.none
    | Sig_item {Step.sync} | Sig_in_str_modtype {Step.sync}
    | Sig_in_sig_module  {Step.sync} | Sig_in_sig_modtype {Step.sync} ->
      sync

  let sync = function
    | Sig sg  -> sig_sync sg
    | Str str -> str_sync str

  let same = function
    | Dom.Sig sg  -> Sync.same sg
    | Dom.Str str -> Sync.same str

  let rewind dom cod = 
    let pd = Dom.position dom and pc = position cod in
    match
      try_ntimes (pd - pc) Dom.previous dom, 
      try_ntimes (pc - pd) previous cod
    with
    | None, _ | _, None -> assert false
    | Some dom, Some cod ->
    let rec aux dom cod =
      if same dom (sync cod)
      then dom, cod
      else match Dom.previous dom, previous cod with
        | None, Some _ | Some _, None -> assert false
        | Some dom, Some cod -> aux dom cod
        | None, None ->
        match dom with
        | Dom.Sig (Dom.Sig_root step as sg) ->
          dom, Sig (Sig_root (initial sg (Fold.sig_root step)))
        | Dom.Str (Dom.Str_root step as str) ->
          dom, Str (Str_root (initial str (Fold.str_root step)))
        | _ -> assert false
    in
    aux dom cod

  let get_sig = function
    | Sig sg -> sg
    | Str _  -> assert false

  let get_str = function
    | Str str -> str
    | Sig _  -> assert false

  let previous' = function
    | Some cod -> previous cod
    | None -> None

  let update dom cod =
    let pd = Dom.position dom in
    let cod = match cod with
      | None -> None
      | Some cod -> 
        match try_ntimes (position cod - pd) previous cod with
        | None -> assert false
        | Some cod as result ->
          assert (position cod = pd);
          result
    in
    let rec fold_str dom cod k =
      match cod with
      | Some cod when Sync.same dom (sync cod) -> k (get_str cod)
      | _ ->
      match dom with
      | Dom.Str_root step -> k (Str_root (initial dom (Fold.str_root step)))
      | Dom.Str_item step ->
        fold_str (Dom.parent step) (previous' cod)
          (fun cod ->
             let state, item = Fold.str_item step (str_state cod) in
             k (Str_item (str_step dom cod state item)))

      | Dom.Str_in_module step ->
        let value = Dom.value step in
        fold_str (Dom.parent step) (previous' cod)
          (fun cod ->
             let state = Fold.str_in_module step (str_state cod) in
             k (Str_in_module (str_step dom cod state value)))
    and fold_sig dom cod k =
      match cod with
      | Some cod when Sync.same dom (sync cod) -> k (get_sig cod)
      | _ ->
      match dom with
      | Dom.Sig_root step -> k (Sig_root (initial dom (Fold.sig_root step)))

      | Dom.Sig_item step ->
        fold_sig (Dom.parent step) (previous' cod)
          (fun cod ->
             let state, item = Fold.sig_item step (sig_state cod) in
             k (Sig_item (sig_step dom cod state item)))

      | Dom.Sig_in_sig_modtype step ->
        let value = Dom.value step in
        fold_sig (Dom.parent step) (previous' cod)
          (fun cod ->
             let state = Fold.sig_in_sig_modtype step (sig_state cod) in
             k (Sig_in_sig_modtype (sig_step dom cod state value)))

      | Dom.Sig_in_sig_module step ->
        let value = Dom.value step in
        fold_sig (Dom.parent step) (previous' cod)
          (fun cod ->
             let state = Fold.sig_in_sig_module step (sig_state cod) in
             k (Sig_in_sig_module (sig_step dom cod state value)))

      | Dom.Sig_in_str_modtype step ->
        let value = Dom.value step in
        fold_str (Dom.parent step) (previous' cod)
          (fun cod ->
             let state = Fold.sig_in_str_modtype step (str_state cod) in
             k (Sig_in_str_modtype (str_step dom cod state value)))
    in
    match dom with
    | Dom.Sig dom -> fold_sig dom cod (fun r -> Sig r)
    | Dom.Str dom -> fold_str dom cod (fun r -> Str r)
end
