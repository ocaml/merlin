(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                          Ulysse Gérard, Tarides                        *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Uid = struct
  type t =
    | Compilation_unit of string
    | Item of { comp_unit: string; id: int }
    | Internal
    | Predef of string

  include Identifiable.Make(struct
    type nonrec t = t

    let equal (x : t) y = x = y
    let compare (x : t) y = compare x y
    let hash (x : t) = Hashtbl.hash x

    let print fmt = function
      | Internal -> Format.pp_print_string fmt "<internal>"
      | Predef name -> Format.fprintf fmt "<predef:%s>" name
      | Compilation_unit s -> Format.pp_print_string fmt s
      | Item { comp_unit; id } -> Format.fprintf fmt "%s.%d" comp_unit id

    let output oc t =
      let fmt = Format.formatter_of_out_channel oc in
      print fmt t
  end)

  let id = ref (-1)

  let reinit () = id := (-1)

  let mk  ~current_unit =
      incr id;
      Item { comp_unit = current_unit; id = !id }

  let of_compilation_unit_id id =
    if not (Ident.persistent id) then
      Misc.fatal_errorf "Types.Uid.of_compilation_unit_id %S" (Ident.name id);
    Compilation_unit (Ident.name id)

  let of_predef_id id =
    if not (Ident.is_predef id) then
      Misc.fatal_errorf "Types.Uid.of_predef_id %S" (Ident.name id);
    Predef (Ident.name id)

  let internal_not_actually_unique = Internal

  let for_actual_declaration = function
    | Item _ -> true
    | _ -> false
end

module Sig_component_kind = struct
  type t =
    | Value
    | Type
    | Module
    | Module_type
    | Extension_constructor
    | Class
    | Class_type

  let to_string = function
    | Value -> "value"
    | Type -> "type"
    | Module -> "module"
    | Module_type -> "module type"
    | Extension_constructor -> "extension constructor"
    | Class -> "class"
    | Class_type -> "class type"

  let can_appear_in_types = function
    | Value
    | Extension_constructor ->
        false
    | Type
    | Module
    | Module_type
    | Class
    | Class_type ->
        true
end

module Item = struct
  module T = struct
    type t = string * Sig_component_kind.t
    let compare = compare

    let make str ns = str, ns

    let value id = Ident.name id, Sig_component_kind.Value
    let type_ id = Ident.name id, Sig_component_kind.Type
    let module_ id = Ident.name id, Sig_component_kind.Module
    let module_type id = Ident.name id, Sig_component_kind.Module_type
    let extension_constructor id =
      Ident.name id, Sig_component_kind.Extension_constructor
    let class_ id =
      Ident.name id, Sig_component_kind.Class
    let class_type id =
      Ident.name id, Sig_component_kind.Class_type
  end

  include T

  module Map = Map.Make(T)
end

type var = Ident.t
type t = { uid: Uid.t option; desc: desc }
and desc =
  | Var of var
  | Abs of var * t
  | App of t * t
  | Struct of t Item.Map.t
  | Leaf
  | Proj of t * Item.t
  | Comp_unit of string

let print fmt =
  let rec aux fmt { uid; desc } =
    match uid with
    | None -> print_desc fmt desc
    | Some uid ->
        Format.fprintf fmt "%a:@ %a"
          Uid.print uid
          print_desc desc
  and print_desc fmt = function
    | Var id -> Format.fprintf fmt "%a" Ident.print id
    | Abs (id, t) ->
        Format.fprintf fmt "Abs(@[%a,@ @[%a@]@])"
          Ident.print id aux t
    | App (t1, t2) -> Format.fprintf fmt "@[%a(@,%a)@]" aux t1 aux t2
    | Leaf -> Format.fprintf fmt "·"
    | Proj (t, (name, ns)) ->
        Format.fprintf fmt "@[%a@ .@ %S[%s]@]"
          aux t
          name
          (Sig_component_kind.to_string ns)
    | Comp_unit name -> Format.fprintf fmt "CU %s" name
    | Struct map ->
        let print_map fmt =
          Item.Map.iter (fun (name, ns) t ->
              Format.fprintf fmt "@[<hv 4>(%S, %s) ->@ %a;@]@,"
                name
                (Sig_component_kind.to_string ns)
                aux t
            )
        in
        Format.fprintf fmt "{@[<v>%a@]}" print_map map
  in
  Format.fprintf fmt"@[%a@]@." aux

let overwrite_uid uid t =
  match uid with
  | None -> t
  | Some _ -> { t with uid }

let fresh_var ?(name="shape-var") uid =
  let var = Ident.create_local name in
  var, { uid = Some uid; desc = Var var }

let for_unnamed_functor_param = Ident.create_local "()"

let var uid id =
  { uid = Some uid; desc = Var id }

let abs ?uid var body =
  { uid; desc = Abs (var, body) }

let str ?uid map =
  { uid; desc = Struct map }

let leaf uid =
  { uid = Some uid; desc = Leaf }

let proj ?uid t item =
  match t.desc with
  | Leaf ->
      (* When stuck projecting in a leaf we propagate the leaf as a best effort *)
      t
  | Struct map ->
      begin try Item.Map.find item map
      with Not_found -> t (* ill-typed program *)
      end
  | _ ->
      { uid; desc = Proj (t, item) }

let rec app ?uid f ~arg =
  match f.desc with
  | Abs (var, body) ->
      let res = subst var ~arg body in
      overwrite_uid uid res
  | _ ->
      { uid; desc = App (f, arg) }

and subst var ~arg t =
  match t.desc with
  | Var id when var = id -> arg
  | Abs (v, e) ->
      abs ?uid:t.uid v (subst var ~arg e)
  | App (f, e) ->
      app ?uid:t.uid (subst var ~arg f) ~arg:(subst var ~arg e)
  | Struct m ->
      { t with desc = Struct (Item.Map.map (fun s -> subst var ~arg s) m) }
  | Proj (t, item) ->
      proj ?uid:t.uid (subst var ~arg t) item
  | Comp_unit _ | Leaf | Var _ ->
      t


module Make_reduce(Params : sig
  val fuel : int
  val read_unit_shape : unit_name:string -> t option
  val find_shape : Ident.t -> t
end) = struct
  let rec reduce fuel t =
    let reduce_if_gas =
      if fuel > 0
      then reduce (fuel -1)
      else Fun.id
    in
    let reduce = reduce fuel in
    match t.desc with
    | Comp_unit unit_name ->
        begin match Params.read_unit_shape ~unit_name with
        | Some t -> reduce t
        | None -> t
        end
    | App(f, arg) ->
        app ?uid:t.uid (reduce f) ~arg:(reduce arg)
    | Proj(str, item) ->
        let r = proj ?uid:t.uid (reduce str) item in
        if r = t
        then t
        else reduce r
    | Abs(var, body) ->
        { t with desc = Abs(var, reduce body) }
    | Var id ->
        begin try
          let res = Params.find_shape id in
          if res = t then
            raise Not_found
          else
            reduce_if_gas res
        with Not_found -> { t with desc = Leaf } (* avoid loops. *)
        end
    | _ ->
        t

  let reduce = reduce Params.fuel
end

let dummy_mod = { uid = None; desc = Struct Item.Map.empty }

let rec of_path ~find_shape ?(ns = Sig_component_kind.Module) =
  let ns_mod = Sig_component_kind.Module in
  function
  | Path.Pident id -> find_shape ns id
  | Path.Pdot (path, name) ->
      let t = of_path ~find_shape ~ns:ns_mod path in
      proj t (name, ns)
  | Path.Papply (p1, p2) ->
      app (of_path ~find_shape ~ns:ns_mod p1)
        ~arg:(of_path ~find_shape ~ns:ns_mod p2)

let for_persistent_unit s = { uid = None; desc = Comp_unit s }

let set_uid_if_none t uid =
  match t.uid with
  | None -> { t with uid = Some uid }
  | _ -> t

module Map = struct
  type shape = t
  type nonrec t = t Item.Map.t

  let empty = Item.Map.empty

  let add t item shape = Item.Map.add item shape t

  let add_value t id uid = Item.Map.add (Item.value id) (leaf uid) t
  let add_value_proj t id shape =
    let item = Item.value id in
    Item.Map.add item (proj shape item) t

  let add_type t id uid = Item.Map.add (Item.type_ id) (leaf uid) t
  let add_type_proj t id shape =
    let item = Item.type_ id in
    Item.Map.add item (proj shape item) t

  let add_module t id shape = Item.Map.add (Item.module_ id) shape t
  let add_module_proj t id shape =
    let item = Item.module_ id in
    Item.Map.add item (proj shape item) t

  let add_module_type t id uid =
    Item.Map.add (Item.module_type id) (leaf uid) t
  let add_module_type_proj t id shape =
    let item = Item.module_type id in
    Item.Map.add item (proj shape item) t

  let add_extcons t id uid =
    Item.Map.add (Item.extension_constructor id) (leaf uid) t
  let add_extcons_proj t id shape =
    let item = Item.extension_constructor id in
    Item.Map.add item (proj shape item) t

  let add_class t id uid = Item.Map.add (Item.class_ id) (leaf uid) t
  let add_class_proj t id shape =
    let item = Item.class_ id in
    Item.Map.add item (proj shape item) t

  let add_class_type t id uid = Item.Map.add (Item.class_type id) (leaf uid) t
  let add_class_type_proj t id shape =
    let item = Item.class_type id in
    Item.Map.add item (proj shape item) t
end
