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
type t =
  | Var of var
  | Abs of var * t
  | App of t * t
  | Struct of Uid.t option * t Item.Map.t
  | Leaf of Uid.t
  | Proj of t * Item.t
  | Comp_unit of string

let print fmt =
  let rec aux fmt = function
    | Var id -> Format.fprintf fmt "%a" Ident.print id
    | Abs (id, t) ->
        Format.fprintf fmt "Abs(@[%a,@ @[%a@]@])" Ident.print id aux t
    | App (t1, t2) -> Format.fprintf fmt "@[%a(@,%a)@]" aux t1 aux t2
    | Leaf uid -> Format.fprintf fmt "<%a>" Uid.print uid
    | Proj (t, (name, ns)) ->
        Format.fprintf fmt "@[%a@ .@ %S[%s]@]"
          aux t
          name
          (Sig_component_kind.to_string ns)
    | Comp_unit name -> Format.fprintf fmt "CU %s" name
    | Struct (uid, map) ->
        let print_map fmt =
          Item.Map.iter (fun (name, ns) shape ->
              Format.fprintf fmt "@[<hv 4>(%S, %s) ->@ %a;@]@,"
                name
                (Sig_component_kind.to_string ns)
                aux shape
            )
        in
        Format.fprintf fmt "{@[<v>%a@,%a@]}"
          (Format.pp_print_option Uid.print) uid print_map map
  in
  Format.fprintf fmt"@[%a@]@." aux

let load_shape = ref (fun _ -> failwith "filled in by Cms_format")

let fresh_var ?(name="shape-var") () =
  let var = Ident.create_local name in
  var, Var var

let rec subst var ~arg = function
  | Var v when var = v -> arg
  | Abs (v, t) -> Abs(v, subst var ~arg t)
  | App (abs, t) -> App(subst var ~arg abs, subst var ~arg t) |> reduce_app
  | Struct (uid, m) ->
      Struct (uid, Item.Map.map (fun s -> subst var ~arg s) m)
  | Proj (t, item) -> Proj(subst var ~arg t, item) |> reduce_proj
  | (Comp_unit _ | Leaf _ | Var _) as body -> body

and reduce_app = function
  | App (Abs (var, body), arg) -> subst var ~arg body
  | t -> t

and reduce_proj = function
  | Proj (Struct (_uid, map), item) as t ->
      (try Item.Map.find item map
        with Not_found -> t) (* SHould never happen ?*)
  | t -> t

let rec reduce ~env_lookup t =
  let reduce = reduce ~env_lookup in
  let read_shape unit_name =
    match Load_path.find_uncap (unit_name ^ ".cms") with
    | filename -> Some (!load_shape filename)
    | exception Not_found -> None
  in
  match t with
  | Comp_unit name as t ->
      begin match read_shape name with
      | Some t -> reduce t
      | None -> t
      end
  | App(abs, body) ->
    reduce_app (App(reduce abs, reduce body))
  | Proj(str, item) -> reduce_proj (Proj(reduce str, item))
  | Abs(var, body) -> Abs(var, reduce body)
  | Struct (uid, map) -> Struct (uid, Item.Map.map reduce map)
  | Var id as t ->
      begin try env_lookup id
      with Not_found -> t (* stuck. *)
      end
  | t -> t

let dummy_mod = Struct (None, Item.Map.empty)

let rec of_path ~find_shape ?(ns = Sig_component_kind.Module) =
  let ns_mod = Sig_component_kind.Module in
  function
  | Path.Pident id -> find_shape ns id
  | Path.Pdot (path, name) ->
      let t = of_path ~find_shape ~ns:ns_mod path in
      Proj (t, (name, ns)) |> reduce_proj
  | Path.Papply (p1, p2) -> App(
      of_path ~find_shape ~ns:ns_mod p1,
      of_path ~find_shape ~ns:ns_mod p2
    )

let make_var var = Var var

let make_abs var t = Abs(var, t)

let proj t elt = Proj (t, elt) |> reduce_proj

let make_const_fun t = Abs(fresh_var () |> fst, t)

let make_persistent s = Comp_unit s

let make_functor ~param body =
  match param with
  | None -> body
  | Some id -> Abs(id, body)

let make_app ~arg f = App(f, arg) |> reduce_app

let make_structure uid shapes = Struct (uid, shapes)
let add_struct_uid shape uid = match shape with
  | Struct (None, map) -> Struct (Some uid, map)
  | t -> t


module Map = struct
  type shape = t
  type nonrec t = t Item.Map.t

  let empty = Item.Map.empty

  let add_value t id uid = Item.Map.add (Item.value id) (Leaf uid) t
  let add_value_proj t id shape =
    let item = Item.value id in
    Item.Map.add item (proj shape item) t

  let add_type t id uid = Item.Map.add (Item.type_ id) (Leaf uid) t
  let add_type_proj t id shape =
    let item = Item.type_ id in
    Item.Map.add item (proj shape item) t

  let add_module t id shape = Item.Map.add (Item.module_ id) shape t
  let add_module_proj t id shape =
    let item = Item.module_ id in
    Item.Map.add item (proj shape item) t

  let add_module_type t id uid =
    Item.Map.add (Item.module_type id) (Leaf uid) t
  let add_module_type_proj t id shape =
    let item = Item.module_type id in
    Item.Map.add item (proj shape item) t

  let add_extcons t id uid =
    Item.Map.add (Item.extension_constructor id) (Leaf uid) t
  let add_extcons_proj t id shape =
    let item = Item.extension_constructor id in
    Item.Map.add item (proj shape item) t

  let add_class t id uid = Item.Map.add (Item.class_ id) (Leaf uid) t
  let add_class_proj t id shape =
    let item = Item.class_ id in
    Item.Map.add item (proj shape item) t

  let add_class_type t id uid = Item.Map.add (Item.class_type id) (Leaf uid) t
  let add_class_type_proj t id shape =
    let item = Item.class_type id in
    Item.Map.add item (proj shape item) t
end
