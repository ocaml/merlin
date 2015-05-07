open Std

type info = {
  path: string list;
  index: int;
  locations: Location.t list;
}

module IdentCounter = struct
  module StringMap = Map.Make(String)
  module IntMap = Map.Make(struct
      type t = int
      let compare (a : int) b = compare a b
    end)
  type t = (int * int IntMap.t) StringMap.t

  let empty = StringMap.empty

  let add t ident =
    let name = Ident.name ident in
    let time = Ident.binding_time ident in
    let card, table =
      try StringMap.find name t
      with Not_found -> 0, IntMap.empty
    in
    if IntMap.mem time table then
      t
    else
      StringMap.add name (card + 1, IntMap.add time card table) t

  let index ident t =
    let name = Ident.name ident in
    let time = Ident.binding_time ident in
    try IntMap.find time (snd (StringMap.find name t))
    with Not_found -> -1
end

type t = IdentCounter.t

type diff = info Path.PathMap.t

let rec all_paths acc t =
  let acc = BrowseT.node_paths t.BrowseT.t_node @ acc in
  List.fold_left ~f:all_paths ~init:acc (Lazy.force t.BrowseT.t_children)

let empty = IdentCounter.empty
let empty_diff = Path.PathMap.empty

(*FIXME: won't work with applicative paths *)
let rec naive_head = function
  | Path.Pdot (p,_,_) | Path.Papply (p,_) -> naive_head p
  | Path.Pident id -> id

let rec naive_name acc = function
  | Path.Papply (p,_) -> naive_name [] p
  | Path.Pdot (p,name,_) -> naive_name (name :: acc) p
  | Path.Pident ident -> (Ident.name ident :: acc), ident

let add_path index pathmap path_loc =
  let path = path_loc.Location.txt in
  let location = path_loc.Location.loc in
  let info =
    try Path.PathMap.find path pathmap
    with Not_found ->
      let names, ident = naive_name [] path in
      let color = IdentCounter.index ident index in
      { path = names; index = color; locations = [] }
  in
  let info = {info with locations = location :: info.locations} in
  Path.PathMap.add path info pathmap

let update content (diff,t) =
  let browse = Browse.of_typer_contents [content, ()] in
  let paths = List.fold_left ~f:all_paths ~init:[] browse in
  let naive_head t = naive_head t.Location.txt in
  let idents = List.rev_map ~f:naive_head paths in
  let idents =
    List.sort ~cmp:(fun i1 i2 ->
        compare (Ident.binding_time i1) (Ident.binding_time i2))
      idents
  in
  let t = List.fold_left ~f:IdentCounter.add ~init:t idents in
  List.fold_left ~f:(add_path t) paths ~init:diff, t

let get_diff diff =
  Path.PathMap.fold (fun _ info acc -> info :: acc) diff []
