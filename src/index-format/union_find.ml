module Uid = Shape.Uid
module Uid_map = Granular_map.Make (Uid)

type 'a elt_handle = Uid.t

type 'a content = Root of { value : 'a; rank : int } | Link of 'a elt_handle

type 'a store = 'a content Uid_map.t

let empty () = Uid_map.empty ()

let new_root store uid value =
  (Uid_map.add uid (Root { value; rank = 0 }) store, uid)

let rec find_and_compress store uid =
  match Uid_map.find uid store with
  | Root _ as root -> (store, uid, root)
  | Link parent ->
    let store, root_uid, root = find_and_compress store parent in
    let store =
      (* Path compression: point [uid] to the root. *)
      if Uid.equal parent root_uid then store
      else Uid_map.add uid (Link root_uid) store
    in
    (store, root_uid, root)

let rec find store uid =
  match Uid_map.find uid store with
  | Root _ -> uid
  | Link parent -> find store parent

let get store uid =
  let root = find store uid in
  match Uid_map.find root store with
  | Root { value; _ } -> value
  | Link _ -> assert false

let union ~f store x y =
  let store, x, x_root = find_and_compress store x in
  let store, y, y_root = find_and_compress store y in
  if Uid.equal x y then (store, x)
  else
    match (x_root, y_root) with
    | ( Root { value = value_x; rank = rank_x },
        Root { value = value_y; rank = rank_y } ) ->
      let value = f value_x value_y in
      if rank_x < rank_y then
        let store =
          Uid_map.add x (Link y) store
          |> Uid_map.add y (Root { value; rank = rank_y })
        in
        (store, y)
      else if rank_x > rank_y then
        let store =
          Uid_map.add y (Link x) store
          |> Uid_map.add x (Root { value; rank = rank_x })
        in
        (store, x)
      else
        let store =
          Uid_map.add y (Link x) store
          |> Uid_map.add x (Root { value; rank = rank_x + 1 })
        in
        (store, x)
    | Link _, Root _ | Root _, Link _ | Link _, Link _ -> assert false

let merge ~f (s1 : 'a store) (s2 : 'a store) =
  let ensure store uid =
    let value = get s2 uid in
    match find_and_compress store uid with
    | exception Not_found -> fst (new_root store uid value)
    | store, root, Root { value = v0; rank } ->
      Uid_map.add root (Root { value = f v0 value; rank }) store
    | _, _, Link _ -> assert false
  in
  Uid_map.fold
    (fun uid content store ->
      match content with
      | Root _ -> ensure store uid
      | Link parent ->
        let store = ensure store uid in
        let store = ensure store parent in
        let store, _ = union ~f store uid parent in
        store)
    s2 s1
