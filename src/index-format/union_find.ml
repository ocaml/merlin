type 'a content =
  | Root of { mutable value : 'a; mutable rank : int }
  | Link of { mutable parent : 'a element }
and 'a element = { mutable content : 'a content }

let make value = { content = Root { value; rank = 0 } }

let rec find x =
  match x.content with
  | Root _ -> x
  | Link ({ parent; _ } as link) ->
    let root = find parent in
    if root <> parent then link.parent <- root;
    root

let union ~f x y =
  let x = find x in
  let y = find y in
  match (x, y) with
  | x, y when x == y -> x
  | ( { content = Root ({ rank = rank_x; value = value_x } as root_x); _ },
      { content = Root ({ rank = rank_y; value = value_y } as root_y); _ } ) ->
    let new_value = f value_x value_y in
    if rank_x < rank_y then (
      x.content <- Link { parent = y };
      root_y.value <- new_value;
      y)
    else (
      y.content <- Link { parent = x };
      root_x.value <- new_value;
      if rank_x = rank_y then root_x.rank <- root_x.rank + 1;
      x)
  | _ -> assert false

let get elt =
  match (find elt).content with
  | Root { value; _ } -> value
  | Link _ -> assert false
