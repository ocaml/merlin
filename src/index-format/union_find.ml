type 'a content =
  | Root of { mutable value : 'a; mutable rank : int }
  | Link of { mutable parent : 'a element }
and 'a element = 'a content ref

let make value = ref (Root { value; rank = 0 })

let rec find x =
  match !x with
  | Root _ -> x
  | Link ({ parent; _ } as link) ->
    let root = find parent in
    if root != parent then link.parent <- root;
    root

let union ~f x y =
  let x = find x in
  let y = find y in
  if x == y then x
  else begin
    match (!x, !y) with
    | ( Root ({ rank = rank_x; value = value_x } as root_x),
        Root ({ rank = rank_y; value = value_y } as root_y) ) ->
      let new_value = f value_x value_y in
      if rank_x < rank_y then (
        x := Link { parent = y };
        root_y.value <- new_value;
        y)
      else (
        y := Link { parent = x };
        root_x.value <- new_value;
        if rank_x = rank_y then root_x.rank <- root_x.rank + 1;
        x)
    | _ -> assert false
  end

let get elt =
  match !(find elt) with
  | Root { value; _ } -> value
  | Link _ -> assert false
