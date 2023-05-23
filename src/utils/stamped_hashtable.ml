
type cell =
    Cell : {
      stamp: int;
      table: ('a, 'b) Hashtbl.t;
      key: 'a;
    } -> cell

type changelog = {
  mutable recent: cell list;
  mutable sorted: cell list;
}

let create_changelog () = {
  recent = [];
  sorted = [];
}

type ('a, 'b) t = {
  table: ('a, 'b) Hashtbl.t;
  changelog: changelog;
}

let create changelog n = {
  table = Hashtbl.create n;
  changelog;
}

let add {table; changelog} ?stamp key value =
  Hashtbl.add table key value;
  match stamp with
  | None -> ()
  | Some stamp ->
    changelog.recent <- Cell {stamp; key; table} :: changelog.recent

let mem t a =
  Hashtbl.mem t.table a

let find t a =
  Hashtbl.find t.table a

(* Sort by decreasing stamps *)
let order (Cell c1) (Cell c2) =
  Int.compare c2.stamp c1.stamp

let rec filter_prefix pred = function
  | x :: xs when not (pred x) ->
    filter_prefix pred xs
  | xs -> xs

let backtrack cs ~stamp =
  let process (Cell c) =
    if c.stamp > stamp then (
      Hashtbl.remove c.table c.key;
      false
    ) else
      true
  in
  let recent =
    cs.recent
    |> List.filter process
    |> List.fast_sort order
  in
  cs.recent <- [];
  let sorted =
    cs.sorted
    |> filter_prefix process
    |> List.merge order recent
  in
  cs.sorted <- sorted
