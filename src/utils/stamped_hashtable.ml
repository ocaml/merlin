type ('a, 'b) t = {
  table: ('a, 'b) Hashtbl.t;
  mutable recent: (int * 'a) list;
  mutable sorted: (int * 'a) list;
}

let create n = {
  table = Hashtbl.create n;
  recent = [];
  sorted = [];
}

let add t ~stamp a b =
  Hashtbl.add t.table a b;
  t.recent <- (stamp, a) :: t.recent

let mem t a =
  Hashtbl.mem t.table a

let find t a =
  Hashtbl.find t.table a

(* Sort by decreasing stamps *)
let order (i1, _) (i2, _) =
  Int.compare i2 i1

let rec filter_prefix pred = function
  | x :: xs when not (pred x) ->
    filter_prefix pred xs
  | xs -> xs

let backtrack t ~stamp =
  let process (stamp', path) =
    if stamp' > stamp then (
      Hashtbl.remove t.table path;
      false
    ) else
      true
  in
  let recent =
    t.recent
    |> List.filter process
    |> List.fast_sort order
  in
  t.recent <- [];
  let sorted =
    t.sorted
    |> filter_prefix process
    |> List.merge order recent
  in
  t.sorted <- sorted
