(* A cell, recording a single change of the changelog.
   It needs to be a GADT to hide the parameters of the Hashtbl. *)
type cell =
    Cell : {
      stamp: int;
      table: ('a, 'b) Hashtbl.t;
      key: 'a;
    } -> cell

type changelog = {
  mutable recent: cell list;
  (* The [recent] list contains the changes that happened since the last
     call to backtrack, in reverse order (the most recent change is first
     in the list). *)
  mutable sorted: cell list;
  (* Cells in the [sorted] list are sorted by decreasing stamp, such that
     listing all cells greater than a threshold is a simple, in order,
     traversal. *)
}

let create_changelog () = {
  recent = [];
  sorted = [];
}

(* Wrappers around [Hashtbl] *)

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

let replace t k v =
  Hashtbl.replace t.table k v

let mem t a =
  Hashtbl.mem t.table a

let find t a =
  Hashtbl.find t.table a

let fold f t acc =
  Hashtbl.fold f t.table acc

let clear t =
  Hashtbl.clear t.table;
  t.changelog.recent <- [];
  t.changelog.sorted <- []

(* Implementation of backtracking *)

(* Helper to sort by decreasing stamps *)
let order (Cell c1) (Cell c2) =
  Int.compare c2.stamp c1.stamp

(* Drop the prefix not satisfying a certain predicate *)
let rec filter_prefix pred = function
  | x :: xs when not (pred x) ->
    filter_prefix pred xs
  | xs -> xs

let backtrack cs ~stamp =
  (* Check if a cell is still valid (older than [stamp]).
     If not, remove it from its table. *)
  let process (Cell c) =
    if c.stamp > stamp then (
      Hashtbl.remove c.table c.key;
      false
    ) else
      true
  in
  (* Process recent list:
     - remove items newer than [stamp]
     - sort the remainder *)
  let recent =
    cs.recent
    |> List.filter process
    |> List.fast_sort order
  in
  cs.recent <- [];
  (* Process sorted list:
     - remove prefix items newer than [stamp]
     - merge remaining items with the recent ones
  *)
  let sorted =
    cs.sorted
    |> filter_prefix process
    |> List.merge order recent
  in
  cs.sorted <- sorted
