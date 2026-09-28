type 'a cell =
  { content : 'a; weight : int; mutable prev : 'a cell; mutable next : 'a cell }

type stats =
  { mutable total_cap : int;
    mutable promote_count : int;
    mutable add_count : int;
    mutable discard_count : int;
    mutable add_size : int;
    mutable discarded_size : int
  }

type 'a dbll =
  | Nil of int
  | List of { first : 'a cell; last : 'a cell; size : int; cap : int }

type 'a t = { mutable dbll : 'a dbll; stats : stats }

exception Action_on_empty_list of string

let pp_stats t =
  let size =
    match t.dbll with
    | Nil _ -> 0
    | List l -> l.size
  in
  Printf.eprintf
    "total_cap \t\t: %d\n\
     size \t\t: %d\n\
     promote_count \t: %d\n\
     add_count \t\t: %d\n\
     discard_count \t: %d\n\
     add_size \t\t: %d\n\
     discard_size \t: %d\n\
     volume_conservation \t: %d = %d + %d : %b\n\
     %!"
    t.stats.total_cap size t.stats.promote_count t.stats.add_count
    t.stats.discard_count t.stats.add_size t.stats.discarded_size
    t.stats.add_size t.stats.discarded_size size
    (t.stats.add_size = t.stats.discarded_size + size)

let create cap =
  let stats =
    { total_cap = cap;
      promote_count = 0;
      add_count = 0;
      discard_count = 0;
      add_size = 0;
      discarded_size = 0
    }
  in
  { dbll = Nil cap; stats }

let add_front t (v, w) =
  t.stats.add_count <- t.stats.add_count + 1;
  t.stats.add_size <- t.stats.add_size + w;
  match t.dbll with
  | Nil cap ->
    let rec c = { content = v; weight = w; prev = c; next = c } in
    t.dbll <- List { first = c; last = c; size = w; cap };
    c
  | List l ->
    let rec new_first =
      { content = v; weight = w; prev = new_first; next = l.first }
    in
    l.first.prev <- new_first;
    t.dbll <-
      List { first = new_first; last = l.last; size = l.size + w; cap = l.cap };
    new_first

let discard t =
  t.stats.discard_count <- t.stats.discard_count + 1;
  match t.dbll with
  | Nil _ ->
    raise
      (Action_on_empty_list
         "Unable to discard the last element, the doubly linked list is empty.")
  | List l ->
    if l.first == l.last then (
      t.dbll <- Nil l.cap;
      t.stats.discarded_size <- t.stats.discarded_size + l.last.weight;
      l.last.content)
    else
      let discarded_value = l.last.content in
      let discarded_weight = l.last.weight in
      t.stats.discarded_size <- t.stats.discarded_size + discarded_weight;
      let new_last = l.last.prev in
      (* TODO Should we explicitely disconnect last's pointers ? *)
      new_last.next <- new_last;
      (* TODO Int.max 0 (l.size - discarded_weight) does seems useless.
         We could use an assert to check it. *)
      (* Unlinking the discaded cell is not strictly necessary but not doing it
         could lead to memory leaks if the user of the cache keeps a reference
         to the cell. *)
      l.last.next <- l.last;
      l.last.prev <- l.last;
      t.dbll <-
        List
          { first = l.first;
            last = new_last;
            size = Int.max 0 (l.size - discarded_weight);
            cap = l.cap
          };
      discarded_value

let discard_size t s =
  (* this is fold not iter *)
  let rec iter acc t =
    match t.dbll with
    | Nil _ -> acc
    | List l -> if l.size + s <= l.cap then acc else iter (discard t :: acc) t
  in
  iter [] t

let promote t c =
  t.stats.promote_count <- t.stats.promote_count + 1;
  match t.dbll with
  | Nil _ ->
    raise
      (Action_on_empty_list
         "Unable to promote a cell, the doubly linked list is empty.")
  | List l ->
    if l.first == c then ()
    else if l.last == c then (
      let new_last = l.last.prev in
      new_last.next <- new_last;
      let new_first = c in
      new_first.next <- l.first;
      new_first.prev <- new_first;
      l.first.prev <- new_first;
      t.dbll <-
        List { first = new_first; last = new_last; size = l.size; cap = l.cap })
    else
      let voisin_prev = c.prev in
      let voisin_next = c.next in
      voisin_prev.next <- voisin_next;
      voisin_next.prev <- voisin_prev;
      let new_first = c in
      new_first.prev <- new_first;
      new_first.next <- l.first;
      l.first.prev <- new_first;
      t.dbll <-
        List { first = new_first; last = l.last; size = l.size; cap = l.cap }

let get c = c.content
