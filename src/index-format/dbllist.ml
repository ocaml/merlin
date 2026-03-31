type 'a cell =
  { mutable content : 'a;
    weight : int;
    mutable prev : 'a cell;
    mutable next : 'a cell
  }

type 'a dbll =
  | Nil of int
  | List of
      { mutable first : 'a cell;
        mutable last : 'a cell;
        mutable size : int;
        cap : int
      }

type 'a t = 'a dbll ref

exception Action_on_empty_list of string

let clear t =
  match !t with
  | Nil _ -> ()
  | List l -> t := Nil l.cap

let create cap = ref (Nil cap)

let is_empty l =
  match !l with
  | Nil _ -> true
  | List _ -> false

let add_front t (v, w) =
  match !t with
  | Nil cap ->
    let rec c = { content = v; weight = w; prev = c; next = c } in
    t := List { first = c; last = c; size = w; cap };
    c
  | List l ->
    let rec new_first =
      { content = v; weight = w; prev = new_first; next = l.first }
    in
    l.first.prev <- new_first;
    t :=
      List { first = new_first; last = l.last; size = l.size + w; cap = l.cap };
    new_first

let discard t =
  match !t with
  | Nil _ ->
    raise
      (Action_on_empty_list
         "Unable to discard the last element, the doubly linked list is empty.")
  | List l ->
    if l.first == l.last then (
      t := Nil l.cap;
      l.last.content)
    else
      let discarded_value = l.last.content in
      let discarded_weight = l.last.weight in
      let new_last = l.last.prev in
      new_last.next <- new_last;
      t :=
        List
          { first = l.first;
            last = new_last;
            size = l.size - discarded_weight;
            cap = l.cap
          };
      discarded_value

let discard_size t s =
  let rec iter acc t =
    match !t with
    | Nil _ -> acc
    | List l -> if l.size + s <= l.cap then acc else iter (discard t :: acc) t
  in
  iter [] t

let discard_cell t c =
  match !t with
  | Nil _ ->
    raise
      (Action_on_empty_list
         "Unable to discard a cell, the doubly linked list is empty.")
  | List l ->
    (if l.first == c && l.last == c then t := Nil l.cap
     else if l.last == c then (
       l.last <- c.prev;
       c.prev.next <- c.prev)
     else if l.first == c then (
       l.first <- c.next;
       c.next.prev <- c.next)
     else
       let voisin_prev = c.prev in
       let voisin_next = c.next in
       voisin_prev.next <- voisin_next;
       voisin_next.prev <- voisin_prev);
    c.content

let promote_update t c v =
  match !t with
  | Nil _ ->
    raise
      (Action_on_empty_list
         "Unable to promote a cell, the doubly linked list is empty.")
  | List l ->
    c.content <- v;
    if l.first == c then ()
    else if l.last == c then (
      let new_last = l.last.prev in
      new_last.next <- new_last;
      let new_first = c in
      new_first.next <- l.first;
      new_first.prev <- new_first;
      l.first.prev <- new_first;
      t :=
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
      t := List { first = new_first; last = l.last; size = l.size; cap = l.cap }

let promote t c = promote_update t c c.content

let get c = c.content
