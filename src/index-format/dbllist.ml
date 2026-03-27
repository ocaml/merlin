type 'a cell = {
  mutable content : 'a;
  mutable prev : 'a cell;
  mutable next : 'a cell;
}

type 'a dbll = Nil | List of { mutable first : 'a cell; mutable last : 'a cell }
type 'a t = 'a dbll ref

exception Action_on_empty_list of string

let create () = ref Nil

let is_empty l = match !l with Nil -> true | List _ -> false

let add_front t v =
  match !t with
  | Nil ->
    let rec c = { content = v; prev = c; next = c; } in
    t := List {first = c; last = c};
    c
  | List l ->
    let rec new_first = {
      content = v;
      prev = new_first;
      next = l.first
    } in
    l.first.prev <- new_first;
    t := List {first = new_first; last = l.last};
    new_first

let discard t =
  match !t with
  | Nil -> raise (Action_on_empty_list "Unable to discard the last element, the doubly linked list is empty.")
  | List l ->
    if l.first == l.last then (
      t := Nil;
      l.last.content
    ) else (
      let discarded_value = l.last.content in
      let new_last = l.last.prev in
      new_last.next <- new_last;
      t := List {first = l.first; last = new_last};
      discarded_value
    )

let promote_update t c v =
  match !t with
  | Nil -> raise (Action_on_empty_list "Unable to promote a cell, the doubly linked list is empty.")
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
      t := List {first = new_first; last = new_last}
    ) else (
      let voisin_prev = c.prev in
      let voisin_next = c.next in
      voisin_prev.next <- voisin_next;
      voisin_next.prev <- voisin_prev;
      let new_first = c in
      new_first.prev <- new_first;
      new_first.next <- l.first;
      l.first.prev <- new_first;
      t := List {first = new_first; last = l.last}
    )

let promote t c =
  promote_update t c c.content

let get c = c.content
