type 'a table = { ref: 'a ref; init: unit -> 'a }
type 'a immutable = { ref: 'a ref; mutable snapshot: 'a }

type ref_and_reset =
  | Table : 'a table -> ref_and_reset
  | Immutable : 'a immutable -> ref_and_reset

type bindings = {
  mutable refs: ref_and_reset list;
  mutable frozen : bool;
  is_bound: bool ref
}

let new_bindings () =
  { refs = []; is_bound = ref false; frozen = false }

let is_bound t = !(t.is_bound)

let reset t =
  assert (is_bound t);
  List.iter (function
    | Table { ref; init } -> ref := init ()
    | Immutable { ref; snapshot } -> ref := snapshot
  ) t.refs

let table t create size =
  let init () = create size in
  let ref = ref (init ()) in
  assert (not t.frozen);
  t.refs <- (Table { ref; init }) :: t.refs;
  ref

let ref t k =
  let ref = ref k in
  assert (not t.frozen);
  t.refs <- (Immutable { ref; snapshot = k }) :: t.refs;
  ref

type 'a cell = { ref : 'a ref; mutable value : 'a }
type slot = Slot : 'a cell -> slot
type scope = { slots: slot list; scope_bound : bool ref }

let fresh t =
  let slots =
    List.map (function
      | Table { ref; init } -> Slot {ref; value = init ()}
      | Immutable r ->
          if not t.frozen then r.snapshot <- !(r.ref);
          Slot { ref = r.ref; value = r.snapshot }
    ) t.refs
  in
  t.frozen <- true;
  { slots; scope_bound = t.is_bound }

let with_scope { slots; scope_bound } f =
  assert (not !scope_bound);
  scope_bound := true;
  List.iter (fun (Slot {ref;value}) -> ref := value) slots;
  match f () with
  | res ->
    List.iter (fun (Slot s) -> s.value <- !(s.ref)) slots;
    scope_bound := false;
    res
  | exception exn ->
    List.iter (fun (Slot s) -> s.value <- !(s.ref)) slots;
    scope_bound := false;
    raise exn

module Compiler = struct
  let compiler_state = new_bindings ()
  let s_table f n = table compiler_state f n
  let s_ref k = ref compiler_state k
end
