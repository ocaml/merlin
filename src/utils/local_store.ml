open Std

type ref_and_reset = F : 'a ref * (unit -> 'a) -> ref_and_reset
type bindings = { mutable refs: ref_and_reset list }

let new_bindings () =
  { refs = [] }

let ref t f =
  let result = ref (f ()) in
  t.refs <- (F (result, f)) :: t.refs;
  result

type 'a slot = { ref : 'a ref; mutable value : 'a }
type a_slot = Slot : 'a slot -> a_slot
type scope = a_slot list

let fresh t =
  List.map ~f:(fun (F(ref,f)) -> Slot {ref; value = f ()}) t.refs

let merge = (@)

type ref_and_value = V : 'a ref * 'a -> ref_and_value
let restore l = List.iter ~f:(fun (V(r,v)) -> r := v) l

let with_scope scope f =
  let backup = List.rev_map ~f:(fun (Slot {ref;_}) -> V (ref,!ref)) scope in
  List.iter ~f:(fun (Slot {ref;value}) -> ref := value) scope;
  match f () with
  | x ->
    List.iter ~f:(fun (Slot s) -> s.value <- !(s.ref)) scope;
    restore backup;
    x
  | exception exn ->
    List.iter ~f:(fun (Slot s) -> s.value <- !(s.ref)) scope;
    restore backup;
    reraise exn
