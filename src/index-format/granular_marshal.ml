type store = in_channel

type 'a link = 'a repr ref

and 'a repr =
  | Small of 'a
  | Serialized of { loc : int }
  | On_disk of { fd : store; loc : int; schema : 'a schema }
  | In_memory of 'a

and 'a schema = iter -> 'a -> unit

and iter = { yield : 'a. 'a link -> 'a schema -> unit }

let schema_no_sublinks : _ schema = fun _ _ -> ()

let link v = ref (In_memory v)

let fetch_loc fd loc schema =
  seek_in fd loc;
  let v = Marshal.from_channel fd in
  let rec iter =
    { yield =
        (fun lnk schema ->
          match !lnk with
          | Serialized { loc } -> lnk := On_disk { fd; loc; schema }
          | Small v ->
            schema iter v;
            lnk := In_memory v
          | _ -> assert false)
    }
  in
  schema iter v;
  v

let fetch lnk =
  match !lnk with
  | In_memory v -> v
  | Serialized _ | Small _ -> invalid_arg "fetch: serialized"
  | On_disk { fd; loc; schema } ->
    let v = fetch_loc fd loc schema in
    lnk := In_memory v;
    v

let ptr_size = 8

let binstring_of_int v =
  String.init ptr_size (fun i -> Char.chr ((v lsr i lsl 3) land 255))

let int_of_binstring s =
  Array.fold_right
    (fun v acc -> (acc lsl 8) + v)
    (Array.init ptr_size (fun i -> Char.code s.[i]))
    0

let write ?(flags = []) fd root_schema root_value =
  let pt_root = pos_out fd in
  output_string fd (String.make ptr_size '\000');
  let rec iter =
    { yield =
        (fun lnk schema ->
          match !lnk with
          | Serialized _ | Small _ -> ()
          | In_memory v -> write_child fd lnk schema v
          | On_disk _ -> write_child fd lnk schema (fetch lnk))
    }
  and write_child : type a. out_channel -> a link -> a schema -> a -> unit =
   fun fd lnk schema v ->
    schema iter v;
    if Stdlib.Obj.(reachable_words (repr v)) > 500 then (
      lnk := Serialized { loc = pos_out fd };
      Marshal.to_channel fd v flags)
    else lnk := Small v
  in
  root_schema iter root_value;
  let root_loc = pos_out fd in
  Marshal.to_channel fd root_value flags;
  seek_out fd pt_root;
  output_string fd (binstring_of_int root_loc)

let read fd root_schema =
  let root_loc = int_of_binstring (really_input_string fd 8) in
  let root_value = fetch_loc fd root_loc root_schema in
  root_value

let close store = close_in store
