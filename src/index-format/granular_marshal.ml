module Cache = Hashtbl.Make (Int)

type store = { filename : string; cache : any_link Cache.t }

and any_link = Link : 'a link * 'a link Type.Id.t -> any_link

and 'a link = 'a repr ref

and 'a repr =
  | Small of 'a
  | Serialized of { loc : int }
  | Serialized_reused of { loc : int }
  | On_disk of { store : store; loc : int; schema : 'a schema }
  | In_memory of 'a
  | In_memory_reused of 'a
  | Duplicate of 'a link
  | Placeholder

and 'a schema = iter -> 'a -> unit

and iter = { yield : 'a. 'a link -> 'a link Type.Id.t -> 'a schema -> unit }

let schema_no_sublinks : _ schema = fun _ _ -> ()

let link v = ref (In_memory v)

let rec normalize lnk =
  match !lnk with
  | Duplicate lnk -> normalize lnk
  | _ -> lnk

let read_loc store fd loc schema =
  seek_in fd loc;
  let v = Marshal.from_channel fd in
  let rec iter =
    { yield =
        (fun (type a) (lnk : a link) type_id schema ->
          match !lnk with
          | Small v ->
            schema iter v;
            lnk := In_memory v
          | Serialized { loc } -> lnk := On_disk { store; loc; schema }
          | Serialized_reused { loc } -> (
            match Cache.find store.cache loc with
            | Link (type b) ((lnk', type_id') : b link * _) -> (
              match Type.Id.provably_equal type_id type_id' with
              | Some (Equal : (a link, b link) Type.eq) ->
                lnk := Duplicate (normalize lnk')
              | None ->
                invalid_arg
                  "Granular_marshal.read_loc: reuse of a different type")
            | exception Not_found ->
              lnk := On_disk { store; loc; schema };
              Cache.add store.cache loc (Link (lnk, type_id)))
          | In_memory _ | In_memory_reused _ | On_disk _ | Duplicate _ -> ()
          | Placeholder -> invalid_arg "Granular_marshal.read_loc: Placeholder")
    }
  in
  schema iter v;
  v

let last_open_store = ref None

let () =
  at_exit (fun () ->
      match !last_open_store with
      | None -> ()
      | Some (_, fd) -> close_in fd)

let force_open_store store =
  let fd = open_in_bin store.filename in
  last_open_store := Some (store, fd);
  fd

let open_store store =
  match !last_open_store with
  | Some (store', fd) when store == store' -> fd
  | Some (_, fd) ->
    close_in fd;
    force_open_store store
  | None -> force_open_store store

let fetch_loc store loc schema =
  let fd = open_store store in
  let v = read_loc store fd loc schema in
  v

let rec fetch lnk =
  match !lnk with
  | In_memory v | In_memory_reused v -> v
  | Serialized _ | Serialized_reused _ | Small _ ->
    invalid_arg "Granular_marshal.fetch: serialized"
  | Placeholder -> invalid_arg "Granular_marshal.fetch: during a write"
  | Duplicate original_lnk ->
    let v = fetch original_lnk in
    lnk := In_memory v;
    v
  | On_disk { store; loc; schema } ->
    let v = fetch_loc store loc schema in
    lnk := In_memory v;
    v

let reuse lnk =
  match !lnk with
  | In_memory v -> lnk := In_memory_reused v
  | In_memory_reused _ -> ()
  | _ -> invalid_arg "Granular_marshal.reuse: not in memory"

let cache (type a) (module Key : Hashtbl.HashedType with type t = a) =
  let module H = Hashtbl.Make (Key) in
  let cache = H.create 16 in
  fun (lnk : a link) ->
    let key = fetch lnk in
    match H.find cache key with
    | original_lnk ->
      assert (original_lnk != lnk);
      reuse original_lnk;
      lnk := Duplicate original_lnk
    | exception Not_found -> H.add cache key lnk

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
  let rec iter size ~placeholders ~restore =
    { yield =
        (fun (type a) (lnk : a link) _type_id (schema : a schema) : unit ->
          match !lnk with
          | Serialized _ | Serialized_reused _ | Small _ -> ()
          | Placeholder -> failwith "big nono"
          | In_memory_reused v -> write_child_reused lnk schema v
          | Duplicate original_lnk ->
            (match !original_lnk with
            | Serialized_reused _ -> ()
            | In_memory_reused v -> write_child_reused original_lnk schema v
            | _ -> failwith "Granular_marshal.write: duplicate not reused");
            lnk := !original_lnk
          | In_memory v -> write_child lnk schema v size ~placeholders ~restore
          | On_disk _ ->
            write_child lnk schema (fetch lnk) size ~placeholders ~restore)
    }
  and write_child : type a. a link -> a schema -> a -> _ =
   fun lnk schema v size ~placeholders ~restore ->
    let v_size = write_children schema v in
    if v_size > 1024 then (
      lnk := Serialized { loc = pos_out fd };
      Marshal.to_channel fd v flags)
    else (
      size := !size + v_size;
      placeholders := (fun () -> lnk := Placeholder) :: !placeholders;
      restore := (fun () -> lnk := Small v) :: !restore)
  and write_children : type a. a schema -> a -> int =
   fun schema v ->
    let children_size = ref 0 in
    let placeholders = ref [] in
    let restore = ref [] in
    schema (iter children_size ~placeholders ~restore) v;
    List.iter (fun placehold -> placehold ()) !placeholders;
    let v_size = Obj.(reachable_words (repr v)) in
    List.iter (fun restore -> restore ()) !restore;
    !children_size + v_size
  and write_child_reused : type a. a link -> a schema -> a -> _ =
   fun lnk schema v ->
    let children_size = ref 0 in
    let placeholders = ref [] in
    let restore = ref [] in
    schema (iter children_size ~placeholders ~restore) v;
    lnk := Serialized_reused { loc = pos_out fd };
    Marshal.to_channel fd v flags
  in
  let _ : int = write_children root_schema root_value in
  let root_loc = pos_out fd in
  Marshal.to_channel fd root_value flags;
  seek_out fd pt_root;
  output_string fd (binstring_of_int root_loc)

let read filename fd root_schema =
  let store = { filename; cache = Cache.create 0 } in
  let root_loc = int_of_binstring (really_input_string fd 8) in
  let root_value = read_loc store fd root_loc root_schema in
  root_value
