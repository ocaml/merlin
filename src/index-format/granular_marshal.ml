module Cache = Hashtbl.Make (Int)

type store = { filename : string; id : int; cache : cache }

and cache = any_link Cache.t

and any_link = Link : 'a link * 'a link Type.Id.t -> any_link

and parent_link = PLink : 'a link -> parent_link

and any_value = Value : 'a -> any_value

and cached = Cached : 'a link * int * store * 'a schema -> cached

and 'a link = 'a repr ref

and 'a repr =
  | Small of 'a
  | Small_child of { parent : parent_link; pos : int }
  | Serialized of { loc : int }
  | Serialized_reused of { loc : int }
  | On_disk of { store : store; loc : int; schema : 'a schema }
  | On_disk_ptr of { filename : string; loc : int; id : int }
  | In_memory of 'a
  | In_cache of 'a * cached Dbllist.cell * any_value array
  | In_memory_reused of 'a
  | Duplicate of 'a link
  | Placeholder

and 'a schema = iter -> 'a -> unit

and iter = { yield : 'a. 'a link -> 'a link Type.Id.t -> 'a schema -> unit }

exception
  Outdated_store of
    { filename : string; reason : [ `Missing_file | `Index_ids_do_not_match ] }

let lru_dbllist : cached Dbllist.t option ref = ref None

(* let fetch_count = Hashtbl.create 16

let debug h =
  let r = Hashtbl.create 16 in
  Hashtbl.iter (fun _k v ->
    let count = try Hashtbl.find r v with Not_found -> 0 in
    Hashtbl.replace r v (count + 1);
  ) h;
  let acc = ref 0 in
  Hashtbl.iter (fun k v ->
    acc := !acc + v;
    Format.eprintf "fetché %d fois -> %d valeurs\n%!" k v
  ) r;
  Format.eprintf "en tout : %d valeurs\n%!" !acc *)

let create_lru cap = lru_dbllist := Some (Dbllist.create cap)

let get_lru () =
  match !lru_dbllist with
  | Some lru -> lru
  | None ->
    let lru = Dbllist.create 1_000_000 in
    lru_dbllist := Some lru;
    lru

let schema_no_sublinks : _ schema = fun _ _ -> ()

let link v = ref (In_memory v)

let is_on_disk lnk =
  match !lnk with
  | On_disk _ | On_disk_ptr _ | In_cache _ -> true
  | _ -> false

let rec normalize lnk =
  match !lnk with
  | Duplicate lnk -> normalize lnk
  | _ -> lnk

module Cache_cache = File_cache.Make (struct
  type t = cache
  let read _filename = Cache.create 0

  let cache_name = "Cache_cache"
end)

let ptr_size = 8

let binstring_of_int v =
  String.init ptr_size (fun i -> Char.chr ((v lsr i lsl 3) land 255))

let int_of_binstring s =
  Array.fold_right
    (fun v acc -> (acc lsl 8) + v)
    (Array.init ptr_size (fun i -> Char.code s.[i]))
    0

let last_open_store = ref None

let () =
  at_exit (fun () ->
      match !last_open_store with
      | None -> ()
      | Some (_, fd) -> close_in fd)

let force_open_store store =
  try
    let fd = open_in_bin store.filename in
    seek_in fd (String.length Config.index_magic_number);
    let required_id = int_of_binstring (really_input_string fd ptr_size) in
    if required_id = store.id then (
      last_open_store := Some (store, fd);
      fd)
    else
      raise
        (Outdated_store
           { filename = store.filename; reason = `Index_ids_do_not_match })
  with Sys_error _ ->
    raise (Outdated_store { filename = store.filename; reason = `Missing_file })

let open_store store =
  match !last_open_store with
  | Some (store', fd)
    when Int.equal store.id store'.id
         && String.equal store.filename store'.filename -> fd
  | Some (_, fd) ->
    close_in fd;
    force_open_store store
  | None -> force_open_store store

let read_loc store fd loc schema parent_link =
  seek_in fd loc;
  let v = Marshal.from_channel fd in
  let size_read = pos_in fd - loc in
  let child_pos = ref 0 in
  let child_smalls = ref [] in
  let rec iter =
    { yield =
        (fun (type a) (lnk : a link) type_id schema ->
          match !lnk with
          | Small v ->
            schema iter v;
            child_smalls:= (Value v) :: !child_smalls;
            lnk := Small_child { parent = parent_link; pos = !child_pos };
            child_pos := !child_pos + 1
          | Serialized { loc } -> lnk := On_disk { store; loc; schema }
          | Serialized_reused { loc } -> (
            match Cache.find_opt store.cache loc with
            | Some (Link (type b) ((lnk', type_id') : b link * _)) -> (
              match Type.Id.provably_equal type_id type_id' with
              | Some (Equal : (a link, b link) Type.eq) ->
                lnk := Duplicate (normalize lnk')
              | None ->
                invalid_arg
                  "Granular_marshal.read_loc: reuse of a different type")
            | None ->
              lnk := On_disk { store; loc; schema };
              Cache.add store.cache loc (Link (lnk, type_id)))
          | In_memory _ | In_cache _ | In_memory_reused _ | On_disk _ | Small_child _ | Duplicate _ -> ()
          | On_disk_ptr { filename; loc; id } ->
            let store = { filename; id; cache = Cache_cache.read filename } in
            lnk := On_disk { store; loc; schema }
          | Placeholder -> invalid_arg "Granular_marshal.read_loc: Placeholder");
    }
  in
  schema iter v;
  let small_poses = Array.of_list (List.rev !child_smalls) in
  (v, size_read, small_poses)

let () =
  at_exit (fun () ->
    (* debug fetch_count; *)
    match !lru_dbllist with
    | None -> ()
    | Some lru ->
      Dbllist.pp_stats lru;
      )

let fetch_loc store loc schema parent_link =
  let fd = open_store store in
  let v, size, small_poses = read_loc store fd loc schema parent_link in
  (v, size, small_poses)

let rec fetch : type a. a link -> a = fun lnk ->
  match !lnk with
  | In_cache (v, cell, _) ->
    let Cached (_, _loc, _, _) = Dbllist.get cell in
    Dbllist.promote (get_lru ()) cell;
    v
  | In_memory v | In_memory_reused v -> v
  | Serialized _ | Serialized_reused _ | Small _ | On_disk_ptr _ ->
    invalid_arg "Granular_marshal.fetch: serialized"
  | Placeholder -> invalid_arg "Granular_marshal.fetch: during a write"
  | Duplicate original_lnk -> fetch original_lnk
  | Small_child { parent; pos } ->
    let PLink parent = parent in
      ignore(fetch parent);
      (match !parent with
      | In_cache (_, _, small_poses) ->
        let Value v = small_poses.(pos) in
        Obj.magic v
      | _ -> assert false)
  | On_disk { store; loc; schema } ->
    (* let count = try Hashtbl.find fetch_count (loc, store.filename) with Not_found -> 0 in
    Hashtbl.replace fetch_count (loc, store.filename) (count + 1); *)
    let v, size, small_poses = fetch_loc store loc schema (PLink lnk) in
    let discarded = Dbllist.discard_size (get_lru ()) size in
    let cell =
      Dbllist.add_front (get_lru ()) (Cached (lnk, loc, store, schema), size)
    in
    List.iter
      (fun (Cached (link, loc, store, schema)) ->
        link := On_disk { store; loc; schema })
      discarded;
    lnk := In_cache (v, cell, small_poses);
    v

let rec reuse lnk =
  match !lnk with
  | In_memory v | In_cache (v, _, _) -> lnk := In_memory_reused v
  | In_memory_reused _ -> ()
  | On_disk _ -> ()
  | Duplicate original_lnk -> reuse original_lnk
  | _ -> invalid_arg "Granular_marshal.reuse: not in memory"

let cache (type a) (module Key : Hashtbl.HashedType with type t = a) =
  let module H = Hashtbl.Make (Key) in
  let cache = H.create 16 in
  fun (lnk : a link) ->
    let key = fetch lnk in
    match H.find cache key with
    | original_lnk ->
      assert (original_lnk != lnk);
      let original_lnk = normalize original_lnk in
      reuse original_lnk;
      lnk := Duplicate original_lnk
    | exception Not_found -> H.add cache key lnk

let write ?(flags = []) fd id root_schema root_value =
  let id = binstring_of_int id in
  output_string fd id;
  let pt_root = pos_out fd in
  output_string fd (String.make ptr_size '\000');
  let rec iter size ~placeholders ~restore =
    { yield =
        (fun (type a) (lnk : a link) _type_id (schema : a schema) : unit ->
          match !lnk with
          | Serialized _ | Serialized_reused _ | Small _ | On_disk_ptr _ -> ()
          | Placeholder -> failwith "big nono"
          | In_memory_reused v -> write_child_reused lnk schema v
          | Duplicate original_lnk ->
            (match !original_lnk with
            | Serialized_reused _ | On_disk_ptr _ -> ()
            | In_memory_reused v -> write_child_reused original_lnk schema v
            | _ -> failwith "Granular_marshal.write: duplicate not reused");
            lnk := !original_lnk
          | In_memory v -> write_child lnk schema v size ~placeholders ~restore
          | Small_child _ ->
            let v = fetch lnk in
            write_child lnk schema v size ~placeholders ~restore
          | In_cache (_v, t, _children) ->
            let Cached (_, loc, {filename; id; _}, _) = t.content in
            lnk := On_disk_ptr { filename; id; loc }
          | On_disk { store = { filename; id; _ }; loc; _ } ->
            lnk := On_disk_ptr { filename; id; loc })
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
  let id = int_of_binstring (really_input_string fd 8) in
  let store = { filename; id; cache = Cache_cache.read filename } in
  let root_loc = int_of_binstring (really_input_string fd 8) in
  let parent_link = ref (On_disk { loc = root_loc; store; schema = root_schema }) in
  let root_value, _, _ = read_loc store fd root_loc root_schema (PLink parent_link) in
  root_value
