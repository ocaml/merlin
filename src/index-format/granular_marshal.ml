module Cache = Hashtbl.Make (Int)

type store = { filename : string; id : int; cache : cache }

and cache = any_link Cache.t

and any_link = Link : 'a link * 'a link Type.Id.t option -> any_link

and parent_link = PLink : 'a link -> parent_link
and any_value =
  | Value : 'a * 'a link Type.Id.t -> any_value
  | Unknown : 'a -> any_value
      (** Marks a small that has not been cleaned yet. Usually because its
          schema was unknown when it was read from the disk. *)
and any_val = V : 'a -> any_val
and any_val_link = Vlink : 'a * 'a link -> any_val_link

and cached = Cached : 'a link -> cached

and 'a link = 'a repr ref

(** Links descriptions.
  There are two different realms: on disk and in memory.
  Things such as On_disk cannot live on disk since the contain a function, schema.
  _ Type.Id.t cannot survive to marshalling either.

  We call "cleaning a value" the process of translates links from the Disk Realm
  to the Memory Realm.

  A lot of the complexity stems from the "small values" optimization. It can be
  seen as an inlining of small-enough values with their parent value. This is
  important both for speed and file size. It removes the overhead of having many
  links which is not worth for small values.

  When reading a small value, its parent value might have an unknown schema,
  resulting in a dirty cache entry. This is marked by [Dirty_unknown_schema].
  Silimarly, small values are dirty until they are explicitely needed, and thus
  their schema known.
*)
and 'a repr =
  (*
   * On-disk realm
   *)
  | Serialized of { loc : int }
      (** {i on-disk} A pointer to a serialized value in the file. *)
  | Small of int
      (** {i on-disk} A "small value" placeholder. Contains the index of this
          small's actual value in the array stored by its parent value. *)
  | Serialized_small of { loc : int; pos : int }
      (** {i on-disk} A pointer to an already serialized small value. *)
  | On_disk_ptr of { filename : string; loc : int; id : int }
      (** {i on-disk} A pointer to a serialized value in another file. The
          optional `pos` field is used to target small values. *)
  | On_disk_small_ptr of { filename : string; loc : int; id : int; pos : int }
  (*
   * In-memory realm
   *)
  | On_disk of { store : store; loc : int; schema : 'a schema option }
      (** {i in-memory} A value that can be read from the disk. *)
  | On_disk_small of
      { parent : parent_link; (* Either the parent or an On_disk_ptr *)
        small_type_id : 'a link Type.Id.t;
        small_pos : int;
        small_schema : 'a schema
      }
      (** {i in-memory} A small value whose parent can be read from the disk. *)
  | In_memory of 'a
      (** {i in-memory} A value that has been created in memory. *)
  | In_memory_reused of 'a
      (** {i in-memory} A value that has been created in memory and is used
          multiple times. *)
  | In_cache of
      { value : 'a;
        store : store;
        loc : int;
        mutable schema : 'a schema option;
        cell : cached Dbllist.cell;
        small_values : any_value array
      }
      (** {i in-memory} A value and its small that has been already read from
          the disk. Both the values and the smalls might be "unclean". They will
          be promoted to clean if read with their expected schema. *)
  | Duplicate of 'a link

and 'a schema = iter -> 'a -> unit

and iter = { yield : 'a. 'a link -> 'a link Type.Id.t -> 'a schema -> unit }

let string_of_link : type a. a link -> string =
 fun link ->
  match !link with
  | Small _ -> Printf.sprintf "Small"
  | Serialized { loc } -> Printf.sprintf "Serialized(loc=%d)" loc
  | On_disk { loc; _ } -> Printf.sprintf "On_disk(loc=%d)" loc
  | On_disk_small { small_pos; _ } ->
    Printf.sprintf "On_disk_small(small_pos=%d)" small_pos
  | Serialized_small { loc; pos } ->
    Printf.sprintf "Serialized_small(loc=%d;small_pos=%d)" loc pos
  | On_disk_ptr { loc; _ } -> Printf.sprintf "On_disk_ptr(loc=%d)" loc
  | On_disk_small_ptr { loc; pos; _ } ->
    Printf.sprintf "On_disk_ptr(loc=%d, pos=%d)" loc pos
  | In_memory _ -> "In_memory"
  | In_cache { schema; loc; _ } ->
    let clean_dirty =
      match schema with
      | Some _ -> "Clean"
      | None -> "Dirty"
    in
    Printf.sprintf "In_cache(%s; loc=%i)" clean_dirty loc
  | In_memory_reused _ -> "In_memory_reused"
  | Duplicate _ -> "Duplicate"

exception
  Outdated_store of
    { filename : string; reason : [ `Missing_file | `Index_ids_do_not_match ] }

let lru_dbllist : cached Dbllist.t option ref = ref None
let lru_size = ref 1_000_000
let set_lru_size i = lru_size := i

let get_lru () =
  match !lru_dbllist with
  | Some lru -> lru
  | None ->
    let lru = Dbllist.create !lru_size in
    lru_dbllist := Some lru;
    lru

let schema_no_sublinks : _ schema = fun _ _ -> ()

let link v = ref (In_memory v)

let rec normalize lnk =
  match !lnk with
  | Duplicate lnk -> normalize lnk
  | _ -> lnk

let is_on_disk lnk =
  match !(normalize lnk) with
  | On_disk _ | On_disk_ptr _ | On_disk_small _ | In_cache _ -> true
  | _ -> false

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

let resolve_filename store ~filename =
  if Filename.is_relative filename then
    Filename.concat (Filename.dirname store.filename) filename
  else filename

(** This iterator translates links from the Disk Realm to the Memory Realm.
    This is the process we refer too as "cleaning a value". *)
let rec disk_to_memory_iter store parent_link =
  { yield =
      (fun (type a)
        (lnk : a link)
        (type_id : a link Type.Id.t)
        (schema : a schema)
      ->
        match !lnk with
        | Small pos ->
          lnk :=
            On_disk_small
              { parent = parent_link;
                small_pos = pos;
                small_type_id = type_id;
                small_schema = schema
              }
        | Serialized_small { loc; pos } ->
          let parent =
            match Cache.find_opt store.cache loc with
            | Some (Link (lnk, _)) -> PLink (normalize lnk)
            | None ->
              let lnk = ref (On_disk { store; loc; schema = None }) in
              Cache.add store.cache loc (Link (lnk, None));
              PLink lnk
          in
          lnk :=
            On_disk_small
              { parent;
                small_type_id = type_id;
                small_schema = schema;
                small_pos = pos
              }
        | Serialized { loc } ->
          maybe_reuse_or_upgrade lnk store loc type_id schema
        | On_disk_ptr { filename; loc; id } ->
          let filename = resolve_filename store ~filename in
          let store = { filename; id; cache = Cache_cache.read filename } in
          maybe_reuse_or_upgrade lnk store loc type_id schema
        | On_disk_small_ptr { filename; loc; id; pos = small_pos } ->
          let filename = resolve_filename store ~filename in
          let store = { filename; id; cache = Cache_cache.read filename } in
          let parent =
            match Cache.find_opt store.cache loc with
            | Some (Link (lnk, _)) -> PLink (normalize lnk)
            | None ->
              let lnk = ref (On_disk { store; loc; schema = None }) in
              Cache.add store.cache loc (Link (lnk, None));
              PLink lnk
          in
          lnk :=
            On_disk_small
              { parent;
                small_type_id = type_id;
                small_schema = schema;
                small_pos
              }
        | In_memory _
        | In_cache _
        | In_memory_reused _
        | On_disk_small _
        | On_disk _
        | Duplicate _ -> (* These are already "clean" *) ())
  }

(* Checks if a loc has already been read, clean it if it was read without
   schema, or store it in the cache if it was never read.*)
and maybe_reuse_or_upgrade : type a.
    a link -> store -> int -> a link Type.Id.t -> a schema -> unit =
 fun lnk store loc type_id schema ->
  match Cache.find_opt store.cache loc with
  | Some (Link (type b) ((lnk', Some type_id') : b link * _)) -> (
    match Type.Id.provably_equal type_id type_id' with
    | Some (Equal : (a link, b link) Type.eq) ->
      lnk := Duplicate (normalize lnk')
    | None -> invalid_arg "Granular_marshal.read_loc: reuse of a different type"
    )
  | Some (Link (lnk', None)) ->
    let lnk' = Obj.magic lnk' in
    upgrade_reused_link_schema lnk' store schema;
    Cache.replace store.cache loc (Link (lnk', Some type_id));
    lnk := Duplicate (normalize lnk')
  | None ->
    lnk := On_disk { store; loc; schema = Some schema };
    Cache.add store.cache loc (Link (lnk, Some type_id))

(* Sometimes we reuse a parent whose schema was unknown so far.
   This function updates this parent's schema. *)
and upgrade_reused_link_schema : type a. a link -> store -> a schema -> unit =
 fun lnk store schema ->
  match !lnk with
  | On_disk { store; loc; schema = None; _ } ->
    (* This case only happens if the previous read was an
                    [On_disc_ptr { pos = Some_; _}] with a parent of unknown
                    schema. *)
    lnk := On_disk { store; loc; schema = Some schema }
  | In_cache ({ value; schema = None; _ } as c) ->
    (* If we already have the value in cache we must clean it. *)
    schema (disk_to_memory_iter store (PLink lnk)) value;
    c.schema <- Some schema
  | Small _
  | Serialized _
  | Serialized_small _
  | On_disk _
  | On_disk_small _
  | On_disk_ptr _
  | On_disk_small_ptr _
  | In_memory _
  | In_cache _
  | In_memory_reused _
  | Duplicate _ -> assert false

let add_to_cache value lnk ~loc store ~size small_values schema =
  let discarded = Dbllist.discard_size (get_lru ()) size in
  let cell = Dbllist.add_front (get_lru ()) (Cached lnk, size) in
  List.iter
    (fun (Cached link) ->
      (* This also free the smalls that are stored in the link *)
      match !link with
      | In_cache { loc; store; schema; _ } ->
        link := On_disk { store; loc; schema }
      | _ -> assert false)
    discarded;
  lnk := In_cache { value; loc; store; schema; cell; small_values }

(** Read one value and its smalls from the disk.  *)
let read_loc_dirty fd loc =
  seek_in fd loc;
  let v, small_children = Marshal.from_channel fd in
  let size_read = pos_in fd - loc in
  let small_children = Array.map (fun (V v) -> Unknown v) small_children in
  (v, size_read, small_children)

(** Read one value and its smalls from the disk. Clean it. The smalls are not
    cleaned yet because their schema is unknown at that point.  *)
let read_loc store fd loc schema parent_link =
  let v, size_read, small_children = read_loc_dirty fd loc in
  let iter = disk_to_memory_iter store parent_link in
  schema iter v;
  (v, size_read, small_children)

(** Reads a value with its smalls, clean it and add it to the cache *)
let fetch_on_disk lnk store loc schema =
  let fd = open_store store in
  let parent_link = PLink lnk in
  let v, size, small_values = read_loc store fd loc schema parent_link in
  add_to_cache v lnk ~loc store ~size small_values (Some schema);
  (v, small_values)

let fetch_on_disk_dirty lnk store loc =
  let fd = open_store store in
  let v, size, small_children = read_loc_dirty fd loc in
  add_to_cache v lnk ~loc store ~size small_children None;
  small_children

(* Smalls are stored along their parents so they share the same loc *)
let store_and_loc_of_parent (PLink lnk : parent_link) =
  match !lnk with
  | In_cache { store; loc; _ } | On_disk { store; loc; _ } -> (store, loc)
  | _ ->
    invalid_arg
      ("Granular_marshal.fetch_parent: Unexpected parent link "
     ^ string_of_link lnk)

(** Fetch the parent of a small value in order to read its smalls. If the parent
  has not yet been loaded in memory it will be read from the disk and kept dirty
  because its schema is unknown.*)
let fetch_parent_smalls : parent_link -> any_value array * store =
 fun (PLink parent_link) ->
  match !parent_link with
  | In_cache { store; cell; small_values; _ } ->
    Dbllist.promote (get_lru ()) cell;
    (small_values, store)
  | On_disk { store; loc; schema = None } ->
    (fetch_on_disk_dirty parent_link store loc, store)
  | On_disk { store; loc; schema = Some schema } ->
    (snd (fetch_on_disk parent_link store loc schema), store)
  | _ ->
    invalid_arg
      ("Granular_marshal.fetch_parent: Unexpected parent link "
     ^ string_of_link parent_link)

let rec fetch : type a. a link -> a =
 fun lnk ->
  match !lnk with
  | In_cache { value; schema = Some _; cell; _ } ->
    Dbllist.promote (get_lru ()) cell;
    value
  | In_cache { schema = None; _ } ->
    invalid_arg "Granular_marshal.fetch: accessing dirty cached value"
  | Duplicate original_lnk -> fetch original_lnk
  | On_disk { store; loc; schema = Some schema } ->
    fst (fetch_on_disk lnk store loc schema)
  | On_disk_small { parent; small_pos; small_type_id; small_schema } -> (
    let smalls, store = fetch_parent_smalls parent in
    match smalls.(small_pos) with
    | Value (type b) ((v, type_id') : b * _) -> (
      match Type.Id.provably_equal small_type_id type_id' with
      | None -> invalid_arg "Granular_marshal.read_loc: small has wrong type"
      | Some (Equal : (a link, b link) Type.eq) -> v)
    | Unknown v ->
      let v = Obj.magic v in
      small_schema (disk_to_memory_iter store parent) v;
      smalls.(small_pos) <- Value (v, small_type_id);
      v)
  | In_memory v | In_memory_reused v -> v
  | Serialized _
  | Serialized_small _
  | Small _
  | On_disk_ptr _
  | On_disk_small_ptr _
  | On_disk { schema = None; _ } ->
    invalid_arg
      ("Granular_marshal.fetch: accesssing dirty link " ^ string_of_link lnk)

let rec reuse original_lnk =
  match !original_lnk with
  | In_memory v -> original_lnk := In_memory_reused v
  | In_memory_reused _ -> ()
  | On_disk _ -> ()
  | Duplicate link -> reuse link
  | _ ->
    invalid_arg
    @@ Printf.sprintf "Granular_marshal.reuse: not in memory, got %s"
         (string_of_link original_lnk)

let cache (type a) (module Key : Hashtbl.HashedType with type t = a) =
  let module H = Hashtbl.Make (Key) in
  let cache = H.create 16 in
  fun (lnk : a link) ->
    if not (is_on_disk lnk) then
      let key = fetch lnk in
      match H.find cache key with
      | original_lnk ->
        assert (original_lnk != lnk);
        reuse original_lnk;
        lnk := Duplicate original_lnk
      | exception Not_found -> H.add cache key lnk

let relativize ~wrt:path =
  let path_segments = Misc.split_path path in
  let rec aux path target =
    match (path, target) with
    | p :: tl, t :: tl_target when p = t -> aux tl tl_target
    | [], target -> target
    | _ :: _, [] -> List.map (Fun.const "..") path
    | _ :: _, _ :: _ -> List.map (Fun.const "..") path @ target
  in
  fun target ->
    let target_segments = Misc.split_path target in
    List.fold_left Filename.concat "" (aux path_segments target_segments)

let write ?(flags = []) fd ~filename ~id root_schema root_value =
  let relativize =
    relativize ~wrt:Filename.(dirname (concat (Unix.getcwd ()) filename))
  in
  let id' = binstring_of_int id in
  output_string fd id';
  let pt_root = pos_out fd in
  output_string fd (String.make ptr_size '\000');
  let rec iter size ~small_children =
    { yield =
        (fun (type a) (lnk : a link) _type_id (schema : a schema) : unit ->
          match !lnk with
          | Serialized _
          | Serialized_small _
          | Small _
          | On_disk_ptr _
          | On_disk_small_ptr _ -> ()
          | In_memory_reused v -> write_child_reused lnk schema v
          | Duplicate original_lnk -> (
            match !original_lnk with
            | Serialized _ | Serialized_small _ | On_disk_ptr _ ->
              lnk := !original_lnk
            | In_memory_reused v ->
              write_child_reused original_lnk schema v;
              lnk := !original_lnk
            | On_disk { store = { filename; id; _ }; loc; _ }
            | In_cache { loc; store = { filename; id; _ }; _ } ->
              lnk := On_disk_ptr { filename; id; loc }
            | _ ->
              failwith
                (Format.sprintf
                   "Granular_marshal.write: duplicate not reused got %s"
                   (string_of_link original_lnk)))
          | In_memory v -> write_child lnk schema v size ~small_children
          | In_cache { loc; store = { filename; id; _ }; _ } ->
            let filename = relativize filename in
            lnk := On_disk_ptr { filename; id; loc }
          | On_disk { store = { filename; id; _ }; loc; _ } ->
            (* TODO we could have all the possible filenames wrote once
               somewhere in the file. *)
            let filename = relativize filename in
            lnk := On_disk_ptr { filename; id; loc }
          | On_disk_small { parent; small_pos; _ } ->
            let { filename; id; _ }, loc = store_and_loc_of_parent parent in
            let filename = relativize filename in
            lnk := On_disk_small_ptr { filename; id; loc; pos = small_pos })
    }
  and output_and_mark (V v) (small_children : any_val_link list) =
    let new_smalls =
      (* Some smalls might have been already serialized by another value *)
      List.filter
        (fun (Vlink (_v, lnk)) ->
          match !lnk with
          | On_disk_small_ptr _ | Serialized_small _ ->
            (* This small has already been serialized by another owner *) false
          | _ -> true)
        small_children
    in
    let smalls =
      (* We iter on the smalls to set their links with the position in the array and *)
      List.mapi
        (fun i (Vlink (v, lnk)) ->
          lnk := Small i;
          V v)
        new_smalls
      |> Array.of_list
    in
    let loc = pos_out fd in
    Marshal.to_channel fd (v, smalls) flags;
    (* Now we replace the links by an indirection in case they are reused *)
    List.iteri
      (fun i (Vlink (_v, lnk)) -> lnk := Serialized_small { loc; pos = i })
      new_smalls
  and write_child : type a. a link -> a schema -> a -> _ =
   fun lnk schema v size ~small_children ->
    let v_size, v_smalls = write_children schema v in
    if v_size > 4096 then (
      lnk := Serialized { loc = pos_out fd };
      output_and_mark (V v) v_smalls)
    else (
      size := !size + v_size;
      (* We don't care about the order since smalls are numbered right before
         writing to the disk. *)
      let smalls = List.rev_append v_smalls !small_children in
      small_children := Vlink (v, lnk) :: smalls)
  and write_children : type a. a schema -> a -> _ =
   fun schema v ->
    let children_size = ref 0 in
    let small_children = ref [] in
    schema (iter children_size ~small_children) v;
    let v_size = Obj.(reachable_words (repr v)) in
    (!children_size + v_size, !small_children)
  and write_child_reused : type a. a link -> a schema -> a -> unit =
   fun lnk schema v ->
    let _v_size, v_smalls = write_children schema v in
    lnk := Serialized { loc = pos_out fd };
    output_and_mark (V v) v_smalls
  in
  let _, root_value_smalls = write_children root_schema root_value in
  let root_loc = pos_out fd in
  output_and_mark (V root_value) root_value_smalls;
  seek_out fd pt_root;
  output_string fd (binstring_of_int root_loc)

let read filename fd root_schema =
  let id = int_of_binstring (really_input_string fd 8) in
  let filename =
    if Filename.is_relative filename then
      Filename.concat (Unix.getcwd ()) filename
    else filename
  in
  let store = { filename; id; cache = Cache_cache.read filename } in
  let root_loc = int_of_binstring (really_input_string fd 8) in
  let parent_link =
    ref (On_disk { loc = root_loc; store; schema = Some root_schema })
  in
  let root_value, _, _ =
    read_loc store fd root_loc root_schema (PLink parent_link)
  in
  root_value

let () =
  at_exit (fun () ->
      match !last_open_store with
      | None -> ()
      | Some (_, fd) -> close_in fd)
