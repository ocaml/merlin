type file_id = Unix.stats option
(** An instance of file_id represents the identity of a file contents.
    Use this to quickly detect if a file has changed.
    (Detection is done by checking some fields from stat syscall,
    it an be tricked but should behave well in regular cases.
    FIXME: precision of mtime is still the second?!
*)

let cache = ref None

let with_cache k =
  Std.let_ref cache (Some (Hashtbl.create 7)) k

let file_id filename =
  try Some (Unix.stat filename)
  with _ -> None

let file_id_check a b =
  let open Unix in
  match a, b with
  | None, None -> true
  | Some a, Some b ->
    a.st_mtime = b.st_mtime &&
    a.st_size = b.st_size &&
    a.st_ino = b.st_ino &&
    a.st_dev = b.st_dev
  | Some _, None | None, Some _ -> false

let cached_file_id filename =
  match !cache with
  | None -> file_id filename
  | Some table ->
    match Hashtbl.find table filename with
    | stats ->
      Logger.log "dir_cache" "reuse cache" filename;
      stats
    | exception Not_found ->
      let stats = file_id filename in
      Hashtbl.add table filename stats;
      stats
