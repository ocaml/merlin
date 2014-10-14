module Make(Input : sig
  type t
  val read : string -> t
end) = struct
  let cache : (string, float * Input.t) Hashtbl.t
            = Hashtbl.create 17

  let read filename =
    let mtime = Misc.file_mtime filename in
    try
      let mtime', file = Hashtbl.find cache filename in
      if mtime <> mtime' then raise Not_found;
      file
    with Not_found ->
    try
      let file = Input.read filename in
      Hashtbl.replace cache filename (mtime, file);
      file
    with exn ->
      Hashtbl.remove cache filename;
      raise exn

  let flush () =
    let invalid =
      Hashtbl.fold
        (fun filename (mtime, _) lst ->
          if Misc.file_mtime filename <> mtime
          then filename :: lst
          else lst)
        cache []
    in
    List.iter (Hashtbl.remove cache) invalid
end
