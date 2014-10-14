let cache : (string, float * Cmt_format.cmt_infos) Hashtbl.t
          = Hashtbl.create 17

let read_cmt filename =
  let mtime = Misc.file_mtime filename in
  try
    let mtime', cmt = Hashtbl.find cache filename in
    if mtime <> mtime' then raise Not_found;
    cmt
  with Not_found ->
  try
    let cmt = Cmt_format.read_cmt filename in
    Hashtbl.replace cache filename (mtime, cmt);
    cmt
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
