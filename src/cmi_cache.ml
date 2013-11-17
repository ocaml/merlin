let cache : (string, float * Cmi_format.cmi_infos) Hashtbl.t
          = Hashtbl.create 17

let read_cmi filename =
  let mtime = Misc.file_mtime filename in
  try 
    let mtime', cmi = Hashtbl.find cache filename in
    if mtime <> mtime' then raise Not_found;
    cmi
  with Not_found ->
  try
    let cmi = Cmi_format.read_cmi filename in
    Hashtbl.replace cache filename (mtime, cmi);
    cmi
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
