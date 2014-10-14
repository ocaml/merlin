include File_cache.Make (struct
  type t   = Cmt_format.cmt_infos
  let read = Cmt_format.read_cmt
end)
