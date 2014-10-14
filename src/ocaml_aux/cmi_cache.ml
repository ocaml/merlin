include File_cache.Make (struct
  type t   = Cmi_format.cmi_infos
  let read = Cmi_format.read_cmi
end)
