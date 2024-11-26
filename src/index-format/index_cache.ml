include File_cache.Make (struct
  type t = Index_format.index * in_channel
  let read file = Index_format.read_exn ~file
  let cache_name = "Index_cache"
  let dispose (_, ic) = close_in ic
end)
