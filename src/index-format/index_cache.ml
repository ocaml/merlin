include File_cache.Make (struct
  type t = Index_format.index
  let read file = Index_format.read_exn ~file
  let cache_name = "Index_cache"
end)
