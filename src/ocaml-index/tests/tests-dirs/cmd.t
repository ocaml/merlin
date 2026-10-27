  $ ocaml-index aggregate
  $ ocaml-index aggregate --debug
  [debug] Debug log is enabled
  total_cap 		: 1000000000
  size 		: 0
  promote_count 	: 0
  add_count 		: 0
  discard_count 	: 0
  add_size 		: 0
  discard_size 	: 0
  volume_conservation 	: 0 = 0 + 0 : true

  $ ocaml-index --help
  ocaml-index [COMMAND] [-verbose] <file1> [<file2>] ... -o <output>
    --verbose Output more information
    --debug Output debugging information
    -o Set output file name. Pointers to sub-indexes are stored relative to the output file's directory.
    --root Set the root path for all relative locations
    --rewrite-root Rewrite locations paths using the provided root
    --store-shapes Aggregate input-indexes shapes and store them in the new index
    --standalone The output index will not contain pointers to the input index/shape files. All data is copied into the output index.
    --no-standalone The output may contain pointers to the input index/shape files. This will make the output index smaller, but to use it, the input index/shape files must still be available. (default)
    -I An extra directory to add to the load path
    -H An extra hidden directory to add to the load path
    --no-cmt-load-path Do not initialize the load path with the paths found in the first input cmt file
    --cache-size Set LRU cache size in kb. Will bound memory usage in read-heavy scenarios. Defaults to 1_000_000 (1gb)
    -help  Display this list of options
    --help  Display this list of options
