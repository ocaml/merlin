let main () =
  match List.tl (Array.to_list Sys.argv) with
  | "merlin3" :: args -> New_merlin.run args
  | "merlin-old" :: args -> Old_merlin.run args
  | args -> Old_merlin.run args

let () = main ()
