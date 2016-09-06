let main () =
  match List.tl (Array.to_list Sys.argv) with
  | "single" :: args -> New_merlin.run args
  | "daemon" :: _ -> failwith "TODO"
  | "stop-daemon" :: _ -> failwith "TODO"
  | "old-protocol" :: args -> Old_merlin.run args
  | args -> Old_merlin.run args

let () = main ()
