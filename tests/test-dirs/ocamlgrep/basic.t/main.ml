let () =
  let xs = [ 1; 2; 3 ] in
  let n = List.length xs in
  let m = List.length [ "a"; "b" ] in
  Printf.printf "%d %d\n" n m
