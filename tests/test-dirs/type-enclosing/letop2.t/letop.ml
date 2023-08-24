let (let+) x f = Option.map f x

let (and+) x y =
  Option.bind x @@ fun x ->
  Option.map (fun y -> (x, y)) y

let minus_three (tbl1, tbl2) (key1, key2) =
  let+ foo = Hashtbl.find_opt tbl1 key1
  and+ bar = Hashtbl.find_opt tbl2 key2
  and+ man = Hashtbl.find_opt tbl2 key2 in
  foo + bar - man
