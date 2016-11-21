let to_string_list p =
  let rec aux acc = function
  | Path.Pident id -> id.Ident.name :: acc
  | Path.Pdot (p, str, _) -> aux (str :: acc) p
  | _ -> assert false
  in
  aux [] p
