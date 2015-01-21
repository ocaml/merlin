module StringMap = Map.Make(struct
    type t = string
    let compare (a : string) b = compare a b
  end)

type opened = {
  opened: bool;
  subs: opened StringMap.t;
}

let opened = ref None

let opened_empty = {opened = true; subs = StringMap.empty}

let get_opened env =
  if env == Env.empty then Tbl.empty else
  match !opened with
  | Some map -> map
  | None ->
    let rec add_opens map summary =
      match Raw_compat.summary_prev summary with
      | None -> map
      | Some s ->
        match Raw_compat.summary_open_path summary with
        | None -> add_opens map s
        | Some p ->
          let rec create_path = function
            | x :: xs ->
              let subs = StringMap.singleton x (create_path xs) in
              {opened = false; subs}
            | [] -> opened_empty
          in
          let rec add_path map = function
            | x :: xs ->
              let map' =
                match
                  try Some (StringMap.find x map.subs) with Not_found -> None
                with
                | None -> create_path xs
                | Some map -> add_path map xs
              in
              { map with subs = StringMap.add x map' map.subs }
            | [] -> {map with opened = true}
          in
          let add_path map id dots =
            let sub =
              match try Some (Tbl.find id map) with Not_found -> None with
              | None -> create_path dots
              | Some sub -> add_path sub dots
            in
            Tbl.add id sub map
          in
          let rec traverse_path acc = function
            | Path.Papply _ -> add_opens map s
            | Path.Pdot (path, name, _) -> traverse_path (name :: acc) path
            | Path.Pident id -> add_opens (add_path map id acc) s
          in
          traverse_path [] p
    in
    let opens = add_opens Tbl.empty (Env.summary env) in
    opened := Some opens;
    opens

let shorten env path =
  let opened = get_opened env in
  let rec apply_dots path = function
    | x :: xs -> apply_dots (Path.Pdot (path, x, 0)) xs
    | [] -> path
  in
  let rec traverse_path org acc = function
    | Path.Pident id ->
      simplify org id acc
    | Path.Pdot (p, dot, _) ->
      traverse_path org (dot :: acc) p
    | Path.Papply (p1,p2) ->
      let t1 = traverse_path p1 [] p1 and t2 = traverse_path p2 [] p2 in
      if t1 == p1 && p2 == t2 then
        org
      else
        apply_dots (Path.Papply (t1, t2)) acc

  and simplify org id path =
    try
      let rec shortest_path map path = match path with
        | [] -> raise Not_found
        | x :: xs ->
          match
            try Some (StringMap.find x map.subs) with Not_found -> None
          with
          | None -> if map.opened then x, xs else raise Not_found
          | Some map' -> shortest_path map' xs
      in
      let x, xs = shortest_path (Tbl.find id opened) path in
      (* FIXME? Fake ident *)
      apply_dots (Path.Pident (Ident.create_persistent x)) xs
    with Not_found -> org
  in

  traverse_path path [] path
