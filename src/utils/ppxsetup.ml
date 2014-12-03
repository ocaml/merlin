module StringSet = Set.Make(String)
module StringListSet = Set.Make(struct
    type t = string list
    let rec compare lx ly = match lx, ly with
      | [], [] -> 0
      | [], (_ :: _) -> -1
      | (_ :: _), [] -> 1
      | (x :: xs), (y :: ys) -> match String.compare x y with
        | 0 -> compare xs ys
        | n -> n
  end)
module StringMap = Map.Make(String)

type t = {
  ppxs: StringSet.t;
  ppxopts: StringListSet.t StringMap.t;
}

let empty = { ppxs = StringSet.empty; ppxopts = StringMap.empty }

let add_ppx ppx t = {t with ppxs = StringSet.add ppx t.ppxs}

let add_ppxopts ppx opts t =
  match opts with
  | [] -> t
  | opts ->
    let opts' =
      try StringMap.find ppx t.ppxopts
      with Not_found -> StringListSet.empty
    in
    let opts' = StringListSet.add opts opts' in
    {t with ppxopts = StringMap.add ppx opts' t.ppxopts}

let union ta tb =
  { ppxs = StringSet.union ta.ppxs tb.ppxs;
    ppxopts = StringMap.merge (fun k a b -> match a, b with
        | v, None | None, v -> v
        | Some a, Some b -> Some (StringListSet.union a b))
        ta.ppxopts tb.ppxopts
  }

let command_line t =
  StringSet.fold (fun ppx ppxs ->
      let basename = Filename.basename ppx in
      let opts =
        try StringMap.find basename t.ppxopts
        with Not_found -> StringListSet.empty
      in
      let opts = StringListSet.fold (@) opts [] in
      String.concat " " (ppx :: opts) :: ppxs)
    t.ppxs []

let dump t =
  let string_list k lst = `String k :: lst in
  `Assoc [
    "preprocessors",
    `List (StringSet.fold string_list t.ppxs []);
    "options",
    `Assoc (
      StringMap.fold (fun k opts acc ->
          let opts = StringListSet.fold (fun opt lst ->
              `List (List.fold_right string_list opt []) :: lst)
              opts [] in
          (k, `List opts) :: acc)
        t.ppxopts []
    )
  ]
