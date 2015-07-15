open Std

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
  ppxs: string list;
  ppxopts: StringListSet.t StringMap.t;
}

let empty = { ppxs = []; ppxopts = StringMap.empty }

let add_ppx ppx t =
  if List.mem ppx t.ppxs
  then t
  else {t with ppxs = ppx :: t.ppxs}

let add_ppxopts ppx opts t =
  match opts with
  | [] -> t
  | opts ->
    let ppx = Filename.basename ppx in
    let opts' =
      try StringMap.find ppx t.ppxopts
      with Not_found -> StringListSet.empty
    in
    let opts' = StringListSet.add opts opts' in
    {t with ppxopts = StringMap.add ppx opts' t.ppxopts}

let union ta tb =
  { ppxs = List.filter_dup (ta.ppxs @ tb.ppxs);
    ppxopts = StringMap.merge (fun k a b -> match a, b with
        | v, None | None, v -> v
        | Some a, Some b -> Some (StringListSet.union a b))
        ta.ppxopts tb.ppxopts
  }

let command_line t =
  List.fold_right ~f:(fun ppx ppxs ->
      let basename = Filename.basename ppx in
      let opts =
        try StringMap.find basename t.ppxopts
        with Not_found -> StringListSet.empty
      in
      let opts = StringListSet.fold (@) opts [] in
      String.concat " " (ppx :: opts) :: ppxs)
    t.ppxs ~init:[]

let dump t =
  let string_list k lst = `String k :: lst in
  `Assoc [
    "preprocessors",
    `List (List.fold_right ~f:string_list t.ppxs ~init:[]);
    "options",
    `Assoc (
      StringMap.fold (fun k opts acc ->
          let opts = StringListSet.fold (fun opt lst ->
              `List (List.fold_right ~f:string_list opt ~init:[]) :: lst)
              opts [] in
          (k, `List opts) :: acc)
        t.ppxopts []
    )
  ]
