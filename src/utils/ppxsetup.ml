open Std

module StringMap = Map.Make(String)

type t = {
  ppxs: string list;
  ppxopts: string list list StringMap.t;
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
    let optss =
      try StringMap.find ppx t.ppxopts
      with Not_found -> []
    in
    if not (List.mem ~set:optss opts) then
      {t with ppxopts = StringMap.add ppx (opts :: optss) t.ppxopts}
    else t

let union ta tb =
  { ppxs = List.filter_dup (ta.ppxs @ tb.ppxs);
    ppxopts = StringMap.merge (fun k a b -> match a, b with
        | v, None | None, v -> v
        | Some a, Some b -> Some (List.filter_dup (a @ b)))
        ta.ppxopts tb.ppxopts
  }

let command_line t =
  List.fold_right ~f:(fun ppx ppxs ->
      let basename = Filename.basename ppx in
      let opts =
        try StringMap.find basename t.ppxopts
        with Not_found -> []
      in
      let opts = List.concat (List.rev opts) in
      String.concat " " (ppx :: opts) :: ppxs)
    t.ppxs ~init:[]

let dump t =
  let string k = `String k in
  let string_list l = `List (List.map ~f:string l) in
  `Assoc [
    "preprocessors",
    string_list t.ppxs;
    "options",
    `Assoc (
      StringMap.fold (fun k opts acc ->
          let opts = List.rev_map ~f:string_list opts in
          (k, `List opts) :: acc)
        t.ppxopts []
    )
  ]
