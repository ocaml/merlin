(* This module exists to prevent a dependency cycle with Types. *)

let rec compare_longidents ?(compare_strings = String.compare)
    (l1 : Longident.t) (l2 : Longident.t) =
  match (l1, l2) with
  | Lident s1, Lident s2 -> compare_strings s1 s2
  | Lident _, _ -> -1
  | _, Lident _ -> 1
  | Ldot (l1, s1), Ldot (l2, s2) ->
    let c = compare_longidents l1.txt l2.txt in
    if c = 0 then compare_strings s1.txt s2.txt else c
  | Lapply (l1, l'1), Lapply (l2, l'2) ->
    let c = compare_longidents l1.txt l2.txt in
    if c = 0 then compare_longidents l'1.txt l'2.txt else c
  | Ldot _, Lapply _ -> -1
  | Lapply _, Ldot _ -> 1

module Lid_set = Set.Make (struct
  type t = Longident.t
  let compare a b = compare_longidents a b
end)

module Lid_map = Map.Make (struct
  type t = Longident.t
  let compare a b = compare_longidents a b
end)

module Item = struct
  type t = Shape.Sig_component_kind.t * Path.t

  (* Since we are versing these paths in a different structure (the
      priority queue) before shortening, it does not seems useful tu use a
      custom path comparison function here. *)

  let compare (_, p1) (_, p2) = Path.compare p1 p2
end

module Paths = Set.Make (Item)

let pp_paths ppf t =
  let pp_sep ppf () = Format.fprintf ppf ";@;" in
  let paths = Paths.elements t |> List.map (fun (_, p) -> p) in
  Format.pp_print_list ~pp_sep (Format_doc.compat Path.print) ppf paths

module String_map = Map.Make (String)

module Kinds = Set.Make (struct
  type t = Shape.Sig_component_kind.t

  (* Only constant constructors: structural comparison is exact. *)
  let compare = Stdlib.compare
end)

module Segment = struct
  (* One step of a path: [Root] is the ident a path starts with, [Apply] holds
     a whole functor argument (so a substitution can replace an argument but
     not reach inside one), [Extra] a [Pextra_ty] suffix. *)
  type t =
    | Root of Ident.t
    | Dot of string
    | Apply of Path.t
    | Extra of Path.extra_ty

  let rank = function
    | Root _ -> 0
    | Dot _ -> 1
    | Apply _ -> 2
    | Extra _ -> 3

  let compare s s' =
    match (s, s') with
    | Root i, Root i' -> Ident.compare i i'
    | Dot n, Dot n' -> String.compare n n'
    | Apply p, Apply p' -> Path.compare p p'
    | Extra e, Extra e' -> Path.compare_extra e e'
    | (Root _ | Dot _ | Apply _ | Extra _), _ -> Int.compare (rank s) (rank s')

  let print ppf = function
    | Root id -> Ident.print ppf id
    | Dot name -> Format.pp_print_string ppf name
    | Apply p -> Format.fprintf ppf "(%a)" (Format_doc.compat Path.print) p
    | Extra (Pcstr_ty c) -> Format.fprintf ppf "#cstr:%s" c
    | Extra Pext_ty -> Format.pp_print_string ppf "#ext"
end

module Segment_map = Map.Make (Segment)

module Path_trie = struct
  (* A trie of path segments. The position of a node in the trie is the apparent
     path it stands for (ie: without opened or substituted away prefixes). The
     original paths are stored inside the leaf. *)
  type t = Trie of Paths.t * t Segment_map.t

  let pp_kinds fmt paths =
    let pp_sep fmt () = Format.fprintf fmt ";@;" in
    Format.fprintf fmt "@[<1>[%a]@]"
      (Format.pp_print_list ~pp_sep (fun fmt (kind, full_path) ->
           Format.fprintf fmt "%s: %a"
             (Shape.Sig_component_kind.to_string kind)
             (Format_doc.compat Path.print)
             full_path))
      (Paths.elements paths)

  let rec pp fmt (Trie (paths, tries)) =
    Format.fprintf fmt "%a :> %a" pp_kinds paths
      (Format.pp_print_seq (fun fmt (segment, trie) ->
           Format.fprintf fmt "@[<v 2>%a: %a@]" Segment.print segment pp trie))
      (Segment_map.to_seq tries)

  let node ?(children = Segment_map.empty) paths = Trie (paths, children)

  let empty = node Paths.empty

  let is_empty (Trie (_, children)) = Segment_map.is_empty children

  let trie_of_path ?children path paths =
    let s_node (segment : Segment.t) acc =
      node ~children:(Segment_map.singleton segment acc) Paths.empty
    in
    let rec aux acc (path : Path.t) =
      match path with
      | Pident id -> s_node (Root id) acc
      | Pdot (path, name) -> aux (s_node (Dot name) acc) path
      | Papply (path, arg) -> aux (s_node (Apply arg) acc) path
      | Pextra_ty (path, extra) -> aux (s_node (Extra extra) acc) path
    in
    aux (node ?children paths) path

  let rec union (Trie (k, m)) (Trie (k', m')) =
    Trie
      ( Paths.union k k',
        Segment_map.union (fun _seg t t' -> Some (union t t')) m m' )

  (* [full_path] defaults to [path]: a name that denotes itself. *)
  let singleton ?full_path path kind =
    let full_path = Option.value ~default:path full_path in
    trie_of_path path (Paths.singleton (kind, full_path))

  let add ?full_path path kind t = union t (singleton ?full_path path kind)

  (* Takes the paths rooted at [id] out of [t]. TODO: callers used to take by
     name, they now have to provide the ident itself. *)
  let take id (Trie (kinds, children)) =
    let root = Segment.Root id in
    match Segment_map.find_opt root children with
    | None -> (None, Trie (kinds, children))
    | Some t ->
      ( Some (Trie (kinds, Segment_map.singleton root t)),
        Trie (kinds, Segment_map.remove root children) )

  let reach t path =
    let child (Trie (_, children)) (segment : Segment.t) =
      Segment_map.find_opt segment children
    in
    let rec aux (path : Path.t) =
      match path with
      | Pident id -> child t (Root id)
      | Pdot (p, name) -> descend p (Dot name)
      | Papply (p, arg) -> descend p (Apply arg)
      | Pextra_ty (p, extra) -> descend p (Extra extra)
    and descend p (segment : Segment.t) =
      Option.bind (aux p) (fun t -> child t segment)
    in
    aux path

  (* The path of a node is its position in the trie: [Root] segments start a
     path, the other ones extend the path of their parent. *)
  let to_seq (Trie (_root_kinds, roots)) =
    let root : Segment.t -> Path.t = function
      | Root id -> Pident id
      | Dot _ | Apply _ | Extra _ ->
        Misc.fatal_error "Discourse_types: path trie root is not an ident"
    in
    let extend path : Segment.t -> Path.t = function
      | Dot name -> Pdot (path, name)
      | Apply arg -> Papply (path, arg)
      | Extra extra -> Pextra_ty (path, extra)
      | Root _ -> Misc.fatal_error "Discourse_types: nested path trie root"
    in
    let rec aux path (Trie (kinds, tries)) seq =
      let seq () =
        Segment_map.fold
          (fun segment t acc -> aux (extend path segment) t acc)
          tries seq
      in
      if not (Paths.is_empty kinds) then Seq.Cons ((path, kinds), seq)
      else seq ()
    in
    fun () ->
      Segment_map.fold
        (fun segment t acc -> aux (root segment) t acc)
        roots Seq.Nil

  let size t =
    let rec aux acc (Trie (kinds, tries)) =
      Segment_map.fold
        (fun _ t acc -> aux (1 + acc) t)
        tries
        (Paths.cardinal kinds + acc)
    in
    aux 0 t

  let pp_path_kinds ppf (path, kinds) =
    Format.fprintf ppf "@[<2>%a@ %a@]"
      (Format_doc.compat Path.print)
      path pp_kinds kinds

  let pp_seq fmt t =
    let pp_sep fmt () = Format.fprintf fmt ";@ " in
    Format.fprintf fmt "%a"
      (Format.pp_print_seq ~pp_sep pp_path_kinds)
      (to_seq t)
end

(* Paths rooted only in predefined idents are not recorded. Paths rooted only in
   global (persistent) idents are kept apart in [extern] since substitutions
   never apply to them. *)
type t = { local : Paths.t; extern : Paths.t }

let empty = { local = Paths.empty; extern = Paths.empty }

let add ?(predef = false) ((_, path) as item) t =
  let heads = Path.heads path in
  if not predef && List.for_all Ident.is_predef heads then t
  else if List.for_all Ident.global heads then
    { t with extern = Paths.add item t.extern }
  else { t with local = Paths.add item t.local }

let singleton i = add i empty

let union t t' =
  { local = Paths.union t.local t'.local;
    extern = Paths.union t.extern t'.extern
  }

let pp fmt t = pp_paths fmt (Paths.union t.local t.extern)

(* A substitution maps a path to the paths it can be replaced with. *)
type substs = Path.Set.t Path.Map.t

type discourse = { paths : Path_trie.t; substs : substs }

let pp_path = Format_doc.compat Path.print

let pp_substs fmt (t : substs) =
  let pp_sep fmt () = Format.fprintf fmt ";@ " in
  let pp_replacements fmt paths =
    Format.fprintf fmt "@[<1>[%a]@]"
      (Format.pp_print_list ~pp_sep pp_path)
      (Path.Set.elements paths)
  in
  let pp_binding fmt (path, replacements) =
    Format.fprintf fmt "@[<2>%a ->@ %a@]" pp_path path pp_replacements
      replacements
  in
  Format.fprintf fmt "%a"
    (Format.pp_print_seq ~pp_sep pp_binding)
    (Path.Map.to_seq t)
