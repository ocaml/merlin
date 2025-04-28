module G = Granular_marshal

type pos = { lnum : int; cnum : int; bol : int }
let pos_of_loc { Lexing.pos_lnum = lnum; pos_cnum = cnum; pos_bol = bol; _ } =
  { lnum; cnum; bol }

let loc_of_pos pos_fname { lnum; cnum; bol } =
  { Lexing.pos_lnum = lnum; pos_cnum = cnum; pos_bol = bol; pos_fname }

type t =
  { longident : Longident.t G.link;
    filename : string G.link;
    start : pos;
    stop : pos;
    ghost : bool
  }

let of_lid { Location.txt; loc = { loc_start; loc_end; loc_ghost } } =
  { filename = G.link loc_start.pos_fname;
    longident = G.link txt;
    ghost = loc_ghost;
    start = pos_of_loc loc_start;
    stop = pos_of_loc loc_end
  }

let to_lid { filename; longident; ghost; start; stop } =
  let filename = G.fetch filename in
  let loc_start = loc_of_pos filename start in
  let loc_end = loc_of_pos filename stop in
  { Location.txt = G.fetch longident;
    loc = { loc_start; loc_end; loc_ghost = ghost }
  }

let pp fmt t =
  let { Location.txt; loc } = to_lid t in
  Format.fprintf fmt "%S: %a"
    (try Longident.flatten txt |> String.concat "." with _ -> "<?>")
    Location.print_loc loc

let compare_pos p1 p2 = Int.compare p1.cnum p2.cnum
let compare_filename t1 t2 =
  String.compare (G.fetch t1.filename) (G.fetch t2.filename)

let compare t1 t2 =
  match compare_filename t1 t2 with
  | 0 -> (
    match compare_pos t1.start t2.start with
    | 0 -> compare_pos t1.stop t2.stop
    | c -> c)
  | c -> c

let type_string : string G.link Type.Id.t = Type.Id.make ()
let type_longident : Longident.t G.link Type.Id.t = Type.Id.make ()

let schema iter t =
  iter.G.yield t.filename type_string G.schema_no_sublinks;
  iter.G.yield t.longident type_longident G.schema_no_sublinks

module Li = struct
  include Longident
  let equal = ( = )
  let hash = Hashtbl.hash
end

let cache () = G.(cache (module String), cache (module Li))

let deduplicate (cache_filename, cache_lid) t =
  cache_filename t.filename;
  cache_lid t.longident
