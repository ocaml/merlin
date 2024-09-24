(* {{{ COPYING *(

     This file is part of Merlin, an helper for ocaml editors

     Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                                Thomas Refis  <refis.thomas(_)gmail.com>
                                Simon Castellan  <simon.castellan(_)iuwt.fr>

     Permission is hereby granted, free of charge, to any person obtaining a
     copy of this software and associated documentation files (the "Software"),
     to deal in the Software without restriction, including without limitation the
     rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
     sell copies of the Software, and to permit persons to whom the Software is
     furnished to do so, subject to the following conditions:

     The above copyright notice and this permission notice shall be included in
     all copies or substantial portions of the Software.

     The Software is provided "as is", without warranty of any kind, express or
     implied, including but not limited to the warranties of merchantability,
     fitness for a particular purpose and noninfringement. In no event shall
     the authors or copyright holders be liable for any claim, damages or other
     liability, whether in an action of contract, tort or otherwise, arising
     from, out of or in connection with the software or the use or other dealings
     in the Software.

   )* }}} *)

open Std

let default_loc = Location.none
let default_env = Env.empty

type t =
  { t_node : Mbrowse.node;
    t_loc : Location.t;
    t_env : Env.t;
    t_children : t list lazy_t
  }

let of_node ?(env = default_env) node =
  let rec one t_env t_node =
    let t_loc = Mbrowse.node_loc t_node in
    let rec t = { t_node; t_env; t_loc; t_children = lazy (aux t) } in
    t
  and aux t =
    Mbrowse.fold_node
      (fun env node acc -> one env node :: acc)
      t.t_env t.t_node []
  in
  one (Browse_raw.node_update_env env node) node

let of_browse b =
  let env, node = Mbrowse.leaf_node b in
  of_node ~env node

let dummy =
  { t_node = Browse_raw.Dummy;
    t_loc = default_loc;
    t_env = default_env;
    t_children = lazy []
  }

let all_occurrences_of_prefix path node =
  let rec path_prefix ~prefix path =
    Path.same prefix path
    ||
    match path with
    | Pdot (p, _) -> path_prefix ~prefix p
    | _ -> false
  in
  let rec aux env node acc =
    let acc =
      let paths_and_lids = Browse_raw.node_paths_and_longident node in
      let has_prefix ({ Location.txt; _ }, _) =
        match txt with
        | Path.Pdot (p, _) -> path_prefix ~prefix:path p
        | _ -> false
      in
      List.fold_right paths_and_lids ~init:acc ~f:(fun elt acc ->
          if has_prefix elt then elt :: acc else acc)
    in
    Browse_raw.fold_node aux env node acc
  in
  aux Env.empty node []
