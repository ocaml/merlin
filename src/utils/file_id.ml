(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2019  Merlin contributors

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

type t = Unix.stats

let null_stat =
  { Unix.
    st_dev = -1; st_ino = -1; st_kind = Unix.S_REG; st_nlink = -1;
    st_perm = -1; st_uid = -1; st_gid = -1; st_rdev = -1; st_size = -1;
    st_atime = nan; st_mtime = nan; st_ctime = nan }

let get filename =
  try Unix.stat filename
  with _ -> null_stat

let check a b =
  a == b || (
    (a != null_stat) && (b != null_stat) &&
    let open Unix in
    a.st_mtime = b.st_mtime &&
    a.st_size = b.st_size &&
    a.st_ino = b.st_ino &&
    a.st_dev = b.st_dev
  )

let cache = ref None

let with_cache k =
  Std.let_ref cache (Some (Hashtbl.create 7)) k

let get filename =
  match !cache with
  | None -> get filename
  | Some table ->
    match Hashtbl.find table filename with
    | stats ->
      Logger.log ~section:"stat_cache" ~title:"reuse cache" "%s" filename;
      stats
    | exception Not_found ->
      let stats = get filename in
      Hashtbl.add table filename stats;
      stats
