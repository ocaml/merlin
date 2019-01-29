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

open Typedtree

module Pat = struct
  let pat_extra = []
  let pat_attributes = []

  let constant ?(loc=Location.none) pat_env pat_type c =
    let pat_desc = Tpat_constant c in
    { pat_desc; pat_loc = loc; pat_extra; pat_attributes; pat_type; pat_env }

  let var ?loc pat_env pat_type str =
    let pat_loc =
      match loc with
      | None -> str.Asttypes.loc
      | Some loc -> loc
    in
    let pat_desc = Tpat_var (Ident.create str.Asttypes.txt, str) in
    { pat_desc; pat_loc; pat_extra; pat_attributes; pat_type; pat_env }

  let record ?(loc=Location.none) pat_env pat_type lst closed_flag =
    let pat_desc = Tpat_record (lst, closed_flag) in
    { pat_desc; pat_loc = loc; pat_extra; pat_attributes; pat_type; pat_env }

  let tuple ?(loc=Location.none) pat_env pat_type lst =
    let pat_desc = Tpat_tuple lst in
    { pat_desc; pat_loc = loc; pat_extra; pat_attributes; pat_type; pat_env }

  let construct ?(loc=Location.none) pat_env pat_type lid cstr_desc args =
    let pat_desc = Tpat_construct (lid, cstr_desc, args) in
    { pat_desc; pat_loc = loc; pat_extra; pat_attributes; pat_type; pat_env }

  let pat_or ?(loc=Location.none) ?row_desc pat_env pat_type p1 p2 =
    let pat_desc = Tpat_or (p1, p2, row_desc) in
    { pat_desc; pat_loc = loc; pat_extra; pat_attributes; pat_type; pat_env }

  let variant ?(loc=Location.none) pat_env pat_type lbl sub rd =
    let pat_desc = Tpat_variant (lbl, sub, rd) in
    { pat_desc; pat_loc = loc; pat_extra; pat_attributes; pat_type; pat_env }
end
