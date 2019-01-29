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

let ocaml_version_val =
  match
    Scanf.sscanf Sys.argv.(1) "%s@.%s@.%d" (fun maj min p -> maj, min, p)
  with
  | "4", "02", _ ->
      "`OCaml_4_02_3"
  | "4", "07", p ->
      Printf.sprintf "`OCaml_4_07_%d" p
  | maj, min, _ ->
      Printf.sprintf "`OCaml_%s_%s_0" maj min

let () =
  Printf.printf {|
let version = "%%VERSION%%"
let ocamlversion :
  [ `OCaml_4_02_0 | `OCaml_4_02_1 | `OCaml_4_02_2 | `OCaml_4_02_3
  | `OCaml_4_03_0 | `OCaml_4_04_0 | `OCaml_4_05_0 | `OCaml_4_06_0
  | `OCaml_4_07_0 | `OCaml_4_07_1 ] = %s
|} ocaml_version_val
