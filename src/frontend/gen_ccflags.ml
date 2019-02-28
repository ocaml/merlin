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

let ccomp_type   = Sys.argv.(1)
let pre_flags_f  = Sys.argv.(2)
let post_flags_f = Sys.argv.(3)

let pre_flags, post_flags =
  if Str.string_match (Str.regexp "msvc") ccomp_type 0 then
    "/Fe", "advapi32.lib"
  else
    "-o", ""

let write_lines f s =
  let oc = open_out f in
  output_string oc s;
  close_out oc

let () =
  write_lines pre_flags_f pre_flags;
  write_lines post_flags_f post_flags
