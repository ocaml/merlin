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

type t = string [@@deriving yojson { strict = false }]

let to_path (uri : t) =
  let proto =
    match Sys.win32 with
    | true -> "file:///"
    | false -> "file://"
  in
  let path =
    match Std.String.chop_prefix ~prefix:proto uri with
    | Some path -> path
    | None -> uri in
  path
  |> Std.String.replace_all ~pattern:"\\" ~with_:"/"
  |> Std.String.replace_all ~pattern:"%3A" ~with_:":"
  |> Std.String.replace_all ~pattern:"%5C" ~with_:"/"

let of_path (path : string) =
  let proto = match Sys.win32 with
    | true -> "file:///"
    | false -> "file://"
  in
  let path =
    path
    |> Std.String.replace_all ~pattern:"\\" ~with_:"/"
    |> Std.String.replace_all ~pattern:":" ~with_:"%3A"
  in
  proto ^ path

let pp fmt uri = Format.fprintf fmt "%s" uri
