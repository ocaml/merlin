type t = string [@@deriving yojson { strict = false }]

let to_path (uri : t) =
  let proto =
    match Sys.win32 with
    | true -> "file:///"
    | false -> "file://"
  in
  let path =
    match Stringext.chop_prefix ~prefix:proto uri with
    | Some path -> path
    | None -> uri in
  path
  |> Stringext.replace_all ~pattern:"\\" ~with_:"/"
  |> Stringext.replace_all ~pattern:"%3A" ~with_:":"
  |> Stringext.replace_all ~pattern:"%5C" ~with_:"/"

let of_path (path : string) =
  let proto = match Sys.win32 with
    | true -> "file:///"
    | false -> "file://"
  in
  let path =
    path
    |> Stringext.replace_all ~pattern:"\\" ~with_:"/"
    |> Stringext.replace_all ~pattern:":" ~with_:"%3A"
  in
  proto ^ path

let pp fmt uri = Format.fprintf fmt "%s" uri
