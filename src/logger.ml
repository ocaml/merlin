(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013  Frédéric Bour  <frederic.bour(_)lakaban.net>
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

module Section = struct
  (* extend as necessary *)
  type t = [
    | `protocol
    | `locate
    | `completion
    | `dot_merlin
  ]

  let to_string = function
    | `protocol -> "protocol"
    | `locate -> "locate"
    | `completion -> "completion"
    | `dot_merlin -> ".merlin"

  let of_string = function
    | "protocol" -> `protocol
    | "locate" -> `locate
    | "completion" -> `completion
    | ".merlin" -> `dot_merlin
    | x -> invalid_arg ("unknown section: " ^ x)
end

let default_destination = ref None

let monitored : (Section.t * out_channel) list ref = ref []

let opened_files : (string, out_channel) Hashtbl.t = Hashtbl.create 4

let get_or_open path =
  try Hashtbl.find opened_files path
  with Not_found ->
    let oc = open_out path in
    Hashtbl.add opened_files path oc ;
    oc

let set_default_destination path =
  let oc = get_or_open path in
  default_destination := Some oc

let monitor ?dest x =
  let dest =
    match dest with
    | Some path -> get_or_open path
    | None ->
      match !default_destination with
      | None -> invalid_arg "no log file specified"
      | Some dest -> dest
  in
  monitored := (x, dest) :: !monitored

let forget x =
  monitored := List.filter (fun (t, _) -> t = x) !monitored

let is_monitored x = List.mem_assoc x !monitored

let log section ?prefix msg =
  let prefix =
    match prefix with
    | Some s -> s
    | None -> Printf.sprintf "%s |" (Section.to_string section)
  in
  try
    let oc = List.assoc section !monitored in
    Printf.fprintf oc "%s %s\n%!" prefix msg
  with Not_found ->
    ()

let error section msg =
  match
    try Some (List.assoc section !monitored)
    with Not_found -> !default_destination
  with
  | None -> ()
  | Some oc ->
    Printf.fprintf oc "ERROR(%s) | %s\n%!" (Section.to_string section) msg

let shutdown () =
  Hashtbl.iter (fun _ oc -> close_out oc) opened_files ;
  Hashtbl.reset opened_files ;
  default_destination := None ;
  monitored := []
