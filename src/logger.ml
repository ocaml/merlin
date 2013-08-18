(* extend as necessary *)
module Section = struct
  type t = [
    | `protocol
    | `locate
    | `completion
  ]

  let to_string = function
    | `protocol -> "protocol"
    | `locate -> "locate"
    | `completion -> "completion"

  let of_string = function
    | "protocol" -> `protocol
    | "locate" -> `locate
    | "completion" -> `completion
    | x -> invalid_arg ("unknown section: " ^ x)
end

let default_destination = ref stderr

let set_default_destination oc =
  default_destination := oc

let monitored : (Section.t * out_channel) list ref = ref []

let monitor ?(dest=(!default_destination)) x =
  monitored := (x, dest) :: !monitored

let forget x =
  monitored := List.filter (fun (t, _) -> t = x) !monitored

let is_monitored x = List.exists (fun (t, _) -> t = x) !monitored

let log section ?prefix msg =
  let prefix =
    match prefix with
    | Some s -> s
    | None -> Printf.sprintf "%s |" (Section.to_string section)
  in
  try
    let _, oc = List.find (fun (t, _) -> t = section) !monitored in
    Printf.fprintf oc "%s %s\n%!" prefix msg
  with Not_found ->
    ()

let error section msg =
  let oc =
    try snd (List.find (fun (t, _) -> t = section) !monitored)
    with Not_found -> !default_destination
  in
  Printf.fprintf oc "ERROR(%s) | %s\n%!" (Section.to_string section) msg
