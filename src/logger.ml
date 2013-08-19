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

let monitored : (Section.t * out_channel) list ref = ref []

let opened_files : (string, out_channel) Hashtbl.t = Hashtbl.create 3

let get_or_open path =
  try Hashtbl.find opened_files path
  with Not_found ->
    let oc = open_out path in
    Hashtbl.add opened_files path oc ;
    oc

let set_default_destination path =
  let oc = get_or_open path in
  default_destination := oc

let monitor ?dest x =
  let dest = 
    match dest with
    | None -> !default_destination
    | Some path -> get_or_open path
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
  let oc =
    try List.assoc section !monitored
    with Not_found -> !default_destination
  in
  Printf.fprintf oc "ERROR(%s) | %s\n%!" (Section.to_string section) msg

let shutdown () =
  Hashtbl.iter (fun _ oc -> close_out oc) opened_files ;
  Hashtbl.reset opened_files ;
  default_destination := stderr ;
  monitored := []
