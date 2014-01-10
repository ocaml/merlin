open Std
open Misc

(* Verbosity *)
type level =
  | Error
  | Info
  | Debug

let default_destination =
  ref None

module Section = struct
  (* extend as necessary *)
  type t = [
    | `protocol
    | `locate
    | `completion
    | `dot_merlin
    | `internal
  ]

  let switches, switch =
    let protocol   = ref Info, ref None, "protocol"
    and locate     = ref Info, ref None, "locate"
    and completion = ref Info, ref None, "completion"
    and dot_merlin = ref Info, ref None, ".merlin"
    and internal   = ref Info, ref None, "internal"
    in
    [protocol; locate; completion; dot_merlin; internal],
    function
    | `protocol   -> protocol
    | `locate     -> locate
    | `completion -> completion
    | `dot_merlin -> dot_merlin
    | `internal   -> internal

  let dest lvl x =
    let rlvl, rdest, _ = switch x in
    let dest = if lvl <= !rlvl then !rdest else None in
    match lvl, dest with
    | Error, None -> !default_destination
    | _, _ -> dest

  let enabled lvl x = match dest lvl x with Some _ -> true | None -> false

  let to_string s = thd3 (switch s)

  let of_string = function
    | "protocol"   -> `protocol
    | "locate"     -> `locate
    | "completion" -> `completion
    | ".merlin"    -> `dot_merlin
    | "internal"   -> `internal
    | x            -> invalid_arg ("unknown section: " ^ x)

end

let opened_files : (string, out_channel) Hashtbl.t =
  Hashtbl.create 4

let get_or_open path =
  try Hashtbl.find opened_files path
  with Not_found ->
    let oc = open_out path in
    Hashtbl.add opened_files path oc;
    oc

let set_default_destination path =
  let oc = get_or_open path in
  default_destination := Some oc

let monitor ?dest x =
  let _lvl, rdest, _ = Section.switch x in
  let dest =
    match dest with
    | Some path -> get_or_open path
    | None ->
      match !default_destination with
      | None -> invalid_arg "no log file specified"
      | Some dest -> dest
  in
  rdest := Some dest

let forget x = snd3 (Section.switch x) := None

let is_monitored x =
  match !(snd3 (Section.switch x)) with
  | Some _ -> true
  | None -> false

let log section ?prefix msg =
  let prefix =
    match prefix with
    | Some s -> s
    | None -> Printf.sprintf "%s |" (Section.to_string section)
  in
  match Section.dest Info section with
  | Some oc -> Printf.fprintf oc "%s %s\n%!" prefix msg
  | None -> ()

let error section msg =
  match Section.dest Error section with
  | Some oc ->
    Printf.fprintf oc "ERROR(%s) | %s\n%!" (Section.to_string section) msg
  | None -> ()

let debug section msg =
  match Section.dest Debug section with
  | Some oc ->
    Printf.fprintf oc "DEBUG(%s) | %s\n%!" (Section.to_string section) msg
  | None -> ()

let format out level section f x =
  if Section.enabled level section then
    let ppf, to_string = Format.to_string () in
    f ppf x; out section (to_string ())

let logf section ?prefix f x =
  format (log ?prefix) Info section f x

let errorf section f x =
  format error Error section f x

let debugf section f x =
  format debug Debug section f x

let shutdown () =
  Hashtbl.iter (fun _ oc -> close_out oc) opened_files;
  Hashtbl.reset opened_files;
  default_destination := None;
  List.iter ~f:(fun (_,rdest,_) -> rdest := None) Section.switches
