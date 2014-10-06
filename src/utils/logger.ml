open Std
open Misc

(* Verbosity *)
type level = [ `error | `info | `debug ]

let log_level = function
  | `error -> 0
  | `info  -> 1
  | `debug -> 2

type section = {
  name: string;
  mutable destination: out_channel option;
  mutable log_level: int;
}

module Section = struct
  type t = section

  let sections = Hashtbl.create 7

  let on_create = ref (fun _ -> ())

  let of_string name =
    try
      Hashtbl.find sections name
    with Not_found ->
      assert (String.length name >= 2);
      let assert_well_formed = function
        | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '-' | '.' -> ()
        | _ ->
          prerr_endline ("Malformed section name: " ^ name);
          invalid_arg "Logger.Section.make"
      in
      String.iter ~f:assert_well_formed name;
      let section = { name; destination = None; log_level = 0 } in
      Hashtbl.add sections name section;
      !on_create name;
      section

  let general = of_string "general"
  let project_load = of_string "project_load"

  let to_string t = t.name

  let dest level x =
    let dest =
      if log_level level > x.log_level
      then None
      else x.destination
    in
    match level, dest with
    | `error, None -> general.destination
    | _, _ -> dest

  let enabled lvl x =
    match dest lvl x with
    | Some _ -> true
    | None -> false
end

let section = Section.of_string
let general = Section.general

let opened_files : (string, out_channel) Hashtbl.t =
  Hashtbl.create 4

let get_or_open path =
  try Hashtbl.find opened_files path
  with Not_found ->
    let oc = open_out path in
    Hashtbl.add opened_files path oc;
    oc

let is_monitored x =
  match x.destination with
  | Some _ -> true
  | None -> false

let start_time = Unix.time ()

let format level section ?title content =
  let at = Unix.time () -. start_time in
  let level = match level with
    | `error -> "error"
    | `info  -> "info"
    | `debug -> "debug"
  in
  `Assoc [
    "time", `Float at;
    "level", `String level;
    "section", `String (Section.to_string section);
    "title", (match title with None -> `Null | Some s -> `String s);
    "content", content
  ]

let output level section ?title j oc =
  Json.to_channel oc (format level section ?title j);
  output_char oc '\n';
  flush oc

let logjf level section ?title f j =
  match Section.dest level section with
  | None -> ()
  | Some oc ->
    output level section ?title (f j) oc

let logj level section ?title j =
  logjf level section ?title (fun x -> x) j

let log level section ?title msg =
  logj level section ?title (`String msg)

let logf level section ?title f x =
  match Section.dest level section with
  | None -> ()
  | Some oc ->
    let ppf, to_string = Format.to_string () in
    f ppf x;
    output level section ?title (`String (to_string ())) oc

let info    x = log   `info  x
let infof   x = logf  `info  x
let infoj   x = logj  `info  x
let infojf  x = logjf `info  x
let error   x = log   `error x
let errorf  x = logf  `error x
let errorj  x = logj  `error x
let errorjf x = logjf `error x
let debug   x = log   `debug x
let debugf  x = logf  `debug x
let debugj  x = logj  `debug x
let debugjf x = logjf `debug x

let monitor ?dest x level =
  let dest =
    match dest with
    | Some path -> get_or_open path
    | None ->
      match general.destination with
      | None -> invalid_arg "no log file specified"
      | Some dest -> dest
  in
  x.destination <- Some dest;
  x.log_level <- log_level level;
  infoj general ~title:"monitor start"
    (`List [`String x.name;
            `String (match level with | `error -> "error"
                                      | `info -> "info"
                                      | `debug -> "debug")]);
  if x == general then
    let sections = Hashtbl.fold
        (fun name _ names -> `String name :: names)
        Section.sections []
    in
    infoj general ~title:"available logging sections" (`List sections)

let () =
  Section.on_create := (fun name ->
    info general ~title:"logging section created" name)

let forget x =
  x.destination <- None;
  info general ~title:"monitor stop" x.name

let set_default_destination dest =
  monitor ~dest general `info

let shutdown () =
  Hashtbl.iter (fun _ oc -> close_out oc) opened_files;
  Hashtbl.reset opened_files;
  Hashtbl.iter (fun _ section -> section.destination <- None)
    Section.sections
