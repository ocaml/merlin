open Std

(** Parse dot-merlin files **)

(* A dot-merlin file is made of one or more directive and an optional project
   name *)
type directive = [
  | `B   of string
  | `CMI of string
  | `CMT of string
  | `EXT of string list
  | `FLG of string
  | `PKG of string list
  | `S   of string
]
type file = {
  project    : string option;
  path       : string;
  directives : directive list;
}

(* After parsing, dot-merlins are turned into a project configuration *)
type config = {
  dot_merlins : string list;
  build_path  : string list;
  source_path : string list;
  cmi_path    : string list;
  cmt_path    : string list;
  packages    : string list;
  flags       : string list list;
  extensions  : string list;
}

(* Find path of the dot-merlin file *)
val find : string -> string option

(* Parse a file from the filesystem. Path is a filename *)
val read : string -> file List.Lazy.t
val empty_config : config
val parse : ?config:config -> file List.Lazy.t -> config

(* If any of the dot-merlin specify a project name, return it *)
val project_name : file List.Lazy.t -> string option
val path_of_packages : string list -> [> `Failures of (string * exn) list | `Ok ] * string list
