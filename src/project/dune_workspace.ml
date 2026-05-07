(* This file is part of the merlin-lib.project library
   See the attached LICENSE file. *)

open Sexplib0.Sexp_conv

(* The following four record types match the exact field shape of
   dune's [--format=csexp --lang 0.1] output. [@@deriving of_sexp]
   gives us the parsing code for free; we only hand-write the
   top-level dispatch (where the same tag may repeat). *)

type module_ =
  { name : string;
    impl : string list;
    intf : string list;
    cmt : string list;
    cmti : string list
  }
[@@deriving of_sexp]

let single = function [ s ] -> Some s | _ -> None
let impl_path m = single m.impl
let intf_path m = single m.intf
let cmt_path m = single m.cmt
let cmti_path m = single m.cmti

type library =
  { name : string;
    uid : string;
    local : bool;
    requires : string list;
    source_dir : string;
    modules : module_ list;
    include_dirs : string list
  }
[@@deriving of_sexp]

type executables =
  { names : string list;
    requires : string list;
    modules : module_ list;
    include_dirs : string list
  }
[@@deriving of_sexp]

type t =
  { root : string;
    build_context : string;
    libraries : library list;
    executables : executables list
  }

(* [Csexp.t] and [Sexplib0.Sexp.t] are structurally identical -- both
   are [Atom of string | List of t list], differing only in their
   on-the-wire encoding. Convert so that ppx_sexp_conv-generated
   parsers can consume what [Csexp.input] returns. *)
let rec sexp_of_csexp : Csexp.t -> Sexplib0.Sexp.t = function
  | Atom s -> Atom s
  | List l -> List (List.map sexp_of_csexp l)

(* Top-level dispatch. The output of [dune describe workspace] is a
   flat list of [(<tag> <payload>)] entries. [(root)] and
   [(build_context)] appear once; [(library)] and [(executables)]
   appear zero or more times. Unknown tags are ignored so newer dune
   versions can extend the format without breaking us. *)
let interpret_entries entries =
  let root = ref None in
  let build_context = ref None in
  let libraries = ref [] in
  let executables = ref [] in
  List.iter
    (fun entry ->
      match (entry : Sexplib0.Sexp.t) with
      | List [ Atom "root"; Atom s ] -> root := Some s
      | List [ Atom "build_context"; Atom s ] -> build_context := Some s
      | List [ Atom "library"; payload ] ->
        libraries := library_of_sexp payload :: !libraries
      | List [ Atom "executables"; payload ] ->
        executables := executables_of_sexp payload :: !executables
      | _ -> ())
    entries;
  match (!root, !build_context) with
  | Some root, Some build_context ->
    Ok
      { root;
        build_context;
        libraries = List.rev !libraries;
        executables = List.rev !executables
      }
  | None, _ -> Error "dune describe workspace: missing [(root ...)] entry"
  | _, None ->
    Error "dune describe workspace: missing [(build_context ...)] entry"

let parse_csexp csexp =
  match sexp_of_csexp csexp with
  | List entries -> interpret_entries entries
  | Atom _ ->
    Error "dune describe workspace: expected a list at the top level"

(* Slurp the rest of an [in_channel] into a string. Used to capture
   stderr in case dune fails. *)
let read_all ic =
  let buf = Buffer.create 4096 in
  let chunk = Bytes.create 4096 in
  let rec loop () =
    match input ic chunk 0 (Bytes.length chunk) with
    | 0 -> Buffer.contents buf
    | n ->
      Buffer.add_subbytes buf chunk 0 n;
      loop ()
  in
  loop ()

let describe ?root () =
  (* dune parses [--root] positionally: it must come *after* the
     [describe workspace] subcommand, not before [describe]. *)
  let base_args = [ "describe"; "workspace"; "--format=csexp"; "--lang"; "0.1" ] in
  let args =
    match root with None -> base_args | Some dir -> base_args @ [ "--root"; dir ]
  in
  let argv = Array.of_list ("dune" :: args) in
  let env = Unix.environment () in
  let ic, oc, ec = Unix.open_process_args_full "dune" argv env in
  close_out oc;
  let sexp_result = Csexp.input ic in
  let stderr_data = read_all ec in
  match Unix.close_process_full (ic, oc, ec) with
  | WEXITED 0 -> begin
    match sexp_result with
    | Ok sexp -> parse_csexp sexp
    | Error msg ->
      Error (Printf.sprintf "could not parse dune's csexp output: %s" msg)
  end
  | WEXITED n ->
    Error
      (Printf.sprintf "dune describe workspace exited with code %d:\n%s" n
         (String.trim stderr_data))
  | WSIGNALED n ->
    Error (Printf.sprintf "dune describe workspace killed by signal %d" n)
  | WSTOPPED n ->
    Error (Printf.sprintf "dune describe workspace stopped by signal %d" n)

let local_cmt_files t =
  let from_modules acc modules =
    List.fold_left
      (fun acc m -> match cmt_path m with None -> acc | Some p -> p :: acc)
      acc modules
  in
  let acc = [] in
  let acc =
    List.fold_left
      (fun acc lib ->
        if lib.local then from_modules acc lib.modules else acc)
      acc t.libraries
  in
  let acc = List.fold_left (fun acc e -> from_modules acc e.modules) acc t.executables in
  List.rev acc
