(* This file is part of the merlin-lib.project library
   See the attached LICENSE file. *)

(* Note: we do not use [ppx_sexp_conv] here even though it would make
   the record parsers below shorter.  [ppx_sexp_conv] depends on
   which pulls in Jane Street's [base] library, and
   merlin avoids that dependency. *)

type module_ =
  { name : string;
    impl : string option;
    intf : string option;
    cmt : string option;
    cmti : string option
  }

type library =
  { name : string;
    uid : string;
    local : bool;
    requires : string list;
    source_dir : string;
    modules : module_ list;
    include_dirs : string list
  }

type executables =
  { names : string list;
    requires : string list;
    modules : module_ list;
    include_dirs : string list
  }

type t =
  { root : string;
    build_context : string;
    libraries : library list;
    executables : executables list
  }

(*** Csexp parsers ***)

(* dune's csexp output represents records as lists of (key value) pairs.
   The helpers below extract typed values from such a field list.

   string option fields use dune's 0-or-1-element list encoding:
     (field ())        -> None
     (field (a/b.ml))  -> Some "a/b.ml"
*)

let field name pairs =
  match
    List.find_opt
      (function Csexp.List (Csexp.Atom k :: _) -> k = name | _ -> false)
      pairs
  with
  | Some (Csexp.List [_; v]) -> v
  | Some _ | None ->
    Printf.ksprintf failwith "dune describe: missing or malformed field %S" name

let string_field name pairs =
  match field name pairs with
  | Csexp.Atom s -> s
  | _ -> Printf.ksprintf failwith "dune describe: field %S: expected a string" name

let bool_field name pairs =
  match string_field name pairs with
  | "true" -> true
  | "false" -> false
  | s -> Printf.ksprintf failwith "dune describe: field %S: not a bool (%S)" name s

let strings_field name pairs =
  match field name pairs with
  | Csexp.List xs ->
    List.map
      (function
        | Csexp.Atom s -> s
        | _ ->
          Printf.ksprintf failwith
            "dune describe: field %S: expected a list of strings" name)
      xs
  | _ ->
    Printf.ksprintf failwith "dune describe: field %S: expected a list" name

let string_opt_field name pairs =
  match field name pairs with
  | Csexp.List [] -> None
  | Csexp.List [Csexp.Atom s] -> Some s
  | _ ->
    Printf.ksprintf failwith
      "dune describe: field %S: expected () or (value)" name

let pairs_of label sexp =
  match sexp with
  | Csexp.List pairs -> pairs
  | _ ->
    Printf.ksprintf failwith "dune describe: %s: expected a list of fields" label

let module_of_csexp sexp =
  let f = pairs_of "module" sexp in
  { name  = string_field     "name" f;
    impl  = string_opt_field "impl" f;
    intf  = string_opt_field "intf" f;
    cmt   = string_opt_field "cmt"  f;
    cmti  = string_opt_field "cmti" f
  }

let modules_field name pairs =
  match field name pairs with
  | Csexp.List xs -> List.map module_of_csexp xs
  | _ ->
    Printf.ksprintf failwith "dune describe: field %S: expected a list" name

let library_of_csexp sexp =
  let f = pairs_of "library" sexp in
  { name        = string_field  "name"        f;
    uid         = string_field  "uid"         f;
    local       = bool_field    "local"       f;
    requires    = strings_field "requires"    f;
    source_dir  = string_field  "source_dir"  f;
    modules     = modules_field "modules"     f;
    include_dirs = strings_field "include_dirs" f
  }

let executables_of_csexp sexp =
  let f = pairs_of "executables" sexp in
  { names       = strings_field "names"        f;
    requires    = strings_field "requires"     f;
    modules     = modules_field "modules"      f;
    include_dirs = strings_field "include_dirs" f
  }

(* Top-level dispatch. The output of [dune describe workspace] is a
   flat list of [(<tag> <payload>)] entries. [(root)] and
   [(build_context)] appear once; [(library)] and [(executables)]
   appear zero or more times. Unknown tags are silently ignored so
   newer dune versions can extend the format without breaking us. *)
let interpret_entries entries =
  let root = ref None in
  let build_context = ref None in
  let libraries = ref [] in
  let executables = ref [] in
  List.iter
    (fun entry ->
      match (entry : Csexp.t) with
      | List [ Atom "root"; Atom s ] -> root := Some s
      | List [ Atom "build_context"; Atom s ] -> build_context := Some s
      | List [ Atom "library"; payload ] ->
        libraries := library_of_csexp payload :: !libraries
      | List [ Atom "executables"; payload ] ->
        executables := executables_of_csexp payload :: !executables
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
  match (csexp : Csexp.t) with
  | List entries ->
    (try interpret_entries entries with Failure msg -> Error msg)
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

let describe ?context ?root () =
  (* dune parses [--root] and [--context] positionally: they must come
     *after* the [describe workspace] subcommand, not before
     [describe]. *)
  let base_args =
    [ "describe"; "workspace"; "--format=csexp"; "--lang"; "0.1" ]
  in
  let args =
    match context with
    | None -> base_args
    | Some name -> base_args @ [ "--context"; name ]
  in
  let args =
    match root with None -> args | Some dir -> args @ [ "--root"; dir ]
  in
  let argv = Array.of_list ("dune" :: args) in
  let env = Unix.environment () in
  match Unix.open_process_args_full "dune" argv env with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
    Error "could not find `dune` in PATH. Install dune or make it available."
  | exception Unix.Unix_error (err, _, _) ->
    Error
      (Printf.sprintf "could not run `dune describe workspace`: %s"
         (Unix.error_message err))
  | ic, oc, ec ->
    close_out oc;
    let sexp_result = Csexp.input ic in
    let stderr_data = read_all ec in
    (match Unix.close_process_full (ic, oc, ec) with
     | WEXITED 0 ->
       (match sexp_result with
        | Ok sexp -> parse_csexp sexp
        | Error msg ->
          Error
            (Printf.sprintf "could not parse dune's csexp output: %s" msg))
     | WEXITED n ->
       Error
         (Printf.sprintf "dune describe workspace exited with code %d:\n%s" n
            (String.trim stderr_data))
     | WSIGNALED n ->
       Error
         (Printf.sprintf "dune describe workspace killed by signal %d" n)
     | WSTOPPED n ->
       Error
         (Printf.sprintf "dune describe workspace stopped by signal %d" n))

let local_cmt_files t =
  let from_modules acc modules =
    List.fold_left
      (fun acc m -> match m.cmt with None -> acc | Some p -> p :: acc)
      acc modules
  in
  let acc = [] in
  let acc =
    List.fold_left
      (fun acc lib ->
        if lib.local then from_modules acc lib.modules else acc)
      acc t.libraries
  in
  let acc =
    List.fold_left (fun acc e -> from_modules acc e.modules) acc t.executables
  in
  List.rev acc
