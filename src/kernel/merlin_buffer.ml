(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
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

open Std

type t = {
  kind: Merlin_parser.kind;
  mutable project: Merlin_project.t;
  mutable source: Merlin_source.t;
  mutable reader: Merlin_reader.t;
  mutable typer: Merlin_typer.t;
}

let compute_unit_name filename =
  let unit_name =
    try String.sub filename ~pos:0 ~len:(String.index filename '.')
    with Not_found -> filename
  in
  String.capitalize unit_name

let compute_context ?(dot_merlins=[]) ?path () =
  let path, filename = match path with
    | None -> None, "*buffer*"
    | Some path -> Some (Filename.dirname path), Filename.basename path
  in
  let dot_merlins = match dot_merlins, path with
    | [], Some path -> [path]
    | [], None -> []
    | xs, cwd -> List.map ~f:(Misc.canonicalize_filename ?cwd) xs
  in
  (dot_merlins, compute_unit_name filename)

let create ?dot_merlins ?path kind =
  let dot_merlins, name = compute_context ?dot_merlins ?path () in
  let project, _ = Merlin_project.get dot_merlins in
  let source = Merlin_source.empty ~name in
  let reader = Merlin_reader.make
      (Merlin_reader.Normal (Merlin_project.extensions project, kind))
      source in
  let typer  = Merlin_typer.make reader String.Set.empty in
  { kind; project; source; reader; typer }

let unit_name t = Merlin_source.name t.source
let project t = t.project

let update t source =
  t.source <- source

let source t =
  t.source

let reader t =
  t.reader <- Merlin_reader.update (source t) t.reader;
  t.reader

let typer t =
  Merlin_project.setup t.project;
  t.typer <- Merlin_typer.update (reader t) t.typer;
  t.typer

(* All top modules of current project, with current module removed *)
let global_modules t =
  List.remove (Merlin_source.name (source t))
    (Merlin_project.global_modules t.project)

(* Try to do a background job, return false if nothing has to be done *)
let idle_job t =
  let typer = typer t in
  Merlin_typer.with_typer typer @@ fun () ->
  ignore (Merlin_typer.result typer);
  Clflags.real_paths () <> `Real &&
  let concr = Env.used_persistent () in
  Types.Concr.exists Printtyp.compute_map_for_pers concr
