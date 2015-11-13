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
  kind: Merlin_parser.state;
  path: string option;
  dot_merlins: string list;
  unit_name : string;
  mutable project : Merlin_project.t;
  mutable stamp : bool ref;
  mutable keywords: Merlin_lexer.keywords;
  mutable lexer: (exn list * Merlin_lexer.item) History.t;
  mutable recover: (Merlin_lexer.item * Merlin_recover.t) History.t;
  mutable typer: Merlin_typer.t;
}

let invalidate t =
  t.stamp := false;
  t.stamp <- ref true

let is_implementation { kind ; _ } = kind = Merlin_parser.implementation

let initial_step kind (_,token) =
  let input = match token with
    | Merlin_lexer.Valid (s,t,e) -> s,t,e
    | _ -> assert false
  in
  (token, Merlin_recover.fresh (Merlin_parser.from kind input))

let autoreload_dot_merlin buffer =
  let project' = buffer.project in
  let project, status = Merlin_project.get buffer.dot_merlins in
  buffer.project <- project;
  match status with
  | `Fresh -> invalidate buffer
  | `Cached ->
    Merlin_project.check_dot_merlin project;
    if project' != project then
      invalidate buffer

let create ?(dot_merlins=[]) ?path kind =
  let path, filename = match path with
    | None -> None, "*buffer*"
    | Some path -> Some (Filename.dirname path), Filename.basename path
  in
  let dot_merlins = match dot_merlins, path with
    | [], Some path -> [path]
    | [], None -> []
    | xs, cwd -> List.map ~f:(Misc.canonicalize_filename ?cwd) xs
  in
  let unit_name =
    try String.sub filename ~pos:0 ~len:(String.index filename '.')
    with Not_found -> filename
  in
  let unit_name = String.capitalize unit_name in
  let lexer = Merlin_lexer.empty ~filename in
  let project =
    match Merlin_project.get dot_merlins with
    | project, `Fresh -> project
    | project, `Cached ->
      Merlin_project.check_dot_merlin project;
      project
  in
  let stamp = ref true in
  Merlin_project.setup project;
  {
    dot_merlins; path; project; lexer; kind; unit_name; stamp;
    keywords = Merlin_project.keywords project;
    recover = History.initial (initial_step kind (History.focused lexer));
    typer = Merlin_typer.fresh
        ~unit_name ~stamp:[Merlin_project.validity_stamp project; stamp]
        (Merlin_project.extensions project);
  }

let setup buffer =
  autoreload_dot_merlin buffer;
  begin match buffer.path with
    | Some path ->
      Merlin_project.set_local_path buffer.project [path];
      begin try
          Sys.chdir path
        with _ -> ()
      end
    | None -> ()
  end;
  Merlin_project.setup buffer.project

let unit_name t = t.unit_name

let project t = t.project

let lexer b = b.lexer
let lexer_errors b = fst (History.focused b.lexer)

let recover_history b = b.recover
let recover b = snd (History.focused b.recover)

let comments b = Merlin_recover.comments (recover b)

let parser b = Merlin_recover.parser (recover b)
let parser_errors b = Merlin_recover.exns (recover b)

let typer b =
  setup b;
  let valid = Merlin_typer.is_valid b.typer &&
              String.Set.equal
                (Merlin_typer.extensions b.typer)
                (Merlin_project.extensions b.project) in
  if not valid then
    b.typer <- Merlin_typer.fresh
        ~unit_name:b.unit_name
        ~stamp:[Merlin_project.validity_stamp b.project; b.stamp]
        (Merlin_project.extensions b.project);
  b.typer <- Merlin_typer.update (parser b) b.typer;
  b.typer

let update t l =
  t.lexer <- l;
  let strong_check (_,token) (token',_) = token == token' in
  let weak_check (_,token) (token',_) = Merlin_lexer.equal token token' in
  let init token = initial_step t.kind token in
  let strong_fold (_,token) (_,recover) = token, Merlin_recover.fold token recover in
  let weak_update (_,token) (_,recover) = (token,recover) in
  let recover', updated = History.sync t.lexer (Some t.recover)
      ~init ~strong_check ~strong_fold ~weak_check ~weak_update in
  t.recover <- recover';
  updated

let start_lexing ?pos b =
  let kw = Merlin_project.keywords b.project in
  if kw != b.keywords then begin
    b.keywords <- kw;
    ignore (update b (History.drop_tail (History.seek_backward
                                           (fun _ -> true) b.lexer)))
  end
  else begin
    let item_pred pos_pred = function
      | _, Merlin_lexer.Valid (cur,_,_) when pos_pred cur -> true
      | _, Merlin_lexer.Valid (p,_,_) when p = Lexing.dummy_pos -> true
      | _, Merlin_lexer.Error _ -> true
      | _ -> false
    in
    let lexer = b.lexer in
    let lexer = match pos with
      | None -> lexer
      | Some pos ->
        let line, _ = Lexing.split_pos pos in
        let pos_pred cur =
          let line', _ = Lexing.split_pos cur in
          line > line'
        in
        History.seek_forward (item_pred pos_pred) lexer
    in
    let pos_pred = match pos with
      | None -> (fun _ -> false)
      | Some pos ->
        let line, _ = Lexing.split_pos pos in
        (fun cur -> let line', _ = Lexing.split_pos cur in line <= line')
    in
    let lexer = History.seek_backward (item_pred pos_pred) lexer in
    let lexer = History.move (-1) lexer in
    ignore (update b lexer)
  end;
  Merlin_lexer.start kw b.lexer

let get_mark t = Merlin_parser.find_marker (parser t)

let has_mark t = function
  | None -> false
  | Some frame -> Merlin_parser.has_marker (parser t) frame

let global_modules t =
  setup t;
  List.remove t.unit_name (Merlin_project.global_modules t.project)

exception Break
let idle_job t =
  Merlin_typer.with_typer (typer t) @@ fun () ->
  Clflags.real_paths () <> `Real &&
  let concr = Env.used_persistent () in
  Types.Concr.exists Printtyp.compute_map_for_pers concr
