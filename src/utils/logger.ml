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
open Sturgeon_stub
open Misc

type title = string
type section = string

let destination = ref None

let set_destination dest =
  begin match !destination with
    | None -> ()
    | Some oc -> close_out_noerr oc
  end;
  destination :=
    begin match dest with
      | None -> None
      | Some filename ->
        Some (open_out filename)
    end

let () =
  set_destination (try Some (Sys.getenv "MERLIN_LOG")
                   with Not_found -> None);
  at_exit (fun () -> set_destination None)

let output_section oc section title =
  let time = (Unix.times ()).Unix.tms_utime in
  output_string oc (Printf.sprintf "# %2.2f %s - %s\n" time section title)

let log section title msg =
  match !destination with
  | None -> ()
  | Some oc ->
    output_section oc section title;
    output_string oc msg;
    output_char oc '\n'

let logf section title =
  Printf.ksprintf (log section title)

let logfmt section title f =
  match !destination with
  | None -> ()
  | Some oc ->
    output_section oc section title;
    let ppf = Format.formatter_of_out_channel oc in
    f ppf;
    Format.pp_print_flush ppf ();
    output_char oc '\n'

let logj section title f =
  match !destination with
  | None -> ()
  | Some oc ->
    output_section oc section title;
    Json.pretty_to_channel oc (f ());
    output_char oc '\n'

let editor_messages
  : (section * string) list ref option fluid
  = fluid None

let notify section =
  let tell msg =
    log section "notify" msg;
    match Fluid.get editor_messages with
    | None -> ()
    | Some r -> r := (section, msg) :: !r
  in
  Printf.ksprintf tell

let with_editor r f =
  Fluid.let' editor_messages (Some r) f

open Sturgeon_stub

let cursor = ref null

module Trace =
struct
  type t = {
    cursor: Sturgeon_stub.cursor;
    limit: int;
    indent: int;
  }

  let t ?(limit=(-1)) () =
    { cursor = Sturgeon_stub.Cursor.sub !cursor; limit; indent = 0 }

  let cursor t = t.cursor

  let is_open t = not (Sturgeon_stub.Cursor.is_closed t.cursor)
  let is_closed t = Sturgeon_stub.Cursor.is_closed t.cursor

  let null = {
    cursor = null;
    limit  = 0;
    indent = 0;
  }

  let sub t =
    let limit = t.limit - 1 in
    if limit <= 0 then null
    else
      let indent = t.indent + 2 in
      let cursor = Cursor.sub t.cursor in
      {limit; cursor; indent}

  let indent n = String.make n ' '

  let enter_open t fmt result =
    let print str f =
      Cursor.text t.cursor (indent t.indent);
      Cursor.text t.cursor (str ^ " \n");
      let t' = sub t in
      match f t' with
      | exception exn ->
        Cursor.text t.cursor (indent (t.indent + 1));
        Cursor.text t.cursor ("RAISE " ^ Printexc.to_string exn);
        raise exn
      | v ->
        Cursor.text t.cursor (indent (t.indent + 1));
        Cursor.text t.cursor ("return " ^ result v);
        v
    in
    Printf.ksprintf print fmt

  let print_closed fmt =
    let print () f = f null in
    let open Printf in
    let open Printf_compat in
    ikfprintf print () fmt

  let enter t fmt result =
    if is_closed t then
      print_closed fmt
    else enter_open t fmt result

  let step_open t fmt result =
    let print str f =
      Cursor.text t.cursor (indent t.indent);
      Cursor.text t.cursor (str ^ " \n");
      match f {t with indent = t.indent + 2} with
      | exception exn ->
        Cursor.text t.cursor (indent (t.indent + 1));
        Cursor.text t.cursor ("RAISE " ^ Printexc.to_string exn);
        raise exn
      | v ->
        Cursor.text t.cursor (indent (t.indent + 1));
        Cursor.text t.cursor ("return " ^ result v);
        v
    in
    Printf.ksprintf print fmt

  let step t fmt result =
    if is_closed t then
      print_closed fmt
    else step_open t fmt result
end
