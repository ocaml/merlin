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
