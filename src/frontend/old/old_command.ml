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
open Old_protocol
module Printtyp = Type_utils.Printtyp

type buffer = {
  mutable config : Mconfig.t;
  mutable source : Msource.t;
}

type state = {
  mutable buffer : buffer;
}

let default_config = ref Mconfig.initial

let normalize_document doc =
  doc.Context.path, doc.Context.dot_merlins

let new_buffer (path, dot_merlins) =
  let open Mconfig in
  let query = match path with
    | None -> !default_config.query
    | Some path -> {
        !default_config.query with
        filename = Filename.basename path;
        directory = Misc.canonicalize_filename (Filename.dirname path);
      }
  and merlin = {
    !default_config.merlin with dotmerlin_to_load =
      (Option.cons (Option.map ~f:Filename.dirname path) (Option.value ~default:[] dot_merlins))
  }
  in
  let config = {!default_config with query; merlin} in
  { config; source = Msource.make config "" }

let new_state document =
  { buffer = new_buffer document }

let checkout_buffer_cache = ref []
let checkout_buffer =
  let cache_size = 8 in
  fun document ->
    let document = normalize_document document in
    try List.assoc document !checkout_buffer_cache
    with Not_found ->
      let buffer = new_buffer document in
      begin match document with
        | Some path, _ ->
          checkout_buffer_cache :=
            (document, buffer) :: List.take_n cache_size !checkout_buffer_cache
        | None, _ -> ()
      end;
      buffer

let print_completion_entries config source entries =
  let input_ref = ref [] and output_ref = ref [] in
  let preprocess entry =
    match Completion.raw_info_printer entry with
    | `String s -> `String s
    | `Print t ->
      let r = ref "" in
      input_ref := t :: !input_ref;
      output_ref := r :: !output_ref;
      `Print r
    | `Concat (s,t) ->
      let r = ref "" in
      input_ref := t :: !input_ref;
      output_ref := r :: !output_ref;
      `Concat (s,r)
  in
  let entries = List.map ~f:(Completion.map_entry preprocess) entries in
  let outcomes = Mreader.print_batch_outcome config source !input_ref in
  List.iter2 (:=) !output_ref outcomes;
  let postprocess = function
    | `String s -> s
    | `Print r -> !r
    | `Concat (s,r) -> s ^ !r
  in
  List.rev_map ~f:(Completion.map_entry postprocess) entries

let make_pipeline buffer =
      Mpipeline.make (Trace.start ()) buffer.config buffer.source

let with_typer ?for_completion buffer f =
  let trace = Trace.start () in
  let pipeline =
    Mpipeline.make ?for_completion trace buffer.config buffer.source in
  let typer = Mpipeline.typer_result pipeline in
  Mtyper.with_typer typer @@ fun () -> f pipeline typer

let dispatch_sync state (type a) : a sync_command -> a = function
  | Idle_job -> false

  | Tell (pos_start, pos_end, text) ->
    let source = Msource.substitute state.source pos_start pos_end text in
    state.source <- source

  | Refresh ->
    checkout_buffer_cache := [];
    Cmi_cache.flush ()

  | Flags_set flags ->
    let open Mconfig in
    let flags_to_apply = [{flag_cwd = None; flag_list = flags}] in
    let config = state.config in
    state.config <- {config with merlin = {config.merlin with flags_to_apply}};
    `Ok

  | Findlib_use packages ->
    let open Mconfig in
    let config = state.config in
    let packages_to_load =
      List.filter_dup (packages @ config.merlin.packages_to_load) in
    state.config <-
      {config with merlin = {config.merlin with packages_to_load}};
    `Ok

  | Extension_set (action,exts) ->
    let f l = match action with
      | `Enabled  -> List.filter_dup (exts @ l)
      | `Disabled -> List.filter l ~f:(fun x -> not (List.mem x ~set:exts))
    in
    let open Mconfig in
    let config = state.config in
    let extensions = f config.merlin.extensions in
    state.config <- {config with merlin = {config.merlin with extensions}};
    `Ok

  | Path (var,action,paths) ->
    let f l = match action with
      | `Add -> List.filter_dup (paths @ l)
      | `Rem -> List.filter l ~f:(fun x -> not (List.mem x ~set:paths))
    in
    let open Mconfig in
    let merlin = state.config.merlin in
    let merlin =
      match var with
      | `Build -> {merlin with build_path = f merlin.build_path}
      | `Source -> {merlin with source_path = f merlin.source_path}
    in
    state.config <- {state.config with merlin}

  | Path_reset ->
    let open Mconfig in
    let merlin = state.config.merlin in
    let merlin = {merlin with build_path = []; source_path = []} in
    state.config <- {state.config with merlin}

  | Protocol_version version ->
    begin match version with
      | None -> ()
      | Some 2 -> Old_IO.current_version := `V2
      | Some 3 -> Old_IO.current_version := `V3
      | Some _ -> ()
    end;
    (`Selected !Old_IO.current_version,
     `Latest Old_IO.latest_version,
     Printf.sprintf "The Merlin toolkit version %s, for Ocaml %s\n"
       My_config.version Sys.ocaml_version)

  | Flags_get ->
    let pipeline = make_pipeline state in
    let config = Mpipeline.final_config pipeline in
    List.concat_map ~f:(fun f -> f.Mconfig.flag_list)
      Mconfig.(config.merlin.flags_to_apply)

  | Project_get ->
    let pipeline = make_pipeline state in
    let config = Mpipeline.final_config pipeline in
    (Mconfig.(config.merlin.dotmerlin_loaded), `Ok) (*TODO*)

  | Checkout _ -> failwith "invalid arguments"

let default_state = lazy (new_state (None, None))

let document_states
  : (string option * string list option, state) Hashtbl.t
  = Hashtbl.create 7

let dispatch (type a) (context : Context.t) (cmd : a command) =
  let open Context in
  (* Document selection *)
  let state = match context.document with
    | None -> Lazy.force default_state
    | Some document ->
      let document = normalize_document document in
      try Hashtbl.find document_states document
      with Not_found ->
        let state = new_state document in
        Hashtbl.add document_states document state;
        state
  in
  let config = state.buffer.config in
  (* Printer verbosity *)
  let config = match context.printer_verbosity with
    | None -> config
    | Some verbosity ->
      Mconfig.({config with query = {config.query with verbosity}})
  in
  let config = match context.printer_width with
    | None -> config
    | Some printer_width ->
      Mconfig.({config with query = {config.query with printer_width}})
  in
  (* Printer width *)
  Format.default_width := Option.value ~default:0 context.printer_width;
  (* Actual dispatch *)
  match cmd with
  | Query q ->
    Mreader.with_ambient_reader config state.buffer.source @@ fun () ->
    Query_commands.dispatch (Trace.start (), config,state.buffer.source) q
  | Sync (Checkout context) when state == Lazy.force default_state ->
    let buffer = checkout_buffer context in
    state.buffer <- buffer
  | Sync s -> dispatch_sync state.buffer s
