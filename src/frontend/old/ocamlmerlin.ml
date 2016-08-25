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

(** # Merlin's pipeline
  *
  * The toplevel read and write JSON objects on stdin/stdout
  * Each read object corresponds to a command in the following format:
  *   ["command_name",arg1,arg2]
  * Arguments are command-specific. The ["help"] command list existing
  * commands.
  * The type of answer is also command-specific following this convention:
  * - ["return",result]
  *   the command executed successfully, returning `result' json object
  * - ["error",e]
  *   the command was not able to do it's job, for instance due to wrong
  *   assumptions, missing files, etc.
  * - ["failure",string]
  *   the command was not invoked correctly (for instance, trying to
  *   execute unknown command, or passing invalid arguments, etc)
  * - ["exception",string]
  *   something bad or unexpected happened, this is probably a bug and
  *   should have been caught as either an error or a failure.
  *   Please report!
  *
  * ## Overview
  *
  * Normal usage relies on the "tell" command, whose parameter is
  * source code:
  *   > ["tell","struct","let foo = 42"]  ; send buffer content
  *   < ["return","false"]
  *   > ["tell","struct",null]            ; signal end-of-buffer
  *   < ["return","true"]
  * The command ["seek","before",{"line":int,"col":int}] moves the cursor.
  * A session is a sequence of tell/seek commands to synchronize the
  * buffer and the editor, and of query commands.
  *
  * ## Incremental analysis
  *
  * The source code analysis pipeline is as follows:
  *   outline_lexer | outline_parser | chunk_parser | typer
  * Modulo some implementation details, we have:
  *   outline_lexer  : Lexing.buffer -> Raw_parser.token
  *   outline_parser : Raw_parser.token -> Outline_utils.kind * Raw_parser.token list
  *   chunk_parser   : Outline_utils.kind * Raw_parser.token list -> Parsetree.structure
  *   typer          : Parsetree.structure -> Env.t * Typedtree.structure
  *
  * Incremental update of those analyses is implemented through the
  * History.t data. Such an history is a list zipper, the cursor
  * position marking the split between "past", "present" and
  * "potential future".
  * The "past" is the list of already-validated definitions (you may
  * think of the highlighted code in Coqide/ProofGeneral), with the
  * element at the left of the cursor being the last wellformed definition.
  * The "potential future" is a list of definitions that have already
  * been validated, but will be invalidated and thrown out if the
  * definition under the cursor changes.
  *)

let version_spec =
  Printf.sprintf "The Merlin toolkit version %s, for Ocaml %s"
    My_config.version Sys.ocaml_version

let ocamlmerlin_args = [
  (
    "-ignore-sigint",
    " Ignore SIGINT, useful when invoked from editor",
    Marg.unit (fun acc ->
        (try ignore (Sys.(signal sigint Signal_ignore))
         with Invalid_argument _ -> ());
        acc
      )
  );
  (
    "-version",
    " Print version and exit",
    Marg.unit (fun _ ->
        print_endline version_spec;
        exit 0
      )
  );
  (
    "-vnum",
    " Print version number and exit",
    Marg.unit (fun _ ->
        Printf.printf "%s\n" My_config.version;
        exit 0
      )
  );
  (
    "-warn-help",
    " Show description of warning numbers",
    Marg.unit (fun _ ->
        Warnings.help_warnings ();
        exit 0
      )
  );
  (
    "-protocol",
    " Select frontend protocol ('json' or 'sexp')",
    Marg.param "protocol" (fun arg _ ->
        match arg with
        | "json" -> `Json
        | "sexp" -> `Sexp
        | _ ->
          prerr_endline "Valid protocols are 'json' and 'sexp'";
          exit 1
      )
  );
]

let signal sg behavior =
  try ignore (Sys.signal sg behavior)
  with Invalid_argument _ (*Sys.signal: unavailable signal*) -> ()

let main_loop () =
  let config, protocol =
    Marg.parse_all ~warning:prerr_endline
      Mconfig.arguments_table ocamlmerlin_args
      (List.tl (Array.to_list Sys.argv))
      Mconfig.initial `Json
  in
  Old_command.default_config := config;
  let protocol = match protocol with
    | `Json -> IO_json.make
    | `Sexp -> IO_sexp.make
  in
  let input, output =
    protocol ~on_read:ignore ~input:Unix.stdin ~output:Unix.stdout in
  let input () = match input () with
    | None -> None
    | Some json ->
      Logger.logj "frontend" "input" (fun () -> json);
      Some (IO.request_of_json json)
  in
  let output ~notifications x =
    let json = IO.json_of_response ~notifications x in
    Logger.logj "frontend" "output" (fun () -> json);
    output json
  in
  let rec loop () =
    let notifications = ref [] in
    Logger.with_notifications notifications @@ fun () ->
    let tr = Trace.start ~limit:2 () in
    match
      Trace.step tr "Merlin main loop"
        ~return:(fun pp -> fprintf pp "continue = %b")
      @@ fun tr ->
      match input () with
      | Some (Protocol.Request (context, request)) ->
        let answer = Old_command.dispatch context request in
        output ~notifications:(List.rev !notifications)
          (Protocol.Return (request, answer));
        true
      | None -> false
    with
    | exception exn ->
      let trace = ("backtrace", Printexc.get_backtrace ()) in
      output ~notifications:(trace :: List.rev !notifications)
        (Protocol.Exception exn);
      loop ()
    | true -> loop ()
    | false -> ()
  in
  loop ()

let () =
  (* Setup signals, unix is a disaster *)
  signal Sys.sigusr1 Sys.Signal_ignore;
  signal Sys.sigpipe Sys.Signal_ignore;
  signal Sys.sighup  Sys.Signal_ignore;

  (* Setup env for extensions *)
  Unix.putenv "__MERLIN_MASTER_PID" (string_of_int (Unix.getpid ()));

  (* Setup sturgeon monitor *)
  (*let monitor = Sturgeon_stub.start Old_command.monitor in*)

  (* Run! *)
  main_loop ();
  (*Sturgeon_stub.stop monitor;*)
  ()
