(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013  Frédéric Bour  <frederic.bour(_)lakaban.net>
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
  *   outline_lexer  : Lexing.buffer -> Chunk_parser.token
  *   outline_parser : Chunk_parser.token -> Outline_utils.kind * Chunk_parser.token list
  *   chunk_parser   : Outline_utils.kind * Chunk_parser.token list -> Parsetree.structure
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

module My_config = My_config

(* Search path (-I) handling *)
let default_build_paths =
  let open Config in
  lazy ("." :: List.rev !Clflags.include_dirs @ !load_path)

let set_default_path () =
  Config.load_path := Lazy.force default_build_paths

let signal behavior = 
  try Sys.signal Sys.sigusr1 behavior
  with Invalid_argument "Sys.signal: unavailable signal" ->
    Sys.Signal_default

let refresh_state_on_signal state f =
  let previous =
    signal (Sys.Signal_handle (fun _ ->
        try state := fst (State.quick_refresh_modules !state)
        with _ -> ()
      ))
  in
  Misc.try_finally f (fun () -> ignore (signal previous))

let main_loop () =
  let log = try Some (open_out (Sys.getenv "MERLIN_LOG"))
            with _ -> None
  in
  let input, output as io = Protocol.make ~input:stdin ~output:stdout ?log in
  try
    let rec loop state =
      let state, answer =
        let state' = ref state in
        try match
          refresh_state_on_signal state' 
            (fun () -> Stream.next input)
        with
          | `List (`String command :: args) ->
            let state = !state' in
            let { Command.handler } =
              try Hashtbl.find Command.commands command
              with Not_found -> failwith "unknown command"
            in
            let state, result = handler io state args in
            state, Protocol.return result
          | _ -> failwith "malformed command"
        with
          | Stream.Failure as exn -> raise exn
          | exn -> !state', Protocol.fail exn
      in
      output answer;
      loop state
    in
    loop State.initial
  with Stream.Failure -> ()

let path_modify pathes (action : [`Add|`Rem]) var ?(raw=false) ?cwd path =
  let r,_ = List.assoc var pathes in
  let d =
    if raw 
    then path
    else Misc.canonicalize_filename ?cwd
          (Misc.expand_directory Config.standard_library path)
  in
  r := List.filter ((<>) d) !r;
  match action with
  | `Add -> r := d :: !r
  | `Rem -> ()

let command_path pathes = Command.({
  name = "path";

  handler =
  begin fun _ state arg->
    match begin match arg with
      | [ `String "list" ] ->
        state, `List (List.map (fun (s,_) -> `String s) pathes)
      | [ `String "list"; `String var ] ->
        let r,_ = List.assoc var pathes in
        state, `List (List.map (fun s -> `String s) !r)
      | [ `String "add"; `String var; `String path ] ->
        path_modify pathes `Add var path;
        state, `Bool true
      | [ `String "remove"; `String var; `String path ] ->
        path_modify pathes `Rem var path;
        state, `Bool true
      | [ `String "raw"; `String "add"; `String var; `String path ] ->
        path_modify pathes ~raw:true `Add var path;
        state, `Bool true
      | [ `String "raw"; `String "remove"; `String var; `String path ] ->
        path_modify pathes ~raw:true `Rem var path;
        state, `Bool true
      | [ `String "reset" ] ->
        List.iter
          (fun (_,(r,reset)) -> r := Lazy.force reset)
          pathes;
        state, `Bool true
      | [ `String "reset"; `String path ] ->
        let r,reset = List.assoc path pathes in
        r := Lazy.force reset;
        state, `Bool true
      | _ -> invalid_arguments ()
    end with
    | state, `Bool true as answer ->
      State.reset_global_modules ();
      answer
    | answer -> answer
  end;
})

let path_modify, command_path = 
  let pathes = [
    "build",  (Config.load_path,default_build_paths);
    "source", (State.source_path, lazy ["."])
  ] in
  path_modify pathes, command_path pathes

let command_project = 
  let rec load_dot_merlin path =  
    let recurse = ref false in 
    let cwd = Filename.dirname path in
    let ic = open_in path in
    begin try
        let drop n s =
          String.sub s n (String.length s- n) in
        let rec aux () = 
          let line = input_line ic in
          if line = "" then ()
          else if Misc.has_prefix "B " line then
            path_modify `Add "build" ~cwd (drop 2 line)
          else if Misc.has_prefix "S " line then
            path_modify `Add "source" ~cwd (drop 2 line)
          else if Misc.has_prefix "PKG " line then
            (Command.load_packages (Misc.rev_split_words (drop 4 line)))
          else if Misc.has_prefix "REC" line then recurse := true
          else if Misc.has_prefix "#" line then ()
          else ();
          aux ()
        in
        aux ()
      with 
      | End_of_file ->
        close_in_noerr ic; 
        if !recurse
        then path :: find_dot_merlin (Filename.dirname (Filename.dirname path))
        else [path]
      | exn -> 
        close_in_noerr ic; raise exn
    end 
  and find_dot_merlin path =
    let rec loop dir =
      let fname = Filename.concat dir ".merlin" in
      if Sys.file_exists fname
      then Some fname
      else
        let parent = Filename.dirname dir in
        if parent <> dir
        then loop parent
        else None
    in
    match loop (Misc.canonicalize_filename path) with
    | Some fname -> load_dot_merlin fname
    | None -> []
  in
  Command.({
    name = "project";
  
    handler =
    begin fun _ state -> function
      | [ `String ("load"|"find" as cmd) ; `String path ] ->
        let f = if cmd = "load" then load_dot_merlin else find_dot_merlin in
        state, `List (List.map (fun s -> `String s) (f path))
      | _ -> Command.invalid_arguments ()
    end;
  })

let () = 
  Command.register command_path;
  Command.register command_project

(** Mimic other Caml tools, entry point *)
let print_version () =
  Printf.printf "The Merlin toolkit version %s, for Ocaml %s\n"
    My_config.version Sys.ocaml_version;
  exit 0

let print_version_num () =
  Printf.printf "%s\n" My_config.version;
  exit 0

let unexpected_argument s =
  failwith ("Unexpected argument:" ^ s)

module Options = Main_args.Make_bytetop_options (struct
  let set r () = r := true
  let clear r () = r := false

  let _absname = set Location.absname
  let _I dir =
    let dir = Misc.expand_directory Config.standard_library dir in
    Clflags.include_dirs := dir :: !Clflags.include_dirs
  let _init s = Clflags.init_file := Some s
  let _labels = clear Clflags.classic
  let _no_app_funct = clear Clflags.applicative_functors
  let _noassert = set Clflags.noassert
  let _nolabels = set Clflags.classic
  let _noprompt = set Clflags.noprompt
  let _nopromptcont = set Clflags.nopromptcont
  let _nostdlib = set Clflags.no_std_include
  let _principal = set Clflags.principal
  let _rectypes = set Clflags.recursive_types
  let _stdin () = main_loop ()
  let _strict_sequence = set Clflags.strict_sequence
  let _unsafe = set Clflags.fast
  let _version () = print_version ()
  let _vnum () = print_version_num ()
  let _w s = Warnings.parse_options false s
  let _warn_error s = Warnings.parse_options true s
  let _warn_help = Warnings.help_warnings
  let _dparsetree = set Clflags.dump_parsetree
  let _drawlambda = set Clflags.dump_rawlambda
  let _dlambda = set Clflags.dump_lambda
  let _dinstr = set Clflags.dump_instr
  let _protocol = Protocol.select_frontend

  let _ignore_sigint () =
    try ignore (Sys.(signal sigint Signal_ignore))
    with Invalid_argument _ -> ()

  let anonymous s = unexpected_argument s
end);;

(* Initialize the search path.
   The current directory is always searched first,
   then the directories specified with the -I option (in command-line order),
   then the standard library directory (unless the -nostdlib option is given).
 *)

let init_path () =
  let dirs =
    if !Clflags.use_threads then "+threads" :: !Clflags.include_dirs
    else if !Clflags.use_vmthreads then "+vmthreads" :: !Clflags.include_dirs
    else !Clflags.include_dirs in
  let exp_dirs =
    List.map (Misc.expand_directory Config.standard_library) dirs in
  Config.load_path := "" :: List.rev_append exp_dirs (Clflags.std_include_dir ());
  Env.reset_cache ()

let main () =
  Arg.parse Options.list unexpected_argument
    "Usage: ocamlmerlin [options]\noptions are:";
  init_path ();
  set_default_path ();
  State.reset_global_modules ();
  Findlib.init ();
  ignore (signal Sys.Signal_ignore);
  main_loop ()

let _ = main ()
