(** # Merlin's pipeline
  *
  * The toplevel read and write JSON objects on stdin/stdout
  * Each read object corresponds to a command in the following format:
  *   ["nom_de_la_commande",arg1,arg2]
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
  *   > ["return","true"]
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

(* Search path (-I) handling *)
let default_build_paths =
  let open Config in
  lazy (List.rev !Clflags.include_dirs @ !load_path)

let set_default_path () =
  Config.load_path := Lazy.force default_build_paths

let main_loop () =
  let io = Protocol.make ~input:stdin ~output:stdout in
  let input, output as io =
    try Protocol.log ~dest:(open_out (Sys.getenv "MERLIN_LOG")) io
    with Not_found -> io
  in
  try
    let rec loop state =
      let state, answer =
        try match Stream.next input with
          | `List (`String command :: args) ->
              let { Command.handler } =
                try Hashtbl.find Command.commands command
                with Not_found -> failwith "unknown command"
              in
              let state, result = handler io state args in
              state, Protocol.return result
          | _ -> failwith "malformed command"
        with
          | Stream.Failure as exn -> raise exn
          | exn -> state, Protocol.fail exn
      in
      output answer;
      loop state
    in
    loop Command.initial_state
  with Stream.Failure -> ()

let command_path pathes = Command.({
  name = "path";

  handler =
  begin fun _ state arg->
    match begin match arg with
      | [ `String "list" ] ->
          state, `List (List.map (fun (s,_) -> `String s) pathes)
      | [ `String "list" ; `String path ] ->
          let r,_ = List.assoc path pathes in
          state, `List (List.map (fun s -> `String s) !r)
      | [ `String "add" ; `String path ; `String d ] ->
          let r,_ = List.assoc path pathes in
          let d = Misc.expand_directory Config.standard_library d in
          r := d :: !r;
          state, `Bool true
      | [ `String "remove" ; `String path; `String s ] ->
          let r,_ = List.assoc path pathes in
          let d = Misc.expand_directory Config.standard_library s in
          r := List.filter (fun d' -> d' <> d) !r;
          state, `Bool true
      | [ `String "reset" ] ->
          List.iter
            (fun (_,(r,reset)) -> r := Lazy.force reset)
            pathes;
          state, `Bool true
      | [ `String "reset" ; `String path ] ->
          let r,reset = List.assoc path pathes in
          r := Lazy.force reset;
          state, `Bool true
      | _ -> invalid_arguments ()
    end with
    | state, `Bool true as answer ->
        reset_global_modules ();
        answer
    | answer -> answer
  end;
})

let _ =
  let command_path = command_path [
    "build",  (Config.load_path,default_build_paths);
    "source", (Command.source_path, lazy [])
  ] in
  Command.register command_path

(** Mimic other Caml tools, entry point *)
let print_version () =
  Printf.printf "The Merlin toolkit for Ocaml version %s\n" Sys.ocaml_version;
  exit 0

let print_version_num () =
  Printf.printf "%s\n" Sys.ocaml_version;
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
  Arg.parse Options.list unexpected_argument "TODO";
  init_path ();
  set_default_path ();
  Command.reset_global_modules ();
  Findlib.init ();
  main_loop ()

let _ = main ()
