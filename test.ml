
let set_paths () =
  (* Add whatever -I options have been specified on the command line,
     but keep the directories that user code linked in with ocamlmktop
     may have added to load_path. *)
  let open Config in
  load_path := !load_path @ [Filename.concat standard_library "camlp4"];
  load_path := "" :: (List.rev !Clflags.include_dirs @ !load_path)

let print_version () =
  Printf.printf "The Outliner toplevel, version %s\n" Sys.ocaml_version;
  exit 0

let print_version_num () =
  Printf.printf "%s\n" Sys.ocaml_version;
  exit 0

let unexpected_argument s =
  failwith ("Unexpected argument:" ^ s)

let main_loop () =
  let buf = Lexing.from_channel stdin in
  let bufpos = ref (Some buf.Lexing.lex_curr_p) in
  let rec loop tokens chunks parsed envs =
    (match History.prev chunks with
      | Some (_,(_,_,_,exns)) -> print_endline (String.concat ", " (List.map Printexc.to_string exns))
      | None -> ());
    (match History.prev envs with
      | Some (_,_,exns) -> print_endline (String.concat ", " (List.map Printexc.to_string exns))
      | None -> ());
    (match !bufpos with
      | Some p ->
          Lexing.(Printf.printf "position: %d:%d @%d"
                    p.pos_lnum (p.pos_cnum - p.pos_bol) p.pos_cnum)
      | None -> ());
    Printf.printf "> %!";
    let prevpos = !bufpos in
    let tokens,chunks = Outline.parse ~bufpos (tokens,chunks) buf in
    match Chunk.append chunks parsed with
      | None, parsed ->
          (* Process directives *)
          let envs = Typer.sync parsed envs in
          loop tokens chunks parsed envs
      | Some directive, parsed ->
          bufpos := prevpos;
          failwith "FIXME: handle directives";
          loop tokens chunks parsed envs
  in
  loop History.empty History.empty History.empty History.empty

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

let main () =
  Arg.parse Options.list unexpected_argument "TODO";
  Compile.init_path();
  set_paths ();
  main_loop ()

let _ = main ()
