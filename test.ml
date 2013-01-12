module Json = Yojson.Basic

let set_paths =
  (* Add whatever -I options have been specified on the command line,
     but keep the directories that user code linked in with ocamlmktop
     may have added to load_path. *)
  let open Config in
  let default_load_path = ref [] in
  fun () -> match !default_load_path with
    | [] ->
        load_path := !load_path @ [Filename.concat standard_library "camlp4"];
        load_path := "" :: (List.rev !Clflags.include_dirs @ !load_path);
        default_load_path := !load_path
    | path -> load_path := path

let print_version () =
  Printf.printf "The Outliner toplevel, version %s\n" Sys.ocaml_version;
  exit 0

let print_version_num () =
  Printf.printf "%s\n" Sys.ocaml_version;
  exit 0

let unexpected_argument s =
  failwith ("Unexpected argument:" ^ s)

let source_path = ref []

let main_loop () =
  let buf = Lexing.from_channel stdin in
  let initialpos = buf.Lexing.lex_curr_p in
  let goteof = ref true in
  let rec loop bufpos oldtokens oldchunks parsed envs =
    (*(match History.prev oldchunks with
      | Some (_,(_,_,_,exns)) -> prerr_endline (String.concat ", " (List.map Printexc.to_string exns))
      | None -> ());
    (match History.prev envs with
      | Some (_,_,exns) -> prerr_endline (String.concat ", " (List.map Printexc.to_string exns))
      | None -> ());*)
    (let p = !bufpos in
     Lexing.(Printf.eprintf "position: %d:%d @%d%!"
               p.pos_lnum (p.pos_cnum - p.pos_bol) p.pos_cnum));
    let tokens = if !goteof
      then
        begin
          Printf.printf "> %!";
          goteof := false;
          fst (History.split oldtokens)
        end
      else
        oldtokens
    in
    let prevpos = !bufpos in
    let tokens,chunks = Outline.parse ~bufpos ~goteof (oldtokens,oldchunks) buf in
    match Chunk.append chunks parsed with
      | None, parsed ->
          (* Process directives *)
          let envs = Typer.sync parsed envs in
          loop bufpos tokens chunks parsed envs
      | Some directive, parsed ->
          let pos_to_json pos =
            Lexing.(`Assoc ["line", `Int pos.pos_lnum;
                            "col", `Int (pos.pos_cnum - pos.pos_bol);
                            "offset", `Int pos.pos_cnum])
          in
          bufpos := prevpos;
          let tokens,chunks = oldtokens,oldchunks in
          let default = (tokens, chunks, parsed, envs) in
          let (tokens, chunks, parsed, envs), result =
          let directive, arg = directive in
          let arg = match arg with
            | Parsetree.Pdir_none -> `Null
            | Parsetree.Pdir_int i -> `Int i
            | Parsetree.Pdir_bool b  -> `Bool b
            | Parsetree.Pdir_ident i -> `Ident i
            | Parsetree.Pdir_string s -> (Json.from_string s :> [ `Ident of Longident.t | Json.json ])
          in
          match directive, arg with
            | "line", `Null -> default,
              pos_to_json prevpos
            | "seek", json ->
                let chunks = (match json with
                  | `Int i -> Outline.Chunked.seek_offset i chunks
                  | `Assoc l ->
                      (match List.assoc "line" l, List.assoc "col" l with
                        | `Int line, `Int col -> 
                            Outline.Chunked.seek_line (line,col) chunks
                        | _, _ -> failwith "Invalid argument")
                  | _ -> failwith "Invalid argument")
                in
                let chunks, _ = History.split chunks in
                let tokens, chunks = History.sync fst tokens chunks in
                let tokens, _ = History.split tokens in
                let _, parsed = Chunk.append chunks parsed in
                let envs = Typer.sync parsed envs in
                let pos = match Outline.Chunked.last_position chunks with Some p -> p | None -> initialpos in
                bufpos := pos;
                (tokens, chunks, parsed, envs),
                pos_to_json pos
            | "reset", `Null ->
                bufpos := initialpos;
                (History.empty,History.empty,History.empty,History.empty),
              (`Null)

              (* Path management *)
            | "which", `String name -> default,
              (let filename =
                 try
                   Misc.find_in_path_uncap !source_path name
                 with Not_found ->
                   Misc.find_in_path_uncap !Config.load_path name
               in
               (`String filename))

            | "source_path", `Null -> default,
              (`List (List.map (fun s -> `String s) !source_path))
            | "build_path", `Null -> default,
              (`List (List.map (fun s -> `String s) !Config.load_path))

            | "source_path", `String s ->
                let d = Misc.expand_directory Config.standard_library s in
                source_path := d :: !source_path;
                default, `Bool true
                  
            | "build_path", `String s ->
                let d = Misc.expand_directory Config.standard_library s in
                Config.load_path := d :: !Config.load_path;
                default, `Bool true

            | "remove_source_path", `String s ->
                let d = Misc.expand_directory Config.standard_library s in
                source_path := List.filter (fun d' -> d' <> d) !source_path;
                default, `Bool true

            | "remove_build_path", `String s ->
                let d = Misc.expand_directory Config.standard_library s in
                Config.load_path := List.filter (fun d' -> d' <> d) !Config.load_path;
                default, `Bool true

            | "clear_source_path", `Null ->
                source_path := [];
                default, `Bool true

            | "clear_build_path", `Null ->
                set_paths ();
                default, `Bool true

            | "cd", `String s ->
                Sys.chdir s;
                default, `Bool true
                
            | _ -> failwith "Unknown directive (FIXME)"
          in
          Json.to_channel stdout result;
          print_newline ();
          loop bufpos tokens chunks parsed envs
            
  (* Directives we want :
     - #line : current position
     - #seek "{line:int,col:int}" : set position to line, col
     - #seek "int"   : set position to offset
     response : {line:int,col:int,offset:int}, nearest position that could be recovered
     - #which "module.{ml,mli}" : find file with given name
     response : /path/to/module.{ml,mli}
     - #reset : reset to initial state

     - #source_path "path"
     - #build_path "path"
     - #remove_source_path "path"
     - #remove_build_path "path"
     - #clear_source_path
     - #clear_build_path
     Next : browsing
  *)
  in
  loop (ref initialpos) History.empty History.empty History.empty History.empty
  

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
