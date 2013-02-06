(** # Pipeline de Merlin
  *
  * Le toplevel lit et écrit des objets JSON sur stdin/stdout.
  * Chaque objet lu correspond à une commande au format suivant:
  * ["nom_de_la_commande",arg1,arg2]
  * Les arguments dépendent de la commande. La commande ["help"] liste les
  * commandes reconnues.
  * Le type d'objet renvoyé est propre à chaque commande (à nettoyer).
  *
  * ## Présentation rapide
  * L'utilisation normale passe par la commande "tell" qui prend le code source
  * en argument:
  *   > ["tell","let foo = 42"]
  *   < true
  * La commande ["seek","position",{"line":int,"col":int}] permet de déplacer le
  * curseur. Une session est composée d'une suite de tell/seek pour synchroniser
  * le buffer avec l'éditeur, et de commande d'interrogations.
  *
  * ## Analyse incrémentale.
  *
  * La pipeline de traitement du code source est la suivante :
  *   outline_lexer | outline_parser | chunk_parser | typer
  * Aux détails d'implantation près :
  *   outline_lexer  : Lexing.buffer -> Chunk_parser.token
  *   outline_parser : Chunk_parser.token -> Outline_utils.kind * Chunk_parser.token list
  *   chunk_parser   : Outline_utils.kind * Chunk_parser.token list -> Parsetree.structure
  *   typer          : Parsetree.structure -> Env.t * Typedtree.structure
  *
  * La mise à jour incrémentale de ces traitements est implanté à l'aide des
  * objets History.t
  * L'historique est juste une liste avec un curseur qui délimite passé, version
  * actuelle et futur éventuel.
  * Le passé contient les constructions déjà validées (penser au surlignage de Coqide),
  * avec l'élément directement à gauche du curseur identifiant la dernière définition,
  * et le futur éventuel représente les définitions déjà analysées, qui seront
  * jetées si la définition sous le curseur est modifiée.
  *)

(* Gestion des chemins *)
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
  doc = "TODO";

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
