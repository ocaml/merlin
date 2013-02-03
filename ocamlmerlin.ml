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
  lazy (List.rev !Clflags.include_dirs @ !load_path @ [Filename.concat standard_library "camlp4"])

let set_default_path () =
  Config.load_path := Lazy.force default_build_paths

let source_path = ref []

type state = {
  pos      : Lexing.position;
  tokens   : Outline.token list;
  outlines : Outline.t;
  chunks   : Chunk.t;
  types    : Typer.t;
}
 
let initial_state = {
  pos      = Lexing.((from_string "").lex_curr_p);
  tokens   = [];
  outlines = History.empty;
  chunks   = History.empty;
  types    = History.empty;
}

let commands = Hashtbl.create 17

let main_loop () =
  let logger =
    try
      let logger = open_out (Sys.getenv "MERLIN_LOG") in
      let log_input json = Printf.fprintf logger "> %s\n%!" (Json.to_string json); json in
      let log_output json = Printf.fprintf logger "< %s\n%!" (Json.to_string json); json in
      Some (log_input, log_output)
    with Not_found -> None
  in
  let input  = Json.stream_from_channel stdin in
  let output =
    let out_json = Json.to_channel stdout in
    fun json ->
      out_json json;
      print_newline ()
  in
  let input, output as io =
    match logger with
      | None -> input, output
      | Some (li,lo) ->
          Stream.from
          begin fun _ ->
            try Some (li (Stream.next input))
            with Stream.Failure -> None
          end,
          (fun out -> output (lo out))
  in
  try
    let rec loop state =
      let state, answer =
        try match Stream.next input with
          | `List (`String command :: args) ->
                let handler, _ =
                  try Hashtbl.find commands command
                  with Not_found -> failwith "unknown command"
                in
                handler io state args
          | _ -> failwith "malformed command"
        with
          | Failure s -> state, `List [`String "failure"; `String s]
          | Stream.Failure as exn -> raise exn
          | exn -> 
              match Error_report.to_json exn with
                | None -> state, `List [`String "exception"; `String (Printexc.to_string exn)]
                | Some json -> state, `List [`String "error" ; json ]
      in
      output answer;
      loop state
    in
    loop initial_state
  with Stream.Failure -> ()

let return_position p = `List [`String "position" ; Outline_utils.pos_to_json p]

let invalid_arguments () = failwith "invalid arguments"

let lex_string ?(offset=0) source refill =
  let pos = ref offset in
  let len = ref (String.length source) in
  let source = ref source in
  Lexing.from_function
    begin fun buf size ->
      let count = min (!len - !pos) size in
      let count = 
        if count <= 0 then
        begin
          source := refill ();
          len := String.length !source;
          pos := 0;
          min !len size
        end
        else count
      in
      if count <= 0 then 0
      else begin
        String.blit !source !pos buf 0 count;
        pos := !pos + count;
        count
      end
    end

type command = Json.json Stream.t * (Json.json -> unit) -> state -> Json.json list -> state * Json.json 

let command_tell : command = fun (i,o) state -> function
  | [`String source] ->
      Env.reset_missing_cmis ();
      let goteof = ref false in
      let lexbuf = lex_string source
        begin fun () ->
          goteof := true;
          o (`Bool false);
          try begin match Stream.next i with
            | `List [`String "tell" ; `String source] -> source
            | `List [`String "tell" ; `Null ] -> ""
            | _ -> invalid_arguments ()
          end with
            Stream.Failure -> invalid_arguments ()
        end
      in
      let _ = Error_report.reset_warnings () in
      let rec loop state =
        let bufpos = ref state.pos in
        let outlines, chunks, types = 
          (History.cutoff state.outlines), 
          (History.cutoff state.chunks), 
          (History.cutoff state.types)
        in
        let tokens, outlines =
          Outline.parse ~bufpos ~goteof
            (History.of_list state.tokens) outlines lexbuf
        in
        let chunks = Chunk.sync outlines chunks in
        let types = Typer.sync chunks types in
        let tokens = History.nexts tokens in
        let pos = !bufpos in
        let state' = { tokens ; outlines ; chunks ; types ; pos } in
        if !goteof || state.tokens = state'.tokens
        then state'
        else loop state'
      in
      let state = loop state in
      let w = Error_report.reset_warnings () in
      let state =
        { state with
          outlines = History.modify
            (fun (r,k,t,e) -> (r,k,t, w @ e))
            state.outlines
        } in
      state, `Bool true
  | _ -> invalid_arguments ()

exception Found of Types.signature_item

let command_type : command = fun _ state -> function
  | [`String "expression"; `String expr] ->
      let lexbuf = Lexing.from_string expr in
      let env = Typer.env state.types in
      let expression = Chunk_parser.top_expr Lexer.token lexbuf in
      let (str, sg, _) =
        Typemod.type_toplevel_phrase env
          Parsetree.([{ pstr_desc = Pstr_eval expression ; pstr_loc = Location.curr lexbuf }])
      in
      (*let sg' = Typemod.simplify_signature sg in*)
      let open Typedtree in
      begin match str.str_items with
        | [ { str_desc = Tstr_eval exp }] ->
            let ppf, to_string = Outline_utils.ppf_to_string () in
            Printtyp.type_scheme ppf exp.exp_type;
            state, (`List [`String "type" ; `String (to_string ())] :> Json.json)
        | _ -> failwith "unhandled expression"
      end

  | [`String "at" ; jpos] ->
    let `Line (ln,cl) = Outline_utils.pos_of_json jpos in
    let trees = Typer.trees state.types in
    begin try
      List.iter
      begin fun (tstr,tsg) ->
        List.iter
        begin fun sg ->
          match Browse.signature_loc sg with
            | Some loc when Browse.compare_loc (ln,cl) loc < 0 ->
                raise Not_found
            | Some loc when Browse.compare_loc (ln,cl) loc = 0 ->
                raise (Found sg)
            | _ -> ()
          end tsg
        end trees;
        raise Not_found
      with Found sg -> 
        let ppf, to_string = Outline_utils.ppf_to_string () in
        Printtyp.signature ppf [sg];
        state, `List [`String "type" ; `String (to_string ())]
    end

  | _ -> invalid_arguments ()

let length_lessthan n l = 
  let rec aux i = function
    | _ :: xs when i < n -> aux (succ i) xs
    | [] -> Some i
    | _ -> None
  in
  aux 0 l

let rec mod_smallerthan n m =
  if n < 0 then None
  else
  let open Types in
  match m with
  | Mty_ident _ -> Some 1
  | Mty_signature s ->
      begin match length_lessthan n s with
        | None -> None
        | Some n' ->
            List.fold_left
            begin fun acc item ->
              match acc, item with
                | None, _ -> None
                | Some n', _ when n' > n -> None
                | Some n1, Sig_modtype (_,Modtype_manifest m)
                | Some n1, Sig_module (_,m,_) ->
                    (match mod_smallerthan (n - n1) m with
                       | Some n2 -> Some (n1 + n2)
                       | None -> None)
                | Some n', _ -> Some (succ n')
            end (Some 0) s
      end
  | Mty_functor (_,m1,m2) ->
      begin
        match mod_smallerthan n m1 with
        | None -> None
        | Some n1 ->
        match mod_smallerthan (n - n1) m2 with
        | None -> None
        | Some n2 -> Some (n1 + n2)
      end

let command_complete : command =
  let has_prefix p =
    let l = String.length p in
    fun s ->
      let l' = String.length s in
      (l' >= l) && (String.sub s 0 l = p)
  in
  let complete_in_env env prefix = 
    let fmt ~exact name path ty =
      let ident = Ident.create (Path.last path) in
      let ppf, to_string = Outline_utils.ppf_to_string () in
      let kind =
        match ty with
        | `Value v -> Printtyp.value_description ident ppf v; "value"
        | `Cons c  -> 
            Format.pp_print_string ppf name;
            Format.pp_print_string ppf " : ";
            Printtyp.type_expr ppf Types.({ level = 0 ; id = 0 ; desc = Tarrow ("",{ level = 0; id = 0; desc = Ttuple c.cstr_args}, c.cstr_res,Cok)});
            "constructor"
        | `Mod m   -> 
            (if exact then
               match mod_smallerthan 200 m with
                 | None -> ()
                 | Some _ -> Printtyp.modtype ppf m
            ); "module"
        | `Typ t ->
            Printtyp.type_declaration ident ppf t; "type"
      in
      let desc, info = match kind with "module" -> "", to_string () | _ -> to_string (), "" in
      `Assoc ["name", `String name ; "kind", `String kind ; "desc", `String desc ; "info", `String info]
    in
    let find ?path prefix compl =
      let valid = has_prefix prefix in
      (* Hack to prevent extensions namespace to leak *)
      let valid name = name <> "_" && valid name in
      let compl = [] in
      let compl = Env.fold_values
        (fun name path v compl ->
           if valid name then (fmt ~exact:(name = prefix) name path (`Value v)) :: compl else compl)
        path env compl
      in
      let compl = Env.fold_constructors
        (fun name path v compl ->
           if valid name then (fmt ~exact:(name = prefix) name path (`Cons v)) :: compl else compl)
        path env compl
      in
      let compl = Env.fold_types
        (fun name path v compl ->
           if valid name then (fmt ~exact:(name = prefix)  name path (`Typ v)) :: compl else compl)
        path env compl
      in
      let compl = Env.fold_modules
        (fun name path v compl ->
           if valid name then (fmt ~exact:(name = prefix)  name path (`Mod v)) :: compl else compl)
        path env compl
      in
      compl
    in
    try
      match Longident.parse prefix with
        | Longident.Ldot (path,prefix) -> find ~path prefix []
        | Longident.Lident prefix -> find prefix []
        | _ -> find prefix []
    with Not_found -> []
  in
  fun _ state -> function
  | [`String "prefix" ; `String prefix] ->
    begin
      let env = Typer.env state.types in
      let compl = complete_in_env env prefix in
      state, `List (List.rev compl)
    end 
  | [`String "prefix" ; `String prefix ; `String "at" ; pos ] ->
    begin
      failwith "TODO"
    end 
  | _ -> invalid_arguments ()

let command_seek : command = fun _ state -> function
  | [`String "position"] ->
      state, return_position state.pos
  | [`String "position" ; jpos] ->
      let `Line (l,c) = Outline_utils.pos_of_json jpos in
      let outlines = Outline.seek_line (l,c) state.outlines in
      let outlines = match History.backward outlines with
        | Some ((_,Outline_utils.Partial_definitions _,_,_), o) -> o
        | _ -> outlines
      in
      let outlines, chunks = History.Sync.rewind fst outlines state.chunks in
      let chunks, types = History.Sync.rewind fst chunks state.types in
      let pos =
        match Outline.last_position outlines with
          | Some p -> p
          | None -> initial_state.pos
      in
      { tokens = [] ; outlines ; chunks ; types ; pos },
      return_position pos
  | [`String "end_of_definition"] ->
      failwith "TODO"
  | [`String "end"] ->
      let outlines = History.seek (fun _ -> 1) state.outlines in
      let chunks = History.Sync.right fst outlines state.chunks in
      let types  = History.Sync.right fst chunks state.types in
      let pos =
        match Outline.last_position outlines with
          | Some p -> p
          | None -> initial_state.pos
      in
      { tokens = [] ; outlines ; chunks ; types ; pos },
      return_position pos

  | [`String "maximize_scope"] ->
      let rec find_end_of_module (depth,outlines) =
        if depth = 0 then (0,outlines)
        else
        match History.forward outlines with
          | None -> (depth,outlines)
          | Some ((_,Outline_utils.Leave_module,_,_),outlines') ->
              find_end_of_module (pred depth, outlines')
          | Some ((_,Outline_utils.Enter_module,_,_),outlines') ->
              find_end_of_module (succ depth, outlines')
          | Some (_,outlines') -> find_end_of_module (depth,outlines')
      in
      let rec loop outlines =
        match History.forward outlines with
          | None -> outlines
          | Some ((_,Outline_utils.Leave_module,_,_),_) ->
              outlines
          | Some ((_,Outline_utils.Enter_module,_,_),outlines') ->
              (match find_end_of_module (1,outlines') with
                | (0,outlines'') -> outlines''
                | _ -> outlines)
          | Some (_,outlines') -> loop outlines'
      in 
      let outlines = loop state.outlines in
      let chunks = History.Sync.right fst outlines state.chunks in
      let types  = History.Sync.right fst chunks state.types in
      let pos =
        match Outline.last_position outlines with
          | Some p -> p
          | None -> initial_state.pos
      in
      { tokens = [] ; outlines ; chunks ; types ; pos },
      return_position pos
  | _ -> invalid_arguments ()

let command_reset : command = fun _ state -> function
  | [] -> initial_state, return_position initial_state.pos
  | _ -> invalid_arguments ()

let command_refresh : command = fun _ state -> function
  | [] -> 
      Env.reset_cache ();
      let types = Typer.sync state.chunks History.empty in
      { state with types }, `Bool true
  | _ -> invalid_arguments ()


(* Path management *)
let command_which : command = fun _ state -> function
  | [`String "path" ; `String s] -> 
      let filename =
        try
          Misc.find_in_path_uncap !source_path s
        with Not_found ->
          Misc.find_in_path_uncap !Config.load_path s
      in
      state, `String filename
  | [`String "with_ext" ; `String ext] ->
      let results =
        List.fold_left 
        begin fun results dir -> 
          try
            Array.fold_left 
            begin fun results file -> 
              if Filename.check_suffix file ext
              then let name = Filename.chop_extension file in
                begin
                  (if String.length name > 1 then
                     name.[0] <- Char.uppercase name.[0]);
                  `String name :: results
                end
              else results
            end results (Sys.readdir dir)
          with Sys_error _ -> results 
        end [] !source_path
      in
      state, `List results
  | _ -> invalid_arguments ()

let command_path pathes : command = fun _ state -> function
  | [ `String "list" ] ->
      state, `List [ `String "variables"; `List (List.map (fun (s,_) -> `String s) pathes)]
  | [ `String "list" ; `String path ] ->
      let r,_ = List.assoc path pathes in
      state, `List [ `String path; `List (List.map (fun s -> `String s) !r)]
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

let list_filter_dup lst =
  let tbl = Hashtbl.create 17 in
  List.rev (List.fold_left (fun a b -> if Hashtbl.mem tbl b then a else (Hashtbl.add tbl b (); b :: a)) [] lst)

let command_find : command = fun _ state -> function
  | (`String "use" :: packages) ->
      let packages = List.map
        (function `String pkg -> pkg | _ -> invalid_arguments ())
        packages
      in
      let packages = Findlib.package_deep_ancestors [] packages in
      let path = List.map Findlib.package_directory packages in
      Config.load_path := list_filter_dup (path @ !Config.load_path);
      state, (`Bool true)
  | [`String "list"] ->
      state, `List (List.map (fun s -> `String s) (Fl_package_base.list_packages ()))
  | _ -> invalid_arguments ()

let command_cd : command = fun _ state -> function
  | [`String s] ->
      Sys.chdir s;
      state, (`Bool true)
  | _ -> invalid_arguments ()

(* Reporting *)      
let command_errors : command = fun _ state -> function
  | [] ->
      let exns = Outline.exns state.outlines @ Typer.exns state.types in
      state, `List [`String "errors" ; `List (Error_report.to_jsons (List.rev exns)) ]
  | _ -> invalid_arguments ()

let command_dump : command =
  let pr_item_desc items =
    (List.map (fun (s,i) -> `List [`String s;`Int i]) (Chunk.dump_chunk items))
  in
  fun _ state -> function
  | [`String "env"] ->
      let sg = Browse.Env.signature_of_env (Typer.env state.types) in
      let aux item =
        let ppf, to_string = Outline_utils.ppf_to_string () in
        Printtyp.signature ppf [item];
        let content = to_string () in
        let ppf, to_string = Outline_utils.ppf_to_string () in
        match Browse.signature_loc item with
          | Some loc ->
              Location.print_loc ppf loc;
              let loc = to_string () in
              `List [`String loc ; `String content]
          | None -> `String content
      in
      state, `List [`String "env" ; `List (List.map aux sg)]
  | [`String "sig"] ->
      let trees = Typer.trees state.types in
      let sg = List.flatten (List.map snd trees) in
      let aux item =
        let ppf, to_string = Outline_utils.ppf_to_string () in
        Printtyp.signature ppf [item];
        let content = to_string () in
        let ppf, to_string = Outline_utils.ppf_to_string () in
        match Browse.signature_loc item with
          | Some loc ->
              Location.print_loc ppf loc;
              let loc = to_string () in
              `List [`String loc ; `String content]
          | None -> `String content
      in
      state, `List [`String "env" ; `List (List.map aux sg)]
  | [`String "chunks"] ->
      state, `List (pr_item_desc state.chunks)
  (*| [`String "types" ; `String "chunks"] ->
      state, `List (List.rev_map (fun item -> `List (pr_item_desc item))
                                 (Typer.chunks state.types))*)
  | _ -> invalid_arguments ()

(* Browsing *)
 
let command_help : command = fun _ state -> function
  | [] -> 
      let helps = Hashtbl.fold (fun cmd (_,doc) cmds -> (cmd,doc) :: cmds) commands [] in
      state, `List [`String "help" ; `Assoc helps ]
  | _ -> invalid_arguments ()

let _ = List.iter (fun (a,b,c) -> Hashtbl.add commands a (b,c))
[
  "tell",  command_tell,
    `String "TODO";
  "seek",  command_seek,
    `String "TODO";
  "reset", command_reset,
    `String "TODO";
  "refresh", command_refresh,
    `String "TODO";
  "cd",    command_cd,
    `String "TODO";
  "which", command_which,
    `String "TODO";
  "path",  command_path ["build",  (Config.load_path,default_build_paths);
                         "source", (source_path, lazy [])],
    `String "TODO";
  "find", command_find,
    `String "TODO";
  "type",  command_type,
    `String "TODO";
  "complete", command_complete,
    `String "TODO";
  "errors", command_errors,
    `String "TODO";
  "dump", command_dump,
    `String "TODO";
  "help", command_help,
    `String "List every command with synopsis and small description";
]

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
  Findlib.init ();
  main_loop ()

let _ = main ()
