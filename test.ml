(* Add whatever -I options have been specified on the command line,
     but keep the directories that user code linked in with ocamlmktop
     may have added to load_path. *)
let default_build_paths =
  let open Config in
  lazy (List.rev !Clflags.include_dirs @ !load_path @ [Filename.concat standard_library "camlp4"])

let set_default_path () =
  Config.load_path := Lazy.force default_build_paths

let source_path = ref []

type state = {
  pos      : Lexing.position;
  tokens   : Outline.Raw.t;
  outlines : Outline.Chunked.t;
  chunks   : Chunk.t;
  envs     : Typer.t;
  synced   : bool;
}
 
let initial_state = {
  pos      = Lexing.((from_string "").lex_curr_p);
  tokens   = History.empty;
  outlines = History.empty;
  chunks   = History.empty;
  envs     = History.empty;
  synced   = true;
}

let commands = Hashtbl.create 17

let main_loop () =
  let logger = open_out "/home/def/outliner.log" in
  let log_input json = Printf.fprintf logger "> %s\n%!" (Json.to_string json); json in
  let log_output json = Printf.fprintf logger "< %s\n%!" (Json.to_string json); json in
  let input  = Json.stream_from_channel stdin in
  let output =
    let out_json = Json.to_channel stdout in
    fun json ->
      out_json json;
      print_newline ()
  in
  try
    let rec loop state =
      let state, answer =
        try match log_input (Stream.next input) with
          | `List (`String command :: args) ->
                let handler =
                  try Hashtbl.find commands command
                  with Not_found -> failwith "unknown command"
                in
                handler state args
          | _ -> failwith "malformed command"
        with
          | Failure s -> state, `List [`String "failure"; `String s]
          | Stream.Failure as exn -> raise exn
          | exn -> state, `List [`String "exception"; `String (Printexc.to_string exn)]
      in
      output (log_output answer);
      loop state
    in
    loop initial_state
  with Stream.Failure -> ()

let return_position p = `List [`String "position" ; Outline_utils.pos_to_json p]

let invalid_arguments () = failwith "invalid arguments"

let lex_string ?(offset=0) string =
  let pos = ref offset in
  let len = String.length string in
  Lexing.from_function
    begin fun buf size ->
      let count = min (len - !pos) size in
      if count <= 0 then 0
      else begin
        pos := !pos + count;
        String.blit string !pos buf 0 count;
        count
      end
    end

type command = state -> Json.json list -> state * Json.json 

let command_tell state = function
  | [`String source] ->
      let goteof = ref false in
      let lexbuf = Lexing.from_string source in
      let rec loop state =
        let bufpos = ref state.pos in
        let tokens, outlines, chunks, envs = if state.synced
          then state.tokens, state.outlines, state.chunks, state.envs
          else fst (History.split state.tokens), 
               fst (History.split state.outlines), 
               fst (History.split state.chunks), 
               fst (History.split state.envs)
        in
        let tokens, outlines =
          Outline.parse ~bufpos ~goteof
            (tokens,outlines) lexbuf
        in
        let chunks = Chunk.sync outlines chunks in
        let envs = Typer.sync chunks envs in
        let state = { tokens ; outlines ; chunks ; envs ; pos = !bufpos ; synced = true } in
        if !goteof
        then state
        else loop state
      in
      loop state, `Bool true
  | _ -> invalid_arguments ()

let command_typeof state = function
  | [`String expr] ->
      let lexbuf = Lexing.from_string expr in
      let env = Typer.env state.envs in
      let expression = Chunk_parser.top_expr Outline_lexer.token lexbuf in
      let (str, sg, _) =
        Typemod.type_toplevel_phrase env
          Parsetree.([{ pstr_desc = Pstr_eval expression ; pstr_loc = Location.curr lexbuf }])
      in
      (*let sg' = Typemod.simplify_signature sg in*)
      let open Typedtree in
      begin match str.str_items with
        | [ { str_desc = Tstr_eval exp }] ->
            let buffer = Buffer.create 16 in
            let ppf = Format.formatter_of_buffer buffer in
            Printtyp.type_scheme ppf exp.exp_type;
            Format.pp_print_flush ppf ();
            state, (`List [`String "type" ; `String (Buffer.contents buffer)] :> Json.json)
        | _ -> failwith "unhandled expression"
      end
  | _ -> invalid_arguments ()

let command_line state = function
  | [] -> state, return_position state.pos
  | _ -> invalid_arguments ()

let command_seek state = function
  | [`String "position" ; `Assoc props] ->
      let pos =
        try match List.assoc "offset" props with
          | `Int i -> failwith "FIXME: offsets are computed incorrectly"; `Offset i
          | _ -> invalid_arguments () 
        with Not_found ->
        try match List.assoc "line" props, List.assoc "col" props with
          | `Int line, `Int col -> `Line (line, col)
          | _ -> invalid_arguments ()
        with Not_found -> invalid_arguments ()
      in
      let outlines =
        match pos with
          | `Offset o -> Outline.Chunked.seek_offset o state.outlines
          | `Line (l,c) -> Outline.Chunked.seek_line (l,c) state.outlines
      in
      let tokens, outlines = History.sync fst state.tokens outlines in
      let outlines, chunks = History.sync_backward fst outlines state.chunks in
      let chunks, envs = History.sync_backward Misc.fst3 chunks state.envs in
      let pos =
        match Outline.Chunked.last_position outlines with
          | Some p -> p
          | None -> initial_state.pos
      in
      { tokens ; outlines ; chunks ; envs ; pos ; synced = false },
      return_position pos
  | [`String "end_of_definition"] ->
      failwith "TODO"
  | [`String "maximize_scope"] ->
        let rec find_end_of_module (depth,outlines) =
          if depth = 0 then (0,outlines)
          else
          match History.forward outlines with
            | None -> (depth,outlines)
            | Some ((_,(_,Outline_utils.Leave_module,_,_)),outlines') ->
                find_end_of_module (pred depth, outlines')
            | Some ((_,(_,Outline_utils.Enter_module,_,_)),outlines') ->
                find_end_of_module (succ depth, outlines')
            | Some (_,outlines') -> find_end_of_module (depth,outlines')
        in
        let rec loop outlines =
          match History.forward outlines with
            | None -> outlines
            | Some ((_,(_,Outline_utils.Leave_module,_,_)),_) ->
                outlines
            | Some ((_,(_,Outline_utils.Enter_module,_,_)),outlines') ->
                (match find_end_of_module (1,outlines') with
                  | (0,outlines'') -> outlines''
                  | _ -> outlines)
            | Some (_,outlines') -> loop outlines'
        in 
        let outlines = loop state.outlines in
        let tokens = History.sync_left_forward fst state.tokens outlines in
        let chunks = History.sync_right_forward fst outlines state.chunks in
        let envs   = History.sync_right_forward Misc.fst3 chunks state.envs in
        let pos =
          match Outline.Chunked.last_position outlines with
            | Some p -> p
            | None -> initial_state.pos
        in
        { tokens ; outlines ; chunks ; envs ; pos ; synced = false },
        return_position pos
  | _ -> invalid_arguments ()

let command_reset state = function
  | [] -> initial_state, return_position initial_state.pos
  | _ -> invalid_arguments ()

(* Path management *)
let command_which state = function
  | [`String s] -> 
      let filename =
        try
          Misc.find_in_path_uncap !source_path s
        with Not_found ->
          Misc.find_in_path_uncap !Config.load_path s
      in
      state, `String filename
  | _ -> invalid_arguments ()

let command_path ~reset r state = function
  | [ `String "list" ] ->
      state, `List (List.map (fun s -> `String s) !r)
  | [ `String "add" ; `String s ] ->
      let d = Misc.expand_directory Config.standard_library s in
      r := d :: !r;
      state, `Bool true
  | [ `String "remove" ; `String s ] ->
      let d = Misc.expand_directory Config.standard_library s in
      r := List.filter (fun d' -> d' <> d) !r;
      state, `Bool true
  | [ `String "reset" ] ->
      r := Lazy.force reset;
      state, `Bool true
  | _ -> invalid_arguments ()

let command_cd state = function
  | [`String s] ->
      Sys.chdir s;
      state, (`Bool true)
  | _ -> invalid_arguments ()

(* Reporting *)      
let command_report_errors state = function
  | [] ->
      let exns =
        (match History.prev state.outlines with
          | Some (_,(_,_,_,exns)) -> exns
          | None -> []) @
          (match History.prev state.envs with
            | Some (_,_,exns) -> exns
            | None -> [])
      in
      state, `List [`String "errors" ; `List (Error_report.to_jsons exns) ]
  | _ -> invalid_arguments ()

(* Browsing *)
(*let command_complete state = function
  | [`String "expression" ; `String base] ->
  | _ -> invalid_arguments ()*)
  
let _ = List.iter (fun (a,b) -> Hashtbl.add commands a b) [
  "tell",  (command_tell  :> command);
  "line",  (command_line  :> command);
  "seek",  (command_seek  :> command);
  "reset", (command_reset :> command);
  "cd",    (command_cd    :> command);
  "which", (command_which :> command);
  "source_path", (command_path ~reset:default_build_paths source_path :> command);
  "build_path",  (command_path ~reset:(lazy []) Config.load_path :> command);
  "typeof", (command_typeof :> command);
  "report_errors", (command_report_errors :> command);
]

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
  
let print_version () =
  Printf.printf "The Outliner toplevel, version %s\n" Sys.ocaml_version;
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

let main () =
  Arg.parse Options.list unexpected_argument "TODO";
  Compile.init_path ();
  set_default_path ();
  main_loop ()

let _ = main ()
