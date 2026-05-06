(* This file is part of the ocamlgrep package *)
(* See the attached LICENSE file.            *)
(* Copyright (C) 2000-2026 LexiFi            *)

open Printf

type finding = {
  source: string;
  start_line: int;     (* 1-based *)
  start_col: int;      (* 0-based column on [start_line] *)
  end_line: int;       (* 1-based; equals [start_line] for single-line matches *)
  end_col: int;        (* 0-based column on [end_line] *)
  lines: string list;  (* source lines [start_line..end_line] inclusive *)
}

type event =
  | Scan_file of string
  | Finding of finding
  | Warning of string

(* This is used to make a path relative to another.
   Similar to Fpath.relativize. *)
let drop_prefix ~prefix s =
  if String.starts_with ~prefix s then
    String.sub s (String.length prefix) (String.length s - String.length prefix)
  else
    s

let read_lines fn =
  String.split_on_char '\n' (In_channel.with_open_text fn In_channel.input_all)

(*** Structured search ***)

let incremental_search
    (paths : Paths.t)
    (handle_event : event -> unit)
    query : unit =
  (* Parse the user query as a single OCaml expression. We use Merlin's
     vendored parser ([Parser_raw.parse_expression]) rather than upstream
     [Parse.implementation], following the pattern in
     [src/analysis/type_utils.ml]. *)
  let expr =
    let lexbuf = Lexing.from_string query in
    let state = Lexer_raw.make (Lexer_raw.keywords []) in
    let rec lexer = function
      | Lexer_raw.Fail (e, l) -> raise (Lexer_raw.Error (e, l))
      | Lexer_raw.Return token -> token
      | Lexer_raw.Refill k -> lexer (k ())
    in
    let lexer lexbuf = lexer (Lexer_raw.token_without_comments state lexbuf) in
    try Parser_raw.parse_expression lexer lexbuf
    with _ -> failwith "Could not parse search expression."
  in
  let rec walk dir =
    Array.iter (fun entry ->
        let entry = Filename.concat dir entry in
        if Sys.is_directory entry then
          walk entry
        else if Filename.check_suffix entry ".cmt" then begin
          match Cmt_format.read_cmt entry with
          | {Cmt_format.cmt_sourcefile = Some source; cmt_source_digest = Some digest; _} as cmt ->
              (* source = user-friendly path to the source file, relative to
                          the search root (typically cwd)
                 pp_source = any valid path to the preprocessed ml file *)
              let source, pp_source =
                if Filename.check_suffix source ".pp.ml" then
                  Filename.chop_suffix source ".pp.ml" ^ ".ml",
                  Paths.in_build_dir paths source
                else
                  let source =
                    drop_prefix
                      ~prefix:(Paths.project_relative_search_root paths)
                      source in
                  source, source
              in
              handle_event (Scan_file source);
              if not (Sys.file_exists pp_source) then ()
              else if digest <> Digest.file pp_source then
                handle_event (Warning (
                  sprintf "%s does not correspond to %s (ignoring)"
                    entry pp_source
                ))
              else begin
                match Match.search_cmt cmt expr with
                | exception Match.Cannot_parse_type exn ->
                    failwith (
                      Format.asprintf "%s: could not parse type: %a."
                        entry Location.report_exception exn
                    )
                | exception exn ->
                    handle_event (Warning (
                      Format.asprintf "error while analysing %s: %a"
                        entry Location.report_exception exn
                    ))
                | [] -> ()
                | _ :: _ as locs ->
                    let src_lines = Array.of_list (read_lines source) in
                    let nb_lines = Array.length src_lines in
                    List.iter
                      (fun {Location.loc_start; loc_end; _} ->
                         let start_line = loc_start.pos_lnum in
                         let start_col =
                           loc_start.pos_cnum - loc_start.pos_bol
                         in
                         let end_line = loc_end.pos_lnum in
                         let end_col = loc_end.pos_cnum - loc_end.pos_bol in
                         (* Clamp to the actual file extent. With a matching
                            digest this should be a no-op, but it keeps us
                            from raising on edge cases. *)
                         let s = max 1 (min nb_lines start_line) in
                         let e = max s (min nb_lines end_line) in
                         let lines =
                           List.init (e - s + 1) (fun k -> src_lines.(s - 1 + k))
                         in
                         handle_event (Finding {
                           source;
                           start_line;
                           start_col;
                           end_line;
                           end_col;
                           lines;
                         })
                      ) locs
              end
          | {cmt_sourcefile = None; _} | {cmt_source_digest = None; _} ->
              ()
          | exception Cmt_format.Error (Cmt_format.Not_a_typedtree _) ->
              failwith ("error reading cmt file: " ^ entry)
        end
      ) (Sys.readdir dir)
  in
  let search_root_in_build_dir =
    Paths.in_build_dir paths (Paths.project_relative_search_root paths)
  in
  walk search_root_in_build_dir

let search paths query =
  let events = ref [] in
  let handle_event ev =
    events := ev :: !events in
  incremental_search paths handle_event query;
  List.rev !events
