open MenhirSdk
open Cmly_format

let g = Cmly_io.read_file Sys.argv.(1)

let is_attribute names (name', stretch : attribute) =
  List.mem (Positions.value name') names

let string_of_stretch s =
  s.Stretch.stretch_raw_content

let string_of_type = function
  | Stretch.Inferred s -> s
  | Stretch.Declared s -> string_of_stretch s

let printf = Printf.printf
let sprintf = Printf.sprintf

let menhir = "MenhirInterpreter"

(** Print header, if any *)

let print_header () =
  let name = Filename.chop_extension (Filename.basename Sys.argv.(1)) in
  printf "open %s\n" (String.capitalize name);
  List.iter
    (fun (_, stretch) -> printf "%s\n" (string_of_stretch stretch))
    (List.filter (is_attribute ["header"; "printer.header"]) g.g_attributes)

(** Printer from attributes *)

let symbol_printer default attribs =
  match List.find (is_attribute ["symbol"]) attribs with
  | _, stretch -> (string_of_stretch stretch)
  | exception Not_found ->
    sprintf "%S" default

let print_symbol () =
  let case_t t =
    match t.t_kind with
    | `REGULAR | `ERROR | `EOF ->
      printf "  | %s.X (%s.T %s.T_%s) -> %s\n"
        menhir menhir menhir
        t.t_name
        (symbol_printer t.t_name t.t_attributes)
    | `PSEUDO -> ()
  and case_n n =
    if n.n_kind = `REGULAR then
      printf "  | %s.X (%s.N %s.N_%s) -> %s\n"
        menhir menhir menhir
        n.n_mangled_name
        (symbol_printer n.n_mangled_name n.n_attributes)
  in
  printf "let print_symbol = function\n";
  Array.iter case_t g.g_terminals;
  Array.iter case_n g.g_nonterminals

let value_printer default attribs =
  match List.find (is_attribute ["printer"]) attribs with
  | _, stretch ->
    sprintf "(%s)" (string_of_stretch stretch)
  | exception Not_found ->
    sprintf "(fun _ -> %s)" (symbol_printer default attribs)

let print_value () =
  let case_t t =
    match t.t_kind with
    | `REGULAR | `ERROR | `EOF->
      printf "  | %s.T %s.T_%s -> %s\n"
        menhir menhir
        t.t_name (value_printer t.t_name t.t_attributes)
    | `PSEUDO -> ()
  and case_n n =
    if n.n_kind = `REGULAR then
      printf "  | %s.N %s.N_%s -> %s\n"
        menhir menhir
        n.n_mangled_name (value_printer n.n_mangled_name n.n_attributes)
  in
  printf "let print_value (type a) : a %s.symbol -> a -> string = function\n"
    menhir;
  Array.iter case_t g.g_terminals;
  Array.iter case_n g.g_nonterminals

let print_token () =
  let case t =
    match t.t_kind with
    | `REGULAR | `EOF ->
      printf "  | %s%s -> print_value (%s.T %s.T_%s) %s\n"
        t.t_name
        (match t.t_type with | None -> "" | Some typ -> " v")
        menhir menhir
        t.t_name
        (match t.t_type with | None -> "()" | Some typ -> "v")
    | `PSEUDO | `ERROR -> ()
  in
  printf "let print_token = function\n";
  Array.iter case g.g_terminals

let print_token_of_terminal () =
  let case t =
    match t.t_kind with
    | `REGULAR | `EOF ->
      printf "  | %s.T_%s -> %s%s\n"
        menhir t.t_name
        t.t_name (if t.t_type <> None then " v" else "")
    | `ERROR ->
      printf "  | %s.T_%s -> assert false\n"
        menhir t.t_name
    | `PSEUDO -> ()
  in
  printf
    "let token_of_terminal (type a) (t : a %s.terminal) (v : a) : token =\n\
    \  match t with\n"
    menhir;
  Array.iter case g.g_terminals

let () =
  print_header ();
  print_newline ();
  print_symbol ();
  print_newline ();
  print_value ();
  print_newline ();
  print_token ();
  print_newline ();
  print_token_of_terminal ()
