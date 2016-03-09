(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

type error =
  | CannotRun of string
  | WrongMagic of string

exception Error of error

(* Note: some of the functions here should go to Ast_mapper instead,
   which would encapsulate the "binary AST" protocol. *)

let write_ast magic ast =
  let fn = Filename.temp_file "camlppx" "" in
  let oc = open_out_bin fn in
  output_string oc magic;
  output_value oc !Location.input_name;
  output_value oc ast;
  close_out oc;
  fn

let null = match Sys.os_type with "Win32" -> " NUL" | _ -> "/dev/null"

let ppx_commandline cmd fn_in fn_out =
  Printf.sprintf "%s %s %s 1>%s"
    cmd (Filename.quote fn_in) (Filename.quote fn_out) null

let pp_commandline cmd fn_in fn_out =
  Printf.sprintf "%s %s 1>%s"
    cmd (Filename.quote fn_in) (Filename.quote fn_out)

let apply_rewriter magic fn_in ppx =
  let fn_out = Filename.temp_file "camlppx" "" in
  let comm = ppx_commandline ppx fn_in fn_out in
  let ok = Sys.command comm = 0 in
  if ok then
    Misc.remove_file fn_in
  else begin
    try
      Sys.rename fn_in
        (Filename.concat (Filename.get_temp_dir_name ()) "camlppx.lastfail")
    with _ -> ()
  end;
  if not ok then begin
    Misc.remove_file fn_out;
    raise (Error (CannotRun comm));
  end;
  if not (Sys.file_exists fn_out) then
    raise (Error (WrongMagic comm));
  (* check magic before passing to the next ppx *)
  let ic = open_in_bin fn_out in
  let buffer =
    try really_input_string ic (String.length magic) with End_of_file -> "" in
  close_in ic;
  if buffer <> magic then begin
    Misc.remove_file fn_out;
    raise (Error (WrongMagic comm));
  end;
  fn_out

let read_ast magic fn =
  let ic = open_in_bin fn in
  try
    let buffer = really_input_string ic (String.length magic) in
    assert(buffer = magic); (* already checked by apply_rewriter *)
    Location.input_name := input_value ic;
    let ast = input_value ic in
    close_in ic;
    Misc.remove_file fn;
    ast
  with exn ->
    close_in ic;
    Misc.remove_file fn;
    raise exn

let rewrite magic ast ppxs =
  read_ast magic
    (List.fold_left (apply_rewriter magic) (write_ast magic ast)
       (List.rev ppxs))

let apply_rewriters_str ?(restore = true) ~tool_name ast =
  match Ppxsetup.command_line !Clflags.ppx with
  | [] -> ast
  | ppxs ->
      let ast = Ast_mapper.add_ppx_context_str ~tool_name ast in
      let ast = rewrite Config.ast_impl_magic_number ast ppxs in
      Ast_mapper.drop_ppx_context_str ~restore ast

let apply_rewriters_sig ?(restore = true) ~tool_name ast =
  match Ppxsetup.command_line !Clflags.ppx with
  | [] -> ast
  | ppxs ->
      let ast = Ast_mapper.add_ppx_context_sig ~tool_name ast in
      let ast = rewrite Config.ast_intf_magic_number ast ppxs in
      Ast_mapper.drop_ppx_context_sig ~restore ast

let read_ast magic fn =
  let ic = open_in_bin fn in
  try
    let buffer = really_input_string ic (String.length magic) in
    assert(buffer = magic); (* already checked by apply_rewriter *)
    Location.input_name := input_value ic;
    let ast = input_value ic in
    close_in ic;
    Misc.remove_file fn;
    ast
  with exn ->
    close_in ic;
    Misc.remove_file fn;
    raise exn

let apply_pp ~filename ~source ~pp =
  let fn_in = Filename.temp_file "merlinpp" (Filename.basename filename) in
  begin
    let oc = open_out_bin fn_in in
    output_string oc source;
    close_out oc
  end;
  let fn_out = fn_in ^ ".out" in
  let comm = pp_commandline pp fn_in fn_out in
  let ok = Sys.command comm = 0 in
  if not ok then begin
    Misc.remove_file fn_out;
    raise (Error (CannotRun comm));
  end;
  if not (Sys.file_exists fn_out) then
    raise (Error (WrongMagic comm));
  Misc.remove_file fn_in;
  let ic = open_in_bin fn_out in
  let buffer = really_input_string ic
      (String.length Config.ast_impl_magic_number) in
  close_in ic;
  if buffer = Config.ast_impl_magic_number then
    `Structure (read_ast Config.ast_impl_magic_number fn_out
                : Parsetree.structure)
  else if buffer = Config.ast_intf_magic_number then
    `Signature (read_ast Config.ast_intf_magic_number fn_out
                : Parsetree.signature)
  else
    Misc.fatal_error "OCaml and preprocessor have incompatible versions"

let report_error ppf = function
  | CannotRun cmd ->
    Format.fprintf ppf "Error while running external preprocessor@.\
                 Command line: %s@." cmd
  | WrongMagic cmd ->
    Format.fprintf ppf "External preprocessor does not produce a valid file@.\
                 Command line: %s@." cmd

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
