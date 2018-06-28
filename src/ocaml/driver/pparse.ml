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

let report_error = function
  | CannotRun cmd ->
    Logger.logf "Pparse" "report_error"
      "Error while running external preprocessor. Command line: %s" cmd
  | WrongMagic cmd ->
    Logger.logf "Pparse" "report_error"
      "External preprocessor does not produce a valid file. Command line: %s" cmd

let ppx_commandline cmd fn_in fn_out =
  Printf.sprintf "%s %s %s 1>&2"
    cmd (Filename.quote fn_in) (Filename.quote fn_out)

let pp_commandline cmd fn_in fn_out =
  Printf.sprintf "%s %s 1>%s"
    cmd (Filename.quote fn_in) (Filename.quote fn_out)

let apply_rewriter magic (fn_in, failures) ppx =
  Logger.log "Pparse" "apply_rewriter" ppx;
  Logger.log_flush ();
  let fn_out = Filename.temp_file "camlppx" "" in
  let comm = ppx_commandline ppx fn_in fn_out in
  let failure =
    let ok = Sys.command comm = 0 in
    if not ok then Some (CannotRun comm)
    else if not (Sys.file_exists fn_out) then
      Some (WrongMagic comm)
    else
      (* check magic before passing to the next ppx *)
      let ic = open_in_bin fn_out in
      let buffer =
        try really_input_string ic (String.length magic)
        with End_of_file -> ""
      in
      close_in ic;
      if buffer <> magic then
        Some (WrongMagic comm)
      else
        None
  in
  match failure with
  | Some err ->
    Misc.remove_file fn_out;
    let fallback =
      let fallback =
        Filename.concat (Filename.get_temp_dir_name ())
          ("camlppx.lastfail" ^ string_of_int failures)
      in
      match Sys.rename fn_in fallback with
      | () -> fallback
      | exception exn ->
        Logger.logf "Pparse" "apply_rewriter"
          "exception while renaming ast: %a"
          (fun () -> Printexc.to_string) exn;
        fn_in
    in
    report_error (CannotRun comm);
    (fallback, failures + 1)
  | None ->
    Misc.remove_file fn_in;
    (fn_out, failures)

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
  let fn_out, _ =
    List.fold_left
      (apply_rewriter magic) (write_ast magic ast, 0) (List.rev ppxs)
  in
  read_ast magic fn_out


let apply_rewriters_str ~ppx ?(restore = true) ~tool_name ast =
  match ppx with
  | [] -> ast
  | ppxs ->
      let ast = Ast_mapper.add_ppx_context_str ~tool_name ast in
      let ast = rewrite Config.ast_impl_magic_number ast ppxs in
      Ast_mapper.drop_ppx_context_str ~restore ast

let apply_rewriters_sig ~ppx ?(restore = true) ~tool_name ast =
  match ppx with
  | [] -> ast
  | ppxs ->
      let ast = Ast_mapper.add_ppx_context_sig ~tool_name ast in
      let ast = rewrite Config.ast_intf_magic_number ast ppxs in
      Ast_mapper.drop_ppx_context_sig ~restore ast

let apply_rewriters ~ppx ?restore ~tool_name = function
  | `Interface ast ->
    `Interface (apply_rewriters_sig ~ppx ?restore ~tool_name ast)
  | `Implementation ast ->
    `Implementation (apply_rewriters_str ~ppx ?restore ~tool_name ast)

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

(*let apply_pp ~filename ~source ~pp =
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
    `Implementation (read_ast Config.ast_impl_magic_number fn_out
                : Parsetree.structure)
  else if buffer = Config.ast_intf_magic_number then
    `Interface (read_ast Config.ast_intf_magic_number fn_out
                : Parsetree.signature)
  else
    Misc.fatal_error "OCaml and preprocessor have incompatible versions"*)
