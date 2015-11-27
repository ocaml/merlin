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

let apply_rewriter magic fn_in ppx =
  let fn_out = Filename.temp_file "camlppx" "" in
  (* TODO: someday, we should try to:
       - read the output, instead of piping it to /dev/null, the output is
         actually useful when things go wrong.
       - don't let vim display a disgusting backtrace, instead plug it into
         merlin's error system. *)
  let comm =
    Printf.sprintf "%s %s %s 1>%s 2>%s"
      ppx
      (Filename.quote fn_in) (Filename.quote fn_out)
      null null
  in
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

let rewrite_cache
  : (string * string list * string * Obj.t, Obj.t) Hashtbl.t
  = Hashtbl.create 7

let cache_size = 200

let rewrite magic ast ppxs =
  let key = (Sys.getcwd (), ppxs, magic, Obj.repr ast) in
  try Obj.obj (Hashtbl.find rewrite_cache key)
  with Not_found ->
    if Hashtbl.length rewrite_cache > cache_size then begin
      (* Cache eviction policy de qualitay, depuis 1870 *)
      let counter = ref (Random.int 3) in
      let to_remove =
        Hashtbl.fold (fun k v acc ->
            if !counter > 0 then
              (decr counter; k :: acc)
            else
              (counter := 2; acc)
          ) rewrite_cache []
      in
      List.iter (Hashtbl.remove rewrite_cache) to_remove;
    end;
    let result =
      read_ast magic
        (List.fold_left (apply_rewriter magic) (write_ast magic ast)
           (List.rev ppxs))
    in
    Hashtbl.add rewrite_cache key (Obj.repr result);
    result

let normalize_sig, normalize_str, restore_sig, restore_str =
  let module M = struct
    open Lexing
    open Location

    let line = ref 0

    let position_mapper pos =
      {pos_fname = pos.pos_fname;
       pos_bol = 0;
       pos_cnum = pos.pos_cnum - pos.pos_bol;
       pos_lnum = pos.pos_lnum - !line;
      }

    let location_mapper mapper loc =
      if !line = 0 then
        line := loc.loc_start.pos_lnum;
      {loc_ghost = loc.loc_ghost;
       loc_start = position_mapper loc.loc_start;
       loc_end   = position_mapper loc.loc_end;
      }

    open Ast_mapper
    let mapper = {default_mapper with location = location_mapper}

    let normalize_sig sg =
      line := 0;
      let sg = mapper.signature mapper sg in
      sg, !line

    let normalize_str str =
      line := 0;
      let str = mapper.structure mapper str in
      str, !line

    let restore_sig line' sg =
      if line' = 0 then sg
      else (line := -line'; mapper.signature mapper sg)

    let restore_str line' str =
      if line' = 0 then str
      else (line := -line'; mapper.structure mapper str)
  end
  in
  M.(normalize_sig, normalize_str, restore_sig, restore_str)

let apply_rewriters_str ?(restore = true) ~tool_name ast =
  match Clflags.ppx () with
  | [] -> ast
  | ppxs ->
      let ast, line = normalize_str ast in
      let ast = Ast_mapper.add_ppx_context_str ~tool_name ast in
      let ast = rewrite Config.ast_impl_magic_number ast ppxs in
      restore_str line (Ast_mapper.drop_ppx_context_str ~restore ast)

let apply_rewriters_sig ?(restore = true) ~tool_name ast =
  match Clflags.ppx () with
  | [] -> ast
  | ppxs ->
      let ast, line = normalize_sig ast in
      let ast = Ast_mapper.add_ppx_context_sig ~tool_name ast in
      let ast = rewrite Config.ast_intf_magic_number ast ppxs in
      restore_sig line (Ast_mapper.drop_ppx_context_sig ~restore ast)

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
