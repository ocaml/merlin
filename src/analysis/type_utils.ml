(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2014  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

open Std
open Merlin_lib

let parse_expr ?(keywords=Raw_lexer.keywords []) expr =
  let lexbuf = Lexing.from_string expr in
  let state = Raw_lexer.make keywords in
  let rec lex parser = function
    | Raw_lexer.Fail (e,l) ->
      assert false
    | Raw_lexer.Refill f ->
      lex parser (f ())
    | Raw_lexer.Return token ->
      parse (`Step (Raw_parser.feed parser
                      (Lexing.dummy_pos,token,Lexing.dummy_pos)))
  and parse = function
    | `Step s -> parse (Raw_parser.step s)
    | `Feed p -> lex p (Raw_lexer.token_without_comments state lexbuf)
    | `Accept (Raw_parser.N_ (Raw_parser.N_parse_expression, e)) ->
      Some (e : Parsetree.expression)
    | `Reject _ -> None
    | `Accept _ -> assert false
  in
  parse (`Step (Raw_parser.initial Raw_parser.parse_expression_state
                  (Lexing.dummy_pos,Raw_parser.ENTRYPOINT,Lexing.dummy_pos)))

let lookup_module_or_modtype name env =
  try
    let path, mty = Raw_compat.lookup_module name env in
    path, Some mty
  with Not_found ->
    Raw_compat.lookup_modtype name env

let verbosity = Fluid.from 0

module Printtyp = struct
  include Printtyp

  let expand_type env ty =
    if Fluid.get verbosity = 0 then ty
    else
      (* Fresh copy of the type to mutilate *)
      let ty = Subst.type_expr Subst.identity ty in
      let marks = Hashtbl.create 7 in
      let mark ty =
        if Hashtbl.mem marks ty.Types.id then false
        else (Hashtbl.add marks ty.Types.id (); true)
      in
      let rec iter d ty0 =
        let ty' = Ctype.repr ty0 in
        if mark ty' then
          let ty'' = Ctype.full_expand env ty' in
          if ty''.Types.desc == ty'.Types.desc then
            Btype.iter_type_expr (iter d) ty0
          else
            begin
              ty0.Types.desc <- ty''.Types.desc;
              if d > 0 then
                Btype.iter_type_expr (iter (pred d)) ty0
            end
      in
      iter (Fluid.get verbosity) ty;
      ty

  let expand_type_decl env ty =
    match ty.Types.type_manifest with
    | Some m -> {ty with Types.type_manifest = Some (expand_type env m)}
    | None -> ty

  let expand_sig = Raw_compat.full_scrape

  let verbose_type_scheme env ppf t =
    Printtyp.type_scheme ppf (expand_type env t)

  let verbose_type_declaration env id ppf t =
    Printtyp.type_declaration id ppf (expand_type_decl env t)

  let verbose_modtype env ppf t =
    Printtyp.modtype ppf (expand_sig env t)

  let select_verbose a b env =
    (if Fluid.get verbosity = 0 then a else b env)

  let type_scheme env ppf ty =
    select_verbose type_scheme verbose_type_scheme env ppf ty

  let type_declaration env id ppf =
    select_verbose type_declaration verbose_type_declaration env id ppf

  let modtype env ppf mty =
    select_verbose modtype verbose_modtype env ppf mty

  let wrap_printing_env env ~verbosity:v f =
    Fluid.let' verbosity v (fun () -> wrap_printing_env env f)
end

(* Check if module is smaller (= has less definition, counting nested ones)
 * than a particular threshold. Return (Some n) if module has size n, or None
 * otherwise (module is bigger than threshold).
 * Used to skip printing big modules in completion. *)
let rec mod_smallerthan n m =
  if n < 0 then None
  else
  let open Types in
  match m with
  | Mty_ident _ -> Some 1
  | Mty_signature (lazy s) ->
    begin match List.length_lessthan n s with
    | None -> None
    | Some _ ->
      List.fold_left s ~init:(Some 0)
      ~f:begin fun acc item ->
        let sub n1 m = match mod_smallerthan (n - n1) m with
           | Some n2 -> Some (n1 + n2)
           | None -> None
        in
        match acc, item with
        | None, _ -> None
        | Some n', _ when n' > n -> None
        | Some n1, Sig_modtype (_,m) ->
            begin match Raw_compat.extract_modtype_declaration m with
              | Some m -> sub n1 m
              | None -> None
            end
        | Some n1, Sig_module (_,m,_) ->
          sub n1 (Raw_compat.extract_module_declaration m)

        | Some n', _ -> Some (succ n')
      end
    end
  | Mty_functor (_,m1,m2) ->
    begin
      match Raw_compat.extract_functor_arg m1 with
      | None -> None
      | Some m1 ->
      match mod_smallerthan n m1 with
      | None -> None
      | Some n1 ->
      match mod_smallerthan (n - n1) m2 with
      | None -> None
      | Some n2 -> Some (n1 + n2)
    end
  | _ -> Some 1


let type_in_env ?(verbosity=0) ?keywords env ppf expr =
  let print_expr expression =
    let (str, _sg, _) =
      Typemod.type_toplevel_phrase env [Ast_helper.Str.eval expression]
    in
    (*let sg' = Typemod.simplify_signature sg in*)
    let open Typedtree in
    let exp = Raw_compat.dest_tstr_eval str in
    Printtyp.type_scheme env ppf exp.exp_type;
  in
  Printtyp.wrap_printing_env env ~verbosity @@ fun () ->
  Typing_aux.uncatch_errors @@ fun () ->
  match parse_expr ?keywords expr with
  | None ->
    Format.pp_print_string ppf "Syntax error";
    false

  | Some e ->
    match Raw_compat.Parsetree.extract_specific_parsing_info e with
    | `Ident longident ->
      begin try
        (* Don't catch type errors *)
        print_expr e;
        true
      with exn ->
        try let p, t = Env.lookup_type longident.Asttypes.txt env in
          Printtyp.type_declaration env (Ident.create (Path.last p)) ppf t;
          true
        with _ ->
          raise exn
      end

    | `Constr longident ->
      begin try
        print_expr e;
        true
      with exn ->
        try
          (* TODO: special processing for module aliases? *)
          match lookup_module_or_modtype longident.Asttypes.txt env with
          | _path, None ->
            Format.pp_print_string ppf "(* abstract module *)";
            true
          | _path, Some md ->
            begin match mod_smallerthan 1000 md with
            | None when verbosity = 0 ->
              Format.pp_print_string ppf "(* large signature, repeat to confirm *)";
            | _ ->
              Printtyp.modtype env ppf md
            end;
            true
        with _ ->
          try
            let cstr_desc =
              Raw_compat.lookup_constructor longident.Asttypes.txt env
            in
                  (*
                     Format.pp_print_string ppf name;
                     Format.pp_print_string ppf " : ";
                  *)
            Browse_misc.print_constructor ppf cstr_desc;
            true
          with _ ->
            raise exn
      end

    | `Other -> print_expr e; true
