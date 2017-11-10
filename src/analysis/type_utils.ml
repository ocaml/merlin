(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
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

let parse_expr ?(keywords=Lexer_raw.keywords []) expr =
  let lexbuf = Lexing.from_string expr in
  let state = Lexer_raw.make keywords in
  let rec lexer = function
    | Lexer_raw.Fail (e,l) -> raise (Lexer_raw.Error (e,l))
    | Lexer_raw.Return token -> token
    | Lexer_raw.Refill k -> lexer (k ())
  in
  let lexer lexbuf = lexer (Lexer_raw.token_without_comments state lexbuf) in
  Parser_raw.parse_expression lexer lexbuf

let lookup_module name env =
  let path = Env.lookup_module ~load:true name env in
  let md = Env.find_module path env in
  path, md.Types.md_type, md.Types.md_attributes

let lookup_modtype name env =
  let path, mdtype = Env.lookup_modtype name env in
  path, mdtype.Types.mtd_type

let lookup_module_or_modtype name env =
  try
    let path, mty, _ = lookup_module name env in
    path, Some mty
  with Not_found ->
    lookup_modtype name env

let verbosity = ref 0

module Printtyp = struct
  include Printtyp

  let expand_type env ty =
    Env.with_cmis @@ fun () -> (* ?? Not sure *)
    if !verbosity = 0 then ty
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
          let open Types in
          let ty'' = Ctype.full_expand env ty' in
          if ty''.desc == ty'.desc then
            Btype.iter_type_expr (iter d) ty0
          else begin
            let desc = match ty''.desc with
              | Tvariant row ->
                Tvariant {row with row_name = None}
              | Tobject (ty, name) ->
                Tobject (ty, ref None)
              | desc -> desc
            in
            ty0.desc <- desc;
            if d > 0 then
              Btype.iter_type_expr (iter (pred d)) ty0
          end
      in
      iter !verbosity ty;
      ty

  let expand_type_decl env ty =
    match ty.Types.type_manifest with
    | Some m -> {ty with Types.type_manifest = Some (expand_type env m)}
    | None -> ty

  let expand_sig env mty =
    Env.with_cmis @@ fun () ->
    Env.scrape_alias env mty

  let verbose_type_scheme env ppf t =
    Printtyp.type_scheme ppf (expand_type env t)

  let verbose_type_declaration env id ppf t =
    Printtyp.type_declaration id ppf (expand_type_decl env t)

  let verbose_modtype env ppf t =
    Printtyp.modtype ppf (expand_sig env t)

  let select_verbose a b env =
    (if !verbosity = 0 then a else b env)

  let type_scheme env ppf ty =
    select_verbose type_scheme verbose_type_scheme env ppf ty

  let type_declaration env id ppf =
    select_verbose type_declaration verbose_type_declaration env id ppf

  let modtype env ppf mty =
    select_verbose modtype verbose_modtype env ppf mty

  let wrap_printing_env env ~verbosity:v f =
    let_ref verbosity v (fun () -> wrap_printing_env env f)
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
  | Mty_signature s ->
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
            begin match m.Types.mtd_type with
              | Some m -> sub n1 m
              | None -> None
            end
        | Some n1, Sig_module (_,m,_) ->
          sub n1 m.Types.md_type

        | Some n', _ -> Some (succ n')
      end
    end
  | Mty_functor (_,m1,m2) ->
    begin
      match m1 with
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

let print_type_with_decl ~verbosity env ppf typ =
  if verbosity > 0 then
    match (Ctype.repr typ).Types.desc with
    | Types.Tconstr (path, params, _) ->
      let decl = Env.find_type path env in
      let is_abstract =
        match decl.Types.type_kind with
        | Types.Type_abstract -> true
        | _ -> false
      in
      (* Print expression only if it is parameterized or abstract *)
      let print_expr = is_abstract || params <> [] in
      if print_expr then
        Printtyp.type_scheme env ppf typ;
      (* If not abstract, also print the declaration *)
      if not is_abstract then
        begin
          (* Separator if expression was printed *)
          if print_expr then
            begin
              Format.pp_print_newline ppf ();
              Format.pp_print_newline ppf ();
            end;
          let ident = match path with
            | Path.Papply _ -> assert false
            | Path.Pdot (_,name,_) -> Ident.create_persistent name
            | Path.Pident ident -> ident
          in
          Printtyp.type_declaration env ident ppf decl
        end
    | _ -> Printtyp.type_scheme env ppf typ
  else
    Printtyp.type_scheme env ppf typ

let print_exn ppf exn =
  let msg = match Location.error_of_exn exn with
    | Some (`Ok {Location. msg}) -> msg
    | None | Some `Already_displayed -> Printexc.to_string exn
  in
  Format.pp_print_string ppf msg

let type_in_env ?(verbosity=0) ?keywords env ppf expr =
  let print_expr expression =
    let (str, _sg, _) =
      Env.with_cmis @@ fun () ->
      Typemod.type_toplevel_phrase env
        [Ast_helper.Str.eval expression]
    in
    let open Typedtree in
    let exp = Raw_compat.dest_tstr_eval str in
    print_type_with_decl ~verbosity env ppf exp.exp_type
  in
  Printtyp.wrap_printing_env env ~verbosity @@ fun () ->
  Msupport.uncatch_errors @@ fun () ->
  match parse_expr ?keywords expr with
  | exception exn -> print_exn ppf exn; false

  | e ->
    let extract_specific_parsing_info e =
      match e.Parsetree.pexp_desc with
      | Parsetree.Pexp_ident longident -> `Ident longident
      | Parsetree.Pexp_construct (longident, _) -> `Constr longident
      | _ -> `Other
    in
    match extract_specific_parsing_info e with
    | `Ident longident ->
      begin try
        (* Don't catch type errors *)
        print_expr e;
        true
      with exn ->
        try
          let p = Env.lookup_type longident.Asttypes.txt env in
          let t = Env.find_type p env in
          Printtyp.type_declaration env (Ident.create (Path.last p)) ppf t;
          true
        with _ -> print_exn ppf exn; false
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
              Env.lookup_constructor longident.Asttypes.txt env
            in
                  (*
                     Format.pp_print_string ppf name;
                     Format.pp_print_string ppf " : ";
                  *)
            (* FIXME: support Reader printer *)
            !Oprint.out_type ppf (Browse_misc.print_constructor cstr_desc);
            true
          with _ -> print_exn ppf exn; false
      end

    | `Other ->
      try print_expr e; true
      with exn -> print_exn ppf exn; false

(* From doc-ock
   https://github.com/lpw25/doc-ock/blob/master/src/docOckAttrs.ml *)
let read_doc_attributes attrs =
  let read_payload =
    let open Parsetree in function
      | PStr[{ pstr_desc = Pstr_eval(
          ({Parsetree. pexp_desc =
              Parsetree.Pexp_constant (Parsetree.Pconst_string (str, _)) } as expr), _)
        }] ->
        Some(str, expr.pexp_loc)
      | _ -> None
  in
  let rec loop = function
    | ({Location.txt =
          ("doc" | "ocaml.doc"); loc}, payload) :: rest ->
      read_payload payload
    | _ :: rest -> loop rest
    | [] -> None
  in
  loop attrs
