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
open Raw_parser

exception Unknown

type t = {
  name : string;
  private_def : string list;
  public_def : string list;
  packages : string list;
  keywords : (string * Raw_parser.token) list;
}

type set = String.Set.t

(* Private definitions are put in a fake module named "_" with the following
 * ident. Use it to test or find private definitions. *)
let ident = Ident.create "_"

(** Definition of each extension *)
let ext_lwt = {
  name = "lwt";
  private_def = [
    "module Lwt : sig
      val un_lwt : 'a Lwt.t -> 'a
      val in_lwt : 'a Lwt.t -> 'a Lwt.t
      val to_lwt : 'a -> 'a Lwt.t
      val finally' : 'a Lwt.t -> unit Lwt.t -> 'a Lwt.t
      val un_stream : 'a Lwt_stream.t -> 'a
      val unit_lwt : unit Lwt.t -> unit Lwt.t
    end"
  ];
  public_def = [
    "val (>>) : unit Lwt.t -> 'a Lwt.t -> 'a Lwt.t
     val raise_lwt : exn -> 'a Lwt.t
     val assert_lwt : bool -> unit Lwt.t"
  ];
  keywords = [
    "lwt", LET_LWT;
    "try_lwt", TRY_LWT;
    "match_lwt", MATCH_LWT;
    "finally", FINALLY_LWT;
    "for_lwt", FOR_LWT;
    "while_lwt", WHILE_LWT;
  ];
  packages = ["lwt.syntax"];
}

let ext_any = {
  name = "any";
  private_def = [
    "module Any : sig
      val val' : 'a
    end"
  ];
  public_def = [];
  keywords = [];
  packages = [];
}

let ext_js = {
  name = "js";
  private_def = [
    "module Js : sig
      val un_js : 'a Js.t -> 'a
      val un_meth : 'a Js.meth -> 'a
      val un_constr : 'a Js.constr -> 'a
      val un_prop : 'a Js.gen_prop -> 'a
    end"
  ];
  public_def = [];
  keywords = ["jsnew", JSNEW; "##", SHARPSHARP];
  packages = ["js_of_ocaml.syntax"];
}

let ext_ounit = {
  name = "ounit";
  private_def = [
    "module OUnit : sig
      val force_bool : bool -> unit
      val force_unit : unit -> unit
      val force_unit_arrow_unit : (unit -> unit) -> unit
      val force_indexed : (int -> unit -> unit) -> int list -> unit
    end"
  ];
  public_def = [];
  keywords = [
    "TEST", OUNIT_TEST;
    "TEST_UNIT", OUNIT_TEST_UNIT;
    "TEST_MODULE", OUNIT_TEST_MODULE;
    "BENCH", OUNIT_BENCH;
    "BENCH_FUN", OUNIT_BENCH_FUN;
    "BENCH_INDEXED", OUNIT_BENCH_INDEXED;
    "BENCH_MODULE", OUNIT_BENCH_MODULE;
  ];
  packages = ["oUnit";"pa_ounit.syntax"];
}

let ext_nonrec = {
  name = "nonrec";
  private_def = [];
  public_def = [];
  keywords = [
    "nonrec", NONREC;
  ];
  packages = [];
}

let ext_here = {
  name = "here";
  private_def = [];
  public_def = ["val _here_ : Lexing.position"];
  keywords = [];
  packages = [];
}

let ext_sexp_option = {
  name = "sexp_option";
  private_def = [];
  public_def = ["type 'a sexp_option = 'a option"];
  keywords = [];
  packages = [];
}

let custom_printf = "custom_printf"
let ext_custom_printf = {
  name = custom_printf;
  private_def = [];
  public_def = [];
  keywords = [
    "!", CUSTOM_BANG;
  ];
  packages = ["custom_printf"];
}

let ext_meta = {
  name = "meta";
  private_def = [
    "module Meta : sig
      val code : 'a -> 'a code
      val uncode : 'a code -> 'a
    end"
  ];
  public_def = [];
  keywords = [
    ">.", GREATERDOT;
  ];
  packages = [];
}

(* Known extensions *)
let registry = [ext_here;ext_lwt;ext_js;ext_ounit;ext_nonrec;ext_custom_printf;ext_meta]
let registry =
  List.fold_left registry ~init:String.Map.empty
    ~f:(fun map ext -> String.Map.add map ~key:ext.name ~data:ext)
let all = String.Set.of_list (String.Map.keys registry)

let lookup s =
  try Some (String.Map.find s registry)
  with Not_found -> None

let empty = String.Set.empty

(* Compute set of extensions from package names (used to enable support for
  "lwt" if "lwt.syntax" is loaded by user. *)
let from_packages pkgs =
  String.Map.fold registry ~init:empty ~f:(fun ~key:name ~data:ext set ->
    if List.exists ~f:(List.mem ~set:ext.packages) pkgs
    then String.Set.add name set
    else set
  )

(* Merlin expects a few extensions to be always enabled, otherwise error
   recovery may fail arbitrarily *)
let default = [ext_any;ext_sexp_option] @
              match My_config.ocamlversion with
              | `OCaml_4_02_2 | `OCaml_4_03_trunk -> [ext_nonrec]
              | _ -> []

let default_kw = List.concat_map ~f:(fun ext -> ext.keywords) default

(* Lexer keywords needed by extensions *)
let keywords set =
  let add_kw ext kws =
    match lookup ext with
    | None -> kws
    | Some def -> def.keywords @ kws
  in
  let all = String.Set.fold set ~init:default_kw ~f:add_kw in
  Raw_lexer.keywords all

(* Register extensions in typing environment *)
let parse_sig =
  let keywords = Raw_lexer.keywords [] in fun str ->
  let buf = Lexing.from_string str in
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
    | `Feed p -> lex p (Raw_lexer.token_without_comments state buf)
    | `Accept (Raw_parser.N_ (Raw_parser.N_interface, sg)) -> (sg : Parsetree.signature)
    | `Reject _ | `Accept _ -> assert false
  in
  parse (`Step (Raw_parser.initial Raw_parser.interface_state
                  (Lexing.dummy_pos,Raw_parser.ENTRYPOINT,Lexing.dummy_pos)))

let type_sig env sg =
  let sg = Typemod.transl_signature env sg in
  sg.Typedtree.sig_type

let register exts env =
  (* Log errors ? *)
  let try_type sg' = try type_sig env sg' with _exn -> [] in
  let exts =
    String.Map.fold registry ~init:default ~f:(fun ~key:name ~data:ext list ->
      if String.Set.mem name exts
      then ext :: list
      else list
    )
  in
  let process_ext e =
    let prv = List.concat_map ~f:parse_sig e.private_def in
    let pub = List.concat_map ~f:parse_sig e.public_def in
    try_type prv, try_type pub
  in
  let fakes, tops = List.split (List.map ~f:process_ext exts) in
  let env = Raw_compat.add_hidden_signature env (List.concat tops) in
  let env = Env.add_module ident (Types.Mty_signature
                                    (Lazy.from_val (List.concat fakes))) env in
  env
