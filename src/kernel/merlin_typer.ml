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

type tree = [
  | `Signature of Typedtree.signature
  | `Structure of Typedtree.structure
]

type ('parsed, 'typed) step = {
  items: 'typed list;
  sign: Types.signature;
  env: Env.t;
  checks: Typecore.delayed_check list;
  errors: exn list;
  snapshot: Btype.snapshot;
  next: ('parsed * ('parsed, 'typed) step lazy_t) option;
}

let flatten_rev l =
  List.fold_left ~f:(fun acc x -> x @ acc) ~init:[] l

let initial_step extensions =
  let env =
    Raw_typer.fresh_env ()
    |> Env.open_pers_signature "Pervasives"
    |> Extension.register extensions
  in
  { items = []; sign = []; env;
    errors = []; checks = [];
    snapshot = Btype.snapshot ();
    next = None
  }

let rec type_steps type_fun env0 = function
  | [] -> None
  | pitem :: pitems ->
    Some (pitem, lazy begin
        let caught = ref [] in
        Front_aux.catch_errors caught @@ fun () ->
        Typecore.delayed_checks := [];
        let titem, sign, env =
          try type_fun env0 pitem
          with exn ->
            caught := exn :: !caught;
            [], [], env0
        in
        {
          items = titem; sign; env;
          errors = !caught;
          checks = !Typecore.delayed_checks;
          snapshot = Btype.snapshot ();
          next = type_steps type_fun env pitems
        }
      end)

let type_signature env sg =
  let sg = Typemod.transl_signature env [sg] in
  (sg.Typedtree.sig_items, sg.Typedtree.sig_type, sg.Typedtree.sig_final_env)

let type_structure env str =
  let str, _, env = Typemod.type_structure env [str] Location.none in
  (str.Typedtree.str_items, str.Typedtree.str_type, env)

type t = {
  reader      : Merlin_reader.t;
  ast         : Merlin_parser.tree;
  steps       : [
    | `Signature of (Parsetree.signature_item, Typedtree.signature_item) step lazy_t
    | `Structure of (Parsetree.structure_item, Typedtree.structure_item) step lazy_t
  ];
  extensions  : String.Set.t;
  btype_state : Btype.state;
  env_state   : Env.state;
  stamp       : int * int ref;
}

let process_ast reader =
  Ast_mapper.state := Ast_mapper.new_state ();
  match Merlin_reader.result reader with
  | `Signature sigs ->
    `Signature (Pparse.apply_rewriters_sig ~tool_name:"merlin" sigs)
  | `Structure strs ->
    `Structure (Pparse.apply_rewriters_str ~tool_name:"merlin" strs)

let common_steps input output =
  let rec aux input output acc =
    match input, output with
    | _, None | [], _ -> (input, acc)
    | (item :: input'), Some (item', _)
      when compare item item' <> 0 -> (input, acc)
    | (item :: input'), Some (_, lazy step)
      when not (Btype.is_valid step.snapshot) -> (input, acc)
    | (item :: input'), Some (_, lazy step) ->
      aux input' step.next ((item, {step with next = None}) :: acc)
  in
  aux input output []

let update_steps extensions typefun items previous =
  let items, head = common_steps items previous.next in
  let step = match head with
    | ((_, step) :: _) -> step
    | [] -> previous
  in
  Btype.backtrack step.snapshot;
  let rec build head tail =
    match head with
    | [] -> tail
    | (item, cell) :: head' ->
      build head' (Some (item, Lazy.from_val {cell with next = tail}))
  in
  {previous with
   next = build head (type_steps typefun step.env items)}

let update_steps extensions ast previous =
  match ast with
  | `Signature items ->
    let f = update_steps extensions type_signature items in
    `Signature (match previous with
        | `Signature previous -> lazy (f (Lazy.force previous))
        | _ -> lazy (f (initial_step extensions))
      )
  | `Structure items ->
    let f = update_steps extensions type_structure items in
    `Structure (match previous with
        | `Structure previous -> lazy (f (Lazy.force previous))
        | _ -> lazy (f (initial_step extensions))
      )

let with_typer t f =
  let open Fluid in
  let' (from_ref Btype.state) t.btype_state @@ fun () ->
  let' (from_ref Env.state)   t.env_state f

let is_valid t =
  with_typer t @@ fun () ->
  Env.check_state_consistency () &&
  fst t.stamp = !(snd t.stamp) &&
  true
  (*FIXME match t.steps with
  | v when not (Lazy.is_val v) -> true
  | lazy List.Lazy.Nil -> true
  | lazy (List.Lazy.Cons (x,_)) -> Btype.is_valid x.snapshot*)

let make reader ~stamp extensions =
  let btype_state = Btype.new_state () in
  let env_state = Env.new_state
      ~unit_name:(Merlin_source.unitname (Merlin_reader.source reader)) in
  let ast = process_ast reader in
  { reader; extensions; btype_state; env_state; stamp = (!stamp, stamp); ast;
    steps = update_steps extensions ast `None }

let update reader t =
  if not (is_valid t) then
    make reader ~stamp:(snd t.stamp) t.extensions
  else if t.reader == reader then
    t
  else if Merlin_reader.compare reader t.reader = 0 then
    {t with reader}
  else
    let steps = update_steps t.extensions (process_ast reader) t.steps in
    {t with reader; steps = steps }

let sig_loc item = item.Parsetree.psig_loc
let str_loc item = item.Parsetree.pstr_loc

let select_items loc_fun offset =
  let rec aux acc = function
    | item :: items
      when (loc_fun item).Location.loc_start.Lexing.pos_cnum <= offset ->
      aux (item :: acc) items
    | rest -> List.rev acc, rest
  in
  aux []

let first_env t = match t.steps with
  | `Signature (lazy step) -> step.env
  | `Structure (lazy step) -> step.env

let structure_item_before pos item =
  Location_aux.compare_pos pos item.Parsetree.pstr_loc >= 0

let signature_item_before pos item =
  Location_aux.compare_pos pos item.Parsetree.psig_loc >= 0

let fold_steps t pred pos steps f acc =
  let pred = match pos with
    | None -> (fun _ -> true)
    | Some pos ->
      let pos =
        Merlin_source.get_lexing_pos (Merlin_reader.source t.reader) pos in
      pred pos
  in
  let rec aux acc = function
    | Some (parse, _) when not (pred parse) -> acc
    | Some (parse, lazy step) ->
      aux (f parse step acc) step.next
    | None ->
      acc
  in
  aux acc (Lazy.force steps).next

(* Public API *)

let processed_ast t = t.ast

let result ?pos t =
  let fold f steps =
    fold_steps t f pos steps
      (fun _parse step (itemsacc, sgacc, _) ->
         (step.items :: itemsacc, step.sign :: sgacc, step.env)
      ) ([], [], first_env t)
  in
  match t.steps with
  | `Signature steps ->
    let items, types, env = fold signature_item_before steps in
    `Signature {
      Typedtree.
      sig_items = flatten_rev items;
      sig_type = flatten_rev types;
      sig_final_env = env;
    }
  | `Structure steps ->
    let items, types, env = fold structure_item_before steps in
    `Structure {
      Typedtree.
      str_items = flatten_rev items;
      str_type = flatten_rev types;
      str_final_env = env;
    }

let errors ?pos t =
  let fold f steps =
    fold_steps t f pos steps (fun _ x acc -> x.errors :: acc) [] in
  let errors = match t.steps with
    | `Signature steps -> fold signature_item_before steps
    | `Structure steps -> fold structure_item_before steps
  in
  flatten_rev errors

let checks ?pos t =
  let fold f steps =
    fold_steps t f pos steps
      (fun _ x (checks, sign, env) ->
         (x.checks :: checks, x.sign :: sign, x.env)
      ) ([], [], first_env t)
  in
  let is_impl, (checks, sign, env) =
    match t.steps with
    | `Signature steps -> false, fold signature_item_before steps
    | `Structure steps -> true, fold structure_item_before steps
  in
  let checks = flatten_rev checks and sign = flatten_rev sign in
  (* Fake coercion to mark globals as used.
     Prevent spurious warnings 32, 34, 37, ...
     FIXME:
     - handle coercion with external interface if one was provided.
     - normalize and check_non_gen ?
  *)
  Typecore.delayed_checks := checks;
  let caught = ref [] in
  begin try
      Front_aux.catch_errors caught (fun () ->
          let modulename =
            Merlin_source.unitname (Merlin_reader.source t.reader) in
          let simple_sign = Typemod.simplify_signature sign in
          (* Rarely useful during development.
                   Typemod.check_nongen_schemes env sign;*)
          Typemod.normalize_signature env simple_sign;
          let _ : Typedtree.module_coercion =
            Includemod.compunit
              env
              modulename
              sign "(inferred signature)"
              simple_sign
          in
          (*if is_impl then ignore (
              let modulename =
                Merlin_source.unitname (Merlin_reader.source t.reader) in
              let sourceintf =
                Misc.chop_extension_if_any
                  (Merlin_source.filename (Merlin_reader.source t.reader))
                ^ ".mli"
              in
              match
                (*if not (Sys.file_exists sourceintf) then raise Not_found;*)
                Env.find_signature modulename
              with
              | target_sign ->
                (* Rarely useful during development.
                   Typemod.check_nongen_schemes env sign;*)
                Includemod.compunit
                  (resume_env_at_steps t [])
                  modulename sign sourceintf target_sign
              | exception Not_found ->
                let simple_sign = Typemod.simplify_signature sign in
                (* Rarely useful during development.
                   Typemod.check_nongen_schemes env sign;*)
                Typemod.normalize_signature env simple_sign;
                Includemod.compunit
                  (resume_env_at_steps t [])
                  modulename
                  sign "(inferred signature)"
                  simple_sign
            ); *)
          Typecore.force_delayed_checks ();
        )
    with exn ->
      caught := exn :: !caught
  end;
  Typecore.delayed_checks := [];
  !caught

let env ?pos t =
  let env = first_env t in
  match t.steps with
  | `Signature steps ->
    fold_steps t signature_item_before pos steps (fun _ x _ -> x.env) env
  | `Structure steps ->
    fold_steps t structure_item_before pos steps (fun _ x _ -> x.env) env

let extensions t = t.extensions

let to_browse = function
  | `Signature s -> Merlin_browse.of_signature s
  | `Structure s -> Merlin_browse.of_structure s

let node_at ?(skip_recovered=false) typer pos_cursor =
  let structures =
    to_browse (result ~pos:(`Logical (Lexing.split_pos pos_cursor)) typer) in
  let rec select = function
    (* If recovery happens, the incorrect node is kept and a recovery node
       is introduced, so the node to check for recovery is the second one. *)
    | List.More ((_,node), (List.More ((_,node'), _) as ancestors))
      when Merlin_browse.is_recovered node' -> select ancestors
    | l -> l
  in
  match Merlin_browse.deepest_before pos_cursor [structures] with
  | Some path when skip_recovered -> select path
  | Some path -> path
  | None -> List.One (env typer, Browse_node.Dummy)

let reader t = t.reader
