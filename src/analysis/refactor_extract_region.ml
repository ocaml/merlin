open Std

module FreshName = struct
  (* Generate a fresh name that does not already exists in given environment. *)
  let gen_val_name basename env =
    let rec loop n =
      let guess = basename ^ Int.to_string n in
      if Env.bound_value guess env then succ n |> loop else guess
    in
    loop 1
end

module Gen = struct
  let unit = Longident.Lident "()" |> Location.mknoloc

  (* Generate [let name = body]. *)
  let toplevel_let name body =
    let open Ast_helper in
    let pattern = Pat.mk (Ppat_var { txt = name; loc = Location.none }) in
    let body = Parsetree_utils.filter_expr_attr body in
    Str.value Nonrecursive [ Vb.mk pattern body ]

  (* Generate [let name = const]. *)
  let let_const_toplevel name const =
    Ast_helper.Exp.constant const |> toplevel_let name

  (* Generate [let name () = body]. *)
  let let_unit_toplevel name body =
    let open Ast_helper in
    let unit_param =
      { Parsetree.pparam_loc = Location.none;
        pparam_desc = Pparam_val (Nolabel, None, Pat.construct unit None)
      }
    in
    let body = Exp.function_ [ unit_param ] None (Pfunction_body body) in
    toplevel_let name body

  (* Generate [let name params = body]. *)
  let toplevel_function params name body =
    let open Ast_helper in
    let params =
      List.map
        ~f:(fun param ->
          let pattern =
            Pat.construct
              (Location.mknoloc (Untypeast.lident_of_path param))
              None
          in
          { Parsetree.pparam_loc = Location.none;
            pparam_desc = Pparam_val (Nolabel, None, pattern)
          })
        params
    in
    let body = Exp.function_ params None (Pfunction_body body) in
    toplevel_let name body

  let ident name =
    Longident.Lident name |> Location.mknoloc |> Ast_helper.Exp.ident

  let fun_apply params called_fun_name =
    let open Ast_helper in
    let params = List.map ~f:(fun p -> (Asttypes.Nolabel, p)) params in
    Exp.apply (ident called_fun_name) params

  let fun_apply_unit = fun_apply [ Ast_helper.Exp.ident unit ]

  let fun_apply_params params =
    params
    |> List.map ~f:(fun param ->
           Ast_helper.Exp.ident
             (Location.mknoloc (Longident.Lident (Path.name param))))
    |> fun_apply
end

(* Find all variable which appears in [node]. *)
let rec find_vars node =
  let concat_set f children =
    List.fold_left
      ~f:(fun acc child -> f child |> Path.Set.union acc)
      ~init:Path.Set.empty children
  in
  let rec find_pattern_var : type a. a Typedtree.general_pattern -> Path.Set.t =
   fun { Typedtree.pat_desc; pat_extra; _ } ->
    let _constr =
      Stdlib.List.find_map
        (fun (pat_extra, _, _) ->
          match pat_extra with
          | Typedtree.Tpat_constraint typ ->
            let () =
              Format.asprintf "%a!" Printtyp.type_expr typ.Typedtree.ctyp_type
              |> prerr_endline
            in
            Some typ
          | _ -> None)
        pat_extra
    in
    match pat_desc with
    | Typedtree.Tpat_var (ident, _, _) -> Path.Set.singleton (Pident ident)
    | Tpat_tuple pats -> concat_set find_pattern_var pats
    | Tpat_alias (pat, ident, _, _) ->
      let open Path.Set in
      union (singleton (Pident ident)) (find_pattern_var pat)
    | Tpat_construct (_, _, pats, _) -> concat_set find_pattern_var pats
    | Tpat_variant (_, Some pat, _) -> find_pattern_var pat
    | Tpat_record (fields, _) ->
      concat_set (fun (_, _, field) -> find_pattern_var field) fields
    | Tpat_array arr -> concat_set find_pattern_var arr
    | Tpat_lazy pat | Tpat_exception pat -> find_pattern_var pat
    | Tpat_value pat ->
      find_pattern_var (pat :> Typedtree.value Typedtree.general_pattern)
    | Tpat_or (l, r, _) ->
      Path.Set.union (find_pattern_var l) (find_pattern_var r)
    | _ -> Path.Set.empty
  in
  let loop acc node =
    match node.Browse_tree.t_node with
    | Browse_raw.Expression { exp_desc = Texp_ident (path, _, vd); _ } ->
      let () =
        Format.asprintf "%a" Printtyp.type_expr vd.val_type |> prerr_endline
      in

      Path.Set.add path acc
    | Pattern pat -> find_pattern_var pat |> Path.Set.union acc
    | _ ->
      let child = Lazy.force node.t_children in
      concat_set find_vars child |> Path.Set.union acc
  in
  loop Path.Set.empty node

let bounded_vars node env ~toplevel_parent_item ~mconfig ~local_defs =
  let enclosing =
    { Location.loc_start = toplevel_parent_item.Typedtree.str_loc.loc_start;
      loc_end = (Browse_raw.node_real_loc Location.none node).loc_start;
      loc_ghost = false
    }
  in
  Browse_tree.of_node ~env node
  |> find_vars
  |> Path.Set.filter (fun var_path ->
         match
           Locate.from_path
             ~config:{ mconfig; ml_or_mli = `ML; traverse_aliases = true }
             ~env ~local_defs ~namespace:Value var_path
         with
         | `Found { location; approximated = false; _ } ->
           (* let () =
             Format.asprintf "%S in args:%b" (Path.name var_path)
               (Location_aux.included location ~into:enclosing)
             |> prerr_endline
           in *)
           Location_aux.included location ~into:enclosing
         | _ -> false)
  |> Path.Set.to_list

(* TODO: Maybe add this in [Msource]? *)
let buffer_sub_loc buf loc =
  let (`Offset start_offset) =
    let line, col = Lexing.split_pos loc.Location.loc_start in
    Msource.get_offset buf (`Logical (line, col))
  in
  let (`Offset end_offset) =
    `Logical (Lexing.split_pos loc.loc_end) |> Msource.get_offset buf
  in
  String.sub (Msource.text buf) ~pos:start_offset
    ~len:(end_offset - start_offset)
  |> Msource.make

let extract_to_toplevel name expr gen_let_binding gen_call buffer ~expr_env
    ~exp_loc ~toplevel_item_loc =
  let val_name =
    match name with
    | `Default name -> FreshName.gen_val_name name expr_env
    | `Given name -> name
  in
  let fresh_call =
    gen_call val_name |> Format.asprintf "%a" Pprintast.expression
  in
  let fresh_let_binding =
    gen_let_binding val_name expr
    |> Format.asprintf "%a" Pprintast.structure_item
  in
  let toplevel_item = buffer_sub_loc buffer toplevel_item_loc in
  let subst_loc =
    let start_lnum =
      1 + exp_loc.Location.loc_start.pos_lnum
      - toplevel_item_loc.loc_start.pos_lnum
    in
    let end_lnum =
      start_lnum + exp_loc.loc_end.pos_lnum - exp_loc.loc_start.pos_lnum
    in
    { exp_loc with
      loc_start = { exp_loc.loc_start with pos_lnum = start_lnum };
      loc_end = { exp_loc.loc_end with pos_lnum = end_lnum }
    }
  in
  let substitued_toplevel_item =
    Msource.substitute toplevel_item
      (`Logical (Lexing.split_pos subst_loc.loc_start))
      (`Logical (Lexing.split_pos subst_loc.loc_end))
      fresh_call
    |> Msource.text
  in
  let selection_range =
    let let_length = String.length "let " in
    { Location.loc_start =
        Lexing.make_pos (toplevel_item_loc.loc_start.pos_lnum, let_length);
      loc_end =
        Lexing.make_pos
          ( toplevel_item_loc.loc_start.pos_lnum,
            let_length + String.length fresh_call );
      loc_ghost = false
    }
  in
  let content = fresh_let_binding ^ "\n" ^ substitued_toplevel_item in
  { Query_protocol.loc = toplevel_item_loc; content; selection_range }

let extract_const_to_toplevel ?extract_name const =
  let name =
    Option.fold extract_name ~none:(`Default "const_name") ~some:(fun name ->
        `Given name)
  in
  extract_to_toplevel name (Untypeast.constant const) Gen.let_const_toplevel
    Gen.ident

let extract_expr_to_toplevel ?extract_name node expr ~expr_env
    ~toplevel_parent_item ~local_defs ~mconfig =
  let is_function = function
    | { Typedtree.exp_desc = Texp_function _; _ } -> true
    | _ -> false
  in
  let gen_let, gen_call =
    if is_function expr then (Gen.toplevel_let, Gen.ident)
    else
      match
        bounded_vars node expr_env ~toplevel_parent_item ~local_defs ~mconfig
      with
      | [] -> (Gen.let_unit_toplevel, Gen.fun_apply_unit)
      | free_vars ->
        (Gen.toplevel_function free_vars, Gen.fun_apply_params free_vars)
  in
  let name =
    Option.fold extract_name ~none:(`Default "fun_name") ~some:(fun name ->
        `Given name)
  in
  extract_to_toplevel name
    (Untypeast.untype_expression expr)
    gen_let gen_call ~expr_env

(* We select the most inclusive expression contained entirely within the given region. *)
let select_suitable_expr ~start ~stop nodes =
  let region =
    { Location.loc_start = start; loc_end = stop; loc_ghost = true }
  in
  let rec select_among_child env node =
    let node_loc = Mbrowse.node_loc node in
    (* let is = Location_aux.included node_loc ~into:region in
    Format.asprintf "Candidate: %S, contains: %b, loc: %a"
      (Browse_raw.string_of_node node)
      is Location.print_loc node_loc
    |> prerr_endline; *)
    match node with
    | Expression expr ->
      if Location_aux.included node_loc ~into:region then Some (expr, env)
      else
        let node = Browse_tree.of_node ~env node in
        Lazy.force node.t_children |> List.rev
        |> Stdlib.List.find_map (fun node ->
               select_among_child node.Browse_tree.t_env node.t_node)
    | _ -> None
  in
  nodes |> List.rev
  |> Stdlib.List.find_map (fun (env, node) -> select_among_child env node)

let substitute ~start ~stop ?extract_name mconfig buffer typedtree =
  match typedtree with
  | `Interface _ -> None
  | `Implementation structure -> (
    let enclosing =
      Mbrowse.enclosing start [ Mbrowse.of_structure structure ]
    in
    match select_suitable_expr ~start ~stop enclosing with
    | None -> failwith "nothing to do"
    | Some (expr, expr_env) -> begin
      let toplevel_parent_item =
        List.find
          ~f:(fun item ->
            Location_aux.included expr.exp_loc ~into:item.Typedtree.str_loc)
          structure.str_items
      in
      let exp_loc = expr.exp_loc in
      let toplevel_item_loc = toplevel_parent_item.str_loc in
      match expr.exp_desc with
      | Texp_constant const ->
        (* Special case for constant. They can't produce side effect so it's not
         necessary to add a trailing unit parameter to the let binding. *)
        Some
          (extract_const_to_toplevel const buffer ?extract_name ~expr_env
             ~exp_loc ~toplevel_item_loc)
      | _ ->
        Some
          (extract_expr_to_toplevel (Browse_raw.Expression expr) expr buffer
             ?extract_name ~expr_env ~exp_loc ~toplevel_item_loc
             ~toplevel_parent_item ~local_defs:typedtree ~mconfig)
    end)

(* Ajouter test récursion mutuelle *)
(* + de tests *)

(* préserver type contraine de ce qui est extrait *)
