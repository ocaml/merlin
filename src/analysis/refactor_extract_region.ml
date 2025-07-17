open Std

exception Nothing_to_do
exception Not_allowed_in_interface_file

let () =
  Location.register_error_of_exn (function
    | Nothing_to_do -> Some (Location.error "Nothing to do")
    | Not_allowed_in_interface_file ->
      Some
        (Location.error
           "Expression extraction is only allowed in implementation file")
    | _ -> None)

module Fresh_name = struct
  (* Generate a fresh name that does not already exist in given environment. *)
  let gen_val_name basename env =
    let rec loop n =
      let guess = basename ^ Int.to_string n in
      if Env.bound_value guess env then succ n |> loop else guess
    in
    loop 1
end

module Gen = struct
  let unit = Longident.Lident "()" |> Location.mknoloc

  (* Generates [let name = body]. *)
  let toplevel_let ~name ~body =
    let open Ast_helper in
    let pattern = Pat.mk (Ppat_var { txt = name; loc = Location.none }) in
    let body = Parsetree_utils.expr_remove_merlin_attributes body in
    Str.value Nonrecursive [ Vb.mk pattern body ]

  (* Generates [let name () = body]. *)
  let let_unit_toplevel ~name ~body =
    let open Ast_helper in
    let unit_param =
      { Parsetree.pparam_loc = Location.none;
        pparam_desc = Pparam_val (Nolabel, None, Pat.construct unit None)
      }
    in
    let body = Exp.function_ [ unit_param ] None (Pfunction_body body) in
    toplevel_let ~name ~body

  (* Generates [let name params = body]. *)
  let toplevel_function params ~name ~body =
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
    toplevel_let ~name ~body

  let ident ~name =
    Longident.Lident name |> Location.mknoloc |> Ast_helper.Exp.ident

  let fun_apply params ~name =
    let open Ast_helper in
    let params = List.map ~f:(fun p -> (Asttypes.Nolabel, p)) params in
    Exp.apply (ident ~name) params

  let fun_apply_unit = fun_apply [ Ast_helper.Exp.ident unit ]

  let fun_apply_params params =
    params
    |> List.map ~f:(fun param ->
           Ast_helper.Exp.ident
             (Location.mknoloc (Longident.Lident (Path.name param))))
    |> fun_apply
end

let source_sub_loc src loc =
  let (`Offset start_offset) =
    let line, col = Lexing.split_pos loc.Location.loc_start in
    Msource.get_offset src (`Logical (line, col))
  in
  let (`Offset end_offset) =
    `Logical (Lexing.split_pos loc.loc_end) |> Msource.get_offset src
  in
  String.sub (Msource.text src) ~pos:start_offset
    ~len:(end_offset - start_offset)
  |> Msource.make

type analysis = { bounded_vars : Path.t list; gen_binding_kind : rec_flag }

and rec_flag = Non_recursive | Rec_and

type extraction =
  { expr : Typedtree.expression;  (** Expression that being extracted *)
    expr_env : Env.t;  (** Environment of the extracted expression *)
    toplevel_item : toplevel_item;
        (** The value binding toplevel or class declaration enclosing the extracted expression. *)
    name : extraction_name;  (** Binding name of the extracted expression. *)
    gen_binding_kind : rec_flag;
    generated_binding : generated_binding;
    generated_call : generated_call
  }

and extraction_name = Default of { basename : string } | Fixed of string

and toplevel_item = { rec_flag : Asttypes.rec_flag; loc : Location.t }
(* A convenient type for grouping info. *)

and generated_binding =
  name:string -> body:Parsetree.expression -> Parsetree.structure_item

and generated_call = name:string -> Parsetree.expression

let is_recursive = function
  | { rec_flag = Asttypes.Recursive; _ } -> true
  | { rec_flag = Nonrecursive; _ } -> false

let rec occuring_vars node =
  let rec find_pattern_var : type a. a Typedtree.general_pattern -> Path.t list
      =
   fun { Typedtree.pat_desc; _ } ->
    match pat_desc with
    | Typedtree.Tpat_var (ident, _, _) -> [ Pident ident ]
    | Tpat_tuple pats -> List.concat_map ~f:find_pattern_var pats
    | Tpat_alias (pat, ident, _, _) -> Pident ident :: find_pattern_var pat
    | Tpat_construct (_, _, pats, _) -> List.concat_map ~f:find_pattern_var pats
    | Tpat_variant (_, Some pat, _) -> find_pattern_var pat
    | Tpat_record (fields, _) ->
      List.concat_map ~f:(fun (_, _, field) -> find_pattern_var field) fields
    | Tpat_array arr -> List.concat_map ~f:find_pattern_var arr
    | Tpat_lazy pat | Tpat_exception pat -> find_pattern_var pat
    | Tpat_value pat ->
      find_pattern_var (pat :> Typedtree.value Typedtree.general_pattern)
    | Tpat_or (l, r, _) -> find_pattern_var l @ find_pattern_var r
    | _ -> []
  in
  let loop acc node =
    match node.Browse_tree.t_node with
    | Browse_raw.Expression { exp_desc = Texp_ident (path, _, _); _ } ->
      path :: acc
    | Pattern pat -> find_pattern_var pat @ acc
    | _ ->
      Lazy.force node.t_children
      |> List.concat_map ~f:occuring_vars
      |> List.append acc
  in
  loop [] node |> List.rev

let analyze_expr expr env ~toplevel_item ~mconfig ~local_defs =
  let unbounded_enclosing =
    { Location.loc_start = toplevel_item.loc.loc_start;
      loc_end = expr.Typedtree.exp_loc.loc_start;
      loc_ghost = false
    }
  in
  Browse_tree.of_node ~env (Browse_raw.Expression expr)
  |> occuring_vars
  |> List.fold_left
       ~init:{ bounded_vars = []; gen_binding_kind = Non_recursive }
       ~f:(fun acc var_path ->
         match
           Locate.from_path
             ~config:{ mconfig; ml_or_mli = `ML; traverse_aliases = true }
             ~env ~local_defs ~namespace:Value var_path
         with
         | `Found { location; approximated = false; _ } ->
           let acc =
             if Location_aux.included location ~into:unbounded_enclosing then
               { acc with bounded_vars = var_path :: acc.bounded_vars }
             else acc
           in
           if
             is_recursive toplevel_item
             && Location_aux.included location ~into:toplevel_item.loc
           then { acc with gen_binding_kind = Rec_and }
           else acc
         | _ -> acc)

let extract_to_toplevel
    { expr;
      expr_env;
      name;
      gen_binding_kind;
      generated_binding;
      generated_call;
      toplevel_item
    } buffer =
  let val_name =
    match name with
    | Default { basename } -> Fresh_name.gen_val_name basename expr_env
    | Fixed name -> name
  in
  let fresh_call =
    generated_call ~name:val_name |> Format.asprintf "%a" Pprintast.expression
  in
  let toplevel_item_span = source_sub_loc buffer toplevel_item.loc in
  let subst_loc =
    let start_lnum =
      1 + expr.exp_loc.Location.loc_start.pos_lnum
      - toplevel_item.loc.loc_start.pos_lnum
    in
    let end_lnum =
      start_lnum + expr.exp_loc.loc_end.pos_lnum
      - expr.exp_loc.loc_start.pos_lnum
    in
    { expr.exp_loc with
      loc_start = { expr.exp_loc.loc_start with pos_lnum = start_lnum };
      loc_end = { expr.exp_loc.loc_end with pos_lnum = end_lnum }
    }
  in
  let substitued_toplevel_binding =
    Msource.substitute toplevel_item_span
      (`Logical (Lexing.split_pos subst_loc.loc_start))
      (`Logical (Lexing.split_pos subst_loc.loc_end))
      fresh_call
    |> Msource.text
  in
  let untyped_expr = Untypeast.untype_expression expr in
  let content =
    match gen_binding_kind with
    | Non_recursive ->
      let fresh_let_binding =
        generated_binding ~name:val_name ~body:untyped_expr
        |> Format.asprintf "%a" Pprintast.structure_item
      in
      fresh_let_binding ^ "\n" ^ substitued_toplevel_binding
    | Rec_and ->
      let fresh_let_binding =
        generated_binding ~name:val_name ~body:untyped_expr
        |> Format.asprintf "%a" Pprintast.structure_item
      in
      let fresh_and_binding =
        "and" ^ String.drop 3 fresh_let_binding (* Sorry *)
      in
      substitued_toplevel_binding ^ "\n" ^ fresh_and_binding
  in
  let selection_range =
    let lnum =
      match gen_binding_kind with
      | Non_recursive -> toplevel_item.loc.loc_start.pos_lnum
      | Rec_and -> toplevel_item.loc.loc_end.pos_lnum + String.length "\n"
    in
    let prefix_length =
      match gen_binding_kind with
      | Non_recursive ->
        if is_recursive toplevel_item then String.length "let rec "
        else String.length "let "
      | Rec_and -> String.length "and "
    in
    { Location.loc_start = Lexing.make_pos (lnum, prefix_length);
      loc_end = Lexing.make_pos (lnum, prefix_length + String.length val_name);
      loc_ghost = false
    }
  in
  { Query_protocol.loc = toplevel_item.loc; content; selection_range }

let extract_const_to_toplevel ?extract_name expr ~expr_env ~toplevel_item =
  let name =
    match extract_name with
    | None -> Default { basename = "const_name" }
    | Some name -> Fixed name
  in
  extract_to_toplevel
    { expr;
      expr_env;
      toplevel_item;
      name;
      gen_binding_kind = Non_recursive;
      generated_binding = Gen.toplevel_let;
      generated_call = Gen.ident
    }

let extract_expr_to_toplevel ?extract_name expr ~expr_env ~toplevel_item
    ~local_defs ~mconfig =
  let is_function = function
    | { Typedtree.exp_desc = Texp_function _; _ } -> true
    | _ -> false
  in
  let { bounded_vars; gen_binding_kind } =
    analyze_expr expr expr_env ~toplevel_item ~local_defs ~mconfig
  in
  let generated_binding, generated_call =
    match bounded_vars with
    | [] when not (is_function expr) ->
      (* If the extracted expr is already a function, no need to delayed computation
         with a unit parameter. *)
      (Gen.let_unit_toplevel, Gen.fun_apply_unit)
    | _ ->
      (Gen.toplevel_function bounded_vars, Gen.fun_apply_params bounded_vars)
  in
  let name =
    match extract_name with
    | None -> Default { basename = "fun_name" }
    | Some name -> Fixed name
  in
  extract_to_toplevel
    { expr;
      expr_env;
      toplevel_item;
      name;
      gen_binding_kind;
      generated_binding;
      generated_call
    }

let most_inclusive_expr ~start ~stop nodes =
  let is_inside_region =
    Location_aux.included
      ~into:{ Location.loc_start = start; loc_end = stop; loc_ghost = true }
  in
  let rec select_among_child env node =
    let select_deeper node env =
      let node = Browse_tree.of_node ~env node in
      Lazy.force node.t_children |> List.rev
      |> Stdlib.List.find_map (fun node ->
             select_among_child node.Browse_tree.t_env node.t_node)
    in
    let node_loc = Mbrowse.node_loc node in
    let remove_poly expr =
      (* We have to remove poly extra that cause unexpected "!poly!" to be printed
         in generated code. This happens when you try to extract the body of a method. *)
      let open Typedtree in
      { expr with
        exp_extra =
          List.filter
            ~f:(function
              | Texp_poly _, _, _ -> false
              | _ -> true)
            expr.exp_extra
      }
    in
    match node with
    | Expression expr ->
      (* We filter expression that have a ghost location. Otherwise, expression
        such as [let f x = 10 + x] can be extracted and this can lead to invalid 
        code gen.      ^^^^^^^^^^ *)
      if node_loc.loc_ghost = false && is_inside_region node_loc then
        Some (remove_poly expr, env)
      else select_deeper node env
    | _ -> select_deeper node env
  in
  nodes |> List.rev
  |> Stdlib.List.find_map (fun (env, node) -> select_among_child env node)

let find_associated_toplevel_item expr structure =
  Stdlib.List.find_map
    (fun { Typedtree.str_desc; str_loc; _ } ->
      match str_desc with
      | Tstr_value (rec_flag, _)
        when Location_aux.included expr.Typedtree.exp_loc ~into:str_loc ->
        Some { rec_flag; loc = str_loc }
      | Tstr_class cs ->
        Stdlib.List.find_map
          (fun (class_decl, _) ->
            let loc = class_decl.Typedtree.ci_loc in
            if Location_aux.included expr.exp_loc ~into:loc then
              Some { rec_flag = Nonrecursive; loc }
            else None)
          cs
      | _ -> None)
    structure.Typedtree.str_items

let substitute ~start ~stop ?extract_name mconfig buffer typedtree =
  match typedtree with
  | `Interface _ -> raise Not_allowed_in_interface_file
  | `Implementation structure -> (
    let enclosing =
      Mbrowse.enclosing start [ Mbrowse.of_structure structure ]
    in
    match most_inclusive_expr ~start ~stop enclosing with
    | None -> raise Nothing_to_do
    | Some (expr, expr_env) -> (
      match find_associated_toplevel_item expr structure with
      | None -> raise Nothing_to_do
      | Some toplevel_item -> (
        match expr.exp_desc with
        | Texp_constant _ ->
          (* Special case for constant. They can't produce side effect so it's not
         necessary to add a trailing unit parameter to the let binding. *)
          extract_const_to_toplevel ?extract_name expr ~expr_env buffer
            ~toplevel_item
        | _ ->
          extract_expr_to_toplevel ?extract_name expr buffer ~expr_env
            ~toplevel_item ~local_defs:typedtree ~mconfig)))
