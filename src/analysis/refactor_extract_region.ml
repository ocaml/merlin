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
  let gen_val_name ~is_bound basename env =
    let rec loop n =
      let guess = basename ^ Int.to_string n in
      if is_bound guess env then succ n |> loop else guess
    in
    loop 1

  let gen_val_name_env = gen_val_name ~is_bound:Env.bound_value
end

let clean_up_for_printing expr =
  let mapper =
    { Ast_mapper.default_mapper with
      expr =
        (fun mapper expr ->
          match expr.pexp_desc with
          | Pexp_construct
              ( ident,
                Some
                  { pexp_desc =
                      Pexp_tuple
                        (_
                        :: ({ pexp_desc =
                                Pexp_constant
                                  { pconst_desc = Pconst_string _; _ };
                              _
                            } as const)
                        :: _);
                    _
                  } )
            when Longident.head ident.txt = "CamlinternalFormatBasics" ->
            (* We need to retransform format specification which has been desugared into string. *)
            const
          | Pexp_poly (expr, _) ->
            (* We also have to remove poly extra that cause unexpected "!poly!"
               to be printed in generated code. This happens when you try
               to extract the body of a method. *)
            expr
          | _ -> Ast_mapper.default_mapper.expr mapper expr)
    }
  in
  mapper.expr mapper expr |> Parsetree_utils.expr_remove_merlin_attributes

module Gen = struct
  let unit = Longident.Lident "()" |> Location.mknoloc

  let untyped_toplevel_let ~name ~body =
    let open Ast_helper in
    let pattern = Pat.mk (Ppat_var { txt = name; loc = Location.none }) in
    Str.value Nonrecursive [ Vb.mk pattern (clean_up_for_printing body) ]

  (* Generates [let name = body]. *)
  let toplevel_let ~name ~body =
    untyped_toplevel_let ~name ~body:(Untypeast.untype_expression body)

  (* Generates [let name () = body]. *)
  let let_unit_toplevel ~name ~body =
    let open Ast_helper in
    let unit_param =
      { Parsetree.pparam_loc = Location.none;
        pparam_desc = Pparam_val (Nolabel, None, Pat.construct unit None)
      }
    in
    let body =
      Exp.function_ [ unit_param ] None
        (Pfunction_body (Untypeast.untype_expression body))
    in
    untyped_toplevel_let ~name ~body

  module Id_map = Map.Make (struct
    type t = string list

    let compare = List.compare ~cmp:String.compare
  end)

  (* Generates [let name params = body]. *)
  let toplevel_function params ~name ~body =
    let choose_param_name ~basename ~already_used param_path =
      let param_name = Path.last param_path in
      if String.Set.mem param_name already_used then
        let other_name =
          match Path.flatten param_path with
          | `Contains_apply -> assert false
          | `Ok (id, path) ->
            Ident.name id :: path
            |> List.map ~f:String.lowercase_ascii
            |> String.concat ~sep:"_"
        in
        if String.Set.mem other_name already_used then
          Fresh_name.gen_val_name ~is_bound:String.Set.mem other_name
            already_used
        else other_name
      else basename
    in
    let _used_params, params =
      List.fold_left_map
        ~f:(fun already_used param ->
          let param_name =
            choose_param_name ~basename:(Path.last param) ~already_used param
          in
          let param_pattern =
            Ast_helper.Pat.var (Location.mknoloc param_name)
          in
          let fun_param =
            { Parsetree.pparam_loc = Location.none;
              pparam_desc = Pparam_val (Nolabel, None, param_pattern)
            }
          in
          (String.Set.add param_name already_used, fun_param))
        ~init:String.Set.empty params
    in
    let body =
      Ast_helper.Exp.function_ params None
        (Parsetree.Pfunction_body (Untypeast.untype_expression body))
    in
    untyped_toplevel_let ~name ~body

  let ident ~name =
    Longident.Lident name |> Location.mknoloc |> Ast_helper.Exp.ident

  let fun_apply params ~name =
    let open Ast_helper in
    let params =
      List.map
        ~f:(fun param -> (Asttypes.Nolabel, clean_up_for_printing param))
        params
    in
    Exp.apply (ident ~name) params

  (* [fun_apply_unit ~name] generates a call to the function named [name] to which we apply unit. *)
  let fun_apply_unit = fun_apply [ Ast_helper.Exp.ident unit ]

  (* [fun_apply_params params ~name] generates a call to the function named [name]
     to which we apply the list of arguments [params]. *)
  let fun_apply_params params =
    params
    |> List.map ~f:(fun param -> ident ~name:(Path.name param))
    |> fun_apply
end

let extract_source_around_loc src loc =
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

type analysis = { bounded_vars : Path.t list; binding_kind : rec_flag }

and rec_flag = Non_recursive | Rec_and

type extraction =
  { expr : Typedtree.expression;  (** Expression that being extracted *)
    expr_env : Env.t;  (** Environment of the extracted expression *)
    toplevel_item : toplevel_item;
        (** The value binding toplevel or class declaration enclosing the extracted expression. *)
    name : extraction_name;  (** Binding name of the extracted expression. *)
    gen_binding_kind : rec_flag;
    binding_generator :
      name:string -> body:Typedtree.expression -> Parsetree.structure_item;
    call_generator : name:string -> Parsetree.expression;
    call_need_parenthesis : bool
        (** Sometime we must parenthised call in order to type check. *)
  }

and extraction_name = Default of { basename : string } | Fixed of string

and toplevel_item =
  { rec_flag : Asttypes.rec_flag;
    env : Env.t;
    loc : Location.t;
    kind : toplevel_item_kind
  }
(* A convenient type for grouping info. *)

and toplevel_item_kind = Let of Typedtree.value_binding list | Class_decl

let is_recursive = function
  | { rec_flag = Asttypes.Recursive; _ } -> true
  | { rec_flag = Nonrecursive; _ } -> false

let rec find_pattern_var : type a. a Typedtree.general_pattern -> Path.t list =
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

let occuring_vars_path node =
  let rec loop acc node =
    match node.Browse_tree.t_node with
    | Browse_raw.Expression { exp_desc = Texp_ident (path, _, _); _ } ->
      Path.Set.add path acc
    | Pattern pat ->
      let paths = find_pattern_var pat |> List.to_seq in
      Path.Set.add_seq paths acc
    | _ -> Lazy.force node.t_children |> List.fold_left ~f:loop ~init:acc
  in
  loop Path.Set.empty node
  |> Path.Set.filter (fun path ->
         (* Filter identifier that are in Stdlib to avoid cluttering the list
            of generated parameters.
            TODO: there probably a more correct way to do this *)
         Ident.name (Path.head path) <> "Stdlib")

let analyze_expr expr expr_env ~toplevel_item =
  let is_value_unbound path =
    let is_bound path env =
      try
        let _ = Env.find_value path env in
        true
      with Not_found -> false
    in
    is_bound path expr_env && not (is_bound path toplevel_item.env)
  in
  let is_one_of_value_decl var_path bindings =
    List.exists
      ~f:(fun vb ->
        let names = find_pattern_var vb.Typedtree.vb_pat |> Path.Set.of_list in
        Path.Set.mem var_path names)
      bindings
  in
  let vars_path =
    Browse_tree.of_node ~env:expr_env (Browse_raw.Expression expr)
    |> occuring_vars_path
  in
  let analysis =
    Path.Set.fold
      (fun var_path acc ->
        if is_value_unbound var_path then
          match toplevel_item.kind with
          | Let bindings
            when is_recursive toplevel_item
                 && is_one_of_value_decl var_path bindings ->
            { acc with binding_kind = Rec_and }
          | _ -> { acc with bounded_vars = var_path :: acc.bounded_vars }
        else acc)
      vars_path
      { bounded_vars = []; binding_kind = Non_recursive }
  in
  { analysis with bounded_vars = List.rev analysis.bounded_vars }

let choose_name name env =
  match name with
  | Default { basename } -> Fresh_name.gen_val_name_env basename env
  | Fixed name ->
    if Env.bound_value name env then Fresh_name.gen_val_name_env name env
    else name

let extract_to_toplevel
    { expr;
      expr_env;
      name;
      gen_binding_kind;
      binding_generator;
      call_generator;
      toplevel_item;
      call_need_parenthesis
    } buffer =
  let val_name = choose_name name expr_env in
  let fresh_call =
    let parenthised_opt s =
      if call_need_parenthesis then "(" ^ s ^ ")" else s
    in
    call_generator ~name:val_name
    |> Format.asprintf "%a" Pprintast.expression
    |> parenthised_opt
  in
  let toplevel_item_source =
    extract_source_around_loc buffer toplevel_item.loc
  in
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
  let substituted_binding =
    Msource.substitute toplevel_item_source
      (`Logical (Lexing.split_pos subst_loc.loc_start))
      (`Logical (Lexing.split_pos subst_loc.loc_end))
      fresh_call
    |> Msource.text
  in
  let content =
    match gen_binding_kind with
    | Non_recursive ->
      let fresh_let_binding =
        binding_generator ~name:val_name ~body:expr
        |> Format.asprintf "%a" Pprintast.structure_item
      in
      fresh_let_binding ^ "\n" ^ substituted_binding
    | Rec_and ->
      let fresh_let_binding =
        binding_generator ~name:val_name ~body:expr
        |> Format.asprintf "%a" Pprintast.structure_item
      in
      let fresh_and_binding =
        "and" ^ String.drop 3 fresh_let_binding (* Sorry *)
      in
      substituted_binding ^ "\n" ^ fresh_and_binding
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
      binding_generator = Gen.toplevel_let;
      call_generator = Gen.ident;
      call_need_parenthesis = false
    }

let extract_expr_to_toplevel ?extract_name expr ~expr_env ~toplevel_item =
  let is_function = function
    | { Typedtree.exp_desc = Texp_function _; _ } -> true
    | _ -> false
  in
  let is_module_bound_in_toplevel_env path =
    try
      let _ = Env.find_module path toplevel_item.env in
      false
    with Not_found -> true
  in
  let { bounded_vars; binding_kind } =
    analyze_expr expr expr_env ~toplevel_item
  in
  let bounded_vars_stamp =
    List.map ~f:(fun p -> Path.head p |> Ident.stamp) bounded_vars
  in
  let is_bound_var ident =
    List.exists ~f:(Int.equal (Ident.stamp ident)) bounded_vars_stamp
  in
  let binding_generator, call_generator =
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
  let remove_path_prefix_of_bound_values expr =
    (* We need to unqualify bound values. Otherwise, the generated call will use
       the qualified name even if it does not exist in the scope. Examples:

      let f () =
        let module X = struct let x = 10 end in
        X.x
        ^^^ If we extract this, the corresponding extracted call will be:

      let fun_name1 x = X.x *)
    let mapper =
      { Tast_mapper.default with
        expr =
          (fun mapper expr ->
            match expr.Typedtree.exp_desc with
            | Texp_ident (Pdot (path, name), longident, vd)
              when is_bound_var (Path.head path)
                   && is_module_bound_in_toplevel_env path ->
              let ident = { longident with txt = Longident.Lident name } in
              { expr with exp_desc = Texp_ident (path, ident, vd) }
            | _ -> Tast_mapper.default.expr mapper expr)
      }
    in
    mapper.expr mapper expr
  in
  extract_to_toplevel
    { expr = remove_path_prefix_of_bound_values expr;
      expr_env;
      toplevel_item;
      name;
      gen_binding_kind = binding_kind;
      binding_generator;
      call_generator;
      call_need_parenthesis = true
    }

(* [largest_expr_between ~start ~stop nodes] tries to find the most inclusive expression
   within the range [start]-[stop] among [nodes].

   [nodes] is a list of enclosings around the start position from the deepest
   to the topelevel. It's reversed searched for an expression that fits the range. *)
let largest_expr_between ~start ~stop nodes =
  let is_inside_region =
    Location_aux.included
      ~into:{ Location.loc_start = start; loc_end = stop; loc_ghost = true }
  in
  let rec select_among_child env node =
    let node_loc = Mbrowse.node_loc node in
    match node with
    | Expression expr
      when node_loc.loc_ghost = false && is_inside_region node_loc ->
      (* We filter expression that have a ghost location. Otherwise, expression
        such as [let f x = 10 + x] can be extracted and this can lead to invalid 
        code gen.      ^^^^^^^^^^ *)
      Some (expr, env)
    | _ ->
      (* Continue to browse through the child of [node]. *)
      let node = Browse_tree.of_node ~env node in
      Lazy.force node.t_children |> List.rev
      |> Stdlib.List.find_map (fun node ->
             select_among_child node.Browse_tree.t_env node.t_node)
  in
  nodes |> Stdlib.List.find_map (fun (env, node) -> select_among_child env node)

let find_associated_toplevel_item expr enclosing =
  Stdlib.List.find_map
    (fun (_, item) ->
      match item with
      | Browse_raw.Structure_item ({ str_desc; str_loc; str_env }, _) -> begin
        match str_desc with
        | Tstr_value (rec_flag, vb)
          when Location_aux.included expr.Typedtree.exp_loc ~into:str_loc ->
          Some { rec_flag; env = str_env; loc = str_loc; kind = Let vb }
        | Tstr_class cs ->
          Stdlib.List.find_map
            (fun (class_decl, _) ->
              let loc = class_decl.Typedtree.ci_loc in
              if Location_aux.included expr.exp_loc ~into:loc then
                Some
                  { rec_flag = Nonrecursive;
                    env = str_env;
                    loc;
                    kind = Class_decl
                  }
              else None)
            cs
        | _ -> None
      end
      | _ -> None)
    enclosing

let extract_region ~start ~stop enclosing =
  let open Option.Infix in
  (* We want to traverse [enclosing] in ascending order. *)
  let enclosing = List.rev enclosing in
  largest_expr_between ~start ~stop enclosing >>= fun (expr, expr_env) ->
  find_associated_toplevel_item expr enclosing >>| fun toplevel_item ->
  (expr, expr_env, toplevel_item)

let is_region_extractable ~start ~stop enclosing =
  match extract_region ~start ~stop enclosing with
  | None -> false
  | Some _ -> true

let substitute ~start ~stop ?extract_name buffer structure =
  let enclosing = Mbrowse.enclosing start [ Mbrowse.of_structure structure ] in
  match extract_region ~start ~stop enclosing with
  | None -> raise Nothing_to_do
  | Some (expr, expr_env, toplevel_item) -> begin
    match expr.exp_desc with
    | Texp_constant _ ->
      (* Special case for constant. They can't produce side effect so it's not
         necessary to add a trailing unit parameter to the let binding. *)
      extract_const_to_toplevel ?extract_name expr ~expr_env buffer
        ~toplevel_item
    | _ ->
      extract_expr_to_toplevel ?extract_name expr buffer ~expr_env
        ~toplevel_item
  end
