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

  let rec_flag recursive =
    if recursive then Asttypes.Recursive else Nonrecursive

  (* Generate [let name = body]. *)
  let toplevel_let ~recursive name body =
    let open Ast_helper in
    let pattern = Pat.mk (Ppat_var { txt = name; loc = Location.none }) in
    let body = Parsetree_utils.filter_expr_attr body in
    Str.value (rec_flag recursive) [ Vb.mk pattern body ]

  (* Generate [let name = const]. *)
  let let_const_toplevel ~recursive name const =
    Ast_helper.Exp.constant const |> toplevel_let ~recursive name

  (* Generate [let name () = body]. *)
  let let_unit_toplevel ~recursive name body =
    let open Ast_helper in
    let unit_param =
      { Parsetree.pparam_loc = Location.none;
        pparam_desc = Pparam_val (Nolabel, None, Pat.construct unit None)
      }
    in
    let body = Exp.function_ [ unit_param ] None (Pfunction_body body) in
    toplevel_let ~recursive name body

  (* Generate [let name params = body]. *)
  let toplevel_function ~recursive params name body =
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
    toplevel_let ~recursive name body

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

module Msource = struct
  include Msource

  (* TODO: Maybe add this directly in [Msource]? *)
  let sub_loc src loc =
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
end

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

type extraction_name = Default of { basename : string } | Given of string

(* type extraction = { name : extraction_name } *)

module Value_binding = struct
  type t =
    { rec_flag : Asttypes.rec_flag;
      bindings : Typedtree.value_binding list;
      loc : Location.t
    }

  let is_recursive = function
    | { rec_flag = Asttypes.Recursive; _ } -> true
    | { rec_flag = Nonrecursive; _ } -> false
end

type analysis =
  { bounded_vars : Path.t list; generated_let_rec_flag : rec_flag }

and rec_flag =
  | Non_recursive
  | (* Recursive |*)
    Rec_and

let analyze_expr node env ~toplevel_vb ~mconfig ~local_defs =
  let unbounded_enclosing =
    { Location.loc_start = toplevel_vb.Value_binding.loc.loc_start;
      loc_end = (Browse_raw.node_real_loc Location.none node).loc_start;
      loc_ghost = false
    }
  in
  let is_parent_recursive = Value_binding.is_recursive toplevel_vb in
  Browse_tree.of_node ~env node
  |> occuring_vars
  |> List.fold_left
       ~init:{ bounded_vars = []; generated_let_rec_flag = Non_recursive }
       ~f:(fun acc var_path ->
         match
           Locate.from_path
             ~config:{ mconfig; ml_or_mli = `ML; traverse_aliases = true }
             ~env ~local_defs ~namespace:Value var_path
         with
         | `Found { location; approximated = false; _ } ->
           if Location_aux.included location ~into:unbounded_enclosing then
             { acc with bounded_vars = var_path :: acc.bounded_vars }
           else if
             is_parent_recursive
             && Location_aux.included location ~into:toplevel_vb.loc
           then { acc with generated_let_rec_flag = Rec_and }
           else acc
         | _ -> acc)

let extract_to_toplevel name expr gen_let_binding gen_call buffer ~expr_env
    ~exp_loc ~toplevel_vb =
  let val_name =
    match name with
    | Default { basename } -> FreshName.gen_val_name basename expr_env
    | Given name -> name
  in
  let fresh_call =
    gen_call val_name |> Format.asprintf "%a" Pprintast.expression
  in
  let is_fresh_let_recursive = Value_binding.is_recursive toplevel_vb in
  let fresh_let_binding =
    gen_let_binding ~recursive:is_fresh_let_recursive val_name expr
    |> Format.asprintf "%a" Pprintast.structure_item
  in
  let fresh_toplevel_item = Msource.sub_loc buffer toplevel_vb.loc in
  let subst_loc =
    let start_lnum =
      1 + exp_loc.Location.loc_start.pos_lnum
      - toplevel_vb.loc.loc_start.pos_lnum
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
    Msource.substitute fresh_toplevel_item
      (`Logical (Lexing.split_pos subst_loc.loc_start))
      (`Logical (Lexing.split_pos subst_loc.loc_end))
      fresh_call
    |> Msource.text
  in
  let selection_range =
    let prefix_length =
      if is_fresh_let_recursive then String.length "let rec "
      else String.length "let "
    in
    { Location.loc_start =
        Lexing.make_pos (toplevel_vb.loc.loc_start.pos_lnum, prefix_length);
      loc_end =
        Lexing.make_pos
          ( toplevel_vb.loc.loc_start.pos_lnum,
            prefix_length + String.length val_name );
      loc_ghost = false
    }
  in
  let content = fresh_let_binding ^ "\n" ^ substitued_toplevel_item in
  { Query_protocol.loc = toplevel_vb.loc; content; selection_range }

let extract_const_to_toplevel ?extract_name const =
  let name =
    Option.fold extract_name
      ~none:(Default { basename = "const_name" })
      ~some:(fun name -> Given name)
  in
  extract_to_toplevel name (Untypeast.constant const) Gen.let_const_toplevel
    Gen.ident

let extract_expr_to_toplevel ?extract_name node expr ~expr_env ~toplevel_vb
    ~local_defs ~mconfig =
  let is_function = function
    | { Typedtree.exp_desc = Texp_function _; _ } -> true
    | _ -> false
  in
  (* let () =
    let { need_rec; _ } =
      bounded_vars node expr_env ~toplevel_parent_item ~local_defs ~mconfig
    in
    List.iter ~f:(fun path -> Path.last path |> prerr_endline) need_rec
  in *)
  let { bounded_vars; _ } =
    analyze_expr node expr_env ~toplevel_vb ~local_defs ~mconfig
  in
  let gen_let, gen_call =
    match bounded_vars with
    | [] when Fun.negate is_function expr ->
      (Gen.let_unit_toplevel, Gen.fun_apply_unit)
    | _ ->
      (Gen.toplevel_function bounded_vars, Gen.fun_apply_params bounded_vars)
  in
  let name =
    Option.fold extract_name
      ~none:(Default { basename = "fun_name" })
      ~some:(fun name -> Given name)
  in
  extract_to_toplevel name
    (Untypeast.untype_expression expr)
    gen_let gen_call ~expr_env ~toplevel_vb

let most_inclusive_expr ~start ~stop nodes =
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

let find_associated_toplevel_item expr structure =
  Stdlib.List.find_map
    (function
      | { Typedtree.str_desc = Tstr_value (rec_flag, bindings); str_loc; _ }
        when Location_aux.included expr.Typedtree.exp_loc ~into:str_loc ->
        Some { Value_binding.rec_flag; bindings; loc = str_loc }
      | _ -> None)
    structure.Typedtree.str_items

let substitute ~start ~stop ?extract_name mconfig buffer typedtree =
  match typedtree with
  | `Interface _ -> None
  | `Implementation structure -> (
    let enclosing =
      Mbrowse.enclosing start [ Mbrowse.of_structure structure ]
    in
    match most_inclusive_expr ~start ~stop enclosing with
    | None -> failwith "nothing to do"
    | Some (expr, expr_env) -> begin
      match find_associated_toplevel_item expr structure with
      | None -> failwith "nothing to do"
      | Some toplevel_vb -> (
        let exp_loc = expr.exp_loc in
        match expr.exp_desc with
        | Texp_constant const ->
          (* Special case for constant. They can't produce side effect so it's not
         necessary to add a trailing unit parameter to the let binding. *)
          Some
            (extract_const_to_toplevel const buffer ?extract_name ~expr_env
               ~exp_loc ~toplevel_vb)
        | _ ->
          Some
            (extract_expr_to_toplevel (Browse_raw.Expression expr) expr buffer
               ?extract_name ~expr_env ~exp_loc ~toplevel_vb
               ~local_defs:typedtree ~mconfig))
    end)

(*
- Ajouter test récursion mutuelle fonction
- + de tests
*)

(* Gérer cas où on extrait expression valide dans le typedtree mais qui existe pas dans le parsedtree :
let f x = 10 + x
      ^^^^^^^^^^ *)

(*
si l'expression extraite est récursive alors
  remplacer récursivement dans son corps sa mention par la fonction généré
  
*)

(* si il y a des variables dans l'expression extraite définies en dessous et que le toplevel parent est récursive
    alors générer du code en dessous dans un and  *)
