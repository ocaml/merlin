open Std

let rec summarize node =
  let open BrowseT in
  let open Typedtree in
  let pos = node.t_loc.Location.loc_start in
  match node.t_node with
  (*FIXME TODO*)
  | Pattern { pat_desc = Tpat_var (id,_) } ->
    Some { Protocol. name = Ident.name id; kind = `Value; pos; children = [] }
  (*| TypeDecl (i, ty_decl, kind) ->
    let children =
      let helper kind (id, loc) =
        let pos = loc.Location.loc_start in
        { Protocol. name = Ident.name id ; kind ; pos ; children = [] }
      in
      match Lazy.force kind with
      | Abstract -> []
      | Variant cstrs -> List.map cstrs ~f:(helper `Constructor)
      | Record fields -> List.map fields ~f:(helper `Label)
    in
    Some { Protocol. name = Ident.name i ; kind = `Type ; pos ; children }*)
  (*| Module (Named name, _) ->
    let children = handle_browses (Lazy.force node.nodes) in
    Some { Protocol. name ; kind = `Module ; pos ; children }
  | NamedOther id ->
    Some { Protocol. name = Ident.name id ; kind = `Exn ; pos ; children = [] }*)
  | _ -> None

and handle_browses l = []
                       (*let rec remove_indir b =
                          match b.BrowseT.context with
                          | Browse.TopStructure -> Lazy.force b.Browse.nodes
                          | Browse.Module (Browse.Structure, _) ->
                          List.concat_map (Lazy.force b.Browse.nodes) ~f:remove_indir
                          | _ -> [b]
                          in
                          List.filter_map (List.concat_map l ~f:remove_indir) ~f:summarize*)

let get structures = []
                     (*List.concat_map (List.rev structures)
                        ~f:(fun s -> handle_browses (Browse.structure s))*)
