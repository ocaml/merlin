let {Logger. log} = Logger.for_section "locate"

let rec load_shapes ?(fallback = true) comp_unit cmwhat =
  match Load_path.find_uncap (comp_unit ^ cmwhat) with
  | filename ->
    let cms = Cms_cache.read filename in
    let pos_fname = cms.cms_sourcefile in
    Ok (pos_fname, cms)
  | exception Not_found ->
    if cmwhat = ".cmsi" && fallback then load_shapes comp_unit ".cms"
    else Error ()


let from_path ~env ~ml_or_mli uid loc path ns =
  let module Shape_reduce = Shape.Make_reduce (struct
      let fuel = 1
      let read_unit_shape ~unit_name =
        match Load_path.find_uncap (unit_name ^ ".cms") with
        | filename -> (Cms_cache.read filename).cms_impl_shape
        | exception Not_found ->
          log ~title:"read_unit_shape"
            "failed to find %s.cms" unit_name;
          None
      let find_shape id = Env.shape_of_path env (Pident id)
    end)
  in
  let uid, cmwhat = match ml_or_mli with
    | `ML -> begin
        log ~title:"locate_with_shape" "From path: %a\n%!"
          Logger.fmt (fun fmt -> Path.print fmt path);
        let shape = Env.shape_of_path ~ns env path in
        log ~title:"locate_with_shape" "Shape of path: %a\n"
          Logger.fmt (fun fmt -> Shape.print fmt shape);
        let r = Shape_reduce.reduce shape in
        log ~title:"locate_with_shape" "Wich reduces to %a\n"
          Logger.fmt (fun fmt -> Shape.print fmt r);
        r.uid
      end, ".cms"
    | `MLI ->
      log ~title:"locate_with_shape" "Looking for the location of uid: %a\n%!"
        Logger.fmt (fun fmt -> Shape.Uid.print fmt uid);
      Some uid, ".cmsi"
  in
  match uid with
  | Some (Shape.Uid.Item { comp_unit; id } as uid) ->
    let fileopt, locopt =
      if Env.get_unit_name () = comp_unit then begin
          log ~title:"locate_with_shape"
            "We look for %a in the current compilation unit."
           Logger.fmt (fun fmt -> Shape.Uid.print fmt uid);
          let tbl = Env.get_uid_to_loc_tbl () in
          let loc = match Shape.Uid.Tbl.find_opt tbl uid with
            | Some loc ->
              log ~title:"locate_with_shape" "Found location: %a"
                Logger.fmt (fun fmt -> Location.print_loc fmt loc);
              loc
            | None ->
              log ~title:"locate_with_shape" "Uid not found in the local environment. Fallbacking to the node's location: %a"
                Logger.fmt (fun fmt -> Location.print_loc fmt loc);
              loc
          in
          Some comp_unit,
          Some loc
      end else begin
        log ~title:"locate_with_shape"
          "Loading the shapes for unit %S" comp_unit;
        match load_shapes comp_unit cmwhat with
        | Ok (_fname, cms) ->
          log ~title:"locate_with_shape"
            "Shapes succesfully loaded, looking for %a"
            Logger.fmt (fun fmt -> Shape.Uid.print fmt uid);
          let loc = match Shape.Uid.Tbl.find_opt cms.cms_uid_to_loc uid with
            | Some loc ->
              log ~title:"locate_with_shape" "Found location: %a"
                Logger.fmt (fun fmt -> Location.print_loc fmt loc);
              Some loc
            | None ->
              log ~title:"locate_with_shape"
                "Uid not found in the loaded shape.";
              None
            in
          Some comp_unit,
          loc
        | Error () ->
          log ~title:"locate_with_shape"
            "Failed to load the shapes";
          None, None
      end
    in
    let res = Option.map (fun loc -> fileopt, loc) locopt in
    (match res with
    | Some (f, l) -> `Found( l, f)
    | _ -> `Not_found ("todo1", None) (* TODO fallback ?*) )
  | Some (Compilation_unit comp_unit) ->
    begin
      match load_shapes comp_unit cmwhat with
      | Ok (pos_fname, cms) ->
        let pos = Std.Lexing.make_pos ~pos_fname (1, 0) in
        let loc = { Location. loc_start=pos ; loc_end=pos ; loc_ghost=true } in
        `Found(loc, Some comp_unit)
      | Error () ->
        log ~title:"locate_with_shape"
          "Failed to load the shapes";
        `Not_found ("todo2", None) (* TODO fallback ?*)
    end
  | _ ->
    log ~title:"locate_with_shape"
      "No UID found in the shape, fallback to lookup location.";
    `Found (loc, None)
