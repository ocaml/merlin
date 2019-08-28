module EnvTbl2 =
  struct
    (** This module is used to store all kinds of components except
        (labels and constructors) in environments.  We keep a
        representation of each nested "open" and the set of local
        bindings between each of them. *)


    type 'a t = {
      current: 'a Ident.tbl;
      (** Local bindings since the last open *)

      opened: 'a opened option;
      (** Symbolic representation of the last (innermost) open, if any. *)
    }

    and 'a opened = {
      root: Path.t;
      (** The path of the opened module, to be prefixed in front of
          its local names to produce a valid path in the current
          environment. *)

      components: (string, 'a * int) Tbl.t;
      (** Components from the opened module. *)

      using: (string -> ('a * 'a) option -> unit) option;
      (** A callback to be applied when a component is used from this
          "open".  This is used to detect unused "opens".  The
          arguments are used to detect shadowing. *)

      next: 'a t;
      (** The table before opening the module. *)
    }

    let empty = { current = Ident.empty; opened = None }

    let add id x tbl =
      {tbl with current = Ident.add id x tbl.current}

    let add_open slot wrap root components next =
      let using =
        match slot with
        | None -> None
        | Some f -> Some (fun s x -> f s (wrap x))
      in
      {
        current = Ident.empty;
        opened = Some {using; root; components; next};
      }

    let rec find_same id tbl =
      try Ident.find_same id tbl.current
      with Not_found as exn ->
        begin match tbl.opened with
        | Some {next; _} -> find_same id next
        | None -> raise exn
        end

    let rec find_name mark name tbl =
      try
        let (id, desc) = Ident.find_name name tbl.current in
        Pident id, desc
      with Not_found as exn ->
        begin match tbl.opened with
        | Some {using; root; next; components} ->
            begin try
              let (descr, pos) = Tbl.find_str name components in
              let res = Pdot (root, name, pos), descr in
              if mark then begin match using with
              | None -> ()
              | Some f ->
                  begin try f name (Some (snd (find_name false name next), snd res))
                  with Not_found -> f name None
                  end
              end;
              res
            with Not_found ->
              find_name mark name next
            end
        | None ->
            raise exn
        end

    let find_name name tbl = find_name true name tbl

    let rec update name f tbl =
      try
        let (id, desc) = Ident.find_name name tbl.current in
        {tbl with current = Ident.add id (f desc) tbl.current}
      with Not_found ->
        begin match tbl.opened with
        | Some {root; using; next; components} ->
            begin try
              let (desc, pos) = Tbl.find_str name components in
              let components = Tbl.add name (f desc, pos) components in
              {tbl with opened = Some {root; using; next; components}}
            with Not_found ->
              let next = update name f next in
              {tbl with opened = Some {root; using; next; components}}
            end
        | None ->
            tbl
        end



    let rec find_all name tbl =
      List.map (fun (id, desc) -> Pident id, desc) (Ident.find_all name tbl.current) @
      match tbl.opened with
      | None -> []
      | Some {root; using = _; next; components} ->
          try
            let (desc, pos) = Tbl.find_str name components in
            (Pdot (root, name, pos), desc) :: find_all name next
          with Not_found ->
            find_all name next

    let rec fold_name f tbl acc =
      let acc = Ident.fold_name (fun id d -> f (Ident.name id) (Pident id, d)) tbl.current acc in
      match tbl.opened with
      | Some {root; using = _; next; components} ->
          acc
          |> Tbl.fold
            (fun name (desc, pos) -> f name (Pdot (root, name, pos), desc))
            components
          |> fold_name f next
      | None ->
          acc

    let rec local_keys tbl acc =
      let acc = Ident.fold_all (fun k _ accu -> k::accu) tbl.current acc in
      match tbl.opened with
      | Some o -> local_keys o.next acc
      | None -> acc

    let local_keys tbl = local_keys tbl []


    let rec iter f tbl =
      Ident.iter (fun id desc -> f id (Pident id, desc)) tbl.current;
      match tbl.opened with
      | Some {root; using = _; next; components} ->
          Tbl.iter
            (fun s (x, pos) -> f (Ident.hide (Ident.create s) (* ??? *)) (Pdot (root, s, pos), x))
            components;
          iter f next
      | None -> ()

  end
