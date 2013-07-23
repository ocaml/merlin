let errors : (exn list ref * (int,unit) Hashtbl.t) option ref = ref None
let raise_error exn =
  match !errors with
  | Some (l,h) -> l := exn :: !l
  | None -> raise exn

let catch_errors f =
  let caught = ref [] in
  let types  = Hashtbl.create 3 in
  let previous = !errors in
  errors := Some (caught,types);
  let result =
    try Misc.Inr (f())
    with e -> Misc.Inl e
  in
  errors := previous;
  !caught, result
     
let erroneous_type_register te =
  match !errors with
  | Some (l,h) -> Hashtbl.replace h te.Types.id ()
  | None -> ()

let erroneous_type_check te = 
  match !errors with
  | Some (l,h) when Hashtbl.mem h te.Types.id -> true
  | _ -> false
