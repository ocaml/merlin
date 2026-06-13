(* identity pattern: both branches return their argument unchanged. *)
let identity_match x =
  match x with
  | None -> None
  | Some y -> Some y

(* one-arm change: the [Some] branch wraps in a different shape. *)
let wrap_match x =
  match x with
  | None -> None
  | Some y -> Some (y, y)

let () =
  ignore (identity_match (Some 1));
  ignore (wrap_match (Some 2))
