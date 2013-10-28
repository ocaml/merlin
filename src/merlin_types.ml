open Std

exception Weak_error of exn
let relax_typer = fluid false

let errors : (exn list ref * (int,unit) Hashtbl.t) option fluid = fluid None
let raise_error exn =
  match ~!errors with
  | Some (l,h) -> l := (*if ~!relax_typer then Weak_error exn else exn*) exn :: !l
  | None -> raise exn

let catch_errors f =
  let caught = ref [] in
  let result =
    Either.try' (fun () ->
        Fluid.let' errors (Some (caught,Hashtbl.create 3)) f)
  in
  !caught, result

let erroneous_type_register te =
  match ~!errors with
  | Some (l,h) -> Hashtbl.replace h te.Types.id ()
  | None -> ()

let erroneous_type_check te =
  match ~!errors with
  | Some (l,h) when Hashtbl.mem h te.Types.id -> true
  | _ -> false

include Merlin_types_custom
