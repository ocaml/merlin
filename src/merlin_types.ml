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

let rec erroneous_expr_check e =
  (erroneous_type_check e.Typedtree.exp_type) ||
  match e.Typedtree.exp_desc with
  | Typedtree.Texp_ident (p,_,_) 
    when Ident.name (Path.head p) = "_" -> true
  | Typedtree.Texp_apply (e',_) -> erroneous_expr_check e'
  | _ -> false

include Merlin_types_custom
