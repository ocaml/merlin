(*
  rewrites
    `fresh v ...`
  to
    `call_fresh (fun a -> ... )`
*)
open Ppxlib
open Ppxlib.Ast_helper

[@@@ocaml.warnerror "-26"]
let is_fresh e =
  match e.pexp_desc with
  | Pexp_ident { txt = Lident "fresh"; _ } -> true
  | _ -> false
;;

let my_list ~loc es =
  List.fold_right ~init:[%expr []] es ~f:(fun x acc ->
      [%expr [%e x] :: [%e acc]])
;;

let mapper =
  object (self)
    inherit Ast_traverse.map as super

    method! expression e =
      (* let loc = e.pexp_loc in *)
      match e.pexp_desc with
      | Pexp_apply
          ( efresh
          , [ ( Nolabel
              , { pexp_desc =
                    Pexp_ident
                      { txt = Lident var_name
                      ; loc = var_loc
                      }
                ; _
                } )
            ; body
            ] )
        when is_fresh efresh ->
        let new_body = self#expression (snd body) in
        let pat =
          let loc = var_loc in
          Pat.var
            ~loc
              (* we declare a pattern with location of original identifier *)
            (Ast_builder.Default.Located.mk var_name ~loc)
        in
        let loc = efresh.pexp_loc in
        [%expr call_fresh (fun [%p pat] -> [%e new_body])]
      | _ -> super#expression e
  end
;;

let () =
  Ppxlib.Driver.register_transformation
    ~impl:mapper#structure
    "somename"
;;
