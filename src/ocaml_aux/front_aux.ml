(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

open Std

let errors : (exn list ref * unit Btype.TypeHash.t) option fluid = fluid None

let monitor_errors' = ref (ref false)
let monitor_errors () =
  if !(!monitor_errors') then
    monitor_errors' := (ref false);
  !monitor_errors'

let raise_error ?(ignore_unify=false) exn =
  !monitor_errors' := true;
  match ~!errors with
  | Some (l,h) ->
    begin match exn with
      | Ctype.Unify _ when ignore_unify -> ()
      | Ctype.Unify _ | Failure _ ->
        Logger.logfmt "Typing_aux.raise_error"
          (Printexc.exn_slot_name exn)
          (fun fmt ->
             Printexc.record_backtrace true;
             Format.pp_print_string  fmt (Printexc.get_backtrace ())
          )
      | exn -> l := exn :: !l
    end
  | None -> raise exn

exception Resume

let resume_raise exn =
  raise_error exn;
  raise Resume

let catch_errors caught f =
  Fluid.let' errors (Some (caught,Btype.TypeHash.create 3)) f

let uncatch_errors f =
  Fluid.let' errors None f

let erroneous_type_register te =
  match ~!errors with
  | Some (l,h) -> Btype.TypeHash.replace h te ()
  | None -> ()

let erroneous_type_check te =
  match ~!errors with
  | Some (l,h) -> Btype.TypeHash.mem h te
  | _ -> false

let rec erroneous_expr_check e =
  (erroneous_type_check e.Typedtree.exp_type) ||
  match e.Typedtree.exp_desc with
  | Typedtree.Texp_ident (p,_,_)
    when Ident.name (Path.head p) = "_" -> true
  | Typedtree.Texp_apply (e',_) -> erroneous_expr_check e'
  | _ -> false

let erroneous_patt_check p =
  List.exists Browse_node.(node_attributes (Pattern p))
    ~f:(fun (str_loc, _) -> str_loc.Location.txt = "merlin.incorrect")

exception Warning of Location.t * string

let prerr_warning loc w =
  match ~!errors with
  | None -> () (*Location.print_warning loc Format.err_formatter w*)
  | Some (l, _) ->
    let ppf, to_string = Format.to_string () in
    Location.print_warning loc ppf w;
    match to_string () with
      | "" -> ()
      | s ->  l := Warning (loc,s) :: !l

let () = Location.prerr_warning_ref := prerr_warning
