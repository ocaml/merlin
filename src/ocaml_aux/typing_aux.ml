(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2014  Frédéric Bour  <frederic.bour(_)lakaban.net>
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

exception Weak_error of exn
let relax_typer = fluid false

let errors : (exn list ref * (int,unit) Hashtbl.t) option fluid = fluid None
let raise_error exn =
  match ~!errors with
  | Some (l,h) ->
    let exn = if ~!relax_typer then Weak_error exn else exn in
    l := exn :: !l
  | None -> raise exn

let weak_raise exn =
  raise_error exn;
  raise (Weak_error exn)

let catch_errors caught f =
  Fluid.let' errors (Some (caught,Hashtbl.create 3)) f

let uncatch_errors f =
  Fluid.let' errors None f

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

let erroneous_patt_check p =
  List.exists (Raw_compat.pat_attributes p)
    ~f:(fun (str_loc, _) -> str_loc.Location.txt = "merlin.incorrect")
