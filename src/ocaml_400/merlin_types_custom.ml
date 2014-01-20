(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013  Frédéric Bour  <frederic.bour(_)lakaban.net>
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
open Misc

let include_idents l = l

let lookup_constructor id env = snd (Env.lookup_constructor id env)
let lookup_label id env = snd (Env.lookup_label id env)
let fold_types = Env.fold_types

let fold_constructors f =
  Env.fold_constructors (fun name _ descr -> f name descr)
let fold_labels f = Env.fold_labels (fun _ _ -> f)

let extract_subpatterns =
  let open Typedtree in function
  | Tpat_any | Tpat_var _ | Tpat_constant _ | Tpat_variant (_,None,_) -> []
  | Tpat_alias (p,_,_) | Tpat_lazy p | Tpat_variant (_,Some p,_) -> [p]
  | Tpat_array ps | Tpat_tuple ps | Tpat_construct (_,_,_,ps,_) -> ps
  | Tpat_or (p1,p2,_) -> [p1;p2]
  | Tpat_record (r,_) -> List.map ~f:(fun (_,_,_,p) -> p) r

let extract_specific_subexpressions =
  let open Typedtree in function
  | Texp_construct (_,_,_,es,_)  -> es
  | Texp_record (pldes,Some e) -> e :: List.map ~f:fth4 pldes
  | Texp_record (pldes,None)   -> List.map ~f:fth4 pldes
  | Texp_field (ea,_,_,_)        -> [ea]
  | Texp_setfield (ea,_,_,_,eb)  -> [ea;eb]
  | _ -> assert false

let exp_open_env = function
  | Typedtree.Texp_open (_,_,env) -> env
  | _ -> assert false
