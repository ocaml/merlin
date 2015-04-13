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

(* TODO: document all the following functions *)

(* This should NOT be here. *)
val optional_label_sugar :
  Typedtree.expression_desc -> Typedtree.expression option

(* This function might be better somewhere else, considering other analysis
   tools use it as well (locate, destruct, etc).
   
   Is there some completion-specific reason explaining why it's not in [Browse]?
   We need more documentation. *)
val node_at : ?skip_recovered:bool -> Merlin_lib.Typer.t -> Lexing.position
 -> BrowseT.t * BrowseT.t list

val node_complete
  : Merlin_lib.Buffer.t
  -> ?get_doc:([> `Completion_entry of [> `Type | `Vals ] * Path.t * 'a ]
               -> [> `Found of string ])
  -> ?target_type:Types.type_expr
  -> BrowseT.t
  -> string
  -> Protocol.Compl.entry list

val expand_prefix : global_modules:string list -> Env.t -> string
  -> Protocol.Compl.t

val labels_of_application : ?prefix:Asttypes.label -> Typedtree.expression
  -> (Asttypes.label * Types.type_expr) list
