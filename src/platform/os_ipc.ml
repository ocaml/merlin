(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2019  Merlin contributors

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

type server
type context

type client = {
  context : context;
  wd      : string;
  environ : string;
  argv    : string array;
}

(* {1 Server management}
   Listen, accept client and close *)

external server_setup : string -> string -> server option =
  "ml_merlin_server_setup"

external server_accept : server -> timeout:float -> client option =
  "ml_merlin_server_accept"

external server_close : server -> unit =
  "ml_merlin_server_close"

(* {1 Context management (stdin, stdout, stderr)}
   Setup and close *)

external context_setup : context -> unit =
  "ml_merlin_context_setup"

external context_close : context -> return_code:int -> unit =
  "ml_merlin_context_close"

(* {1 Environment management} *)

external merlin_set_environ : string -> unit =
  "ml_merlin_set_environ"
(** completely replace the environment *)
