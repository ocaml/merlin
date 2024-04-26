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

type config = {
  build_path   : string list;
  source_path  : string list;
  hidden_build_path  : string list;
  hidden_source_path : string list;
  cmi_path     : string list;
  cmt_path     : string list;
  flags        : string list with_workdir list;
  extensions   : string list;
  suffixes     : (string * string) list;
  stdlib       : string option;
  reader       : string list;
  exclude_query_dir : bool;
  use_ppx_cache : bool;
}

type context

val get_config : context -> string -> config * string list

val find_project_context : string -> (context * string) option
(** [find_project_config dir] searches for a "project configuration file" in dir
    and its parent directories. Stopping on the first one it finds and returning
    a configuration context along with the path to the configuration file,
    returning None otherwise (if '/' was reached without finding such a file).

    A project configuration files is one of:
    - .merlin
    - dune-project
    - dune-workspace

    They are detected in that order. [dune] and [jbuild] file do not need to be taken into account because any project using a recent version of dune should have a dune-project file which is even auto-generated when it is missing. And only recent versions of dune will stop writing .merlin files.
*)
