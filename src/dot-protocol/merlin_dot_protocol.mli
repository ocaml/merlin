(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2019  Frédéric Bour  <frederic.bour(_)lakaban.net>
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

(* EXCLUDE_QUERY_DIR

If you're building with dune, all your build artifacts will be in
_build, any .cmi (or .cmt) that will be found next to the source file
is likely to be a source of conflicts.
With this directive, .merlin files generated by dune can instruct merlin
to disregard local build artifacts.

This is especially useful when working on the compiler where two build
system coexist: dune (used for development, which will generate the
.merlin) and make, used for the actual build and testing of the compiler.
Build artifacts generated by the makefile build will be at a different
version than the one produced by dune, and understood by merlin. We
really do not want to load them. *)

module Directive : sig
  type include_path =
    [ `B of string | `S of string | `CMI of string | `CMT of string ]

  type no_processing_required =
    [ `EXT of string list
    | `FLG of string list
    | `STDLIB of string
    | `SOURCE_ROOT of string
    | `UNIT_NAME of string
    | `WRAPPING_PREFIX of string
    | `SUFFIX of string
    | `READER of string list
    | `EXCLUDE_QUERY_DIR
    | `USE_PPX_CACHE
    | `UNKNOWN_TAG of string ]

  module Processed : sig
    type acceptable_in_input = [ include_path | no_processing_required ]

    type t =
      [  acceptable_in_input
      | `ERROR_MSG of string ]
  end

  module Raw : sig
    type t =
      [ Processed.acceptable_in_input
      | `PKG of string list
      | `FINDLIB of string
      | `FINDLIB_PATH of string
      | `FINDLIB_TOOLCHAIN of string ]
  end
end

type directive = Directive.Processed.t

type read_error =
  | Unexpected_output of string
  | Csexp_parse_error of string

type command = File of string | Halt | Unknown

module type S = sig
  type 'a io
  type in_chan
  type out_chan

  (** [read] reads one csexp from the channel and returns the list of
      directives it represents *)
  val read :
    in_chan -> (directive list, read_error) Merlin_utils.Std.Result.t io

  val write : out_chan -> directive list -> unit io

  module Commands : sig
    val read_input : in_chan -> command io

    val send_file : out_chan -> string -> unit io

    val halt : out_chan -> unit io
  end
end

(** Provided for projects using merlin as a library in order to use
    custom IO implementation *)
module Make (IO : sig
  type 'a t

  module O : sig
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  end
end) (Chan : sig
  type in_chan
  type out_chan

  val read : in_chan -> (Csexp.t, string) result IO.t

  val write : out_chan -> Csexp.t -> unit IO.t
end) : S
  with type 'a io = 'a IO.t
   and type in_chan = Chan.in_chan
   and type out_chan = Chan.out_chan

module Blocking : S
  with type 'a io = 'a
   and type in_chan = in_channel
   and type out_chan = out_channel
