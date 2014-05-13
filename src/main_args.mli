(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: main_args.mli 12511 2012-05-30 13:29:48Z lefessan $ *)

module type Top_options = sig
  val _debug : string -> unit
  val _timed_logs : unit -> unit
  val _projectfind : string -> unit
  val _real_paths : unit -> unit
  val _absname : unit -> unit
  val _ignore_sigint : unit -> unit
  val _I : string -> unit
  val _init : string -> unit
  val _labels : unit -> unit
  val _no_app_funct : unit -> unit
  val _nolabels : unit -> unit
  val _nostdlib : unit -> unit
  val _principal : unit -> unit
  val _rectypes : unit -> unit
  val _strict_sequence : unit -> unit
  val _unsafe : unit -> unit
  val _version : unit -> unit
  val _vnum : unit -> unit
  val _w : string -> unit
  val _warn_error : string -> unit
  val _warn_help : unit -> unit
  val _protocol : string -> unit

  val anonymous : string -> unit
end;;

module type Arg_list = sig
    val list : (string * Arg.spec * string) list
end;;

module Make_top_options (F : Top_options) : Arg_list;;
