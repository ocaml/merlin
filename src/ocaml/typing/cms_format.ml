(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Ulysse Gérard, Thomas Refis, Tarides                    *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let read_magic_number ic =
  let len_magic_number = String.length Config.cms_magic_number in
  really_input_string ic len_magic_number

type cms_infos = {
  cms_loadpath : string list;
  cms_sourcefile : string option; (* None for packs *)
  cms_source_digest : Digest.t option; (* None for packs *)
  cms_uid_to_loc : Location.t Shape.Uid.Tbl.t;
  cms_impl_shape : Shape.t option; (* None for mli *)
}

let input_cms ic = (input_value ic : cms_infos)

let output_cms oc cms =
  output_string oc Config.cms_magic_number;
  output_value oc (cms : cms_infos)

type error = Not_shapes of string

exception Error of error

let read_cms filename =
  let ic = open_in_bin filename in
  Misc.try_finally ~always:(fun () -> close_in ic) (fun () ->
    let magic_number = read_magic_number ic in
    if magic_number = Config.cms_magic_number then
      input_cms ic
    else
      raise(Error(Not_shapes filename))
  )

let save_shape filename ~source_file shape =
  if !Clflags.shapes && not !Clflags.print_types then
    Misc.output_to_file_via_temporary
       ~mode:[Open_binary] filename
       (fun _temp_file_name oc ->
         let source_digest = Option.map Digest.file source_file in
         let cms = {
           cms_sourcefile = source_file;
           cms_impl_shape = shape;
           cms_loadpath = Load_path.get_paths ();
           cms_source_digest = source_digest;
           cms_uid_to_loc = Env.get_uid_to_loc_tbl ();
         } in
         output_cms oc cms)
