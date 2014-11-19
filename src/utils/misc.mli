(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)
open Std

(* Miscellaneous useful types and functions *)

val fatal_error: string -> 'a
exception Fatal_error

val try_finally : (unit -> 'a) -> (unit -> unit) -> 'a;;

val map_end: ('a -> 'b) -> 'a list -> 'b list -> 'b list
        (* [map_end f l t] is [map f l @ t], just more efficient. *)
val map_left_right: ('a -> 'b) -> 'a list -> 'b list
        (* Like [List.map], with guaranteed left-to-right evaluation order *)
val for_all2: ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
        (* Same as [List.for_all] but for a binary predicate.
           In addition, this [for_all2] never fails: given two lists
           with different lengths, it returns false. *)
val replicate_list: 'a -> int -> 'a list
        (* [replicate_list elem n] is the list with [n] elements
           all identical to [elem]. *)
val list_remove: 'a -> 'a list -> 'a list
        (* [list_remove x l] returns a copy of [l] with the first
           element equal to [x] removed. *)
val split_last: 'a list -> 'a list * 'a
        (* Return the last element and the other elements of the given list. *)
val samelist: ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
        (* Like [List.for_all2] but returns [false] if the two
           lists have different length. *)

val may: ('a -> unit) -> 'a option -> unit
val may_map: ('a -> 'b) -> 'a option -> 'b option

module Path_list : sig
  type t

  val of_list : t list -> t
  val of_string_list_ref : string list ref -> t

  val to_list : t -> string List.Lazy.t
  val to_strict_list : t -> string list
end

val find_in_path: Path_list.t -> string -> string
        (* Search a file in a list of directories. *)
val find_in_path_uncap: ?fallback:string -> Path_list.t -> string -> string
        (* Same, but search also for uncapitalized name, i.e.
           if name is Foo.ml, allow /path/Foo.ml and /path/foo.ml
           to match. *)
val canonicalize_filename : ?cwd:string -> string -> string
        (* Ensure that path is absolute (wrt to cwd), by following ".." and "." *)
val expand_glob : ?filter:(string -> bool) -> string -> string list -> string list
        (* [expand_glob ~filter pattern acc] adds all filenames matching
           [pattern] and satistfying the [filter] predicate to [acc]*)
val split_path : string -> string list -> string list
        (* [split_path path tail] prepends all components of [path] to [tail],
           including implicit "." if path is not absolute.
           [split_path "a/b/c" []] = ["."; "a"; "b"; "c"]
           [split_path "/a/b/c" []] = ["/"; "a"; "b"; "c"]
        FIXME: explain windows behavior
        *)

val remove_file: string -> unit
        (* Delete the given file if it exists. Never raise an error. *)
val expand_directory: string -> string -> string
        (* [expand_directory alt file] eventually expands a [+] at the
           beginning of file into [alt] (an alternate root directory) *)

val create_hashtable: int -> ('a * 'b) list -> ('a, 'b) Hashtbl.t
        (* Create a hashtable of the given size and fills it with the
           given bindings. *)

val copy_file: in_channel -> out_channel -> unit
        (* [copy_file ic oc] reads the contents of file [ic] and copies
           them to [oc]. It stops when encountering EOF on [ic]. *)
val copy_file_chunk: in_channel -> out_channel -> int -> unit
        (* [copy_file_chunk ic oc n] reads [n] bytes from [ic] and copies
           them to [oc]. It raises [End_of_file] when encountering
           EOF on [ic]. *)
val string_of_file: in_channel -> string
        (* [string_of_file ic] reads the contents of file [ic] and copies
           them to a string. It stops when encountering EOF on [ic]. *)
val input_bytes : in_channel -> int -> string;;
        (* [input_bytes ic n] reads [n] bytes from [ic] and returns them
           in a new string.  It raises [End_of_file] if EOF is encountered
           before all the bytes are read. *)

val log2: int -> int
        (* [log2 n] returns [s] such that [n = 1 lsl s]
           if [n] is a power of 2*)
val align: int -> int -> int
        (* [align n a] rounds [n] upwards to a multiple of [a]
           (a power of 2). *)
val no_overflow_add: int -> int -> bool
        (* [no_overflow_add n1 n2] returns [true] if the computation of
           [n1 + n2] does not overflow. *)
val no_overflow_sub: int -> int -> bool
        (* [no_overflow_add n1 n2] returns [true] if the computation of
           [n1 - n2] does not overflow. *)
val no_overflow_lsl: int -> bool
        (* [no_overflow_add n] returns [true] if the computation of
           [n lsl 1] does not overflow. *)

val chop_extension_if_any: string -> string
        (* Like Filename.chop_extension but returns the initial file
           name if it has no extension *)

val chop_extensions: string -> string
        (* Return the given file name without its extensions. The extensions
           is the longest suffix starting with a period and not including
           a directory separator, [.xyz.uvw] for instance.

           Return the given name if it does not contain an extension. *)

val search_substring: string -> string -> int -> int
        (* [search_substring pat str start] returns the position of the first
           occurrence of string [pat] in string [str].  Search starts
           at offset [start] in [str].  Raise [Not_found] if [pat]
           does not occur. *)

val rev_split_words: string -> string list
        (* [rev_split_words s] splits [s] in blank-separated words, and return
           the list of words in reverse order. *)

val rev_string_split: on:char -> string -> string list
        (* [rev_string_split ~on s] splits [s] on [on], and return the list of
           words in reverse order. *)

val get_ref: 'a list ref -> 'a list
        (* [get_ref lr] returns the content of the list reference [lr] and reset
           its content to the empty list. *)

val fst3: 'a * 'b * 'c -> 'a
val snd3: 'a * 'b * 'c -> 'b
val thd3: 'a * 'b * 'c -> 'c

val fst4: 'a * 'b * 'c * 'd -> 'a
val snd4: 'a * 'b * 'c * 'd -> 'b
val thd4: 'a * 'b * 'c * 'd -> 'c
val fth4: 'a * 'b * 'c * 'd -> 'd

        (* [modules_in_path ~ext path] lists ocaml modules corresponding to
         * filenames with extension [ext] in given [path]es.
         * For instance, if there is file "a.ml","a.mli","b.ml" in ".":
         * - modules_in_path ~ext:".ml" ["."] returns ["A";"B"],
         * - modules_in_path ~ext:".mli" ["."] returns ["A"] *)
val modules_in_path : ext:string -> string list -> string list

val (~:) : 'a -> 'a Lazy.t

val file_mtime : string -> float
val file_contents : string -> string
