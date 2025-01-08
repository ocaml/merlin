(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Miscellaneous useful types and functions *)

(** {1 Reporting fatal errors} *)

val fatal_error: string -> 'a
  (** Raise the [Fatal_error] exception with the given string. *)

val fatal_errorf: ('a, Format.formatter, unit, 'b) format4 -> 'a
  (** Format the arguments according to the given format string
      and raise [Fatal_error] with the resulting string. *)

exception Fatal_error of string * Printexc.raw_backtrace

(** {1 Exceptions and finalization} *)

val try_finally :
  ?always:(unit -> unit) ->
  ?exceptionally:(unit -> unit) ->
  (unit -> 'a) -> 'a
(** [try_finally work ~always ~exceptionally] is designed to run code
    in [work] that may fail with an exception, and has two kind of
    cleanup routines: [always], that must be run after any execution
    of the function (typically, freeing system resources), and
    [exceptionally], that should be run only if [work] or [always]
    failed with an exception (typically, undoing user-visible state
    changes that would only make sense if the function completes
    correctly). For example:

    {[
      let objfile = outputprefix ^ ".cmo" in
      let oc = open_out_bin objfile in
      Misc.try_finally
        (fun () ->
           bytecode
           ++ Timings.(accumulate_time (Generate sourcefile))
               (Emitcode.to_file oc modulename objfile);
           Warnings.check_fatal ())
        ~always:(fun () -> close_out oc)
        ~exceptionally:(fun _exn -> remove_file objfile);
    ]}

    If [exceptionally] fail with an exception, it is propagated as
    usual.

    If [always] or [exceptionally] use exceptions internally for
    control-flow but do not raise, then [try_finally] is careful to
    preserve any exception backtrace coming from [work] or [always]
    for easier debugging.
*)

val reraise_preserving_backtrace : exn -> (unit -> unit) -> 'a
(** [reraise_preserving_backtrace e f] is (f (); raise e) except that the
    current backtrace is preserved, even if [f] uses exceptions internally. *)

(** {1 List operations} *)

val map_end: ('a -> 'b) -> 'a list -> 'b list -> 'b list
       (** [map_end f l t] is [map f l @ t], just more efficient. *)

val rev_map_end: ('a -> 'b) -> 'a list -> 'b list -> 'b list
       (** [map_end f l t] is [map f (rev l) @ t], just more efficient. *)

val map_left_right: ('a -> 'b) -> 'a list -> 'b list
       (** Like [List.map], with guaranteed left-to-right evaluation order *)

val for_all2: ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
       (** Same as [List.for_all] but for a binary predicate.
           In addition, this [for_all2] never fails: given two lists
           with different lengths, it returns false. *)

val replicate_list: 'a -> int -> 'a list
       (** [replicate_list elem n] is the list with [n] elements
           all identical to [elem]. *)

val list_remove: 'a -> 'a list -> 'a list
       (** [list_remove x l] returns a copy of [l] with the first
           element equal to [x] removed. *)

val split_last: 'a list -> 'a list * 'a
       (** Return the last element and the other elements of the given list. *)

val may: ('a -> unit) -> 'a option -> unit
val may_map: ('a -> 'b) -> 'a option -> 'b option

type ref_and_value = R : 'a ref * 'a -> ref_and_value

val protect_refs : ref_and_value list -> (unit -> 'a) -> 'a
(** [protect_refs l f] temporarily sets [r] to [v] for each [R (r, v)] in [l]
    while executing [f]. The previous contents of the references is restored
    even if [f] raises an exception. *)

val exact_file_exists : dirname:string -> basename:string -> bool
	(* Like [Sys.file_exists], but takes into account case-insensitive file
	   systems: return true only if the basename (last component of the
           path) has the correct case. *)
val find_in_path: string list -> string -> string
       (** Search a file in a list of directories. *)

val find_in_path_rel: string list -> string -> string
       (** Search a relative file in a list of directories. *)

 (** Normalize file name [Foo.ml] to [foo.ml] *)
val normalized_unit_filename: string -> string

val find_in_path_normalized: ?fallback:string -> string list -> string -> string
(** Same as {!find_in_path_rel} , but search also for normalized unit filename,
    i.e. if name is [Foo.ml], allow [/path/Foo.ml] and [/path/foo.ml] to
    match. *)


val canonicalize_filename : ?cwd:string -> string -> string
        (* Ensure that path is absolute (wrt to cwd), by following ".." and "." *)

val expand_glob : ?filter:(string -> bool) -> string -> string list -> string list
        (* [expand_glob ~filter pattern acc] adds all filenames matching
           [pattern] and satistfying the [filter] predicate to [acc]*)
val split_path : string -> string list
        (* [split_path path] returns the components of [path],
           including implicit "." if path is not absolute.
           [split_path "a/b/c"] = ["."; "a"; "b"; "c"]
           [split_path "/a/b/c"] = ["/"; "a"; "b"; "c"]
        FIXME: explain windows behavior
        *)
val split_path_and_prepend : string -> string list -> string list
        (* [split_path_and_prepend path tail] prepends all components of [path] to [tail],
           including implicit "." if path is not absolute.
        FIXME: explain windows behavior
        *)

val remove_file: string -> unit
       (** Delete the given file if it exists and is a regular file.
           Does nothing for other kinds of files.
           Never raises an error. *)

val expand_directory: string -> string -> string
       (** [expand_directory alt file] eventually expands a [+] at the
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
val output_to_file_via_temporary:
      ?mode:open_flag list -> string -> (string -> out_channel -> 'a) -> 'a
        (* Produce output in temporary file, then rename it
           (as atomically as possible) to the desired output file name.
           [output_to_file_via_temporary filename fn] opens a temporary file
           which is passed to [fn] (name + output channel).  When [fn] returns,
           the channel is closed and the temporary file is renamed to
           [filename]. *)

val input_bytes : in_channel -> int -> bytes
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
        (* [no_overflow_sub n1 n2] returns [true] if the computation of
           [n1 - n2] does not overflow. *)
val no_overflow_mul: int -> int -> bool
        (* [no_overflow_mul n1 n2] returns [true] if the computation of
           [n1 * n2] does not overflow. *)
val no_overflow_lsl: int -> int -> bool
        (* [no_overflow_lsl n k] returns [true] if the computation of
           [n lsl k] does not overflow. *)

val letter_of_int : int -> string

module Int_literal_converter : sig
  val int : string -> int
  val int32 : string -> int32
  val int64 : string -> int64
  val nativeint : string -> nativeint
end

val find_first_mono : (int -> bool) -> int
  (**[find_first_mono p] takes an integer predicate [p : int -> bool]
     that we assume:
     1. is monotonic on natural numbers:
        if [a <= b] then [p a] implies [p b],
     2. is satisfied for some natural numbers in range [0; max_int]
        (this is equivalent to: [p max_int = true]).

     [find_first_mono p] is the smallest natural number N that satisfies [p],
     computed in O(log(N)) calls to [p].

     Our implementation supports two cases where the preconditions on [p]
     are not respected:
     - If [p] is always [false], we silently return [max_int]
       instead of looping or crashing.
     - If [p] is non-monotonic but eventually true,
       we return some satisfying value.
  *)

(** {1 String operations} *)

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

val replace_substring: before:string -> after:string -> string -> string
        (* [replace_substring ~before ~after str] replaces all
           occurrences of [before] with [after] in [str] and returns
           the resulting string. *)

val rev_split_words: string -> string list
        (* [rev_split_words s] splits [s] in blank-separated words, and returns
           the list of words in reverse order. *)

val rev_string_split: on:char -> string -> string list
        (* [rev_string_split ~on s] splits [s] on [on], and return the list of
           words in reverse order. *)

val get_ref: 'a list ref -> 'a list
        (* [get_ref lr] returns the content of the list reference [lr] and reset
           its content to the empty list. *)

val set_or_ignore : ('a -> 'b option) -> 'b option ref -> 'a -> unit
        (* [set_or_ignore f opt x] sets [opt] to [f x] if it returns [Some _],
           or leaves it unmodified if it returns [None]. *)

val fst3: 'a * 'b * 'c -> 'a
val snd3: 'a * 'b * 'c -> 'b
val thd3: 'a * 'b * 'c -> 'c

val fst4: 'a * 'b * 'c * 'd -> 'a
val snd4: 'a * 'b * 'c * 'd -> 'b
val thd4: 'a * 'b * 'c * 'd -> 'c
val for4: 'a * 'b * 'c * 'd -> 'd

(* [modules_in_path ~ext path] lists ocaml modules corresponding to
 * filenames with extension [ext] in given [path]es.
 * For instance, if there is file "a.ml","a.mli","b.ml" in ".":
 * - modules_in_path ~ext:".ml" ["."] returns ["A";"B"],
 * - modules_in_path ~ext:".mli" ["."] returns ["A"] *)
val modules_in_path : ext:string -> string list -> string list

val edit_distance : string -> string -> int -> int option
(** [edit_distance a b cutoff] computes the edit distance between
    strings [a] and [b]. To help efficiency, it uses a cutoff: if the
    distance [d] is smaller than [cutoff], it returns [Some d], else
    [None].

    The distance algorithm currently used is Damerau-Levenshtein: it
    computes the number of insertion, deletion, substitution of
    letters, or swapping of adjacent letters to go from one word to the
    other. The particular algorithm may change in the future.
*)

val spellcheck : string list -> string -> string list
(** [spellcheck env name] takes a list of names [env] that exist in
    the current environment and an erroneous [name], and returns a
    list of suggestions taken from [env], that are close enough to
    [name] that it may be a typo for one of them. *)

val did_you_mean :
    Format_doc.formatter -> (unit -> string list) -> unit
(** [did_you_mean ppf get_choices] hints that the user may have meant
    one of the option returned by calling [get_choices]. It does nothing
    if the returned list is empty.

    The [unit -> ...] thunking is meant to delay any potentially-slow
    computation (typically computing edit-distance with many things
    from the current environment) to when the hint message is to be
    printed. You should print an understandable error message before
    calling [did_you_mean], so that users get a clear notification of
    the failure even if producing the hint is slow.
*)

val cut_at : string -> char -> string * string
(** [String.cut_at s c] returns a pair containing the sub-string before
   the first occurrence of [c] in [s], and the sub-string after the
   first occurrence of [c] in [s].
   [let (before, after) = String.cut_at s c in
    before ^ String.make 1 c ^ after] is the identity if [s] contains [c].

   Raise [Not_found] if the character does not appear in the string
   @since 4.01
*)

val time_spent : unit -> float
(** Returns a more precise measurement of resources usage than
    Sys.times/Unix.times.
    Both user and kernel cpu time is accounted.  *)

module String : sig
  include module type of String
  module Map : Map.S with type key = t
  module Set : Set.S with type elt = t
  module Tbl : Hashtbl.S with type key = t
end

val normalise_eol : string -> string
(** [normalise_eol s] returns a fresh copy of [s] with any '\r' characters
   removed. Intended for pre-processing text which will subsequently be printed
   on a channel which performs EOL transformations (i.e. Windows) *)

val unitname: string -> string
(** Return the name of the OCaml module matching a basename
    (filename without directory).
    Remove the extension and capitalize *)

type filepath = string
type modname = string
type crcs = (modname * Digest.t option) list

type alerts = string String.Map.t

val ordinal_suffix : int -> string
(** [ordinal_suffix n] is the appropriate suffix to append to the numeral [n] as
    an ordinal number: [1] -> ["st"], [2] -> ["nd"], [3] -> ["rd"],
    [4] -> ["th"], and so on.  Handles larger numbers (e.g., [42] -> ["nd"]) and
    the numbers 11--13 (which all get ["th"]) correctly. *)

(** {1 Color support detection }*)
module Color: sig
  type setting = Auto | Always | Never

  val default_setting : setting

end


(** {1 Styling handling for terminal output } *)

module Style : sig
  type color =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White

  type style =
    | FG of color (* foreground *)
    | BG of color (* background *)
    | Bold
    | Reset

  type Format.stag += Style of style list

  val ansi_of_style_l : style list -> string
  (* ANSI escape sequence for the given style *)

  type tag_style ={
    ansi: style list;
    text_open:string;
    text_close:string
  }

  type styles = {
    error: tag_style;
    warning: tag_style;
    loc: tag_style;
    hint: tag_style;
    inline_code: tag_style;
  }

  val as_inline_code: 'a Format_doc.printer -> 'a Format_doc.printer
  val inline_code: string Format_doc.printer

  val default_styles: styles
  val get_styles: unit -> styles
  val set_styles: styles -> unit

  val setup : Color.setting option -> unit
  (* [setup opt] will enable or disable color handling on standard formatters
     according to the value of color setting [opt].
     Only the first call to this function has an effect. *)

  val set_tag_handling : Format.formatter -> unit
  (* adds functions to support color tags to the given formatter. *)
end

val print_see_manual : int list Format_doc.printer
(** See manual section *)


module Utf8_lexeme: sig
  type t = string

  val normalize: string -> (t,t) Result.t
  (** Normalize the given UTF-8 encoded string.
      Invalid UTF-8 sequences results in a error and are replaced
      by U+FFFD.
      Identifier characters are put in NFC normalized form.
      Other Unicode characters are left unchanged. *)

  val capitalize: string -> (t,t) Result.t
  (** Like [normalize], but if the string starts with a lowercase identifier
      character, it is replaced by the corresponding uppercase character.
      Subsequent characters are not changed. *)

  val uncapitalize: string -> (t,t) Result.t
  (** Like [normalize], but if the string starts with an uppercase identifier
      character, it is replaced by the corresponding lowercase character.
      Subsequent characters are not changed. *)

  val is_capitalized: t -> bool
  (** Returns [true] if the given normalized string starts with an
      uppercase identifier character, [false] otherwise.  May return
      wrong results if the string is not normalized. *)

  val is_valid_identifier: t -> bool
  (** Check whether the given normalized string is a valid OCaml identifier:
      - all characters are identifier characters
      - it does not start with a digit or a single quote
  *)

  val is_lowercase: t -> bool
  (** Returns [true] if the given normalized string only contains lowercase
      identifier character, [false] otherwise. May return wrong results if the
      string is not normalized. *)

  type validation_result =
    | Valid
    | Invalid_character of Uchar.t   (** Character not allowed *)
    | Invalid_beginning of Uchar.t   (** Character not allowed as first char *)

  val validate_identifier: ?with_dot:bool -> t -> validation_result
  (** Like [is_valid_identifier], but returns a more detailed error code. Dots
      can be allowed to extend support to path-like identifiers. *)

  val starts_like_a_valid_identifier: t -> bool
  (** Checks whether the given normalized string starts with an identifier
      character other than a digit or a single quote.  Subsequent characters
      are not checked. *)
end

