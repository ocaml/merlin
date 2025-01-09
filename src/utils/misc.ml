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

module CamlString = String

open Std

(* Errors *)

exception Fatal_error of string * Printexc.raw_backtrace

let () = Printexc.register_printer (function
    | Fatal_error (msg, bt) ->
      Some (Printf.sprintf "Fatal error: %s\n%s"
              msg (Printexc.raw_backtrace_to_string bt))
    | _ -> None
  )

let fatal_error msg =
  raise (Fatal_error (msg, Printexc.get_callstack 50))

let fatal_errorf fmt =
  (*Format.kasprintf is not available in 4.02.3 *)
  (*Format.kasprintf fatal_error fmt*)
  ignore (Format.flush_str_formatter ());
  Format.kfprintf
    (fun _ppf -> fatal_error (Format.flush_str_formatter ()))
    Format.str_formatter fmt

(* Exceptions *)

let try_finally ?(always=(fun () -> ())) ?(exceptionally=(fun () -> ())) work =
  match work () with
    | result ->
      begin match always () with
        | () -> result
        | exception always_exn ->
          (* raise_with_backtrace is not available before OCaml 4.05 *)
          (*let always_bt = Printexc.get_raw_backtrace () in*)
          exceptionally ();
          (*Printexc.raise_with_backtrace always_exn always_bt*)
          raise always_exn
      end
    | exception work_exn ->
      (*let work_bt = Printexc.get_raw_backtrace () in*)
      begin match always () with
        | () ->
          exceptionally ();
          (*Printexc.raise_with_backtrace work_exn work_bt*)
          raise work_exn
        | exception always_exn ->
          (*let always_bt = Printexc.get_raw_backtrace () in*)
          exceptionally ();
          (*Printexc.raise_with_backtrace always_exn always_bt*)
          raise always_exn
      end

let reraise_preserving_backtrace e f =
  let bt = Printexc.get_raw_backtrace () in
  f ();
  Printexc.raise_with_backtrace e bt

type ref_and_value = R : 'a ref * 'a -> ref_and_value

let protect_refs =
  let set_refs l = List.iter ~f:(fun (R (r, v)) -> r := v) l in
  fun refs f ->
    let backup = List.map ~f:(fun (R (r, _)) -> R (r, !r)) refs in
    set_refs refs;
    match f () with
    | x           -> set_refs backup; x
    | exception e -> set_refs backup; raise e


(** {1 Minimal support for Unicode characters in identifiers} *)

module Utf8_lexeme = struct

  type t = string

  (* Non-ASCII letters that are allowed in identifiers (currently: Latin-9) *)

  type case = Upper of Uchar.t | Lower of Uchar.t
  let known_chars : (Uchar.t, case) Hashtbl.t = Hashtbl.create 32

  let _ =
    List.iter
      ~f:(fun (upper, lower) ->
        let upper = Uchar.of_int upper and lower = Uchar.of_int lower in
        Hashtbl.add known_chars upper (Upper lower);
        Hashtbl.add known_chars lower (Lower upper))
  [
    (0xc0, 0xe0); (* À, à *)    (0xc1, 0xe1); (* Á, á *)
    (0xc2, 0xe2); (* Â, â *)    (0xc3, 0xe3); (* Ã, ã *)
    (0xc4, 0xe4); (* Ä, ä *)    (0xc5, 0xe5); (* Å, å *)
    (0xc6, 0xe6); (* Æ, æ *)    (0xc7, 0xe7); (* Ç, ç *)
    (0xc8, 0xe8); (* È, è *)    (0xc9, 0xe9); (* É, é *)
    (0xca, 0xea); (* Ê, ê *)    (0xcb, 0xeb); (* Ë, ë *)
    (0xcc, 0xec); (* Ì, ì *)    (0xcd, 0xed); (* Í, í *)
    (0xce, 0xee); (* Î, î *)    (0xcf, 0xef); (* Ï, ï *)
    (0xd0, 0xf0); (* Ð, ð *)    (0xd1, 0xf1); (* Ñ, ñ *)
    (0xd2, 0xf2); (* Ò, ò *)    (0xd3, 0xf3); (* Ó, ó *)
    (0xd4, 0xf4); (* Ô, ô *)    (0xd5, 0xf5); (* Õ, õ *)
    (0xd6, 0xf6); (* Ö, ö *)    (0xd8, 0xf8); (* Ø, ø *)
    (0xd9, 0xf9); (* Ù, ù *)    (0xda, 0xfa); (* Ú, ú *)
    (0xdb, 0xfb); (* Û, û *)    (0xdc, 0xfc); (* Ü, ü *)
    (0xdd, 0xfd); (* Ý, ý *)    (0xde, 0xfe); (* Þ, þ *)
    (0x160, 0x161); (* Š, š *)  (0x17d, 0x17e); (* Ž, ž *)
    (0x152, 0x153); (* Œ, œ *)  (0x178, 0xff); (* Ÿ, ÿ *)
    (0x1e9e, 0xdf); (* ẞ, ß *)
  ]

  (* NFD to NFC conversion table for the letters above *)

  let known_pairs : (Uchar.t * Uchar.t, Uchar.t) Hashtbl.t = Hashtbl.create 32

  let _ =
    List.iter
      ~f:(fun (c1, n2, n) ->
        Hashtbl.add known_pairs
          (Uchar.of_char c1, Uchar.of_int n2) (Uchar.of_int n))
  [
    ('A', 0x300, 0xc0); (* À *)    ('A', 0x301, 0xc1); (* Á *)
    ('A', 0x302, 0xc2); (* Â *)    ('A', 0x303, 0xc3); (* Ã *)
    ('A', 0x308, 0xc4); (* Ä *)    ('A', 0x30a, 0xc5); (* Å *)
    ('C', 0x327, 0xc7); (* Ç *)    ('E', 0x300, 0xc8); (* È *)
    ('E', 0x301, 0xc9); (* É *)    ('E', 0x302, 0xca); (* Ê *)
    ('E', 0x308, 0xcb); (* Ë *)    ('I', 0x300, 0xcc); (* Ì *)
    ('I', 0x301, 0xcd); (* Í *)    ('I', 0x302, 0xce); (* Î *)
    ('I', 0x308, 0xcf); (* Ï *)    ('N', 0x303, 0xd1); (* Ñ *)
    ('O', 0x300, 0xd2); (* Ò *)    ('O', 0x301, 0xd3); (* Ó *)
    ('O', 0x302, 0xd4); (* Ô *)    ('O', 0x303, 0xd5); (* Õ *)
    ('O', 0x308, 0xd6); (* Ö *)
    ('U', 0x300, 0xd9); (* Ù *)    ('U', 0x301, 0xda); (* Ú *)
    ('U', 0x302, 0xdb); (* Û *)    ('U', 0x308, 0xdc); (* Ü *)
    ('Y', 0x301, 0xdd); (* Ý *)    ('Y', 0x308, 0x178);  (* Ÿ *)
    ('S', 0x30c, 0x160); (* Š *)   ('Z', 0x30c, 0x17d); (* Ž *)
    ('a', 0x300, 0xe0); (* à *)    ('a', 0x301, 0xe1); (* á *)
    ('a', 0x302, 0xe2); (* â *)    ('a', 0x303, 0xe3); (* ã *)
    ('a', 0x308, 0xe4); (* ä *)    ('a', 0x30a, 0xe5); (* å *)
    ('c', 0x327, 0xe7); (* ç *)    ('e', 0x300, 0xe8); (* è *)
    ('e', 0x301, 0xe9); (* é *)    ('e', 0x302, 0xea); (* ê *)
    ('e', 0x308, 0xeb); (* ë *)    ('i', 0x300, 0xec); (* ì *)
    ('i', 0x301, 0xed); (* í *)    ('i', 0x302, 0xee); (* î *)
    ('i', 0x308, 0xef); (* ï *)    ('n', 0x303, 0xf1); (* ñ *)
    ('o', 0x300, 0xf2); (* ò *)    ('o', 0x301, 0xf3); (* ó *)
    ('o', 0x302, 0xf4); (* ô *)    ('o', 0x303, 0xf5); (* õ *)
    ('o', 0x308, 0xf6); (* ö *)
    ('u', 0x300, 0xf9); (* ù *)    ('u', 0x301, 0xfa); (* ú *)
    ('u', 0x302, 0xfb); (* û *)    ('u', 0x308, 0xfc); (* ü *)
    ('y', 0x301, 0xfd); (* ý *)    ('y', 0x308, 0xff); (* ÿ *)
    ('s', 0x30c, 0x161); (* š *)   ('z', 0x30c, 0x17e); (* ž *)
  ]

  let normalize_generic ~keep_ascii transform s =
    let rec norm check buf prev i =
      if i >= String.length s then begin
        Buffer.add_utf_8_uchar buf (transform prev)
      end else begin
        let d = String.get_utf_8_uchar s i in
        let u = Uchar.utf_decode_uchar d in
        check d u;
        let i' = i + Uchar.utf_decode_length d in
        match Hashtbl.find_opt known_pairs (prev, u) with
        | Some u' ->
            norm check buf u' i'
        | None ->
            Buffer.add_utf_8_uchar buf (transform prev);
            norm check buf u i'
      end in
    let ascii_limit = 128 in
    if s = ""
    || keep_ascii && String.for_all (fun x -> Char.code x < ascii_limit) s
    then Ok s
    else
      let buf = Buffer.create (String.length s) in
      let valid = ref true in
      let check d u =
        valid := !valid && Uchar.utf_decode_is_valid d && u <> Uchar.rep
      in
      let d = String.get_utf_8_uchar s 0 in
      let u = Uchar.utf_decode_uchar d in
      check d u;
      norm check buf u (Uchar.utf_decode_length d);
      let contents = Buffer.contents buf in
      if !valid then
        Ok contents
      else
        Error contents

  let normalize s =
    normalize_generic ~keep_ascii:true (fun u -> u) s

  (* Capitalization *)

  let uchar_is_uppercase u =
    let c = Uchar.to_int u in
    if c < 0x80 then c >= 65 && c <= 90 else
      match Hashtbl.find_opt known_chars u with
      | Some(Upper _) -> true
      | _ -> false

  let uchar_lowercase u =
    let c = Uchar.to_int u in
    if c < 0x80 then
      if c >= 65 && c <= 90 then Uchar.of_int (c + 32) else u
    else
      match Hashtbl.find_opt known_chars u with
      | Some(Upper u') -> u'
      | _ -> u

  let uchar_uppercase u =
    let c = Uchar.to_int u in
    if c < 0x80 then
      if c >= 97 && c <= 122 then Uchar.of_int (c - 32) else u
    else
      match Hashtbl.find_opt known_chars u with
      | Some(Lower u') -> u'
      | _ -> u

  let capitalize s =
    let first = ref true in
    normalize_generic ~keep_ascii:false
      (fun u -> if !first then (first := false; uchar_uppercase u) else u)
      s

  let uncapitalize s =
    let first = ref true in
    normalize_generic ~keep_ascii:false
      (fun u -> if !first then (first := false; uchar_lowercase u) else u)
      s

  let is_capitalized s =
    s <> "" &&
    uchar_is_uppercase (Uchar.utf_decode_uchar (String.get_utf_8_uchar s 0))

  (* Characters allowed in identifiers after normalization is applied.
     Currently:
       - ASCII letters, underscore
       - Latin-9 letters, represented in NFC
       - ASCII digits, single quote (but not as first character)
       - dot if [with_dot] = true
  *)
  let uchar_valid_in_identifier ~with_dot u =
    let c = Uchar.to_int u in
    if c < 0x80 then
         c >= 97 (* a *) && c <= 122 (* z *)
      || c >= 65 (* A *) && c <= 90 (* Z *)
      || c >= 48 (* 0 *) && c <= 57 (* 9 *)
      || c = 95 (* underscore *)
      || c = 39 (* single quote *)
      || (with_dot && c = 46) (* dot *)
    else
      Hashtbl.mem known_chars u

  let uchar_not_identifier_start u =
    let c = Uchar.to_int u in
       c >= 48 (* 0 *) && c <= 57 (* 9 *)
    || c = 39  (* single quote *)

  (* Check whether a normalized string is a valid OCaml identifier. *)

  type validation_result =
    | Valid
    | Invalid_character of Uchar.t   (** Character not allowed *)
    | Invalid_beginning of Uchar.t   (** Character not allowed as first char *)

  let validate_identifier ?(with_dot=false) s =
    let rec check i =
      if i >= String.length s then Valid else begin
        let d = String.get_utf_8_uchar s i in
        let u = Uchar.utf_decode_uchar d in
        let i' = i + Uchar.utf_decode_length d in
        if not (uchar_valid_in_identifier ~with_dot u) then
          Invalid_character u
        else if i = 0 && uchar_not_identifier_start u then
          Invalid_beginning u
        else
          check i'
      end
    in check 0

  let is_valid_identifier s =
    validate_identifier s = Valid

  let starts_like_a_valid_identifier s =
    s <> "" &&
    (let u = Uchar.utf_decode_uchar (String.get_utf_8_uchar s 0) in
     uchar_valid_in_identifier ~with_dot:false u
     && not (uchar_not_identifier_start u))

  let is_lowercase s =
    let rec is_lowercase_at len s n =
      if n >= len then true
      else
        let d = String.get_utf_8_uchar s n in
        let u = Uchar.utf_decode_uchar d in
        (uchar_valid_in_identifier ~with_dot:false  u)
        && not (uchar_is_uppercase u)
        && is_lowercase_at len s (n+Uchar.utf_decode_length d)
    in
    is_lowercase_at (String.length s) s 0
end


(* List functions *)

let map_end f l1 l2 = List.map_end ~f l1 l2

let rev_map_end f l1 l2 =
  let rec rmap_f accu = function
    | [] -> accu
    | hd::tl -> rmap_f (f hd :: accu) tl
  in
  rmap_f l2 l1

let rec map_left_right f = function
    [] -> []
  | hd::tl -> let res = f hd in res :: map_left_right f tl

let for_all2 pred l1 l2 = List.for_all2 ~f:pred l1 l2

let replicate_list = List.replicate

let list_remove x = List.remove ~phys:false x

let rec split_last = function
    [] -> assert false
  | [x] -> ([], x)
  | hd :: tl ->
      let (lst, last) = split_last tl in
      (hd :: lst, last)

(* Options *)

let may f x = Option.iter ~f x
let may_map f x = Option.map ~f x

(* File functions *)

let remove_file filename =
  try
    if Sys.is_regular_file filename
    then Sys.remove filename
  with Sys_error _msg -> ()

let rec split_path_and_prepend path acc =
  match Filename.dirname path with
  | dir when dir = path ->
    let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') in
    let dir =
      if not Sys.unix && String.length dir > 2 && is_letter dir.[0] && dir.[1] = ':'
      then
        (* We do two things here:
            - We use an uppercase letter to match Dune's behavior
            - We also add the separator ousrselves because [Filename.concat]
            does not if its first argument is of the form ["C:"] *)
        Printf.sprintf "%c:%s"
          (Char.uppercase_ascii dir.[0])
          Filename.dir_sep
      else dir
    in
    dir :: acc
  | dir -> split_path_and_prepend dir (Filename.basename path :: acc)

let split_path path = split_path_and_prepend path []

(* Deal with case insensitive FS *)

external fs_exact_case : string -> string = "ml_merlin_fs_exact_case"
external fs_exact_case_basename: string -> string option = "ml_merlin_fs_exact_case_basename"

(* A replacement for sys_file_exists that makes use of stat_cache *)
module Exists_in_directory = File_cache.Make(struct
    let cache_name = "Exists_in_directory"
    type t = string -> bool
    let read dir =
      if Sys.file_exists dir &&
         Sys.is_directory dir
      then
        let cache = Hashtbl.create 4 in
        (fun filename ->
           match Hashtbl.find cache filename with
           | x -> x
           | exception Not_found ->
             let exists = Sys.file_exists (Filename.concat dir filename) in
             Hashtbl.add cache filename exists;
             exists)
      else (fun _ -> false)
  end)

let exact_file_exists ~dirname ~basename =
  Exists_in_directory.read dirname basename &&
  let path = Filename.concat dirname basename in
  match fs_exact_case_basename path with
  | None ->
    let path' = fs_exact_case path in
    path == path' || (* only on macos *) basename = Filename.basename path'
  | Some bn ->
    (* only on windows *)
    basename = bn

let canonicalize_filename ?cwd path =
  let parts =
    match split_path path with
    | dot :: rest when dot = Filename.current_dir_name ->
      split_path_and_prepend (match cwd with None -> Sys.getcwd () | Some c -> c) rest
    | parts -> parts
  in
  let goup path = function
    | dir when dir = Filename.parent_dir_name ->
      (match path with _ :: t -> t | [] -> [])
    | dir when dir = Filename.current_dir_name ->
      path
    | dir -> dir :: path
  in
  let parts = List.rev (List.fold_left ~f:goup ~init:[] parts) in
  let filename_concats = function
    | [] -> ""
    | root :: subs -> List.fold_left ~f:Filename.concat ~init:root subs
  in
  fs_exact_case (filename_concats parts)

let rec expand_glob ~filter acc root = function
  | [] -> root :: acc
  | Glob.Wildwild :: _tl -> (* FIXME: why is tl not used? *)
    let rec append acc root =
      let items = try Sys.readdir root with Sys_error _ -> [||] in
      let process acc dir =
        let filename = Filename.concat root dir in
        if filter filename
        then append (filename :: acc) filename
        else acc
      in
      Array.fold_left process (root :: acc) items
    in
    append acc root
  | Glob.Exact component :: tl ->
    let filename = Filename.concat root component in
    expand_glob ~filter acc filename tl
  | pattern :: tl ->
    let items = try Sys.readdir root with Sys_error _ -> [||] in
    let process acc dir =
      if Glob.match_pattern pattern dir then
        let root' = Filename.concat root dir in
        if filter root' then
          expand_glob ~filter acc root' tl
        else acc
      else acc
    in
    Array.fold_left process acc items

let expand_glob ?(filter=fun _ -> true) path acc =
  match split_path path with
  | [] -> acc
  | root :: subs ->
    let patterns = List.map ~f:Glob.compile_pattern subs in
    expand_glob ~filter acc root patterns

let find_in_path path name =
  canonicalize_filename
  begin
    if not (Filename.is_implicit name) then
      if exact_file_exists
          ~dirname:(Filename.dirname name)
          ~basename:(Filename.basename name)
      then name
      else raise Not_found
    else List.find_map path ~f:(fun dirname ->
        if exact_file_exists ~dirname ~basename:name
        then Some (Filename.concat dirname name)
        else None
      )
  end

let find_in_path_rel path name =
  let rec simplify s =
    let open Filename in
    let base = basename s in
    let dir = dirname s in
    if dir = s then dir
    else if base = current_dir_name then simplify dir
    else concat (simplify dir) base
  in
  let rec try_dir = function
    | [] -> raise Not_found
    | dir::rem ->
      let dir = simplify dir in
      if Exists_in_directory.read dir name
      then Filename.concat dir name
      else try_dir rem
  in try_dir path

let normalized_unit_filename = String.uncapitalize_ascii

let find_in_path_normalized ?(fallback="") path name =
  let has_fallback = fallback <> "" in
  canonicalize_filename
  begin
    let uname = normalized_unit_filename name in
    let ufallback = normalized_unit_filename fallback in
    List.find_map path ~f:(fun dirname ->
        if exact_file_exists ~dirname ~basename:uname
        then Some (Filename.concat dirname uname)
        else if exact_file_exists ~dirname ~basename:name
        then Some (Filename.concat dirname name)
        else
          let () = Logger.log
            ~section:"locate"
            ~title:"find_in_path_uncap"
            "Failed to load %s/%s" dirname name
          in
          if has_fallback && exact_file_exists ~dirname ~basename:ufallback
        then Some (Filename.concat dirname ufallback)
        else if has_fallback && exact_file_exists ~dirname ~basename:fallback
        then Some (Filename.concat dirname fallback)
        else None
      )
  end

(* Expand a -I option: if it starts with +, make it relative to the standard
   library directory *)

let expand_directory alt s =
  if String.length s > 0 && s.[0] = '+'
  then Filename.concat alt
                       (String.sub s ~pos:1 ~len:(String.length s - 1))
  else s

(* Hashtable functions *)

let create_hashtable size init =
  let tbl = Hashtbl.create size in
  List.iter ~f:(fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

(* File copy *)

let copy_file ic oc =
  let buff = Bytes.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then () else (output oc buff 0 n; copy())
  in copy()

let copy_file_chunk ic oc len =
  let buff = Bytes.create 0x1000 in
  let rec copy n =
    if n <= 0 then () else begin
      let r = input ic buff 0 (min n 0x1000) in
      if r = 0 then raise End_of_file else (output oc buff 0 r; copy(n-r))
    end
  in copy len

let string_of_file ic =
  let b = Buffer.create 0x10000 in
  let buff = Bytes.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then Buffer.contents b else
      (Buffer.add_subbytes b buff 0 n; copy())
  in copy()

let output_to_file_via_temporary ?(mode = [Open_text]) filename fn =
  let (temp_filename, oc) =
    Filename.open_temp_file
       ~mode (*~perms:0o666*) ~temp_dir:(Filename.dirname filename)
       (Filename.basename filename) ".tmp" in
    (* The 0o666 permissions will be modified by the umask.  It's just
       like what [open_out] and [open_out_bin] do.
       With temp_dir = dirname filename, we ensure that the returned
       temp file is in the same directory as filename itself, making
       it safe to rename temp_filename to filename later.
       With prefix = basename filename, we are almost certain that
       the first generated name will be unique.  A fixed prefix
       would work too but might generate more collisions if many
       files are being produced simultaneously in the same directory. *)
  match fn temp_filename oc with
  | res ->
      close_out oc;
      begin try
        Sys.rename temp_filename filename; res
      with exn ->
        remove_file temp_filename; raise exn
      end
  | exception exn ->
      close_out oc; remove_file temp_filename; raise exn

(* Reading from a channel *)

let input_bytes ic n =
  let result = Bytes.create n in
  really_input ic result 0 n;
  result

(* Integer operations *)

let rec log2 n =
  if n <= 1 then 0 else 1 + log2(n asr 1)

let align n a =
  if n >= 0 then (n + a - 1) land (-a) else n land (-a)

let no_overflow_add a b = (a lxor b) lor (a lxor (lnot (a+b))) < 0

let no_overflow_sub a b = (a lxor (lnot b)) lor (b lxor (a-b)) < 0

(* Taken from Hacker's Delight, chapter "Overflow Detection" *)
let no_overflow_mul a b =
  not ((a = min_int && b < 0) || (b <> 0 && (a * b) / b <> a))

let no_overflow_lsl a k =
  0 <= k && k < Sys.word_size - 1 && min_int asr k <= a && a <= max_int asr k

let letter_of_int n =
  let letter = String.make 1 (Char.chr (Char.code 'a' + n mod 26)) in
  let num = n / 26 in
  if num = 0 then letter
  else letter ^ Int.to_string num

module Int_literal_converter = struct
  (* To convert integer literals, allowing max_int + 1 (PR#4210) *)
  let cvt_int_aux str neg of_string =
    if String.length str = 0 || str.[0]= '-'
    then of_string str
    else neg (of_string ("-" ^ str))
  let int s = cvt_int_aux s (~-) int_of_string
  let int32 s = cvt_int_aux s Int32.neg Int32.of_string
  let int64 s = cvt_int_aux s Int64.neg Int64.of_string
  let nativeint s = cvt_int_aux s Nativeint.neg Nativeint.of_string
end

(* [find_first_mono p] assumes that there exists a natural number
   N such that [p] is false on [0; N[ and true on [N; max_int], and
   returns this N. (See misc.mli for the detailed specification.) *)
let find_first_mono =
  let rec find p ~low ~jump ~high =
    (* Invariants:
       [low, jump, high] are non-negative with [low < high],
       [p low = false],
       [p high = true]. *)
    if low + 1 = high then high
    (* ensure that [low + jump] is in ]low; high[ *)
    else if jump < 1 then find p ~low ~jump:1 ~high
    else if jump >= high - low then find p ~low ~jump:((high - low) / 2) ~high
    else if p (low + jump) then
      (* We jumped too high: continue with a smaller jump and lower limit *)
      find p ~low:low ~jump:(jump / 2) ~high:(low + jump)
    else
      (* we jumped too low:
         continue from [low + jump] with a larger jump *)
      let next_jump = max jump (2 * jump) (* avoid overflows *) in
      find p ~low:(low + jump) ~jump:next_jump ~high
  in
  fun p ->
    if p 0 then 0
    else find p ~low:0 ~jump:1 ~high:max_int

(* String operations *)

(* let split_null_terminated s =
  let[@tail_mod_cons] rec discard_last_sep = function
    | [] | [""] -> []
    | x :: xs -> x :: discard_last_sep xs
  in
  discard_last_sep (String.split_on_char ~sep:' ' s) *)

(* let concat_null_terminated = function
  | [] -> ""
  | l -> String.concat ~sep:" " (l @ [""]) *)

let chop_extension_if_any fname =
  try Filename.chop_extension fname with Invalid_argument _ -> fname

let chop_extensions file =
  let dirname = Filename.dirname file and basename = Filename.basename file in
  try
    let pos = String.index basename '.' in
    let basename = String.sub basename ~pos:0 ~len:pos in
    if Filename.is_implicit file && dirname = Filename.current_dir_name then
      basename
    else
      Filename.concat dirname basename
  with Not_found -> file

let search_substring pat str start =
  let rec search i j =
    if j >= String.length pat then i
    else if i + j >= String.length str then raise Not_found
    else if str.[i + j] = pat.[j] then search i (j+1)
    else search (i+1) 0
  in search start 0

let replace_substring ~before ~after str =
  let rec search acc curr =
    match search_substring before str curr with
      | next ->
         let prefix = String.sub str ~pos:curr ~len:(next - curr) in
         search (prefix :: acc) (next + String.length before)
      | exception Not_found ->
        let suffix = String.sub str ~pos:curr ~len:(String.length str - curr) in
        List.rev (suffix :: acc)
  in String.concat ~sep:after (search [] 0)


let rev_split_string cond s =
  let rec split1 res i =
    if i >= String.length s then res else begin
      if cond s.[i] then
        split1 res (i+1)
      else
        split2 res i (i+1)
    end
  and split2 res i j =
    if j >= String.length s then String.sub s ~pos:i ~len:(j-i) :: res else begin
      if cond s.[j] then
        split1 (String.sub s ~pos:i ~len:(j-i) :: res) (j+1)
      else
        split2 res i (j+1)
    end
  in split1 [] 0

let rev_split_words s =
  let helper = function
    | ' ' | '\t' | '\r' | '\n' -> true
    | _ -> false
  in
  rev_split_string helper s

let rev_string_split ~on s =
  rev_split_string ((=) on) s

let get_ref r =
  let v = !r in
  r := []; v

let set_or_ignore f opt x =
  match f x with
  | None -> ()
  | Some y -> opt := Some y

let fst3 (x, _, _) = x
let snd3 (_,x,_) = x
let thd3 (_,_,x) = x

let fst4 (x, _, _, _) = x
let snd4 (_,x,_, _) = x
let thd4 (_,_,x,_) = x
let for4 (_,_,_,x) = x

let cut_at s c =
  let pos = String.index s c in
  String.sub s ~pos:0 ~len:pos,
  String.sub s ~pos:(pos+1) ~len:(String.length s - pos - 1)

let ordinal_suffix n =
  let teen = (n mod 100)/10 = 1 in
  match n mod 10 with
  | 1 when not teen -> "st"
  | 2 when not teen -> "nd"
  | 3 when not teen -> "rd"
  | _ -> "th"

(* Color support handling *)
module Color = struct
  external isatty : out_channel -> bool = "caml_sys_isatty"

  (* reasonable heuristic on whether colors should be enabled *)
  let should_enable_color () =
    let term = try Sys.getenv "TERM" with Not_found -> "" in
    term <> "dumb"
    && term <> ""
    && isatty stderr

  type setting = Auto | Always | Never

  let default_setting = Auto
  let enabled = ref true

end

(* Terminal styling handling *)
module Style = struct
  (* use ANSI color codes, see https://en.wikipedia.org/wiki/ANSI_escape_code *)
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

  let ansi_of_color = function
    | Black -> "0"
    | Red -> "1"
    | Green -> "2"
    | Yellow -> "3"
    | Blue -> "4"
    | Magenta -> "5"
    | Cyan -> "6"
    | White -> "7"

  let code_of_style = function
    | FG c -> "3" ^ ansi_of_color c
    | BG c -> "4" ^ ansi_of_color c
    | Bold -> "1"
    | Reset -> "0"

  let ansi_of_style_l l =
    let s = match l with
      | [] -> code_of_style Reset
      | [s] -> code_of_style s
      | _ -> String.concat ~sep:";" (List.map ~f:code_of_style l)
    in
    "\x1b[" ^ s ^ "m"


  type Format.stag += Style of style list

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

  let no_markup stl = { ansi = stl; text_close = ""; text_open = "" }

  let default_styles = {
      warning = no_markup [Bold; FG Magenta];
      error = no_markup [Bold; FG Red];
      loc = no_markup [Bold];
      hint = no_markup [Bold; FG Blue];
      inline_code= { ansi=[Bold]; text_open = {|"|}; text_close = {|"|} }
    }

  let cur_styles = ref default_styles
  let get_styles () = !cur_styles
  let set_styles s = cur_styles := s

  (* map a tag to a style, if the tag is known.
     @raise Not_found otherwise *)
  let style_of_tag s = match s with
    | Format.String_tag "error" ->  (!cur_styles).error
    | Format.String_tag "warning" ->(!cur_styles).warning
    | Format.String_tag "loc" -> (!cur_styles).loc
    | Format.String_tag "hint" -> (!cur_styles).hint
    | Format.String_tag "inline_code" -> (!cur_styles).inline_code
    | Style s -> no_markup s
    | _ -> raise Not_found

  let as_inline_code printer ppf x =
    let open Format_doc in
    pp_open_stag ppf (Format.String_tag "inline_code");
    printer ppf x;
    pp_close_stag ppf ()

  let inline_code ppf s = as_inline_code Format_doc.pp_print_string ppf s

  (* either prints the tag of [s] or delegates to [or_else] *)
  let mark_open_tag ~or_else s =
    try
      let style = style_of_tag s in
      if !Color.enabled then ansi_of_style_l style.ansi else style.text_open
    with Not_found -> or_else s

  let mark_close_tag ~or_else s =
    try
      let style = style_of_tag s in
      if !Color.enabled then ansi_of_style_l [Reset] else style.text_close
    with Not_found -> or_else s

  (* add tag handling to formatter [ppf] *)
  let set_tag_handling ppf =
    let open Format in
    let functions = pp_get_formatter_stag_functions ppf () in
    let functions' = {functions with
      mark_open_stag=(mark_open_tag ~or_else:functions.mark_open_stag);
      mark_close_stag=(mark_close_tag ~or_else:functions.mark_close_stag);
    } in
    pp_set_mark_tags ppf true; (* enable tags *)
    pp_set_formatter_stag_functions ppf functions';
    ()

  let setup =
    let first = ref true in (* initialize only once *)
    let formatter_l =
      [Format.std_formatter; Format.err_formatter; Format.str_formatter]
    in
    let enable_color = function
      | Color.Auto -> Color.should_enable_color ()
      | Color.Always -> true
      | Color.Never -> false
    in
    fun o ->
      if !first then (
        first := false;
        Format.set_mark_tags true;
        List.iter ~f:set_tag_handling formatter_l;
        Color.enabled := (match o with
          | Some s -> enable_color s
          | None -> enable_color Color.default_setting)
      );
      ()
end

let edit_distance a b cutoff =
  let la, lb = String.length a, String.length b in
  let cutoff =
    (* using max_int for cutoff would cause overflows in (i + cutoff + 1);
       we bring it back to the (max la lb) worstcase *)
    Int.min (Int.max la lb) cutoff in
  if abs (la - lb) > cutoff then None
  else begin
    (* initialize with 'cutoff + 1' so that not-yet-written-to cases have
       the worst possible cost; this is useful when computing the cost of
       a case just at the boundary of the cutoff diagonal. *)
    let m = Array.make_matrix (la + 1) (lb + 1) (cutoff + 1) in
    m.(0).(0) <- 0;
    for i = 1 to la do
      m.(i).(0) <- i;
    done;
    for j = 1 to lb do
      m.(0).(j) <- j;
    done;
    for i = 1 to la do
      for j = Int.max 1 (i - cutoff - 1) to Int.min lb (i + cutoff + 1) do
        let cost = if a.[i-1] = b.[j-1] then 0 else 1 in
        let best =
          (* insert, delete or substitute *)
          Int.min (1 + Int.min m.(i-1).(j) m.(i).(j-1)) (m.(i-1).(j-1) + cost)
        in
        let best =
          (* swap two adjacent letters; we use "cost" again in case of
             a swap between two identical letters; this is slightly
             redundant as this is a double-substitution case, but it
             was done this way in most online implementations and
             imitation has its virtues *)
          if not (i > 1 && j > 1 && a.[i-1] = b.[j-2] && a.[i-2] = b.[j-1])
          then best
          else Int.min best (m.(i-2).(j-2) + cost)
        in
        m.(i).(j) <- best
      done;
    done;
    let result = m.(la).(lb) in
    if result > cutoff
    then None
    else Some result
  end

let spellcheck env name =
  let cutoff =
    match String.length name with
      | 1 | 2 -> 0
      | 3 | 4 -> 1
      | 5 | 6 -> 2
      | _ -> 3
  in
  let compare target acc head =
    match edit_distance target head cutoff with
      | None -> acc
      | Some dist ->
         let (best_choice, best_dist) = acc in
         if dist < best_dist then ([head], dist)
         else if dist = best_dist then (head :: best_choice, dist)
         else acc
  in
  let env = List.sort_uniq ~cmp:(fun s1 s2 -> String.compare s2 s1) env in
  fst (List.fold_left ~f:(compare name) ~init:([], max_int) env)


let did_you_mean ppf get_choices =
  let open Format_doc in
  (* flush now to get the error report early, in the (unheard of) case
     where the search in the get_choices function would take a bit of
     time; in the worst case, the user has seen the error, she can
     interrupt the process before the spell-checking terminates. *)
  fprintf ppf "@?";
  match get_choices () with
  | [] -> ()
  | choices ->
    let rest, last = split_last choices in
     fprintf ppf "@\n@[@{<hint>Hint@}: Did you mean %a%s%a?@]"
       (pp_print_list ~pp_sep:comma Style.inline_code) rest
       (if rest = [] then "" else " or ")
       Style.inline_code last

let print_see_manual ppf manual_section =
  let open Format_doc in
  fprintf ppf "(see manual section %a)"
    (pp_print_list ~pp_sep:(fun f () -> pp_print_char f '.') pp_print_int)
    manual_section

let time_spent () =
  let open Unix in
  let t = times () in
  ((t.tms_utime +. t.tms_stime +. t.tms_cutime +. t.tms_cstime) *. 1000.0)

let normalise_eol s =
  let b = Buffer.create 80 in
    for i = 0 to String.length s - 1 do
      if s.[i] <> '\r' then Buffer.add_char b s.[i]
    done;
    Buffer.contents b

let unitname filename =
  let unitname =
    try String.sub filename ~pos:0 ~len:(String.index filename '.')
    with Not_found -> filename
  in
  String.capitalize unitname

(* [modules_in_path ~ext path] lists ocaml modules corresponding to
                    * filenames with extension [ext] in given [path]es.
                    * For instance, if there is file "a.ml","a.mli","b.ml" in ".":
                    * - modules_in_path ~ext:".ml" ["."] returns ["A";"B"],
                    * - modules_in_path ~ext:".mli" ["."] returns ["A"] *)
let modules_in_path ~ext path =
  let seen = Hashtbl.create 7 in
  List.fold_left ~init:[] path
    ~f:begin fun results dir ->
      try
        Array.fold_left
          begin fun results file ->
            if Filename.check_suffix file ext
            then let name = Filename.chop_extension file in
              (if Hashtbl.mem seen name
               then results
               else
                 (Hashtbl.add seen name (); String.capitalize name :: results))
            else results
          end results (Sys.readdir dir)
      with Sys_error _ -> results
    end

module String = struct
  include CamlString
  module Ord = struct
    type t = string
    let compare = String.compare
  end
  module Set = Set.Make (Ord)
  module Map = Map.Make (Ord)
  module Tbl = Hashtbl.Make (struct
      type t = string
      let equal (x : string) (y : string) : bool = (x = y)
      let hash = Hashtbl.hash
    end)
end


type filepath = string
type modname = string
type crcs = (modname * Digest.t option) list

type alerts = string String.Map.t
