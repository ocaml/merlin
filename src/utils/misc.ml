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

(* $Id: misc.ml 12800 2012-07-30 18:59:07Z doligez $ *)

open Std

let () = Findlib.init ()

(* Errors *)

exception Fatal_error

let fatal_error msg =
  prerr_string ">> Fatal error: "; prerr_endline msg; raise Fatal_error

(* Exceptions *)

let try_finally work cleanup =
  let result = (try work () with e -> cleanup (); raise e) in
  cleanup ();
  result
;;

(* List functions *)

let map_end f l1 l2 = List.map_end ~f l1 l2

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

let samelist pred l1 l2 = List.same   ~f:pred l1 l2

(* Options *)

let may f x = Option.iter ~f x
let may_map f x = Option.map ~f x

(* File functions *)

let remove_file filename =
  try  Sys.remove filename
  with Sys_error _msg -> ()

let rec split_path path acc =
  match Filename.dirname path, Filename.basename path with
  | dir, _ when dir = path -> dir :: acc
  | dir, base -> split_path dir (base :: acc)

let canonicalize_filename ?cwd path =
  let parts =
    match split_path path [] with
    | dot :: rest when dot = Filename.current_dir_name ->
      split_path (match cwd with None -> Sys.getcwd () | Some c -> c) rest
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
  filename_concats parts

let rec expand_glob ~filter acc root = function
  | [] -> root :: acc
  | [Glob.Joker; Glob.Joker] :: tl ->
    let rec append acc root =
      let items = try Sys.readdir root with Sys_error _ -> [||] in
      let process acc dir =
        let filename = Filename.concat root dir in
        if filter filename
        then append (filename :: acc) filename
        else acc
      in
      Array.fold_left process acc items
    in
    append acc root
  | [Glob.Exact component] :: tl ->
    let filename = Filename.concat root component in
    if Sys.file_exists filename && filter filename then
      expand_glob ~filter acc filename tl
    else
      acc
  | pattern :: tl ->
    let items = try Sys.readdir root with Sys_error _ -> [||] in
    let process acc dir =
      if Glob.match_pattern dir pattern then
        let root' = Filename.concat root dir in
        if filter root' then
          expand_glob ~filter acc root' tl
        else acc
      else acc
    in
    Array.fold_left process acc items

let expand_glob ?(filter=fun _ -> true) path acc =
  match split_path path [] with
  | [] -> acc
  | root :: subs ->
    let patterns = List.map ~f:Glob.compile_pattern subs in
    expand_glob ~filter acc root patterns

module Path_list = struct
  type t =
    | StringList of string list ref
    | List of t list

  let of_list l = List l

  let of_string_list_ref l = StringList l

  let rec to_list k = function
    | List l -> from_list k l
    | StringList l -> from_string_list k !l

  and from_list k = function
    | t :: l -> to_list (lazy (from_list k l)) t
    | []     -> Lazy.force k

  and from_string_list k = function
    | s :: l -> List.Lazy.Cons (s, lazy (from_string_list k l))
    | [] -> Lazy.force k

  let to_list t = to_list (lazy List.Lazy.Nil) t

  let to_strict_list t = List.Lazy.to_strict (to_list t)
end

let find_in_path path name =
  canonicalize_filename
  begin
    if not (Filename.is_implicit name) then
      if Sys.file_exists name then name else raise Not_found
    else begin
      let rec try_dir = function
        | List.Lazy.Nil -> raise Not_found
        | List.Lazy.Cons (dir, rem) ->
          let fullname = Filename.concat dir name in
          if Sys.file_exists fullname
          then fullname
          else try_dir (Lazy.force rem)
      in try_dir (Path_list.to_list path)
    end
  end

let find_in_path_uncap ?(fallback="") path name =
  let has_fallback = fallback <> "" in
  canonicalize_filename
  begin
    let uname = String.uncapitalize name in
    let ufbck = String.uncapitalize fallback in
    let rec try_dir = function
      | List.Lazy.Nil -> raise Not_found
      | List.Lazy.Cons (dir, rem) ->
          let fullname = Filename.concat dir name in
          let ufullname = Filename.concat dir uname in
          let ufallback = Filename.concat dir ufbck in
          if Sys.file_exists ufullname then ufullname
          else if Sys.file_exists fullname then fullname
          else if has_fallback && Sys.file_exists ufallback then ufallback
          else if has_fallback && Sys.file_exists fallback then fallback
          else try_dir (Lazy.force rem)
    in try_dir (Path_list.to_list path)
  end

(* Expand a -I option: if it starts with +, make it relative to the standard
   library directory *)

let expand_directory alt s =
  if String.length s > 0 && s.[0] = '+'
  then Filename.concat alt
                       (String.sub s 1 (String.length s - 1))
  else s

(* Hashtable functions *)

let create_hashtable size init =
  let tbl = Hashtbl.create size in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

(* File copy *)

let copy_file ic oc =
  let buff = String.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then () else (output oc buff 0 n; copy())
  in copy()

let copy_file_chunk ic oc len =
  let buff = String.create 0x1000 in
  let rec copy n =
    if n <= 0 then () else begin
      let r = input ic buff 0 (min n 0x1000) in
      if r = 0 then raise End_of_file else (output oc buff 0 r; copy(n-r))
    end
  in copy len

let string_of_file ic =
  let b = Buffer.create 0x10000 in
  let buff = String.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then Buffer.contents b else
      (Buffer.add_substring b buff 0 n; copy())
  in copy()



(* Reading from a channel *)

let input_bytes ic n =
  let result = String.create n in
  really_input ic result 0 n;
  result
;;

(* Integer operations *)

let rec log2 n =
  if n <= 1 then 0 else 1 + log2(n asr 1)

let align n a =
  if n >= 0 then (n + a - 1) land (-a) else n land (-a)

let no_overflow_add a b = (a lxor b) lor (a lxor (lnot (a+b))) < 0

let no_overflow_sub a b = (a lxor (lnot b)) lor (b lxor (a-b)) < 0

let no_overflow_lsl a = min_int asr 1 <= a && a <= max_int asr 1

(* String operations *)

let chop_extension_if_any fname =
  try Filename.chop_extension fname with Invalid_argument _ -> fname

let chop_extensions file =
  let dirname = Filename.dirname file and basename = Filename.basename file in
  try
    let pos = String.index basename '.' in
    let basename = String.sub basename 0 pos in
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

let rev_split_string cond s =
  let rec split1 res i =
    if i >= String.length s then res else begin
      if cond s.[i] then
        split1 res (i+1)
      else
        split2 res i (i+1)
    end
  and split2 res i j =
    if j >= String.length s then String.sub s i (j-i) :: res else begin
      if cond s.[j] then
        split1 (String.sub s i (j-i) :: res) (j+1)
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

let fst3 (x,_,_) = x
let snd3 (_,x,_) = x
let thd3 (_,_,x) = x

let fst4 (x,_,_,_) = x
let snd4 (_,x,_,_) = x
let thd4 (_,_,x,_) = x
let fth4 (_,_,_,x) = x

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

let (~:) = Lazy.from_val

let file_mtime filename =
  try Unix.((stat filename).st_mtime)
  with _ -> nan

let file_contents filename =
  let ic = open_in filename in
  try
    let str = String.create 1024 in
    let buf = Buffer.create 1024 in
    let rec loop () =
      match input ic str 0 1024 with
      | 0 -> ()
      | n ->
        Buffer.add_substring buf str 0 n;
        loop ()
    in
    loop ();
    close_in_noerr ic;
    Buffer.contents buf
  with exn ->
    close_in_noerr ic;
    raise exn
