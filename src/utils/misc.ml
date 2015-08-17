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
      Array.fold_left process (root :: acc) items
    in
    append acc root
  | [Glob.Exact component] :: tl ->
    let filename = Filename.concat root component in
    expand_glob ~filter acc filename tl
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
    | StringListRef of string list ref
    | StringList of string list
    | Fun of (unit -> t list)
    | List of t list

  let of_list l = List l

  let of_string_list_ref l = StringListRef l
  let of_string_list l = StringList l
  let of_fun f = Fun f

  let rec to_list k = function
    | List l -> from_list k l
    | Fun f -> from_list k (f ())
    | StringList s -> from_string_list k s
    | StringListRef l -> from_string_list k !l

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

let no_overflow_mul a b = b <> 0 && (a * b) / b = a

let no_overflow_lsl a k = 0 <= k && k < Sys.word_size && min_int asr k <= a && a <= max_int asr k

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

let replace_substring ~before ~after str =
  let rec search acc curr =
    match
      try Some (search_substring before str curr) with Not_found -> None
    with
    | Some next ->
      let prefix = String.sub str curr (next - curr) in
      search (prefix :: acc) (next + String.length before)
    | None ->
      let suffix = String.sub str curr (String.length str - curr) in
      List.rev (suffix :: acc)
  in String.concat after (search [] 0)

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


module LongString = struct
  type t = string array

  let create str_size =
    let tbl_size = str_size / Sys.max_string_length + 1 in
    let tbl = Array.make tbl_size "" in
    for i = 0 to tbl_size - 2 do
      tbl.(i) <- String.create Sys.max_string_length;
    done;
    tbl.(tbl_size - 1) <- String.create (str_size mod Sys.max_string_length);
    tbl

  let length tbl =
    let tbl_size = Array.length tbl in
    Sys.max_string_length * (tbl_size - 1) + String.length tbl.(tbl_size - 1)

  let get tbl ind =
    String.get tbl.(ind / Sys.max_string_length) (ind mod Sys.max_string_length)

  let set tbl ind c =
    String.set tbl.(ind / Sys.max_string_length) (ind mod Sys.max_string_length)
              c

  let blit src srcoff dst dstoff len =
    for i = 0 to len - 1 do
      set dst (dstoff + i) (get src (srcoff + i))
    done

  let output oc tbl pos len =
    for i = pos to pos + len - 1 do
      output_char oc (get tbl i)
    done

  let unsafe_blit_to_bytes src srcoff dst dstoff len =
    for i = 0 to len - 1 do
      String.unsafe_set dst (dstoff + i) (get src (srcoff + i))
    done

  let input_bytes ic len =
    let tbl = create len in
    Array.iter (fun str -> really_input ic str 0 (String.length str)) tbl;
    tbl
end


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

let edit_distance a b cutoff =
  let la, lb = String.length a, String.length b in
  let cutoff =
    (* using max_int for cutoff would cause overflows in (i + cutoff + 1);
       we bring it back to the (max la lb) worstcase *)
    min (max la lb) cutoff in
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
      for j = max 1 (i - cutoff - 1) to min lb (i + cutoff + 1) do
        let cost = if a.[i-1] = b.[j-1] then 0 else 1 in
        let best =
          (* insert, delete or substitute *)
          min (1 + min m.(i-1).(j) m.(i).(j-1)) (m.(i-1).(j-1) + cost)
        in
        let best =
          (* swap two adjacent letters; we use "cost" again in case of
             a swap between two identical letters; this is slightly
             redundant as this is a double-substitution case, but it
             was done this way in most online implementations and
             imitation has its virtues *)
          if not (i > 1 && j > 1 && a.[i-1] = b.[j-2] && a.[i-2] = b.[j-1])
          then best
          else min best (m.(i-2).(j-2) + cost)
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
  fst (List.fold_left ~f:(compare name) ~init:([], max_int) env)

let did_you_mean ppf get_choices =
  (* flush now to get the error report early, in the (unheard of) case
     where the search in the get_choices function would take a bit of
     time; in the worst case, the user has seen the error, she can
     interrupt the process before the spell-checking terminates. *)
  Format.fprintf ppf "@?";
  match get_choices () with
  | [] -> ()
  | choices ->
     let rest, last = split_last choices in
     Format.fprintf ppf "@\nHint: Did you mean %s%s%s?"
       (String.concat ", " rest)
       (if rest = [] then "" else " or ")
       last

(* split a string [s] at every char [c], and return the list of sub-strings *)
let split s c =
  let len = String.length s in
  let rec iter pos to_rev =
    if pos = len then List.rev ("" :: to_rev) else
      match try
              Some ( String.index_from s pos c )
        with Not_found -> None
      with
          Some pos2 ->
            if pos2 = pos then iter (pos+1) ("" :: to_rev) else
              iter (pos2+1) ((String.sub s pos (pos2-pos)) :: to_rev)
        | None -> List.rev ( String.sub s pos (len-pos) :: to_rev )
  in
  iter 0 []

let cut_at s c =
  let pos = String.index s c in
  String.sub s 0 pos, String.sub s (pos+1) (String.length s - pos - 1)

type file_id = Unix.stats option

let file_id filename =
  try Some (Unix.stat filename)
  with _ -> None

let file_id_check a b =
  let open Unix in
  match a, b with
  | None, None -> true
  | Some a, Some b ->
    a.st_mtime = b.st_mtime &&
    a.st_size = b.st_size &&
    a.st_ino = b.st_ino &&
    a.st_dev = b.st_dev
  | Some _, None | None, Some _ -> false

