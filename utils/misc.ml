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

let rec map_end f l1 l2 =
  match l1 with
    [] -> l2
  | hd::tl -> f hd :: map_end f tl l2

let rec map_left_right f = function
    [] -> []
  | hd::tl -> let res = f hd in res :: map_left_right f tl

let rec for_all2 pred l1 l2 =
  match (l1, l2) with
    ([], []) -> true
  | (hd1::tl1, hd2::tl2) -> pred hd1 hd2 && for_all2 pred tl1 tl2
  | (_, _) -> false

let rec replicate_list elem n =
  if n <= 0 then [] else elem :: replicate_list elem (n-1)

let rec list_remove x = function
    [] -> []
  | hd :: tl ->
      if hd = x then tl else hd :: list_remove x tl

let rec split_last = function
    [] -> assert false
  | [x] -> ([], x)
  | hd :: tl ->
      let (lst, last) = split_last tl in
      (hd :: lst, last)

let rec samelist pred l1 l2 =
  match (l1, l2) with
  | ([], []) -> true
  | (hd1 :: tl1, hd2 :: tl2) -> pred hd1 hd2 && samelist pred tl1 tl2
  | (_, _) -> false

(* Options *)

let may f = function
    Some x -> f x
  | None -> ()

let may_map f = function
    Some x -> Some (f x)
  | None -> None

(* File functions *)

let find_in_path path name =
  if not (Filename.is_implicit name) then
    if Sys.file_exists name then name else raise Not_found
  else begin
    let rec try_dir = function
      [] -> raise Not_found
    | dir::rem ->
        let fullname = Filename.concat dir name in
        if Sys.file_exists fullname then fullname else try_dir rem
    in try_dir path
  end

let find_in_path_uncap path name =
  let uname = String.uncapitalize name in
  let rec try_dir = function
    [] -> raise Not_found
  | dir::rem ->
      let fullname = Filename.concat dir name
      and ufullname = Filename.concat dir uname in
      if Sys.file_exists ufullname then ufullname
      else if Sys.file_exists fullname then fullname
      else try_dir rem
  in try_dir path

let remove_file filename =
  try
    Sys.remove filename
  with Sys_error msg ->
    ()

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

let rev_split_words s =
  let rec split1 res i =
    if i >= String.length s then res else begin
      match s.[i] with
        ' ' | '\t' | '\r' | '\n' -> split1 res (i+1)
      | _ -> split2 res i (i+1)
    end
  and split2 res i j =
    if j >= String.length s then String.sub s i (j-i) :: res else begin
      match s.[j] with
        ' ' | '\t' | '\r' | '\n' -> split1 (String.sub s i (j-i) :: res) (j+1)
      | _ -> split2 res i (j+1)
    end
  in split1 [] 0

let get_ref r =
  let v = !r in
  r := []; v

let fst3 (x, _, _) = x
let snd3 (_,x,_) = x
let thd3 (_,_,x) = x

let fst4 (x, _, _, _) = x
let snd4 (_,x,_, _) = x
let thd4 (_,_,x,_) = x

        (* [ppf_to_string ()] gives a fresh formatter and a function to easily
         * gets its content as a string *)
let ppf_to_string ?(width=0) () =
  let b = Buffer.create 32 in
  let ppf = Format.formatter_of_buffer b in
  Format.pp_set_margin ppf width;
  ppf,
  (fun () ->
    Format.pp_print_flush ppf ();
    Buffer.contents b)

        (* [lex_strings s f] makes a lexing buffer from the string [s]
         * (like a Lexer.from_string) and call [f] to refill the buffer *)
let lex_strings source refill =
  let pos = ref 0 in
  let len = ref (String.length source) in
  let source = ref source in
  Lexing.from_function
    begin fun buf size ->
      let count = min (!len - !pos) size in
      let count = 
        if count <= 0 then
        begin
          source := refill ();
          len := String.length !source;
          pos := 0;
          min !len size
        end
        else count
      in
      if count <= 0 then 0
      else begin
        String.blit !source !pos buf 0 count;
        pos := !pos + count;
        count
      end
    end

        (* [length_lessthan n l] returns
         *   Some (List.length l) if List.length l <= n
         *   None otherwise *)
let length_lessthan n l = 
  let rec aux i = function
    | _ :: xs when i < n -> aux (succ i) xs
    | [] -> Some i
    | _ -> None
  in
  aux 0 l

       (* [has_prefix p s] returns true iff p is a prefix of s *)
let has_prefix p =
  let l = String.length p in fun s ->
  let l' = String.length s in
  (l' >= l) && 
  (try
     for i = 0 to pred l do
       if s.[i] <> p.[i] then
         raise Not_found
     done;
     true
   with Not_found -> false)

        (* [modules_in_path ~ext path] lists ocaml modules corresponding to
         * filenames with extension [ext] in given [path]es.
         * For instance, if there is file "a.ml","a.mli","b.ml" in ".":
         * - modules_in_path ~ext:".ml" ["."] returns ["A";"B"],
         * - modules_in_path ~ext:".mli" ["."] returns ["A"] *)
let modules_in_path ~ext path = 
  let seen = Hashtbl.create 7 in
  List.fold_left 
  begin fun results dir -> 
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
  end [] path

        (* Remove duplicates from list *)
let list_filter_dup lst =
  let tbl = Hashtbl.create 17 in
  List.rev (List.fold_left (fun a b -> if Hashtbl.mem tbl b then a else (Hashtbl.add tbl b (); b :: a)) [] lst)

