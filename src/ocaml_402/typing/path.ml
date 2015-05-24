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

type t =
    Pident of Ident.t
  | Pdot of t * string * int
  | Papply of t * t

let nopos = -1

let rec same p1 p2 =
  match (p1, p2) with
    (Pident id1, Pident id2) -> Ident.same id1 id2
  | (Pdot(p1, s1, pos1), Pdot(p2, s2, pos2)) -> s1 = s2 && same p1 p2
  | (Papply(fun1, arg1), Papply(fun2, arg2)) ->
       same fun1 fun2 && same arg1 arg2
  | (_, _) -> false

let rec isfree id = function
    Pident id' -> Ident.same id id'
  | Pdot(p, s, pos) -> isfree id p
  | Papply(p1, p2) -> isfree id p1 || isfree id p2

let rec binding_time = function
    Pident id -> Ident.binding_time id
  | Pdot(p, s, pos) -> binding_time p
  | Papply(p1, p2) -> max (binding_time p1) (binding_time p2)

let kfalse x = false

let rec name ?(paren=kfalse) = function
    Pident id -> Ident.name id
  | Pdot(p, s, pos) ->
      name ~paren p ^ if paren s then ".( " ^ s ^ " )" else "." ^ s
  | Papply(p1, p2) -> name ~paren p1 ^ "(" ^ name ~paren p2 ^ ")"

let rec head = function
    Pident id -> id
  | Pdot(p, s, pos) -> head p
  | Papply(p1, p2) -> assert false

let rec last = function
  | Pident id -> Ident.name id
  | Pdot(_, s, _) -> s
  | Papply(_, p) -> last p

let to_string_list p =
  let rec aux acc = function
    | Pident id -> id.Ident.name :: acc
    | Pdot (p, str, _) -> aux (str :: acc) p
    | _ -> assert false
  in
  aux [] p

module PathOrd = struct
  type path = t
  type t = path
  let rec compare p1 p2 =
    (* must ignore position when comparing paths *)
    if p1 == p2 then 0 else
      match (p1, p2) with
        (Pdot(p1, s1, pos1), Pdot(p2, s2, pos2)) ->
        let c = compare p1 p2 in
        if c <> 0 then c else String.compare s1 s2
      | (Papply(fun1, arg1), Papply(fun2, arg2)) ->
        let c = compare fun1 fun2 in
        if c <> 0 then c else compare arg1 arg2
      | _ -> Pervasives.compare p1 p2
end

module PathMap = Mymap.Make (PathOrd)
module PathSet = Set.Make (PathOrd)
