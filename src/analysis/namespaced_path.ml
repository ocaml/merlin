open Std

module Namespace = struct
  type t = [
    | `Vals
    | `Type
    | `Constr
    | `Mod
    | `Modtype
    | `Functor
    | `Labels
    | `Unknown
    | `Apply
  ]

  let to_tag_string = function
    | `Mod -> ""
    | `Functor -> "[functor]"
    | `Labels -> "[label]"
    | `Constr -> "[cstr]"
    | `Type -> "[type]"
    | `Vals -> "[val]"
    | `Modtype -> "[Mty]"
    | `Unknown -> "[?]"
    | `Apply -> "[functor application]"

  let to_string = function
    | `Mod -> "(module) "
    | `Functor -> "(functor)"
    | `Labels -> "(label) "
    | `Constr -> "(constructor) "
    | `Type -> "(type) "
    | `Vals -> "(value) "
    | `Modtype -> "(module type) "
    | `Unknown -> "(unknown)"
    | `Apply -> "(functor application)"
end

module Ident = struct
  type t =
    | Id of Ident.t
    | String of string

  let name = function
    | Id id -> Ident.name id
    | String s -> s

  let equal mi1 mi2 =
    match mi1, mi2 with
    | Id i1, Id i2 -> Ident.equal i1 i2
    | Id i, String s
    | String s, Id i -> (Ident.name i) = s
    | String s1, String s2 -> s1 = s2
end

type t =
  | TPident of Ident.t * Namespace.t
  | TPdot of t * string * Namespace.t
  | TPapply of t * t
(* type path = (string * namespace) list *)

let rec to_string = function
  | TPident (id, ns) -> Ident.name id ^ Namespace.to_tag_string ns
  | TPdot (p, s, ns) -> to_string p ^ "." ^ s ^ Namespace.to_tag_string ns
  | TPapply (p1, p2) -> to_string p1 ^ "(" ^ to_string p2 ^ ")"

let rec head = function
  | TPident (id, ns) -> id, ns
  | TPdot(p, _, _) -> head p
  | TPapply _ -> assert false

let rec peal_head_exn = function
  | TPident _ -> invalid_arg "peal_head_exn"
  | TPdot(TPident _, s, ns) -> TPident (String s, ns)
  | TPdot(p, s, ns) -> TPdot(peal_head_exn p, s, ns)
  | TPapply (TPident _, _) -> invalid_arg "peal_head_exn"
  | TPapply (p1, p2) -> TPapply (peal_head_exn p1, p2)

let peal_head p =
  try Some (peal_head_exn p)
  with Invalid_argument _ -> None

let rec equal p1 p2 =
  match p1, p2 with
  | TPident (i1, ns1), TPident (i2, ns2) ->
    Ident.equal i1 i2 && ns1 = ns2
  | TPdot(p1, s1, ns1), TPdot(p2, s2, ns2) ->
    s1 = s2 && ns1 = ns2 && equal p1 p2
  | TPapply(p11, p21), TPapply(p12, p22) ->
    equal p11 p12 && equal p21 p22
  | _, _ -> false

let rec rewrite_path ~new_prefix = function
  | TPident (id, ns) -> TPdot(new_prefix, Ident.name id, ns)
  | TPdot(p, s, ns) -> TPdot (rewrite_path ~new_prefix p, s, ns)
  | TPapply (p1, p2) -> TPapply (rewrite_path ~new_prefix p1, p2)

let of_path ~namespace =
  let rec aux ns =
    let open Path in
    function
    | Pident id -> TPident (Id id, ns)
    | Pdot (p, str, _) ->
      (* FIXME: not always `Mod *)
      TPdot (aux `Mod p, str, ns)
    | Papply (p1, p2) ->
      TPapply (aux `Mod p1, aux `Mod p2)
  in
  aux namespace
