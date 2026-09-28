module Uid = Ocaml_typing.Shape.Uid
module Ident = Ocaml_typing.Ident
module Uid_set = Ocaml_typing.Shape.Uid.Set
module Union_find = Merlin_index_format.Union_find

let f = Uid_set.union
let uid name = Uid.of_compilation_unit_id (Ident.create_persistent name)
let a = uid "A"
let b = uid "B"
let c = uid "C"
let d = uid "D"

(* Build a store: each uid starts as a singleton root, then the listed pairs
   are unioned together. *)
let store uids unions =
  let store, handles =
    List.fold_left
      (fun (store, handles) u ->
        let store, h = Union_find.new_root store u (Uid_set.singleton u) in
        (store, (u, h) :: handles))
      (Union_find.empty (), [])
      uids
  in
  let handle u = List.assoc u handles in
  List.fold_left
    (fun store (x, y) ->
      let store, _ = Union_find.union ~f store (handle x) (handle y) in
      store)
    store unions

let uid_set = Alcotest.testable Uid_set.print Uid_set.equal

(* Every member of [members] must resolve to exactly the set [members]. *)
let check_class store ~msg members =
  let expected = Uid_set.of_list members in
  List.iter
    (fun u ->
      Alcotest.check uid_set
        (Format.asprintf "%s: related set of %a" msg Uid.print u)
        expected (Union_find.get store u))
    members

(* s1: {A,B,C}; s2: {A}, {B,D}. B is shared and only a link in s1, so the
   relation A~B~C (s1) and B~D (s2) must close into a single class {A,B,C,D}.
   The previous [merge] dropped D from A's and C's class. *)
let transitive_merge () =
  let s1 = store [ a; b; c ] [ (a, b); (a, c) ] in
  let s2 = store [ a; b; d ] [ (b, d) ] in
  let merged = Union_find.merge ~f s1 s2 in
  check_class merged ~msg:"transitive" [ a; b; c; d ]

let self_merge () =
  let s = store [ a; b; c ] [ (a, b); (a, c) ] in
  let merged = Union_find.merge ~f s s in
  check_class merged ~msg:"self" [ a; b; c ]

let disjoint_merge () =
  let s1 = store [ a; b ] [ (a, b) ] in
  let s2 = store [ c; d ] [ (c, d) ] in
  let merged = Union_find.merge ~f s1 s2 in
  check_class merged ~msg:"disjoint/left" [ a; b ];
  check_class merged ~msg:"disjoint/right" [ c; d ];
  Alcotest.(check bool)
    "disjoint classes stay separate" false
    (Uid_set.mem c (Union_find.get merged a))

(* Both stores have A as a lone root but carry different related sets. The old
   [merge] kept only s1's value and dropped s2's; the merged value must be the
   union of both. *)
let conflicting_root_values () =
  let s1 =
    fst (Union_find.new_root (Union_find.empty ()) a (Uid_set.of_list [ a; b ]))
  in
  let s2 =
    fst (Union_find.new_root (Union_find.empty ()) a (Uid_set.of_list [ a; c ]))
  in
  let merged = Union_find.merge ~f s1 s2 in
  Alcotest.check uid_set "merged root value is the union of both"
    (Uid_set.of_list [ a; b; c ])
    (Union_find.get merged a)

let cases =
  ( "union_find_merge",
    Alcotest.
      [ test_case "merges transitive classes across stores" `Quick
          transitive_merge;
        test_case "self-merge preserves classes" `Quick self_merge;
        test_case "disjoint stores keep separate classes" `Quick disjoint_merge;
        test_case "shared root combines conflicting values" `Quick
          conflicting_root_values
      ] )

let () = Alcotest.run "merlin-lib.index_format.union_find" [ cases ]
