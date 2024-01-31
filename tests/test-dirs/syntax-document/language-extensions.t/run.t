Syntax-Documentation Test - Language-extentions
  $ alias syn_doc="$MERLIN single syntax-document -position "

Recursive definition of values
  $ cat > main.ml << EOF
  > let rec name1 = 1 :: name2 and name2 = 2 :: name1
  > EOF

on rec
  $ syn_doc 1:6 \
  > -filename ./main.ml < ./main.ml | jq '.value.name'
  "Recursive value definition"

Recursive modules
  $ cat > rec-modules.ml << EOF
  > module rec A : sig
  >   type t = Leaf of string | Node of ASet.t
  >   val compare: t -> t -> int
  > end = struct
  > type t = Leaf of string | Node of ASet.t
  > let compare t1 t2 =
  >   match (t1, t2) with
  >    | (Leaf s1, Leaf s2) -> Stdlib.compare s1 s2
  >    | (Leaf _, Node _) -> 1
  >    | (Node _, Leaf _) -> -1
  >    | (Node n1, Node n2) -> ASet.compare n1 n2
  > end
  > and ASet : Set.S with type elt = A.t
  > = Set.Make(A)
  > module B = struct
  >    module rec A : sig
  >      type t
  >      val empty: t
  >    end = struct
  >      type t = Empty | Node of int * t * t
  >      let empty = Empty
  >    end
  >  end
  > EOF
on rec
  $ syn_doc 1:9 \
  > -filename ./rec-modules.ml < ./rec-modules.ml | jq '.value.name'
  "Recursive module"
On type t = Leaf of stri...
  $ syn_doc 5:5 \
  > -filename ./rec-modules.ml < ./rec-modules.ml | jq '.value.name'
  "Variant Type"
On rec .. Nested recurvise module
  $ syn_doc 16:12 \
  > -filename ./rec-modules.ml < ./rec-modules.ml | jq '.value.name'
  "Recursive module"


Recovering the type of a module
  $ cat > rec-mod-type.ml << EOF 
  > module type MYHASH = sig
  >   include module type of struct include Hashtbl end
  >   val replace: ('a, 'b) t -> 'a -> 'b -> unit
  > end
  > module MySet : module type of Set = struct
  > end
  > EOF
on module type of..
  $ syn_doc 2:23 \
  > -filename ./rec-mod-type.ml < ./rec-mod-type.ml | jq '.value.name'
  "Recovering module type"
on module type of..
  $ syn_doc 5:28 \
  > -filename ./rec-mod-type.ml < ./rec-mod-type.ml | jq '.value.name'
  "Recovering module type"


// Signature Substitutions
  $ cat > sig-subs.ml << EOF
  > module type Printable = sig
  >    type t
  >    val print : Format.formatter -> t -> unit
  >  end
  >  module type Comparable = sig
  >    type t
  >    val compare : t -> t -> int
  >  end
  >  module type PrintableComparable = sig
  >    include Printable
  >    include Comparable with type t := t
  >  end
  >  module type S = sig
  >    type t
  >    module Sub : sig
  >      type outer := t
  >      type t
  >      val to_outer : t -> outer
  >    end
  >  end
  >  module type ENDO = sig
  >    module type T
  >    module F: T -> T
  >  end
  > module Endo(X: sig module type T end): ENDO with module type T = X.T =
  >  struct
  >      module type T = X.T
  >      module F(X:T) = X
  >  end
  > EOF
// Destructive substitutions
On 'with':
  $ syn_doc 11:24 -filename ./sig-subs.ml < ./sig-subs.ml | jq '.value'
  "No documentation found"

On 'type t :='
  $ syn_doc 11:27 -filename ./sig-subs.ml < ./sig-subs.ml | jq '.value.name'
  "Destructive substitution"
  $ syn_doc 11:32 -filename ./sig-subs.ml < ./sig-subs.ml | jq '.value.name'
  "Destructive substitution"
  $ syn_doc 11:34 -filename ./sig-subs.ml < ./sig-subs.ml | jq '.value.name'
  "Destructive substitution"
On '... t'
  $ syn_doc 11:37 -filename ./sig-subs.ml < ./sig-subs.ml | jq '.value'
  "No documentation found"
On type t
  $ syn_doc 2:9 -filename ./sig-subs.ml < ./sig-subs.ml | jq '.value.name'
  "Abstract Type"
// Local substitutions
  $ syn_doc 16:12 -filename ./sig-subs.ml < ./sig-subs.ml | jq '.value.name'
  "Local substitution"
// Module type substitutions
  $ syn_doc 25:57 \
  > -filename ./sig-subs.ml < ./sig-subs.ml | jq '.value.name'
  "Module substitution"


// Types
  $ cat > types.ml << EOF 
  > type a1 = ..
  > type a2 = A
  > type a3 = |
  > type a4 = {x: int}
  > type a5 = int
  > EOF
on type a1..
  $ syn_doc 1:5 \
  > -filename ./types.ml < ./types.ml | jq '.value.name'
  "Extensible Variant Type"
on type a2..
  $ syn_doc 2:5 \
  > -filename ./types.ml < ./types.ml | jq '.value.name'
  "Variant Type"
on type a3..
  $ syn_doc 3:6 \
  > -filename ./types.ml < ./types.ml | jq '.value.name'
  "Empty Variant Type"
on type a4..
  $ syn_doc 4:5 \
  > -filename ./types.ml < ./types.ml | jq '.value.name'
  "Record Type"
on type a5..
  $ syn_doc 5:5 \
  > -filename ./types.ml < ./types.ml | jq '.value'
  "No documentation found"

// Private types
// Extensible
  $ cat > p-types.ml << EOF
  > type b1 = private ..
  > type b2 = private A
  > type b3 = private A of int
  > type b4 = private { x:int }
  > type b5 = private int 
  > module N : sig
  >   type t = private int
  >   val of_int: int -> t
  >   val to_int: t -> int
  > end = struct
  >   type t = int
  >   let of_int n = assert (n >= 0); n
  >   let to_int n = n
  > end
  > EOF
on type b1..
  $ syn_doc 1:14 \
  > -filename ./p-types.ml < ./p-types.ml | jq '.value.name'
  "Private Extensible Variant Type"
on type b2..
  $ syn_doc 2:14 \
  > -filename ./private-types.ml < ./p-types.ml | jq '.value.name'
  "Private Variant Type"
on type b3..
  $ syn_doc 3:14 \
  > -filename ./p-types.ml < ./p-types.ml | jq '.value.name'
  "Private Variant Type"
on type b4..
  $ syn_doc 4:14 \
  > -filename ./p-types.ml < ./p-types.ml | jq '.value.name'
  "Private Record Type"
on b5..
  $ syn_doc 5:14 \
  > -filename ./p-types.ml < ./p-types.ml | jq '.value.name'
  "Private Type Abbreviation"
on type t = private int..
  $ syn_doc 7:14 \
  > -filename ./p-types.ml < ./p-types.ml | jq '.value.name'
  "Private Type Abbreviation"
on type t = int..
  $ syn_doc 11:7 \
  > -filename ./p-types.ml < ./p-types.ml | jq '.value'
  "No documentation found"

// Locally abstract data types
  $ cat > locally-abstract-dt.ml << EOF 
  > let f = fun (type t) (x: t) -> x = x
  > let sort_uniq (type s) (cmp : s -> s -> int) =
  >   let module S = Set.Make(struct type t = s let compare = cmp end) in
  >   fun l ->
  >     S.elements (List.fold_right S.add l S.empty)
  > EOF
// Locally abstract data types
on type t..
  $ syn_doc 1:17 \
  > -filename ./locally-abstract-dt.ml < ./locally-abstract-dt.ml | jq '.value.name'
  "Locally Abstract Type"
On fun..
  $ syn_doc 1:9 \
  > -filename ./locally-abstract-dt.ml < ./locally-abstract-dt.ml | jq '.value'
  "No documentation found"
On x
  $ syn_doc 1:39 \
  > -filename ./locally-abstract-dt.ml < ./locally-abstract-dt.ml | jq '.value'
  "No documentation found"


// First class Modules
  $ cat > first-class-modules.ml << EOF
  > type picture = { x : int; y : int }
  > module type DEVICE = sig
  >   val draw : picture -> unit
  > end
  > let devices : (string, (module DEVICE)) Hashtbl.t = Hashtbl.create 17
  > module SVG = struct end
  > module PNG = struct end
  > let _svg = Hashtbl.add devices "SVG" (module SVG : DEVICE)
  > let _png = Hashtbl.add devices "PNG" (module PNG : SVG)
  > let sort (type s) (module Set : Set.S with type elt = s) l =
  >   Set.elements (List.fold_right Set.add l Set.empty)
  > let make_set (type s) cmp =
  >   let module S = Set.Make(struct
  >     type t = s
  >     let compare = cmp
  >   end) in
  >   (module S : Set.S with type elt = s)
  > EOF
on type picture
  $ syn_doc 1:6 \
  > -filename ./first-class-modules.ml < ./first-class-modules.ml | jq '.value.name'
  "Record Type"
on module SVG..
  $ syn_doc 6:6 \
  > -filename ./first-class-modules.ml < ./first-class-modules.ml | jq '.value'
  "No documentation found"
on (module SVG : DEVICE)
  $ syn_doc 8:43 \
  > -filename ./first-class-modules.ml < ./first-class-modules.ml | jq '.value.name'
  "First class module"
on (module PNG : SVG)
  $ syn_doc 9:43 \
  > -filename ./first-class-modules.ml < ./first-class-modules.ml | jq '.value'
  "No documentation found"
on type t = s..
  $ syn_doc 14:10 \
  > -filename ./first-class-modules.ml < ./first-class-modules.ml | jq '.value'
  "No documentation found"
on (module S : Set.S with type elt = s)
  $ syn_doc 17:2 \
  > -filename ./first-class-modules.ml < ./first-class-modules.ml | jq '.value.name'
  "First class module"

