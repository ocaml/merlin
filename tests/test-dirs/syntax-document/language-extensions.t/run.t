// Syntax-Documentation Test - Language-extentions
  $ alias syn_doc="$MERLIN single syntax-document -position "

// Recursive definition of values
  $ cat > main.ml << EOF
  > let rec name1 = 1 :: name2 and name2 = 2 :: name1
  > EOF

  $ syn_doc 1:6 \
  > -filename ./main.ml < ./main.ml
  {
    "class": "return",
    "value": {
      "name": "Recursive value definition",
      "description": "Supports a certain class of recursive definitions of non-functional values.",
      "url": "https://v2.ocaml.org/releases/4.14/htmlman/letrecvalues.html"
    },
    "notifications": []
  }


// Recursive modules
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

  $ syn_doc 1:8 \
  > -filename ./rec-modules.ml < ./rec-modules.ml
  {
    "class": "return",
    "value": {
      "name": "Recursive module",
      "description": "A simultaneous definition of modules that can refer recursively to each others.",
      "url": "https://v2.ocaml.org/releases/4.14/htmlman/recursivemodules.html"
    },
    "notifications": []
  }
// Types in modules
  $ syn_doc 5:5 \
  > -filename ./rec-modules.ml < ./rec-modules.ml
  {
    "class": "return",
    "value": {
      "name": "Variant Type",
      "description": "Represent data that may take on multiple different forms.",
      "url": "https://v2.ocaml.org/releases/4.14/htmlman/typedecl.html#ss:typedefs"
    },
    "notifications": []
  }
// Nested recurvise module
  $ syn_doc 16:11 \
  > -filename ./rec-modules.ml < ./rec-modules.ml
  {
    "class": "return",
    "value": {
      "name": "Recursive module",
      "description": "A simultaneous definition of modules that can refer recursively to each others.",
      "url": "https://v2.ocaml.org/releases/4.14/htmlman/recursivemodules.html"
    },
    "notifications": []
  }


// Recovering the type of a module
  $ cat > rec-mod-type.ml << EOF 
  > module type MYHASH = sig
  >   include module type of struct include Hashtbl end
  >   val replace: ('a, 'b) t -> 'a -> 'b -> unit
  > end
  > module MySet : module type of Set = struct
  > end
  > EOF

  $ syn_doc 2:23 \
  > -filename ./rec-mod-type.ml < ./rec-mod-type.ml
  {
    "class": "return",
    "value": {
      "name": "Recovering module type",
      "description": "Expands to the module type (signature or functor type) inferred for the module expression `module-expr`. ",
      "url": "https://v2.ocaml.org/releases/4.14/htmlman/moduletypeof.html"
    },
    "notifications": []
  }

  $ syn_doc 5:29 \
  > -filename ./rec-mod-type.ml < ./rec-mod-type.ml
  {
    "class": "return",
    "value": {
      "name": "Recovering module type",
      "description": "Expands to the module type (signature or functor type) inferred for the module expression `module-expr`. ",
      "url": "https://v2.ocaml.org/releases/4.14/htmlman/moduletypeof.html"
    },
    "notifications": []
  }


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

// Local substitutions
  $ syn_doc 16:12 \
  > -filename ./sig-subs.ml < ./sig-subs.ml
  {
    "class": "return",
    "value": {
      "name": "Local substitution",
      "description": "Behaves like destructive substitution but is introduced during the specification of the signature, and will apply to all the items that follow.",
      "url": "https://v2.ocaml.org/releases/4.14/htmlman/signaturesubstitution.html#ss:local-substitution"
    },
    "notifications": []
  }
 // Module type substitutions
$ syn_doc 26:58 \
> -filename ./sig-subs.ml < ./sig-subs.ml


// Types
  $ cat > types.ml << EOF 
  > type a1 = ..;
  > type a2 = A;
  > type a3 = |;
  > type a4 = {x: int};
  > type a5 = int;
// Extensible Variant types
  $ syn_doc 1:5 \
  > -filename ./types.ml < ./types.ml
  {
    "class": "return",
    "value": {
      "name": "Extensible variant type",
      "description": "Can be extended with new variant constructors using `+=`.",
      "url": "https://v2.ocaml.org/releases/4.14/htmlman/extensiblevariants.html"
    },
    "notifications": []
  }
// Variant types
  $ syn_doc 2:5 \
  > -filename ./types.ml < ./types.ml
  {
    "class": "return",
    "value": {
      "name": "Variant Type",
      "description": "Represent data that may take on multiple different forms.",
      "url": "https://v2.ocaml.org/releases/4.14/htmlman/typedecl.html#ss:typedefs"
    },
    "notifications": []
  }
// Empty variant types
  $ syn_doc 3:5 \
  > -filename ./types.ml < ./types.ml
  {
    "class": "return",
    "value": {
      "name": "Empty Variant type",
      "description": "An empty variant type.",
      "url": "https://v2.ocaml.org/releases/4.14/htmlman/emptyvariants.html"
    },
    "notifications": []
  }
// Record types
  $ syn_doc 4:5 \
  > -filename ./types.ml < ./types.ml
  {
    "class": "return",
    "value": {
      "name": "Record type",
      "description": "Define variants with a fixed set of fields",
      "url": "https://v2.ocaml.org/releases/4.14/htmlman/typedecl.html#ss:typedefs"
    },
    "notifications": []
  }
// Abstract types
  $ syn_doc 5:5 \
  > -filename ./types.ml < ./types.ml
  {
    "class": "return",
    "value": {
      "name": "Abstract type",
      "description": "Define variants with arbitrary data structures, including other variants, records, and functions",
      "url": "https://v2.ocaml.org/releases/4.14/htmlman/typedecl.html#ss:typedefs"
    },
    "notifications": []
  }

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
// Private extensible 
  $ syn_doc 1:14 \
  > -filename ./p-types.ml < ./p-types.ml 
  {
    "class": "return",
    "value": {
      "name": "Private Extensible Variant Type",
      "description": "Prevents new constructors from being declared directly, but allows extension constructors to be referred to in interfaces.",
      "url": "https://v2.ocaml.org/releases/4.14/htmlman/extensiblevariants.html#ss:private-extensible"
    },
    "notifications": []
  }
// Private variant
  $ syn_doc 2:14 \
  > -filename ./private-types.ml < ./p-types.ml 
  {
    "class": "return",
    "value": {
      "name": "Private Type",
      "description": "Can be de-structured normally in pattern-matching but cannot be constructed directly by constructor application.",
      "url": "https://v2.ocaml.org/releases/4.14/htmlman/privatetypes.html#ss:private-types-variant"
    },
    "notifications": []
  }
// Private variant b
  $ syn_doc 3:14 \
  > -filename ./p-types.ml < ./p-types.ml
  {
    "class": "return",
    "value": {
      "name": "Private Type",
      "description": "Can be de-structured normally in pattern-matching but cannot be constructed directly by constructor application.",
      "url": "https://v2.ocaml.org/releases/4.14/htmlman/privatetypes.html#ss:private-types-variant"
    },
    "notifications": []
  }
// Private record
  $ syn_doc 4:14 \
  > -filename ./p-types.ml < ./p-types.ml 
  {
    "class": "return",
    "value": {
      "name": "Private Record Type",
      "description": "Can be de-structured normally in pattern-matching but cannot be constructed directly by constructor application.",
      "url": "https://v2.ocaml.org/releases/4.14/htmlman/privatetypes.html#ss:private-types-variant"
    },
    "notifications": []
  }
// Private types - Abbreviations
  $ syn_doc 5:14 \
  > -filename ./p-types.ml < ./p-types.ml 
  {
    "class": "return",
    "value": {
      "name": "Private Type Abbreviation",
      "description": "Declares a type that is distinct from its implementation type `typexpr`.",
      "url": "https://v2.ocaml.org/releases/4.14/htmlman/privatetypes.html#ss:private-types-abbrev"
    },
    "notifications": []
  }

  $ syn_doc 7:14 \
  > -filename ./p-types.ml < ./p-types.ml
  {
    "class": "return",
    "value": {
      "name": "Private Type Abbreviation",
      "description": "Declares a type that is distinct from its implementation type `typexpr`.",
      "url": "https://v2.ocaml.org/releases/4.14/htmlman/privatetypes.html#ss:private-types-abbrev"
    },
    "notifications": []
  }
// Abstract
  $ syn_doc 11:7 \
  > -filename ./p-types.ml < ./p-types.ml
  {
    "class": "return",
    "value": {
      "name": "Abstract type",
      "description": "Define variants with arbitrary data structures, including other variants, records, and functions",
      "url": "https://v2.ocaml.org/releases/4.14/htmlman/typedecl.html#ss:typedefs"
    },
    "notifications": []
  }


// Locally abstract data types
  $ cat > locally-abstract-dt.ml << EOF 
  > let f = fun (type t) (x: t)
  >  -> x = x
  >  let sort_uniq (type s) (cmp : s -> s -> int) =
  >    let module S = Set.Make(struct type t = s let compare = cmp end) in
  >    fun l ->
  >      S.elements (List.fold_right S.add l S.empty)
  > EOF 

// Locally abstract data types
  $ syn_doc 1:17 \
  > -filename ./locally-abstract-dt.ml < ./locally-abstract-dt.ml
  {
    "class": "return",
    "value": {
      "name": "Locally Abstract Type",
      "description": "Type constructor which is considered abstract in the scope of the sub-expression and replaced by a fresh type variable.",
      "url": "https://v2.ocaml.org/releases/4.14/htmlman/locallyabstract.html"
    },
    "notifications": []
  }
// Locally abstract data types B
$ syn_doc 3:20 \
> -filename ./locally-abstract-dt.ml < ./locally-abstract-dt.ml


