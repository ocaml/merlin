// Syntax-Documentation Test - Language-extentions

// Recursive definition of values
  $ cat > main.ml << EOF
  > let rec name1 = 1 :: name2 and name2 = 2 :: name1
  > EOF

  $ $MERLIN single syntax-document -position 1:6 \
  > -filename ./main.ml < ./main.ml
  {
    "class": "return",
    "value": {
      "name": "Recursive value definition",
      "description": "The `let rec` binding construct, in addition to the definition of recursive functions, also supports a certain class of recursive definitions of non-functional values, such as `let rec name1 = 1 :: name2 and name2 = 2 :: name1 in expr` which binds `name1` to the cyclic list `1::2::1::2::…`, and `name2` to the cyclic list `2::1::2::1::…`",
      "url": "https://v2.ocaml.org/releases/5.1/htmlman/recvalues.html"
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

  $ $MERLIN single syntax-document -position 1:8 \
  > -filename ./rec-modules.ml < ./rec-modules.ml
  {
    "class": "return",
    "value": {
      "name": "Recursive module",
      "description": "Recursive module definitions, introduced by the `module rec …and …` construction, generalize regular module definitions `module module-name = module-expr` and module specifications `module module-name : module-type` by allowing the defining `module-expr` and the `module-type` to refer recursively to the module identifiers being defined.",
      "url": "https://v2.ocaml.org/releases/5.1/htmlman/recursivemodules.html"
    },
    "notifications": []
  }
// Types in modules
  $ $MERLIN single syntax-document -position 5:5 \
  > -filename ./rec-modules.ml < ./rec-modules.ml
  {
    "class": "return",
    "value": {
      "name": "Variant Type",
      "description": "Let's you represent data that may take on multiple different forms.",
      "url": "https://v2.ocaml.org/manual/coreexamples.html#s:tut-recvariants"
    },
    "notifications": []
  }
// Nested recurvise module
  $ $MERLIN single syntax-document -position 16:11 \
  > -filename ./rec-modules.ml < ./rec-modules.ml
  {
    "class": "return",
    "value": {
      "name": "Recursive module",
      "description": "Recursive module definitions, introduced by the `module rec …and …` construction, generalize regular module definitions `module module-name = module-expr` and module specifications `module module-name : module-type` by allowing the defining `module-expr` and the `module-type` to refer recursively to the module identifiers being defined.",
      "url": "https://v2.ocaml.org/releases/5.1/htmlman/recursivemodules.html"
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

  $ $MERLIN single syntax-document -position 2:23 \
  > -filename ./rec-mod-type.ml < ./rec-mod-type.ml
  {
    "class": "return",
    "value": {
      "name": "Recovering module type",
      "description": "The construction `module type of module-expr` expands to the module type (signature or functor type) inferred for the module expression `module-expr`. ",
      "url": "https://v2.ocaml.org/releases/5.1/htmlman/moduypeof.html"
    },
    "notifications": []
  }

  $ $MERLIN single syntax-document -position 5:29 \
  > -filename ./rec-mod-type.ml < ./rec-mod-type.ml
  {
    "class": "return",
    "value": {
      "name": "Recovering module type",
      "description": "The construction `module type of module-expr` expands to the module type (signature or functor type) inferred for the module expression `module-expr`. ",
      "url": "https://v2.ocaml.org/releases/5.1/htmlman/moduypeof.html"
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
  $ $MERLIN single syntax-document -position 11:27 \
  > -filename ./sig-subs.ml < ./sig-subs.ml
  {
    "class": "return",
    "value": {
      "name": "Destructive substitution",
      "description": "Behaves essentially like normal signature constraints, but it additionally removes the redefined type or module from the signature.",
      "url": "https://v2.ocaml.org/releases/5.1/htmlman/signaturesubstitution.html#ss:destructive-substitution"
    },
    "notifications": []
  }
// Local substitutions
  $ $MERLIN single syntax-document -position 16:12 \
  > -filename ./sig-subs.ml < ./sig-subs.ml
  {
    "class": "return",
    "value": {
      "name": "Local substitution",
      "description": "Local substitutions behave like destructive substitutions `(with ... := ...)` but instead of being applied to a whole signature after the fact, they are introduced during the specification of the signature, and will apply to all the items that follow.",
      "url": "https://v2.ocaml.org/releases/5.1/htmlman/signaturesubstitution.html#ss:local-substitution"
    },
    "notifications": []
  }
 // Module type substitutions
$ $MERLIN single syntax-document -position 26:58 \
> -filename ./sig-subs.ml < ./sig-subs.ml


// Types
  $ cat > types.ml << EOF 
  > type a1 = ..;
  > type a2 = A;
  > type a3 = |;
  > type a4 = {x: int};
  > type a5 = int;
// Extensible Variant types
  $ $MERLIN single syntax-document -position 1:5 \
  > -filename ./types.ml < ./types.ml
  {
    "class": "return",
    "value": {
      "name": "Extensible variant type",
      "description": "Can be extended with new variant constructors using +=.",
      "url": "https://v2.ocaml.org/manual/extensiblevariants.html"
    },
    "notifications": []
  }
// Variant types
  $ $MERLIN single syntax-document -position 2:5 \
  > -filename ./types.ml < ./types.ml
  {
    "class": "return",
    "value": {
      "name": "Variant Type",
      "description": "Let's you represent data that may take on multiple different forms.",
      "url": "https://v2.ocaml.org/manual/coreexamples.html#s:tut-recvariants"
    },
    "notifications": []
  }
// Empty variant types
  $ $MERLIN single syntax-document -position 3:5 \
  > -filename ./types.ml < ./types.ml
  {
    "class": "return",
    "value": {
      "name": "Empty Variant type",
      "description": "This extension allows the user to define empty variants.",
      "url": "https://v2.ocaml.org/manual/emptyvariants.html"
    },
    "notifications": []
  }
// Record types
  $ $MERLIN single syntax-document -position 4:5 \
  > -filename ./types.ml < ./types.ml
  {
    "class": "return",
    "value": {
      "name": "Record type",
      "description": "Allows you to define variants with a fixed set of fields, and all of the constructors for a record variant type must have the same fields",
      "url": "https://v2.ocaml.org/manual/extensiblevariants.html"
    },
    "notifications": []
  }
// Abstract types
  $ $MERLIN single syntax-document -position 5:5 \
  > -filename ./types.ml < ./types.ml
  {
    "class": "return",
    "value": {
      "name": "Abstract type",
      "description": "Allows you to define variants with arbitrary data structures, including other variants, records, and functions",
      "url": "https://v2.ocaml.org/manual/extensiblevariants.html"
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
  $ $MERLIN single syntax-document -position 1:14 \
  > -filename ./p-types.ml < ./p-types.ml 
  {
    "class": "return",
    "value": {
      "name": "Private Extensible Type",
      "description": "Enable libraries to reveal , but not all aspects of the implementation of a type to clients of the library",
      "url": "https://v2.ocaml.org/releases/5.1/htmlman/privatetypes.html#start-section"
    },
    "notifications": []
  }
// Private variant
  $ $MERLIN single syntax-document -position 2:14 \
  > -filename ./private-types.ml < ./p-types.ml 
  {
    "class": "return",
    "value": {
      "name": "Private Type",
      "description": "Values of a variant type declared private can be de-structured normally in pattern-matching. However, values of these types cannot be constructed directly by constructor application.",
      "url": "https://v2.ocaml.org/releases/5.1/htmlman/privatetypes.html#ss:private-types-variant"
    },
    "notifications": []
  }
// Private variant b
  $ $MERLIN single syntax-document -position 3:14 \
  > -filename ./p-types.ml < ./p-types.ml
  {
    "class": "return",
    "value": {
      "name": "Private Type",
      "description": "Values of a variant type declared private can be de-structured normally in pattern-matching. However, values of these types cannot be constructed directly by constructor application.",
      "url": "https://v2.ocaml.org/releases/5.1/htmlman/privatetypes.html#ss:private-types-variant"
    },
    "notifications": []
  }
// Private record
  $ $MERLIN single syntax-document -position 4:14 \
  > -filename ./p-types.ml < ./p-types.ml 
  {
    "class": "return",
    "value": {
      "name": "Private Record Type",
      "description": "Values of a record type declared private can be de-structured via the expr . field notation. However, values of these types cannot be constructed directly by record construction.",
      "url": "https://v2.ocaml.org/releases/5.1/htmlman/privatetypes.html#ss:private-types-variant"
    },
    "notifications": []
  }
// Private types - Abbreviations
  $ $MERLIN single syntax-document -position 5:14 \
  > -filename ./p-types.ml < ./p-types.ml 
  {
    "class": "return",
    "value": {
      "name": "Private Type Abbreviation",
      "description": "A private type abbreviation declares a type that is distinct from its implementation type `typexpr`.",
      "url": "https://v2.ocaml.org/releases/5.1/htmlman/privatetypes.html#ss:private-types-abbrev"
    },
    "notifications": []
  }

  $ $MERLIN single syntax-document -position 7:14 \
  > -filename ./p-types.ml < ./p-types.ml
  {
    "class": "return",
    "value": {
      "name": "Private Type Abbreviation",
      "description": "A private type abbreviation declares a type that is distinct from its implementation type `typexpr`.",
      "url": "https://v2.ocaml.org/releases/5.1/htmlman/privatetypes.html#ss:private-types-abbrev"
    },
    "notifications": []
  }
// Abstract
  $ $MERLIN single syntax-document -position 11:7 \
  > -filename ./p-types.ml < ./p-types.ml
  {
    "class": "return",
    "value": {
      "name": "Abstract type",
      "description": "Allows you to define variants with arbitrary data structures, including other variants, records, and functions",
      "url": "https://v2.ocaml.org/manual/extensiblevariants.html"
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
  $ $MERLIN single syntax-document -position 1:17 \
  > -filename ./locally-abstract-dt.ml < ./locally-abstract-dt.ml
  {
    "class": "return",
    "value": {
      "name": "Locally Abstract Type",
      "description": "The expression `fun ( type typeconstr-name ) -> expr` introduces a type constructor named `typeconstr-name` which is considered abstract in the scope of the sub-expression, but then replaced by a fresh type variable.",
      "url": "https://v2.ocaml.org/releases/5.1/htmlman/locallyabstract.html"
    },
    "notifications": []
  }
// Locally abstract data types B
$ $MERLIN single syntax-document -position 3:20 \
> -filename ./locally-abstract-dt.ml < ./locally-abstract-dt.ml


