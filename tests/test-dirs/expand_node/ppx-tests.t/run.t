Dune setup
  $ cat > dune-project << EOF
  > (lang dune 2.9)
  > EOF

  $ cat > dune << EOF
  > (executable
  >  (name apt)
  >  (preprocess (pps c_ppx)))
  > EOF

Type declaration in structure
  $ cat > apt.ml << EOF
  > module MyModule = struct
  >  type point = {x:int; y:int} [@@deriving rename]
  > end
  > EOF

  $ dune build

on keyword module
  $ $MERLIN single expand-node -position 1:4 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": "No PPX deriver/extension node found on this position",
    "notifications": []
  }

on module name "MyModule"
  $ $MERLIN single expand-node -position 1:11 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": "No PPX deriver/extension node found on this position",
    "notifications": []
  }

on keyword struct
  $ $MERLIN single expand-node -position 1:21 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": "No PPX deriver/extension node found on this position",
    "notifications": []
  }

on keyword type  
  $ $MERLIN single expand-node -position 2:3 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": "No PPX deriver/extension node found on this position",
    "notifications": []
  }

on type name "point"
  $ $MERLIN single expand-node -position 2:9 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": "No PPX deriver/extension node found on this position",
    "notifications": []
  }

on label x  
  $ $MERLIN single expand-node -position 2:16 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": "No PPX deriver/extension node found on this position",
    "notifications": []
  }

on core type "int"
  $ $MERLIN single expand-node -position 2:24 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": "No PPX deriver/extension node found on this position",
    "notifications": []
  }

on attribute name "deriving"
  $ $MERLIN single expand-node -position 2:36 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": "[ module MyModule =
    struct
      type point = {
        x: int ;
        y: int }[@@deriving rename]
      include
        struct
          let _ = fun (_ : point) -> ()
          type point_renamed = {
            x: int ;
            y: int }
        end[@@ocaml.doc \"@inline\"][@@merlin.hide ]
    end
   ]",
    "notifications": []
  }

on attribute payload name "rename"
  $ $MERLIN single expand-node -position 2:46 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": "[ module MyModule =
    struct
      type point = {
        x: int ;
        y: int }[@@deriving rename]
      include
        struct
          let _ = fun (_ : point) -> ()
          type point_renamed = {
            x: int ;
            y: int }
        end[@@ocaml.doc \"@inline\"][@@merlin.hide ]
    end
   ]",
    "notifications": []
  }

Type declaration in signature
  $ cat > apt.ml << EOF
  > module type MyModuleSig = sig
  >   type tttt = Red | Green [@@deriving rename]
  > end
  > EOF

  $ dune build

on keyword module
  $ $MERLIN single expand-node -position 1:4 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": "No PPX deriver/extension node found on this position",
    "notifications": []
  }

on module name "MyModule"
  $ $MERLIN single expand-node -position 1:11 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": "No PPX deriver/extension node found on this position",
    "notifications": []
  }

on keyword struct
  $ $MERLIN single expand-node -position 1:21 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": "No PPX deriver/extension node found on this position",
    "notifications": []
  }

on keyword type  
  $ $MERLIN single expand-node -position 2:3 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": "No PPX deriver/extension node found on this position",
    "notifications": []
  }

on type name "point"
  $ $MERLIN single expand-node -position 2:9 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": "No PPX deriver/extension node found on this position",
    "notifications": []
  }

on label x  
  $ $MERLIN single expand-node -position 2:16 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": "No PPX deriver/extension node found on this position",
    "notifications": []
  }

on core type "int"
  $ $MERLIN single expand-node -position 2:24 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": "No PPX deriver/extension node found on this position",
    "notifications": []
  }

on attribute name "deriving"
  $ $MERLIN single expand-node -position 2:36 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": "[ module type MyModuleSig  =
    sig
      type tttt =
        | Red 
        | Green [@@deriving rename]
      include
        sig [@@@ocaml.warning \"-32\"] type tttt_renamed =
                                       | Red 
                                       | Green  end[@@ocaml.doc \"@inline\"]
      [@@merlin.hide ]
    end
   ]",
    "notifications": []
  }

on attribute payload name "rename"
  $ $MERLIN single expand-node -position 2:46 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": "[ module type MyModuleSig  =
    sig
      type tttt =
        | Red 
        | Green [@@deriving rename]
      include
        sig [@@@ocaml.warning \"-32\"] type tttt_renamed =
                                       | Red 
                                       | Green  end[@@ocaml.doc \"@inline\"]
      [@@merlin.hide ]
    end
   ]",
    "notifications": []
  }


Type declaration in structure
  $ cat > apt.ml << EOF
  > type yyyy = int [@@deriving rename]
  > EOF

  $ dune build

  $ $MERLIN single expand-node -position 1:23 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": "[ type yyyy = int[@@deriving rename]
  ; include struct let _ = fun (_ : yyyy) -> ()
                 type yyyy_renamed = int end[@@ocaml.doc \"@inline\"][@@merlin.hide
                                                                     ]
   ]",
    "notifications": []
  }


Type declaration in signature
  $ cat > apt.mli << EOF
  > type yyyy = int [@@deriving rename]
  > EOF

  $ dune build


  $ $MERLIN single expand-node -position 1:23 -filename ./apt.mli < ./apt.mli
  {
    "class": "return",
    "value": "[ type yyyy = int[@@deriving rename]
  ; include sig [@@@ocaml.warning \"-32\"] type yyyy_renamed = int end[@@ocaml.doc
                                                                    \"@inline\"]
  [@@merlin.hide ]
   ]",
    "notifications": []
  }


Type extension in structure
  $ cat > apy.ml << EOF
  > type pppp = .. [@@deriving rename]
  > type pppp += Int of int [@@deriving rename]
  > EOF

  $ dune build

  $ $MERLIN single expand-node -position 1:22 -filename ./apy.ml < ./apy.ml
  {
    "class": "return",
    "value": "[ type pppp = ..[@@deriving rename]
  ; include struct let _ = fun (_ : pppp) -> ()
                 type pppp_renamed = .. end[@@ocaml.doc \"@inline\"][@@merlin.hide
                                                                    ]
   ]",
    "notifications": []
  }
  $ $MERLIN single expand-node -position 2:30 -filename ./apy.ml < ./apy.ml
  {
    "class": "return",
    "value": "[ type pppp +=  
    | Int of int [@@deriving rename]
  ; include struct type pppp_renamed +=  
                   | Int of int  end[@@ocaml.doc \"@inline\"][@@merlin.hide ]
   ]",
    "notifications": []
  }


Type extension in signature
  $ cat > apy.mli << EOF
  > type pppp = .. [@@deriving rename]
  > EOF

  $ dune build

  $ $MERLIN single expand-node -position 1:22 -filename ./apy.mli < ./apy.mli
  {
    "class": "return",
    "value": "[ type pppp = ..[@@deriving rename]
  ; include sig [@@@ocaml.warning \"-32\"] type pppp_renamed = .. end[@@ocaml.doc
                                                                   \"@inline\"]
  [@@merlin.hide ]
   ]",
    "notifications": []
  }

Exception in structure
  $ cat > apr.ml << EOF
  > exception Foo of string [@@deriving rename]
  > EOF

  $ dune build

  $ $MERLIN single expand-node -position 1:30 -filename ./apr.ml < ./apr.ml
  {
    "class": "return",
    "value": "[ exception Foo of string [@@deriving rename]
  ; include struct exception Foo_renamed of string  end[@@ocaml.doc \"@inline\"]
  [@@merlin.hide ]
   ]",
    "notifications": []
  }

Exception in signature
  $ cat > apr.mli << EOF
  > exception Foo of string [@@deriving rename]
  > EOF

  $ dune build

  $ $MERLIN single expand-node -position 1:30 -filename ./apr.mli < ./apr.mli
  {
    "class": "return",
    "value": "[ exception Foo of string [@@deriving rename]
  ; include sig [@@@ocaml.warning \"-32\"] exception Foo_renamed of string  end
  [@@ocaml.doc \"@inline\"][@@merlin.hide ]
   ]",
    "notifications": []
  }


Module type declaration in structure
  $ cat > apc.ml << EOF
  > module type Stack = sig 
  >   type t
  >   type stack 
  >   val empty : stack
  >   val is_empty : stack -> bool
  >   val push : t -> stack -> stack 
  >   val pop : stack -> stack
  >   val peek : stack -> t
  > end [@@deriving rename]

  $ dune build

  $ $MERLIN single expand-node -position 9:8 -filename ./apc.ml < ./apc.ml
  {
    "class": "return",
    "value": "[ module type Stack  =
    sig
      type t
      type stack
      val empty : stack
      val is_empty : stack -> bool
      val push : t -> stack -> stack
      val pop : stack -> stack
      val peek : stack -> t
    end[@@deriving rename]
  ; include
    struct
      module type Stack_renamed  =
        sig
          type t
          type stack
          val empty : stack
          val is_empty : stack -> bool
          val push : t -> stack -> stack
          val pop : stack -> stack
          val peek : stack -> t
        end
    end[@@ocaml.doc \"@inline\"][@@merlin.hide ]
   ]",
    "notifications": []
  }

Module type declaration in signature
  $ cat > apc.mli << EOF
  > module type Stack = sig 
  >   type t
  >   type stack 
  >   val empty : stack
  >   val is_empty : stack -> bool
  >   val push : t -> stack -> stack 
  >   val pop : stack -> stack
  >   val peek : stack -> t
  > end [@@deriving rename]

  $ dune build

  $ $MERLIN single expand-node -position 9:8 -filename ./apc.mli < ./apc.mli
  {
    "class": "return",
    "value": "[ module type Stack  =
    sig
      type t
      type stack
      val empty : stack
      val is_empty : stack -> bool
      val push : t -> stack -> stack
      val pop : stack -> stack
      val peek : stack -> t
    end[@@deriving rename]
  ; include
    sig
      [@@@ocaml.warning \"-32\"]
      module type Stack_renamed  =
        sig
          type t
          type stack
          val empty : stack
          val is_empty : stack -> bool
          val push : t -> stack -> stack
          val pop : stack -> stack
          val peek : stack -> t
        end
    end[@@ocaml.doc \"@inline\"][@@merlin.hide ]
   ]",
    "notifications": []
  }

Test for an attribute that's not deriving
  $ cat > apf.ml << EOF
  > type t = int [@@merlin.hide]
  > EOF

  $ dune build

  $ $MERLIN single expand-node -position 1:18 -filename ./apf.ml < ./apf.ml
  {
    "class": "return",
    "value": "No PPX deriver/extension node found on this position",
    "notifications": []
  }
