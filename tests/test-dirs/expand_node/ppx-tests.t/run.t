Dune setup
  $ cat > dune-project << EOF
  > (lang dune 2.9)
  > EOF

  $ cat > dune << EOF
  > (executable
  >  (name apt)
  >  (preprocess (pps c_ppx my_ppx)))
  > EOF

Type declaration in structure
  $ cat > apt.ml << EOF
  > module MyModule = struct
  >  type point = {x:int; y:int} [@@deriving rename]
  > end
  > EOF

  $ dune build

on module name "MyModule"
  $ $MERLIN single expand-ppx -position 1:11 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": "No PPX deriver/extension node found on this position",
    "notifications": []
  }

on keyword type  
  $ $MERLIN single expand-ppx -position 2:3 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": "No PPX deriver/extension node found on this position",
    "notifications": []
  }

on attribute name "deriving"
  $ $MERLIN single expand-ppx -position 2:36 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": {
      "code": "include
    struct
      let _ = fun (_ : point) -> ()
      type point_renamed = {
        x: int ;
        y: int }
    end[@@ocaml.doc \"@inline\"][@@merlin.hide ]",
      "deriver": {
        "start": {
          "line": 2,
          "col": 29
        },
        "end": {
          "line": 2,
          "col": 48
        }
      }
    },
    "notifications": []
  }

on attribute payload name "rename"
  $ $MERLIN single expand-ppx -position 2:46 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": {
      "code": "include
    struct
      let _ = fun (_ : point) -> ()
      type point_renamed = {
        x: int ;
        y: int }
    end[@@ocaml.doc \"@inline\"][@@merlin.hide ]",
      "deriver": {
        "start": {
          "line": 2,
          "col": 29
        },
        "end": {
          "line": 2,
          "col": 48
        }
      }
    },
    "notifications": []
  }

Type declaration in signature
  $ cat > apt.ml << EOF
  > module type MyModuleSig = sig
  >   type tttt = Red | Green [@@deriving rename]
  > end
  > EOF

  $ dune build

on attribute name "deriving"
  $ $MERLIN single expand-ppx -position 2:36 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": {
      "code": "include sig [@@@ocaml.warning \"-32\"] type tttt_renamed =
                                         | Red 
                                         | Green  end[@@ocaml.doc \"@inline\"]
  [@@merlin.hide ]",
      "deriver": {
        "start": {
          "line": 2,
          "col": 26
        },
        "end": {
          "line": 2,
          "col": 45
        }
      }
    },
    "notifications": []
  }

on attribute payload name "rename"
  $ $MERLIN single expand-ppx -position 2:42 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": {
      "code": "include sig [@@@ocaml.warning \"-32\"] type tttt_renamed =
                                         | Red 
                                         | Green  end[@@ocaml.doc \"@inline\"]
  [@@merlin.hide ]",
      "deriver": {
        "start": {
          "line": 2,
          "col": 26
        },
        "end": {
          "line": 2,
          "col": 45
        }
      }
    },
    "notifications": []
  }

Type declaration in structure
  $ cat > apt.ml << EOF
  > type yyyy = int [@@deriving rename]
  > EOF

  $ dune build

  $ $MERLIN single expand-ppx -position 1:23 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": {
      "code": "include struct let _ = fun (_ : yyyy) -> ()
                 type yyyy_renamed = int end[@@ocaml.doc \"@inline\"][@@merlin.hide
                                                                     ]",
      "deriver": {
        "start": {
          "line": 1,
          "col": 16
        },
        "end": {
          "line": 1,
          "col": 35
        }
      }
    },
    "notifications": []
  }

Type declaration in signature
  $ cat > apt.mli << EOF
  > type yyyy = int [@@deriving rename]
  > EOF

  $ dune build

  $ $MERLIN single expand-ppx -position 1:23 -filename ./apt.mli < ./apt.mli
  {
    "class": "return",
    "value": {
      "code": "include sig [@@@ocaml.warning \"-32\"] type yyyy_renamed = int end[@@ocaml.doc
                                                                    \"@inline\"]
  [@@merlin.hide ]",
      "deriver": {
        "start": {
          "line": 1,
          "col": 16
        },
        "end": {
          "line": 1,
          "col": 35
        }
      }
    },
    "notifications": []
  }

Type extension in structure
  $ cat > apy.ml << EOF
  > type pppp = .. [@@deriving rename]
  > EOF

  $ dune build

  $ $MERLIN single expand-ppx -position 1:22 -filename ./apy.ml < ./apy.ml
  {
    "class": "return",
    "value": {
      "code": "include struct let _ = fun (_ : pppp) -> ()
                 type pppp_renamed = .. end[@@ocaml.doc \"@inline\"][@@merlin.hide
                                                                    ]",
      "deriver": {
        "start": {
          "line": 1,
          "col": 15
        },
        "end": {
          "line": 1,
          "col": 34
        }
      }
    },
    "notifications": []
  }

Type extension in signature
  $ cat > apy.mli << EOF
  > type pppp = .. [@@deriving rename]
  > EOF

  $ dune build

  $ $MERLIN single expand-ppx -position 1:22 -filename ./apy.mli < ./apy.mli
  {
    "class": "return",
    "value": {
      "code": "include sig [@@@ocaml.warning \"-32\"] type pppp_renamed = .. end[@@ocaml.doc
                                                                   \"@inline\"]
  [@@merlin.hide ]",
      "deriver": {
        "start": {
          "line": 1,
          "col": 15
        },
        "end": {
          "line": 1,
          "col": 34
        }
      }
    },
    "notifications": []
  }

Exception in structure
  $ cat > apr.ml << EOF
  > exception Foo of string [@@deriving rename]
  > EOF

  $ dune build

  $ $MERLIN single expand-ppx -position 1:30 -filename ./apr.ml < ./apr.ml
  {
    "class": "return",
    "value": {
      "code": "include struct exception Foo_renamed of string  end[@@ocaml.doc \"@inline\"]
  [@@merlin.hide ]",
      "deriver": {
        "start": {
          "line": 1,
          "col": 24
        },
        "end": {
          "line": 1,
          "col": 43
        }
      }
    },
    "notifications": []
  }

Exception in signature
  $ cat > apr.mli << EOF
  > exception Foo of string [@@deriving rename]
  > EOF

  $ dune build

  $ $MERLIN single expand-ppx -position 1:30 -filename ./apr.mli < ./apr.mli
  {
    "class": "return",
    "value": {
      "code": "include sig [@@@ocaml.warning \"-32\"] exception Foo_renamed of string  end
  [@@ocaml.doc \"@inline\"][@@merlin.hide ]",
      "deriver": {
        "start": {
          "line": 1,
          "col": 24
        },
        "end": {
          "line": 1,
          "col": 43
        }
      }
    },
    "notifications": []
  }

Module type declaration in structure
  $ cat > apc.ml << EOF
  > module type Stack = sig 
  >   type t [@@deriving rename]
  >   type stack 
  >   val empty : stack
  >   val is_empty : stack -> bool
  >   val push : t -> stack -> stack 
  >   val pop : stack -> stack
  >   val peek : stack -> t
  > end [@@deriving rename]
  > EOF

  $ dune build

a cursor here should only output the derived t 

(* Type t_renamed is duplicated multiple times because the same type is derived twice, first by it's own ppx and secondly
when the parent ppx on the module type declaration is evaluated. *)
  $ $MERLIN  single expand-ppx -position 2:14 -filename ./apc.ml < ./apc.ml
  {
    "class": "return",
    "value": {
      "code": "include sig [@@@ocaml.warning \"-32\"] type t_renamed end[@@ocaml.doc
                                                           \"@inline\"][@@merlin.hide
                                                                      ]
  include sig [@@@ocaml.warning \"-32\"] type t_renamed end[@@ocaml.doc
                                                           \"@inline\"][@@merlin.hide
                                                                      ]
  include sig [@@@ocaml.warning \"-32\"] type t_renamed end[@@ocaml.doc
                                                           \"@inline\"][@@merlin.hide
                                                                      ]",
      "deriver": {
        "start": {
          "line": 2,
          "col": 9
        },
        "end": {
          "line": 2,
          "col": 28
        }
      }
    },
    "notifications": []
  }

(* Type t_renamed is duplicated multiple times because the same type is derived twice, first by it's own ppx and secondly
when the parent ppx on the module type declaration is evaluated. *)

  $ $MERLIN single expand-ppx -position 9:8 -filename ./apc.ml < ./apc.ml
  {
    "class": "return",
    "value": {
      "code": "include
    struct
      module type Stack_renamed  =
        sig
          type t[@@deriving rename]
          include sig [@@@ocaml.warning \"-32\"] type t_renamed end[@@ocaml.doc
                                                                   \"@inline\"]
          [@@merlin.hide ]
          include sig [@@@ocaml.warning \"-32\"] type t_renamed end[@@ocaml.doc
                                                                   \"@inline\"]
          [@@merlin.hide ]
          type stack
          val empty : stack
          val is_empty : stack -> bool
          val push : t -> stack -> stack
          val pop : stack -> stack
          val peek : stack -> t
        end
    end[@@ocaml.doc \"@inline\"][@@merlin.hide ]",
      "deriver": {
        "start": {
          "line": 9,
          "col": 4
        },
        "end": {
          "line": 9,
          "col": 23
        }
      }
    },
    "notifications": []
  }

Module type declaration in signature
  $ cat > apc.mli << EOF
  > module type Stack = sig 
  >   type t [@@deriving rename]
  >   type stack 
  >   val empty : stack
  >   val is_empty : stack -> bool
  >   val push : t -> stack -> stack 
  >   val pop : stack -> stack
  >   val peek : stack -> t
  > end [@@deriving rename]
  > EOF

  $ dune build

on attribute name deriving of type t
a cursor here should only output the derived t 
  $ $MERLIN  single expand-ppx -position 2:14 -filename ./apc.mli < ./apc.mli
  {
    "class": "return",
    "value": {
      "code": "include sig [@@@ocaml.warning \"-32\"] type t_renamed end[@@ocaml.doc
                                                           \"@inline\"][@@merlin.hide
                                                                      ]",
      "deriver": {
        "start": {
          "line": 2,
          "col": 9
        },
        "end": {
          "line": 2,
          "col": 28
        }
      }
    },
    "notifications": []
  }

on attribute name deriving of module Stack
(* Type t_renamed is duplicated multiple times because the same type is derived twice, first by it's own ppx and secondly
when the parent ppx on the module type declaration is evaluated. *)
  $ $MERLIN single expand-ppx -position 9:8 -filename ./apc.mli < ./apc.mli
  {
    "class": "return",
    "value": {
      "code": "module type Stack  =
    sig
      type t[@@deriving rename]
      include sig [@@@ocaml.warning \"-32\"] type t_renamed end[@@ocaml.doc
                                                               \"@inline\"]
      [@@merlin.hide ]
      type stack
      val empty : stack
      val is_empty : stack -> bool
      val push : t -> stack -> stack
      val pop : stack -> stack
      val peek : stack -> t
    end[@@deriving rename]
  include
    sig
      [@@@ocaml.warning \"-32\"]
      module type Stack_renamed  =
        sig
          type t[@@deriving rename]
          include sig [@@@ocaml.warning \"-32\"] type t_renamed end[@@ocaml.doc
                                                                   \"@inline\"]
          [@@merlin.hide ]
          include sig [@@@ocaml.warning \"-32\"] type t_renamed end[@@ocaml.doc
                                                                   \"@inline\"]
          [@@merlin.hide ]
          type stack
          val empty : stack
          val is_empty : stack -> bool
          val push : t -> stack -> stack
          val pop : stack -> stack
          val peek : stack -> t
        end
    end[@@ocaml.doc \"@inline\"][@@merlin.hide ]",
      "deriver": {
        "start": {
          "line": 9,
          "col": 4
        },
        "end": {
          "line": 9,
          "col": 23
        }
      }
    },
    "notifications": []
  }

Test for an attribute that's not deriving
  $ cat > apf.ml << EOF
  > type y = int * float [@@merlin.hide]
  > EOF

  $ dune build

  $ $MERLIN single expand-ppx -position 1:27 -filename ./apf.ml < ./apf.ml
  {
    "class": "return",
    "value": "No PPX deriver/extension node found on this position",
    "notifications": []
  }

PPx extension
  $ cat > apttt.ml << EOF
  > let phrase = print_string ([%tell_me] ^ ":-)!")
  > EOF

  $ dune build

  $ $MERLIN single expand-ppx -position 1:30 -filename ./apttt.ml < ./apttt.ml
  {
    "class": "return",
    "value": {
      "code": "\"OCaml is so cool\"",
      "deriver": {
        "start": {
          "line": 1,
          "col": 27
        },
        "end": {
          "line": 1,
          "col": 37
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single expand-ppx -position 1:41 -filename ./apttt.ml < ./apttt.ml
  {
    "class": "return",
    "value": "No PPX deriver/extension node found on this position",
    "notifications": []
  }

Show only an output for the hover and not all extensions
  $ cat > aptxc.ml << EOF
  > let phrase = [%tell_me] ^ [%tell_me]
  > EOF

  $ dune build
on the first [%tell_me]
  $ $MERLIN single expand-ppx -position 1:16 -filename ./apttt.ml < ./aptxc.ml
  {
    "class": "return",
    "value": {
      "code": "\"OCaml is so cool\"",
      "deriver": {
        "start": {
          "line": 1,
          "col": 13
        },
        "end": {
          "line": 1,
          "col": 23
        }
      }
    },
    "notifications": []
  }

on the concatenator
  $ $MERLIN single expand-ppx -position 1:24 -filename ./apttt.ml < ./aptxc.ml
  {
    "class": "return",
    "value": "No PPX deriver/extension node found on this position",
    "notifications": []
  }

on the second [%tell_me]
  $ $MERLIN single expand-ppx -position 1:28 -filename ./apttt.ml < ./aptxc.ml
  {
    "class": "return",
    "value": {
      "code": "\"OCaml is so cool\"",
      "deriver": {
        "start": {
          "line": 1,
          "col": 26
        },
        "end": {
          "line": 1,
          "col": 36
        }
      }
    },
    "notifications": []
  }
