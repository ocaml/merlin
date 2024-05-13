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

on type declaration name "point"
  $ $MERLIN single expand-node -position 2:9 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": "No PPX deriver/extension node found on this position",
    "notifications": []
  }

on record label x  
  $ $MERLIN single expand-node -position 2:16 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": "No PPX deriver/extension node found on this position",
    "notifications": []
  }

on record core type "int"
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
    "value": {
      "code": "[ type point = {
    x: int ;
    y: int }[@@deriving rename]
  ; include
    struct
      let _ = fun (_ : point) -> ()
      type point_renamed = {
        x: int ;
        y: int }
    end[@@ocaml.doc \"@inline\"][@@merlin.hide ]
  ; module MyModule =
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
  $ $MERLIN single expand-node -position 2:46 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": {
      "code": "[ type point = {
    x: int ;
    y: int }[@@deriving rename]
  ; include
    struct
      let _ = fun (_ : point) -> ()
      type point_renamed = {
        x: int ;
        y: int }
    end[@@ocaml.doc \"@inline\"][@@merlin.hide ]
  ; module MyModule =
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

on type declaration name "point"
  $ $MERLIN single expand-node -position 2:9 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": "No PPX deriver/extension node found on this position",
    "notifications": []
  }

on record label x  
  $ $MERLIN single expand-node -position 2:16 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": "No PPX deriver/extension node found on this position",
    "notifications": []
  }

on record core type "int"
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
    "value": {
      "code": "[ type tttt =
    | Red 
    | Green [@@deriving rename]
  ; include sig [@@@ocaml.warning \"-32\"] type tttt_renamed =
                                         | Red 
                                         | Green  end[@@ocaml.doc \"@inline\"]
  [@@merlin.hide ]
  ; module type MyModuleSig  =
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
  $ $MERLIN single expand-node -position 2:40 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": {
      "code": "[ type tttt =
    | Red 
    | Green [@@deriving rename]
  ; include sig [@@@ocaml.warning \"-32\"] type tttt_renamed =
                                         | Red 
                                         | Green  end[@@ocaml.doc \"@inline\"]
  [@@merlin.hide ]
  ; module type MyModuleSig  =
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


Type declaration in structure B
  $ cat > apt.ml << EOF
  > type yyyy = int [@@deriving rename]
  > EOF

  $ dune build

  $ $MERLIN single expand-node -position 1:23 -filename ./apt.ml < ./apt.ml
  {
    "class": "return",
    "value": {
      "code": "[ type yyyy = int[@@deriving rename]
  ; include struct let _ = fun (_ : yyyy) -> ()
                 type yyyy_renamed = int end[@@ocaml.doc \"@inline\"][@@merlin.hide
                                                                     ]
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


Type declaration in signature B
  $ cat > apt.mli << EOF
  > type yyyy = int [@@deriving rename]
  > EOF

  $ dune build


  $ $MERLIN single expand-node -position 1:23 -filename ./apt.mli < ./apt.mli
  {
    "class": "return",
    "value": {
      "code": "[ type yyyy = int[@@deriving rename]
  ; include sig [@@@ocaml.warning \"-32\"] type yyyy_renamed = int end[@@ocaml.doc
                                                                    \"@inline\"]
  [@@merlin.hide ]
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


Type extension in structure
  $ cat > apy.ml << EOF
  > type pppp = .. [@@deriving rename]
  > type pppp += Int of int [@@deriving rename]
  > EOF

  $ dune build

  $ $MERLIN single expand-node -position 1:22 -filename ./apy.ml < ./apy.ml
  {
    "class": "return",
    "value": {
      "code": "[ type pppp = ..[@@deriving rename]
  ; include struct let _ = fun (_ : pppp) -> ()
                 type pppp_renamed = .. end[@@ocaml.doc \"@inline\"][@@merlin.hide
                                                                    ]
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
  $ $MERLIN single expand-node -position 2:30 -filename ./apy.ml < ./apy.ml
  {
    "class": "return",
    "value": {
      "code": "[ type pppp +=  
    | Int of int [@@deriving rename]
  ; include struct type pppp_renamed +=  
                   | Int of int  end[@@ocaml.doc \"@inline\"][@@merlin.hide ]
   ]",
      "deriver": {
        "start": {
          "line": 2,
          "col": 24
        },
        "end": {
          "line": 2,
          "col": 43
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

  $ $MERLIN single expand-node -position 1:22 -filename ./apy.mli < ./apy.mli
  {
    "class": "return",
    "value": {
      "code": "[ type pppp = ..[@@deriving rename]
  ; include sig [@@@ocaml.warning \"-32\"] type pppp_renamed = .. end[@@ocaml.doc
                                                                   \"@inline\"]
  [@@merlin.hide ]
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

Exception in structure
  $ cat > apr.ml << EOF
  > exception Foo of string [@@deriving rename]
  > EOF

  $ dune build

  $ $MERLIN single expand-node -position 1:30 -filename ./apr.ml < ./apr.ml
  {
    "class": "return",
    "value": {
      "code": "[ exception Foo of string [@@deriving rename]
  ; include struct exception Foo_renamed of string  end[@@ocaml.doc \"@inline\"]
  [@@merlin.hide ]
   ]",
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

  $ $MERLIN single expand-node -position 1:30 -filename ./apr.mli < ./apr.mli
  {
    "class": "return",
    "value": {
      "code": "[ exception Foo of string [@@deriving rename]
  ; include sig [@@@ocaml.warning \"-32\"] exception Foo_renamed of string  end
  [@@ocaml.doc \"@inline\"][@@merlin.hide ]
   ]",
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
  >   type t
  >   type stack 
  >   val empty : stack
  >   val is_empty : stack -> bool
  >   val push : t -> stack -> stack 
  >   val pop : stack -> stack
  >   val peek : stack -> t
  > end [@@deriving rename]
  > EOF

  $ dune build

  $ $MERLIN single expand-node -position 9:8 -filename ./apc.ml < ./apc.ml
  {
    "class": "return",
    "value": {
      "code": "[ module type Stack  =
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
  >   type t
  >   type stack 
  >   val empty : stack
  >   val is_empty : stack -> bool
  >   val push : t -> stack -> stack 
  >   val pop : stack -> stack
  >   val peek : stack -> t
  > end [@@deriving rename]
  > EOF

  $ dune build

  $ $MERLIN single expand-node -position 9:8 -filename ./apc.mli < ./apc.mli
  {
    "class": "return",
    "value": {
      "code": "[ module type Stack  =
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
  > type t = int [@@merlin.hide]
  > EOF

  $ dune build

  $ $MERLIN single expand-node -position 1:18 -filename ./apf.ml < ./apf.ml
  {
    "class": "return",
    "value": "No PPX deriver/extension node found on this position",
    "notifications": []
  }


Module type declaration in structure
  $ cat > apcc.ml << EOF
  > module type Queue = sig 
  >   type t [@@deriving rename]
  >   type queue 
  >   val enqueue : t -> queue -> queue
  >   val dequeue : queue -> queue
  >   val peek : queue -> t
  > end [@@deriving rename]
  > EOF

  $ dune build

  $ $MERLIN single expand-node -position 2:14 -filename ./apcc.ml < ./apcc.ml
  {
    "class": "return",
    "value": {
      "code": "[ type t[@@deriving rename]
  ; include sig [@@@ocaml.warning \"-32\"] type t_renamed end[@@ocaml.doc
                                                           \"@inline\"][@@merlin.hide
                                                                      ]
  ; module type Queue  =
    sig
      type t[@@deriving rename]
      include sig [@@@ocaml.warning \"-32\"] type t_renamed end[@@ocaml.doc
                                                               \"@inline\"]
      [@@merlin.hide ]
      type queue
      val enqueue : t -> queue -> queue
      val dequeue : queue -> queue
      val peek : queue -> t
    end[@@deriving rename]
  ; include
    struct
      module type Queue_renamed  =
        sig
          type t[@@deriving rename]
          include sig [@@@ocaml.warning \"-32\"] type t_renamed end[@@ocaml.doc
                                                                   \"@inline\"]
          [@@merlin.hide ]
          include sig [@@@ocaml.warning \"-32\"] type t_renamed end[@@ocaml.doc
                                                                   \"@inline\"]
          [@@merlin.hide ]
          type queue
          val enqueue : t -> queue -> queue
          val dequeue : queue -> queue
          val peek : queue -> t
        end
    end[@@ocaml.doc \"@inline\"][@@merlin.hide ]
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


  $ $MERLIN single expand-node -position 7:8 -filename ./apcc.ml < ./apcc.ml
  {
    "class": "return",
    "value": {
      "code": "[ module type Queue  =
    sig
      type t[@@deriving rename]
      include sig [@@@ocaml.warning \"-32\"] type t_renamed end[@@ocaml.doc
                                                               \"@inline\"]
      [@@merlin.hide ]
      type queue
      val enqueue : t -> queue -> queue
      val dequeue : queue -> queue
      val peek : queue -> t
    end[@@deriving rename]
  ; include
    struct
      module type Queue_renamed  =
        sig
          type t[@@deriving rename]
          include sig [@@@ocaml.warning \"-32\"] type t_renamed end[@@ocaml.doc
                                                                   \"@inline\"]
          [@@merlin.hide ]
          include sig [@@@ocaml.warning \"-32\"] type t_renamed end[@@ocaml.doc
                                                                   \"@inline\"]
          [@@merlin.hide ]
          type queue
          val enqueue : t -> queue -> queue
          val dequeue : queue -> queue
          val peek : queue -> t
        end
    end[@@ocaml.doc \"@inline\"][@@merlin.hide ]
   ]",
      "deriver": {
        "start": {
          "line": 7,
          "col": 4
        },
        "end": {
          "line": 7,
          "col": 23
        }
      }
    },
    "notifications": []
  }

Type declaration in structure
  $ cat > ttx.ml << EOF
  > module M = struct
  >   module N = struct
  >      type t = int [@@deriving rename]
  >      let a = 1
  >   end
  >   let b = 2
  > end
  > let c = 3
  > EOF

  $ dune build

on [@@deriving rename]
  $ $MERLIN single expand-node -position 3:23 -filename ./ttx.ml < ./ttx.ml
  {
    "class": "return",
    "value": {
      "code": "[ type t = int[@@deriving rename]
  ; include struct let _ = fun (_ : t) -> ()
                 type t_renamed = int end[@@ocaml.doc \"@inline\"][@@merlin.hide
                                                                  ]
  ; module N =
    struct
      type t = int[@@deriving rename]
      include struct let _ = fun (_ : t) -> ()
                     type t_renamed = int end[@@ocaml.doc \"@inline\"][@@merlin.hide
                                                                      ]
      let a = 1
    end
  ; module M =
    struct
      module N =
        struct
          type t = int[@@deriving rename]
          include struct let _ = fun (_ : t) -> ()
                         type t_renamed = int end[@@ocaml.doc \"@inline\"]
          [@@merlin.hide ]
          let a = 1
        end
      let b = 2
    end
   ]",
      "deriver": {
        "start": {
          "line": 3,
          "col": 18
        },
        "end": {
          "line": 3,
          "col": 37
        }
      }
    },
    "notifications": []
  }


Type declaration in structure
  $ cat > aptt.ml << EOF
  > type tont = float * float [@@deriving rename]
  > EOF

  $ dune build

on [@@deriving rename]
  $ $MERLIN single expand-node -position 1:37 -filename ./aptt.ml < ./aptt.ml
  {
    "class": "return",
    "value": {
      "code": "[ type tont = (float * float)[@@deriving rename]
  ; include
    struct let _ = fun (_ : tont) -> ()
           type tont_renamed = (float * float) end[@@ocaml.doc \"@inline\"]
  [@@merlin.hide ]
   ]",
      "deriver": {
        "start": {
          "line": 1,
          "col": 26
        },
        "end": {
          "line": 1,
          "col": 45
        }
      }
    },
    "notifications": []
  }

PPx extension
  $ cat > apttt.ml << EOF
  > let phrase = print_string ([%tell_me] ^ ":-)!")
  > EOF

  $ dune build
  $ $MERLIN single expand-node -position 1:2 -filename ./apttt.ml < ./apttt.ml
  {
    "class": "return",
    "value": "No PPX deriver/extension node found on this position",
    "notifications": []
  }
  $ $MERLIN single expand-node -position 1:7 -filename ./apttt.ml < ./apttt.ml
  {
    "class": "return",
    "value": "No PPX deriver/extension node found on this position",
    "notifications": []
  }
  $ $MERLIN single expand-node -position 1:19 -filename ./apttt.ml < ./apttt.ml
  {
    "class": "return",
    "value": "No PPX deriver/extension node found on this position",
    "notifications": []
  }
  $ $MERLIN single expand-node -position 1:30 -filename ./apttt.ml < ./apttt.ml
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
  $ $MERLIN single expand-node -position 1:41 -filename ./apttt.ml < ./apttt.ml
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
  $ $MERLIN single expand-node -position 1:16 -filename ./apttt.ml < ./aptxc.ml
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
  $ $MERLIN single expand-node -position 1:24 -filename ./apttt.ml < ./aptxc.ml
  {
    "class": "return",
    "value": "No PPX deriver/extension node found on this position",
    "notifications": []
  }
on the second [%tell_me]
  $ $MERLIN single expand-node -position 1:27 -filename ./apttt.ml < ./aptxc.ml
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

