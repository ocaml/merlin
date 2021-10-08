  $ $MERLIN single locate -look-for ml -position 2:9 -filename ./test.ml <<EOF
  > let x = 3
  > let _ = x
  > EOF
  {
    "class": "return",
    "value": {
      "file": "test.ml",
      "pos": {
        "line": 1,
        "col": 4
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 3:3 \
  > -filename ./test.ml <<EOF
  > let x =
  >   let y = 3 in
  >   y
  > EOF
  {
    "class": "return",
    "value": {
      "file": "test.ml",
      "pos": {
        "line": 2,
        "col": 6
      }
    },
    "notifications": []
  }


  $ $MERLIN single locate -look-for ml -position 3:10 \
  > -filename ./test.ml <<EOF
  > module A = struct let value = 0 end
  > include A
  > let _ = value
  > EOF
  {
    "class": "return",
    "value": {
      "file": "test.ml",
      "pos": {
        "line": 1,
        "col": 22
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 3:10 \
  > -filename ./test.ml <<EOF
  > module A = struct let value = 0 end
  > open A
  > let _ = value
  > EOF
  {
    "class": "return",
    "value": {
      "file": "test.ml",
      "pos": {
        "line": 1,
        "col": 22
      }
    },
    "notifications": []
  }

-log-file - -log-section locate
  $ $MERLIN single locate -look-for ml -position 2:10 \
  > -filename ./test.ml <<EOF
  > type a = A
  > type b = a
  > EOF
  {
    "class": "return",
    "value": {
      "file": "test.ml",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 2:9 \
  > -filename ./test.ml <<EOF
  > type a = A
  > let a = A
  > EOF
  {
    "class": "return",
    "value": {
      "file": "test.ml",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate locate -look-for ml -position 2:12 \
  > -filename ./test.ml <<EOF
  > module A = struct end
  > module B = A
  > EOF
  {
    "class": "return",
    "value": {
      "file": "test.ml",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 3:16 \
  > -filename ./all_local.ml <<EOF
  > module type S = sig type t end
  > module Make(Arg : S) : S = struct
  >   type x = Arg.t
  > end
  > EOF
  {
    "class": "return",
    "value": {
      "file": "all_local.ml",
      "pos": {
        "line": 2,
        "col": 12
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 4:12 \
  > -filename ./all_local.ml <<EOF
  > module type S = sig type t end
  > module Make(Arg : S) : S = struct
  >   include Arg
  >   type x = t
  > end
  > EOF
  {
    "class": "return",
    "value": {
      "file": "all_local.ml",
      "pos": {
        "line": 2,
        "col": 12
      }
    },
    "notifications": []
  }

  $ cat > a.ml <<EOF
  > let x = 3
  > EOF

  $ $OCAMLC -shapes -c a.ml

FIXME
  $ $MERLIN single locate -look-for ml -position 1:12 \
  > -filename ./b.ml <<EOF
  > module B = A
  > EOF
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/a.ml",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }

-log-file - -log-section locate

  $ $MERLIN single locate -look-for ml -position 2:14 \
  > -filename ./b.ml <<EOF
  > module A : sig module B : sig end end = struct module B = struct end end
  > module C = A.B
  > EOF
  {
    "class": "return",
    "value": {
      "file": "b.ml",
      "pos": {
        "line": 1,
        "col": 47
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 3:11 \
  > -filename ./testmake.ml <<EOF
  > module Make (A : sig end) = struct end
  > module T = Make (struct end);;
  > module A = T
  > EOF
  {
    "class": "return",
    "value": {
      "file": "testmake.ml",
      "pos": {
        "line": 2,
        "col": 0
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 3:11 \
  > -filename ./testmake.ml <<EOF
  > module Make () = struct end
  > module T = Make ();;
  > module A = T
  > EOF
  {
    "class": "return",
    "value": {
      "file": "testmake.ml",
      "pos": {
        "line": 2,
        "col": 0
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 2:12 \
  > -filename ./testmake.ml <<EOF
  > module M : sig type t end = struct type t = int end
  > type u = M.t
  > EOF
  {
    "class": "return",
    "value": {
      "file": "testmake.ml",
      "pos": {
        "line": 1,
        "col": 35
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for mli -position 2:12 \
  > -filename ./testmake.ml <<EOF
  > module M : sig type t end = struct type t = int end
  > type u = M.t
  > EOF
  {
    "class": "return",
    "value": {
      "file": "testmake.ml",
      "pos": {
        "line": 1,
        "col": 15
      }
    },
    "notifications": []
  }


  $ $MERLIN single locate -look-for ml -position 2:9 \
  > -filename ./testmake.ml <<EOF
  > type t = A
  > let _ = A
  > EOF
  {
    "class": "return",
    "value": {
      "file": "testmake.ml",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }

-log-file - -log-section locate

FIXME
  $ $MERLIN single locate -look-for ml -position 3:9 \
  > -filename ./testmake.ml <<EOF
  > type t = ..
  > type t += B
  > let _ = B
  > EOF
  {
    "class": "return",
    "value": {
      "file": "testmake.ml",
      "pos": {
        "line": 2,
        "col": 10
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 3:11 \
  > -filename ./testmake.ml <<EOF
  > module A = struct end
  > module B = A
  > module C = B
  > EOF
  {
    "class": "return",
    "value": {
      "file": "testmake.ml",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 4:11 \
  > -filename ./testmake.ml <<EOF
  > module F
  >  (X : sig end) = struct end
  > module M = F (struct end)
  > module A = F
  > EOF
  {
    "class": "return",
    "value": {
      "file": "testmake.ml",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 3:11 \
  > -filename ./testmake.ml <<EOF
  > module F (X : sig end) = struct end
  > module M = F (struct end)
  > module A = M
  > EOF
  {
    "class": "return",
    "value": {
      "file": "testmake.ml",
      "pos": {
        "line": 2,
        "col": 0
      }
    },
    "notifications": []
  }


  $ $MERLIN single locate -look-for ml -position 2:11 \
  > -filename ./testmake.ml <<EOF
  > module M = Set.Make (Int)
  > module A = M
  > EOF
  {
    "class": "return",
    "value": {
      "file": "testmake.ml",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 2:17 \
  > -filename ./testmake.ml <<EOF
  > module Indir : sig module M : Set.S end = struct module M = Set.Make(Int) end
  > module A = Indir.M
  > EOF
  {
    "class": "return",
    "value": {
      "file": "testmake.ml",
      "pos": {
        "line": 1,
        "col": 49
      }
    },
    "notifications": []
  }
