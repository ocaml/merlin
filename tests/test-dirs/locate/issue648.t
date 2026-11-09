Locate classes and class types defined in another compilation unit
(https://github.com/ocaml/merlin/issues/648)

  $ cat >lib.mli <<EOF
  > class c : object
  >   method m : int
  > end
  > 
  > class type ct = object
  >   method m : int
  > end
  > 
  > class virtual vc : object
  >   method virtual v : int
  > end
  > EOF

  $ cat >lib.ml <<EOF
  > class c = object
  >   method m = 42
  > end
  > 
  > class type ct = object
  >   method m : int
  > end
  > 
  > class virtual vc = object (self)
  >   method virtual v : int
  > end
  > EOF

  $ cat >main.ml <<EOF
  > let o = new Lib.c
  > let f (x : Lib.c) = x#m
  > let g (x : #Lib.ct) = x#m
  > class d = object inherit Lib.c end
  > class virtual w = object inherit Lib.vc end
  > class e : Lib.ct = object method m = 1 end
  > class type ct2 = object inherit Lib.ct end
  > EOF

  $ ocamlc -c -bin-annot lib.mli lib.ml main.ml

A class in an expression `new Lib.|c`:
  $ $MERLIN single locate -look-for ml -position 1:16 \
  > -filename ./main.ml <./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/lib.ml",
    "pos": {
      "line": 1,
      "col": 6
    }
  }

  $ $MERLIN single locate -look-for mli -position 1:16 \
  > -filename ./main.ml <./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/lib.mli",
    "pos": {
      "line": 1,
      "col": 6
    }
  }

A class used as a type `(x : Lib.|c)`:
  $ $MERLIN single locate -look-for ml -position 2:15 \
  > -filename ./main.ml <./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/lib.ml",
    "pos": {
      "line": 1,
      "col": 6
    }
  }

  $ $MERLIN single locate -look-for mli -position 2:15 \
  > -filename ./main.ml <./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/lib.mli",
    "pos": {
      "line": 1,
      "col": 6
    }
  }

A class type used as a flexible type `(x : #Lib.|ct)`:
  $ $MERLIN single locate -look-for ml -position 3:16 \
  > -filename ./main.ml <./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/lib.ml",
    "pos": {
      "line": 5,
      "col": 11
    }
  }

  $ $MERLIN single locate -look-for mli -position 3:16 \
  > -filename ./main.ml <./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/lib.mli",
    "pos": {
      "line": 5,
      "col": 11
    }
  }

An inherited class `inherit Lib.|c`:
  $ $MERLIN single locate -look-for ml -position 4:29 \
  > -filename ./main.ml <./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/lib.ml",
    "pos": {
      "line": 1,
      "col": 6
    }
  }

  $ $MERLIN single locate -look-for mli -position 4:29 \
  > -filename ./main.ml <./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/lib.mli",
    "pos": {
      "line": 1,
      "col": 6
    }
  }

An inherited virtual class `inherit Lib.|vc`:
  $ $MERLIN single locate -look-for ml -position 5:37 \
  > -filename ./main.ml <./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/lib.ml",
    "pos": {
      "line": 9,
      "col": 14
    }
  }

  $ $MERLIN single locate -look-for mli -position 5:37 \
  > -filename ./main.ml <./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/lib.mli",
    "pos": {
      "line": 9,
      "col": 14
    }
  }

A class type used as a class constraint `class e : Lib.|ct`:
  $ $MERLIN single locate -look-for ml -position 6:14 \
  > -filename ./main.ml <./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/lib.ml",
    "pos": {
      "line": 5,
      "col": 11
    }
  }

  $ $MERLIN single locate -look-for mli -position 6:14 \
  > -filename ./main.ml <./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/lib.mli",
    "pos": {
      "line": 5,
      "col": 11
    }
  }

An inherited class type `inherit Lib.|ct`:
  $ $MERLIN single locate -look-for ml -position 7:36 \
  > -filename ./main.ml <./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/lib.ml",
    "pos": {
      "line": 5,
      "col": 11
    }
  }

  $ $MERLIN single locate -look-for mli -position 7:36 \
  > -filename ./main.ml <./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/lib.mli",
    "pos": {
      "line": 5,
      "col": 11
    }
  }
