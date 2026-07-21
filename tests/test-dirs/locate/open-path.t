Locating each segment of a dotted `open A.B.C` path must jump to the
corresponding module, both in implementations and interfaces.

  $ cat >test.ml <<'EOF'
  > module A = struct
  >   module B = struct
  >     module C = struct
  >       let x = ()
  >     end
  >   end
  > end
  > 
  > open A.B.C
  > EOF

Cursor on `A` jumps to module `A` (prefix segment, textual fallback):

  $ $MERLIN single locate -look-for ml -position 9:6 \
  > -filename test.ml <test.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/test.ml",
      "pos": {
        "line": 1,
        "col": 7
      }
    },
    "notifications": []
  }

Cursor on `B` jumps to module `B` (prefix segment, textual fallback):

  $ $MERLIN single locate -look-for ml -position 9:8 \
  > -filename test.ml <test.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/test.ml",
      "pos": {
        "line": 2,
        "col": 9
      }
    },
    "notifications": []
  }

Cursor on `C` jumps to module `C` (full open path, resolved by the typer):

  $ $MERLIN single locate -look-for ml -position 9:10 \
  > -filename test.ml <test.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/test.ml",
      "pos": {
        "line": 3,
        "col": 11
      }
    },
    "notifications": []
  }

The same should hold in an interface:

  $ cat >test.mli <<'EOF'
  > module A : sig
  >   module B : sig
  >     module C : sig
  >       val x : unit
  >     end
  >   end
  > end
  > 
  > open A.B.C
  > EOF

Cursor on `A`:

  $ $MERLIN single locate -look-for mli -position 9:6 \
  > -filename test.mli <test.mli
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/test.mli",
      "pos": {
        "line": 1,
        "col": 7
      }
    },
    "notifications": []
  }

Cursor on `B`:

  $ $MERLIN single locate -look-for mli -position 9:8 \
  > -filename test.mli <test.mli
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/test.mli",
      "pos": {
        "line": 2,
        "col": 9
      }
    },
    "notifications": []
  }

Cursor on `C`:

  $ $MERLIN single locate -look-for mli -position 9:10 \
  > -filename test.mli <test.mli
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/test.mli",
      "pos": {
        "line": 3,
        "col": 11
      }
    },
    "notifications": []
  }

Now the same dotted `open A.B.C`, but where the opened module `A.B.C` itself
brings submodules named `A`, `B` and `C` into scope. After the `open`, those
inner submodules shadow the top-level `A`, `B` and `C`. Locating a segment of
the `open` path must still jump to the top-level modules that the path actually
refers to (lines 1/2/3), not to the shadowing submodules (lines 4/5/6).

  $ cat >shadow.ml <<'EOF'
  > module A = struct
  >   module B = struct
  >     module C = struct
  >       module A = struct let dummy_a = () end
  >       module B = struct let dummy_b = () end
  >       module C = struct let dummy_c = () end
  >     end
  >   end
  > end
  > 
  > open A.B.C
  > EOF

Cursor on `A` jumps to the top-level `A` (line 1), not the shadowing `A.B.C.A`:

  $ $MERLIN single locate -look-for ml -position 11:6 \
  > -filename shadow.ml <shadow.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/shadow.ml",
      "pos": {
        "line": 1,
        "col": 7
      }
    },
    "notifications": []
  }

Cursor on `B` jumps to the top-level `A.B` (line 2), not `A.B.C.B`:

  $ $MERLIN single locate -look-for ml -position 11:8 \
  > -filename shadow.ml <shadow.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/shadow.ml",
      "pos": {
        "line": 2,
        "col": 9
      }
    },
    "notifications": []
  }

Cursor on `C` jumps to the top-level `A.B.C` (line 3), not `A.B.C.C`:

  $ $MERLIN single locate -look-for ml -position 11:10 \
  > -filename shadow.ml <shadow.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/shadow.ml",
      "pos": {
        "line": 3,
        "col": 11
      }
    },
    "notifications": []
  }

The same must hold in an interface (this is where issue #1748 originally showed
up: the leaf node of a signature `open` carries the post-open environment, so
re-resolving the text of a segment finds the shadowing submodule):

  $ cat >shadow.mli <<'EOF'
  > module A : sig
  >   module B : sig
  >     module C : sig
  >       module A : sig val dummy_a : unit end
  >       module B : sig val dummy_b : unit end
  >       module C : sig val dummy_c : unit end
  >     end
  >   end
  > end
  > 
  > open A.B.C
  > EOF

Cursor on `A`:

  $ $MERLIN single locate -look-for mli -position 11:6 \
  > -filename shadow.mli <shadow.mli
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/shadow.mli",
      "pos": {
        "line": 1,
        "col": 7
      }
    },
    "notifications": []
  }

Cursor on `B`:

  $ $MERLIN single locate -look-for mli -position 11:8 \
  > -filename shadow.mli <shadow.mli
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/shadow.mli",
      "pos": {
        "line": 2,
        "col": 9
      }
    },
    "notifications": []
  }

Cursor on `C`:

  $ $MERLIN single locate -look-for mli -position 11:10 \
  > -filename shadow.mli <shadow.mli
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/shadow.mli",
      "pos": {
        "line": 3,
        "col": 11
      }
    },
    "notifications": []
  }
