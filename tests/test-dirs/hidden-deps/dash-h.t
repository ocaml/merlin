This test comes from: https://github.com/janestreet/merlin-jst/pull/59

Test "-H".  We want to check both that merlin can check things that require
indirect use of hidden dependencies, and that it doesn't check things which
would require direct use of hidden dependencies.

Create a simple dependency chain Liba -> Libb -> Libc.

  $ mkdir liba libb libc

  $ cat <<EOF > liba/liba.ml
  > type t = int
  > let x = 42
  > type s = Baz
  > EOF

  $ cat <<EOF > libb/libb.ml
  > type t = Liba.t
  > let x : Liba.t = Liba.x
  > let f : Liba.t -> Liba.t = fun x -> x
  > EOF

  $ cat <<EOF > libc/libc.ml
  > type t = Libb.t [@@immediate]
  > let x : t = Libb.x
  > let y = x + 42
  > let z = Liba.Baz
  > EOF

  $ cd liba
  $ $OCAMLC -c liba.ml
  $ cd ../libb
  $ $OCAMLC -I ../liba -c libb.ml
  $ cd ../libc

When compiling libc.ml with liba made available via -H, we expect merlin to be
able to see the first three lines typecheck and the last does not.

Check the type of `x`, should work.

  $ $MERLIN single type-enclosing -position 3:9 -I ../libb -H ../liba \
  >   -filename "libc.ml" < "libc.ml"
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 8
        },
        "end": {
          "line": 3,
          "col": 9
        },
        "type": "t",
        "tail": "no"
      },
      {
        "start": {
          "line": 3,
          "col": 8
        },
        "end": {
          "line": 3,
          "col": 14
        },
        "type": "int",
        "tail": "no"
      }
    ],
    "notifications": []
  }

Check the type of Liba.Baz three ways: With -I liba we should get the type.  -H
liba and leaving the library off entirely should behave the same for a direct
refence like this (and merlin's behavior in this case is to just guess 'a for
the type)

  $ $MERLIN single type-enclosing -position 4:8 -I ../libb -I ../liba \
  >   -filename "libc.ml" < "libc.ml"
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 8
        },
        "end": {
          "line": 4,
          "col": 12
        },
        "type": "sig type t = int val x : int type s = Baz end",
        "tail": "no"
      },
      {
        "start": {
          "line": 4,
          "col": 8
        },
        "end": {
          "line": 4,
          "col": 16
        },
        "type": "Liba.s",
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single type-enclosing -position 4:8 -I ../libb -H ../liba \
  >   -filename "libc.ml" < "libc.ml"
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 8
        },
        "end": {
          "line": 4,
          "col": 16
        },
        "type": "'a",
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single type-enclosing -position 4:8 -I ../libb \
  >   -filename "libc.ml" < "libc.ml"
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 8
        },
        "end": {
          "line": 4,
          "col": 16
        },
        "type": "'a",
        "tail": "no"
      }
    ],
    "notifications": []
  }

Look at the errors: With -I liba, none.  With -H liba, an error on the direct
reference.  With no liba, we also can't see Libb.t is int.

  $ $MERLIN single errors -I ../libb -I ../liba \
  >   -filename "libc.ml" < "libc.ml"
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

  $ $MERLIN single errors -I ../libb -H ../liba \
  >   -filename "libc.ml" < "libc.ml"
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 8
        },
        "end": {
          "line": 4,
          "col": 16
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound module Liba"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single errors -I ../libb \
  >   -filename "libc.ml" < "libc.ml"
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 8
        },
        "end": {
          "line": 3,
          "col": 9
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type t = Liba.t but an expression was expected of type
    int"
      },
      {
        "start": {
          "line": 4,
          "col": 8
        },
        "end": {
          "line": 4,
          "col": 16
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound module Liba"
      }
    ],
    "notifications": []
  }
