Named existentials in patterns
  $ $MERLIN single type-enclosing -position 3:59 -filename test.ml <<EOF | \
  > tr '\n' ' ' |  jq '.value[0:2]' 
  > type _ ty = Int : int ty
  > type dyn = Dyn : 'a ty * 'a -> dyn
  > let f = function Dyn (type a) (w, x : a ty * a) -> ignore (x : a)
  > EOF
  [
    {
      "start": {
        "line": 3,
        "col": 59
      },
      "end": {
        "line": 3,
        "col": 60
      },
      "type": "a",
      "tail": "no"
    },
    {
      "start": {
        "line": 3,
        "col": 51
      },
      "end": {
        "line": 3,
        "col": 65
      },
      "type": "unit",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 3:63 -filename test.ml <<EOF | \
  > tr '\n' ' ' |  jq '.value[0:2]' 
  > type _ ty = Int : int ty
  > type dyn = Dyn : 'a ty * 'a -> dyn
  > let f = function Dyn (type a) (w, x : a ty * a) -> ignore (x : a)
  > EOF
  [
    {
      "start": {
        "line": 3,
        "col": 63
      },
      "end": {
        "line": 3,
        "col": 64
      },
      "type": "type a",
      "tail": "no"
    },
    {
      "start": {
        "line": 3,
        "col": 63
      },
      "end": {
        "line": 3,
        "col": 64
      },
      "type": "a",
      "tail": "no"
    }
  ]

Module types substitutions
  $ cat >mtsubst.ml <<EOF
  > module type ENDO = sig
  >   module type T
  >   module F: T -> T
  > end
  > module Endo(X: sig module type T end): ENDO 
  >   with module type T = X.T = struct
  >   module type T = X.T
  >   module F(X:T) = X
  > end
  > EOF

  $ $MERLIN single type-enclosing -position 6:25 \
  > -filename mtsubst.ml < mtsubst.ml |
  > tr '\n' ' ' |  jq '.value[0:2]'
  [
    {
      "start": {
        "line": 6,
        "col": 23
      },
      "end": {
        "line": 6,
        "col": 26
      },
      "type": "(* abstract module *)",
      "tail": "no"
    },
    {
      "start": {
        "line": 6,
        "col": 23
      },
      "end": {
        "line": 6,
        "col": 26
      },
      "type": "X.T",
      "tail": "no"
    }
  ]

FIXME: this doesn't work anymore with the new occurrences backend
  $ $MERLIN single occurrences -identifier-at 6:19 \
  > -filename mtsubst.ml < mtsubst.ml |
  > tr '\n' ' ' |  jq '.value'
  []

  $ cat >mtsubst.ml <<EOF
  > module type ENDO = sig
  >   module type T
  >   module F: T -> T
  > end
  > module Endo(X: sig module type T end): ENDO 
  >   with module type T := X.T = struct
  >   module type T = X.T
  >   module F(X:T) = X
  > end
  > EOF

  $ $MERLIN single type-enclosing -position 6:26 \
  > -filename mtsubst.ml < mtsubst.ml |
  > tr '\n' ' ' |  jq '.value[0:2]'
  [
    {
      "start": {
        "line": 6,
        "col": 24
      },
      "end": {
        "line": 6,
        "col": 27
      },
      "type": "(* abstract module *)",
      "tail": "no"
    },
    {
      "start": {
        "line": 6,
        "col": 24
      },
      "end": {
        "line": 6,
        "col": 27
      },
      "type": "X.T",
      "tail": "no"
    }
  ]

FIXME: this doesn't work anymore with the new occurrences backend
  $ $MERLIN single occurrences -identifier-at 6:19 \
  > -filename mtsubst.ml < mtsubst.ml |
  > tr '\n' ' ' |  jq '.value'
  []
