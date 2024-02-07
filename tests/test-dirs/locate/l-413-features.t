Named existentials in patterns

  $ $MERLIN single locate -position 3:59 \
  > -filename test.ml <<EOF
  > type _ ty = Int : int ty
  > type dyn = Dyn : 'a ty * 'a -> dyn
  > let f = function Dyn (type a) (w, x : a ty * a) -> ignore (x : a)
  > EOF
  {
    "class": "return",
    "value": {
      "file": "test.ml",
      "pos": {
        "line": 3,
        "col": 34
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -position 3:63 \
  > -filename test.ml <<EOF
  > type _ ty = Int : int ty
  > type dyn = Dyn : 'a ty * 'a -> dyn
  > let f = function Dyn (type a) (w, x : a ty * a) -> ignore (x : a)
  > EOF
  {
    "class": "return",
    "value": {
      "file": "test.ml",
      "pos": {
        "line": 3,
        "col": 27
      }
    },
    "notifications": []
  }


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

  $ $MERLIN single locate -position 6:25 \
  > -filename mtsubst.ml < mtsubst.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/mtsubst.ml",
      "pos": {
        "line": 5,
        "col": 31
      }
    },
    "notifications": []
  }



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

  $ $MERLIN single locate -position 6:26 \
  > -filename mtsubst.ml < mtsubst.ml 
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/mtsubst.ml",
      "pos": {
        "line": 5,
        "col": 31
      }
    },
    "notifications": []
  }
 
