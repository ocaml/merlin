  $ $MERLIN single jump -target let -position 2:2 -filename test.ml <<EOF
  > let x =
  >   5
  > EOF
  {
    "class": "return",
    "value": {
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }

  $ $MERLIN single jump -target module -position 2:3 -filename test.ml <<EOF
  > module T = struct
  >   type t
  > end
  > EOF
  {
    "class": "return",
    "value": {
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }

  $ $MERLIN single jump -target module-type -position 2:3 -filename test.ml <<EOF
  > module type T = sig
  >   type t
  > end
  > EOF
  {
    "class": "return",
    "value": {
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }


Same line should fail:

  $ $MERLIN single jump -target let -position 1:8 -filename test.ml <<EOF
  > let x = 5
  > EOF
  {
    "class": "return",
    "value": "No matching target",
    "notifications": []
  }

  $ $MERLIN single jump -target module -position 2:2 -filename test.ml <<EOF
  > let x =
  >   5
  > EOF
  {
    "class": "return",
    "value": "No matching target",
    "notifications": []
  }

