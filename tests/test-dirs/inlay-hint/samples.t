Optional argument

  $ $MERLIN single inlay-hints -start 1:0 -end 2:0 \
  > -filename inlay.ml <<EOF
  > let f ?x () = x ()
  > EOF
  {
    "class": "return",
    "value": [
      {
        "pos": {
          "line": 1,
          "col": 8
        },
        "label": "'a option"
      }
    ],
    "notifications": []
  }

Optional argument with value

  $ $MERLIN single inlay-hints -start 1:0 -end 2:0 \
  > -filename inlay.ml <<EOF
  > let f ?(x = 1) () = x
  > EOF
  {
    "class": "return",
    "value": [
      {
        "pos": {
          "line": 1,
          "col": 9
        },
        "label": "int"
      }
    ],
    "notifications": []
  }

Labeled argument

  $ $MERLIN single inlay-hints -start 1:0 -end 2:0 \
  > -filename inlay.ml <<EOF
  > let f ~x ~y:z = x + z
  > EOF
  {
    "class": "return",
    "value": [
      {
        "pos": {
          "line": 1,
          "col": 13
        },
        "label": "int"
      },
      {
        "pos": {
          "line": 1,
          "col": 8
        },
        "label": "int"
      }
    ],
    "notifications": []
  }

Case argument

  $ $MERLIN single inlay-hints -start 1:0 -end 2:0 \
  > -filename inlay.ml <<EOF
  > let f (Some x) = x + 1
  > EOF
  {
    "class": "return",
    "value": [
      {
        "pos": {
          "line": 1,
          "col": 13
        },
        "label": "int"
      }
    ],
    "notifications": []
  }

Pair arguments

  $ $MERLIN single inlay-hints -start 1:0 -end 2:0 \
  > -filename inlay.ml <<EOF
  > let f (a,b) = a+b
  > EOF
  {
    "class": "return",
    "value": [
      {
        "pos": {
          "line": 1,
          "col": 10
        },
        "label": "int"
      },
      {
        "pos": {
          "line": 1,
          "col": 8
        },
        "label": "int"
      }
    ],
    "notifications": []
  }

Record arguments

  $ $MERLIN single inlay-hints -start 1:0 -end 3:0 \
  > -filename inlay.ml <<EOF
  > type ('a, 'b) pair = {x:'a; y:'b}
  > let f {x;y=b} = x+b
  > EOF
  {
    "class": "return",
    "value": [
      {
        "pos": {
          "line": 2,
          "col": 12
        },
        "label": "int"
      },
      {
        "pos": {
          "line": 2,
          "col": 8
        },
        "label": "int"
      }
    ],
    "notifications": []
  }

Pattern variables without pattern-binding hint

  $ $MERLIN single inlay-hints -start 1:0 -end 4:26 \
  > -filename inlay.ml <<EOF
  > let f x =
  >   match x with
  >   | Some x -> x
  >   | None -> 0
  > EOF
  {
    "class": "return",
    "value": [
      {
        "pos": {
          "line": 1,
          "col": 7
        },
        "label": "int option"
      }
    ],
    "notifications": []
  }

Pattern variables with pattern-binding hint

  $ $MERLIN single inlay-hints -start 1:0 -end 4:26 \
  > -pattern-binding true \
  > -filename inlay.ml <<EOF
  > let f x =
  >   match x with
  >   | Some x -> x
  >   | None -> 0
  > EOF
  {
    "class": "return",
    "value": [
      {
        "pos": {
          "line": 3,
          "col": 10
        },
        "label": "int"
      },
      {
        "pos": {
          "line": 1,
          "col": 7
        },
        "label": "int option"
      }
    ],
    "notifications": []
  }


Let bindings without let hinting

  $ $MERLIN single inlay-hints -start 1:0 -end 4:26 \
  > -let-binding false \
  > -filename inlay.ml <<EOF
  > let f () = let y = 0 in y
  > EOF
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

Let bindings with let hinting

  $ $MERLIN single inlay-hints -start 1:0 -end 4:26 \
  > -let-binding true \
  > -filename inlay.ml <<EOF
  > let f () = let y = 0 in y
  > EOF
  {
    "class": "return",
    "value": [
      {
        "pos": {
          "line": 1,
          "col": 16
        },
        "label": "int"
      }
    ],
    "notifications": []
  }

Class-level let bindings without let hinting

  $ $MERLIN single inlay-hints -start 1:0 -end 4:26 \
  > -let-binding false \
  > -filename inlay.ml <<EOF
  > class c x = let y = 1 in object method s = x + y end
  > EOF
  {
    "class": "return",
    "value": [
      {
        "pos": {
          "line": 1,
          "col": 9
        },
        "label": "int"
      }
    ],
    "notifications": []
  }

Class-level let bindings with let hinting

  $ $MERLIN single inlay-hints -start 1:0 -end 4:26 \
  > -let-binding true \
  > -filename inlay.ml <<EOF
  > class c x = let y = 1 in object method s = x + y end
  > EOF
  {
    "class": "return",
    "value": [
      {
        "pos": {
          "line": 1,
          "col": 17
        },
        "label": "int"
      },
      {
        "pos": {
          "line": 1,
          "col": 9
        },
        "label": "int"
      }
    ],
    "notifications": []
  }

Top-level let bindings without let hinting

  $ $MERLIN single inlay-hints -start 1:0 -end 4:26 \
  > -let-binding false \
  > -filename inlay.ml <<EOF
  > let y = 0
  > EOF
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

Top-level let bindings with let hinting

  $ $MERLIN single inlay-hints -start 1:0 -end 4:26 \
  > -let-binding true \
  > -filename inlay.ml <<EOF
  > let y = 0
  > EOF
  {
    "class": "return",
    "value": [
      {
        "pos": {
          "line": 1,
          "col": 5
        },
        "label": "int"
      }
    ],
    "notifications": []
  }

Support for @merlin.hide

  $ $MERLIN single inlay-hints -start 1:0 -end 3:0 \
  > -filename inlay.ml <<EOF
  > let[@merlin.hide] f x = 2
  > let f x = (fun y -> x+y+1) [@merlin.hide]
  > EOF
  {
    "class": "return",
    "value": [
      {
        "pos": {
          "line": 2,
          "col": 7
        },
        "label": "int"
      }
    ],
    "notifications": []
  }
