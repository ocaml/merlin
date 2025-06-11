Regular function

  $ $MERLIN single inlay-hints -start 1:0 -end 2:26 -avoid-ghost-location false \
  > -function-params true \
  > -filename inlay.ml <<EOF
  > let f a b c d e f = (a + b, c ^ d, e +. (float_of_string f))
  > EOF
  {
    "class": "return",
    "value": [
      {
        "pos": {
          "line": 1,
          "col": 17
        },
        "label": "string"
      },
      {
        "pos": {
          "line": 1,
          "col": 15
        },
        "label": "float"
      },
      {
        "pos": {
          "line": 1,
          "col": 13
        },
        "label": "string"
      },
      {
        "pos": {
          "line": 1,
          "col": 11
        },
        "label": "string"
      },
      {
        "pos": {
          "line": 1,
          "col": 9
        },
        "label": "int"
      },
      {
        "pos": {
          "line": 1,
          "col": 7
        },
        "label": "int"
      }
    ],
    "notifications": []
  }

Regular function without function-params
FIXME the option is not enforced
  $ $MERLIN single inlay-hints -start 1:0 -end 2:26 -avoid-ghost-location false \
  > -function-params false \
  > -filename inlay.ml <<EOF
  > let f a b c d e f = (a + b, c ^ d, e +. (float_of_string f))
  > EOF
  {
    "class": "return",
    "value": [
      {
        "pos": {
          "line": 1,
          "col": 17
        },
        "label": "string"
      },
      {
        "pos": {
          "line": 1,
          "col": 15
        },
        "label": "float"
      },
      {
        "pos": {
          "line": 1,
          "col": 13
        },
        "label": "string"
      },
      {
        "pos": {
          "line": 1,
          "col": 11
        },
        "label": "string"
      },
      {
        "pos": {
          "line": 1,
          "col": 9
        },
        "label": "int"
      },
      {
        "pos": {
          "line": 1,
          "col": 7
        },
        "label": "int"
      }
    ],
    "notifications": []
  }


Optional argument

  $ $MERLIN single inlay-hints -start 1:0 -end 2:26 -avoid-ghost-location false \
  > -function-params true \
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

Optional argument without function-params
FIXME the option is not enforced

  $ $MERLIN single inlay-hints -start 1:0 -end 2:26 -avoid-ghost-location false \
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

  $ $MERLIN single inlay-hints -start 1:0 -end 2:26 -avoid-ghost-location false \
  > -function-params true \
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

  $ $MERLIN single inlay-hints -start 1:0 -end 2:26 -avoid-ghost-location false \
  > -function-params true \
  > -filename inlay.ml <<EOF
  > let f ~x = x + 1
  > EOF
  {
    "class": "return",
    "value": [
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

  $ $MERLIN single inlay-hints -start 1:0 -end 2:26 -avoid-ghost-location false \
  > -function-params true \
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

Pattern variables without pattern-binding hint

  $ $MERLIN single inlay-hints -start 1:0 -end 4:26 -avoid-ghost-location false \
  > -function-params true \
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

  $ $MERLIN single inlay-hints -start 1:0 -end 4:26 -avoid-ghost-location false \
  > -pattern-binding true -function-params true \
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

  $ $MERLIN single inlay-hints -start 1:0 -end 4:26 -avoid-ghost-location false \
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

  $ $MERLIN single inlay-hints -start 1:0 -end 4:26 -avoid-ghost-location false \
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
