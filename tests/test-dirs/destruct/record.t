Field projections should be handled the same way, regardless of whether we've
selected:

- the whole expression

  $ $MERLIN single case-analysis -start 2:2 -end 2:11 -filename test.ml <<EOF
  > let f (x : int ref) =
  >   x.contents
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 2
        },
        "end": {
          "line": 2,
          "col": 12
        }
      },
      "match x.contents with | 0 -> _ | _ -> _"
    ],
    "notifications": []
  }

- just the label

  $ $MERLIN single case-analysis -start 2:4 -end 2:11 -filename test.ml <<EOF
  > let f (x : int ref) =
  >   x.contents
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 2
        },
        "end": {
          "line": 2,
          "col": 12
        }
      },
      "match x.contents with | 0 -> _ | _ -> _"
    ],
    "notifications": []
  }

However, when calling on the field of a record literal, we should destruct the
whole record expression (even though it's pointless):

  $ $MERLIN single case-analysis -start 2:4 -end 2:11 -filename test.ml <<EOF
  > let f () =
  >   { contents = 3 }
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 2
        },
        "end": {
          "line": 2,
          "col": 18
        }
      },
      "match { contents = 3 } with | { contents } -> _"
    ],
    "notifications": []
  }


Record fields in patterns should also be refinable:

  $ $MERLIN single case-analysis -start 3:6 -end 3:13 -filename test.ml <<EOF
  > let f (x : int ref) =
  >   match x with
  >   | { contents } -> ()
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 4
        },
        "end": {
          "line": 3,
          "col": 16
        }
      },
      "{ contents = 0 } | { contents = _ }"
    ],
    "notifications": []
  }
