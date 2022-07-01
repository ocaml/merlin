This test demonstrates the handling of location of patterns. For a pattern like
(x), the occurrence location should reflect only the identifier.

  $ cat >pat.ml <<EOF
  > let f x =
  >   match x with
  >   | Some (yyy) -> yyy
  >   | None -> assert false
  > EOF
  $ $MERLIN single occurrences -identifier-at 3:10 -filename ./pat.ml < ./pat.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 10
        },
        "end": {
          "line": 3,
          "col": 13
        }
      },
      {
        "start": {
          "line": 3,
          "col": 18
        },
        "end": {
          "line": 3,
          "col": 21
        }
      }
    ],
    "notifications": []
  }
