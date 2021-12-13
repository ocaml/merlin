From issue 1300:
https://github.com/ocaml/merlin/issues/1300
  $ $MERLIN single case-analysis -start 6:5 -end 6:5 -filename i1300.ml <<EOF
  > type t =
  > | A of int
  > | B of int
  > 
  > let f = function
  > | A x (* <<< here *)
  > | B -> 0
  > EOF
  {
    "class": "error",
    "value": "The node on which destruct was called is ill-typed",
    "notifications": []
  }

  $ $MERLIN single case-analysis -start 6:5 -end 6:5 -filename i1300.ml <<EOF
  > type t =
  > | A of int
  > | B of int
  > 
  > let f = function
  > | A x (* <<< here *)
  > | B x -> 0
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 6,
          "col": 2
        },
        "end": {
          "line": 7,
          "col": 5
        }
      },
      "A 0 | B x | A _"
    ],
    "notifications": []
  }

Fixed: Another stacktrace when "no nodes"
  $ $MERLIN single case-analysis -start 7:25 -end 7:25 -filename i1300.ml <<EOF
  > EOF
  {
    "class": "error",
    "value": "Nothing to do",
    "notifications": []
  }
