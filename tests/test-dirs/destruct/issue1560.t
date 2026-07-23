  $ $MERLIN single case-analysis -start 2:29 -end 2:29 \
  > -filename main.ml <<EOF
  > type t = A of int | B | C
  > let f = function A x -> x | _ -> 1
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 28
        },
        "end": {
          "line": 2,
          "col": 29
        }
      },
      "B | C"
    ],
    "notifications": []
  }

  $ $MERLIN single case-analysis -start 2:18 -end 2:18 \
  > -filename main.ml <<EOF
  > type t = A of int | B | C
  > let f = function A x -> x
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 25
        },
        "end": {
          "line": 2,
          "col": 25
        }
      },
      "
  | B | C -> _"
    ],
    "notifications": []
  }


  $ $MERLIN single case-analysis -start 2:29 -end 2:29 \
  > -filename main.ml <<EOF
  > type t = A of int | B | C
  > let f = function A 0 -> 0 | _ -> 1
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 28
        },
        "end": {
          "line": 2,
          "col": 29
        }
      },
      "A _ | B | C"
    ],
    "notifications": []
  }

  $ $MERLIN single case-analysis -start 2:29 -end 2:29 \
  > -filename main.ml <<EOF
  > type t = A of int | B | C
  > let f = function A x -> x | _ -> 1 | C -> 2
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 28
        },
        "end": {
          "line": 2,
          "col": 29
        }
      },
      "B"
    ],
    "notifications": []
  }

  $ $MERLIN single case-analysis -start 2:38 -end 2:38 \
  > -filename main.ml <<EOF
  > type t = A of int | B | C
  > let f = function A x -> x | C -> 2 | _ -> 1
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 37
        },
        "end": {
          "line": 2,
          "col": 38
        }
      },
      "B"
    ],
    "notifications": []
  }

  $ $MERLIN single case-analysis -start 2:47 -end 2:47 \
  > -filename main.ml <<EOF
  > type t = A of int | B | C | D
  > let f = function A x -> x | B -> 2 | D -> 1 | _ -> 3
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 46
        },
        "end": {
          "line": 2,
          "col": 47
        }
      },
      "C"
    ],
    "notifications": []
  }

  $ $MERLIN single case-analysis -start 2:47 -end 2:47 \
  > -filename main.ml <<EOF
  > type t = A of int | B | C | D
  > let f = function A 2 -> 2 | B -> 2 | D -> 1 | _ -> 3
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 46
        },
        "end": {
          "line": 2,
          "col": 47
        }
      },
      "A _ | C"
    ],
    "notifications": []
  }
