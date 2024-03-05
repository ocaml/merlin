  $ $MERLIN single case-analysis -start 2:9 -end 2:9 \
  > -filename main.ml <<EOF
  > type t = {a: int * int; b: string}
  > let f ({a; b} : t) = assert false
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 8
        },
        "end": {
          "line": 2,
          "col": 9
        }
      },
      "a = (_, _)"
    ],
    "notifications": []
  }


  $ $MERLIN single case-analysis -start 2:9 -end 2:9 \
  > -filename main.ml <<EOF
  > type t = {a: int option; b: string}
  > let f ({a; b} : t) = assert false
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 7
        },
        "end": {
          "line": 2,
          "col": 13
        }
      },
      "({ a = None; b } : t) | ({ a = Some _; b } : t)"
    ],
    "notifications": []
  }
