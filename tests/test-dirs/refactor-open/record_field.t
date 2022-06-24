Refactor open for record fields

  $ $MERLIN single refactor-open -action unqualify -position 4:7 <<EOF
  > module M = struct
  >   type r = {i: int}
  > end
  > open M
  > let r = {M.i = 1}
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 5,
          "col": 9
        },
        "end": {
          "line": 5,
          "col": 12
        },
        "content": "i"
      }
    ],
    "notifications": []
  }

Refactor open for record disambiguation

  $ $MERLIN single refactor-open -action qualify -I +unix -position 1:6 <<EOF
  > open Sys
  > let f x = x.patchlevel, x.major
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 12
        },
        "end": {
          "line": 2,
          "col": 22
        },
        "content": "Sys.patchlevel"
      },
      {
        "start": {
          "line": 2,
          "col": 26
        },
        "end": {
          "line": 2,
          "col": 31
        },
        "content": "Sys.major"
      }
    ],
    "notifications": []
  }
