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

  $ $MERLIN single refactor-open -action qualify -position 1:6 <<EOF
  > open Unix
  > let f x = x.tms_stime, x.tms_utime
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
          "col": 21
        },
        "content": "Unix.tms_stime"
      },
      {
        "start": {
          "line": 2,
          "col": 25
        },
        "end": {
          "line": 2,
          "col": 34
        },
        "content": "Unix.tms_utime"
      }
    ],
    "notifications": []
  }
