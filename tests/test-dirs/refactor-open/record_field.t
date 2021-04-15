FIXME refactor open rewriting the whole record instead of a field label
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
          "col": 8
        },
        "end": {
          "line": 5,
          "col": 17
        },
        "content": "i"
      }
    ],
    "notifications": []
  }
