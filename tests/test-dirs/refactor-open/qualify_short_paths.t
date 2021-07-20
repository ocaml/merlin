refactor open qualify should use short paths

  $ $MERLIN single refactor-open -action qualify -position 7:6 <<EOF
  > module Dune__exe = struct
  >   module M = struct
  >     let u = ()
  >   end
  > end
  > open Dune__exe
  > open M
  > let u = u
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 8,
          "col": 8
        },
        "end": {
          "line": 8,
          "col": 9
        },
        "content": "M.u"
      }
    ],
    "notifications": []
  }
