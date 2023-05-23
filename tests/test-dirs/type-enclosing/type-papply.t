  $ cat >main.ml <<EOF
  > module type T = sig
  >   type 'a t
  > end
  > 
  > module N = struct
  >   module M (T : T) = struct
  >     type t = int T.t
  >   end
  > end
  > 
  > module F = struct
  >   module T = struct type 'a t end
  > end
  > 
  > type u = N.M(F.T).t
  > EOF

This should return the type of `t` (line 7)
  $ $MERLIN single type-enclosing -position 15:18 -verbosity 1 \
  > -filename main.ml <main.ml 
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 15,
          "col": 9
        },
        "end": {
          "line": 15,
          "col": 19
        },
        "type": "int F.T.t",
        "tail": "no"
      },
      {
        "start": {
          "line": 15,
          "col": 0
        },
        "end": {
          "line": 15,
          "col": 19
        },
        "type": "type u = int F.T.t",
        "tail": "no"
      }
    ],
    "notifications": []
  }

Type of T  (line 12)
  $ $MERLIN single type-enclosing -position 15:15 \
  > -filename main.ml <main.ml | jq '.value[0].type'
  "sig type 'a t end"

Type of F (line 11)
  $ $MERLIN single type-enclosing -position 15:13 \
  > -filename main.ml <main.ml | jq '.value[0].type'
  "sig module T : sig type 'a t end end"

Type of M (line 6)
  $ $MERLIN single type-enclosing -position 15:11 \
  > -filename main.ml <main.ml | jq '.value[0].type'
  "functor (T : T) -> sig type t = int T.t end"

Type of N (line 5)
  $ $MERLIN single type-enclosing -position 15:9 \
  > -filename main.ml <main.ml | jq '.value[0].type'
  "sig module M : functor (T : T) -> sig type t = int T.t end end"
