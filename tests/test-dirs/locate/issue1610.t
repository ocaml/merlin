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

We should jump to the functor's body (line 7)
  $ $MERLIN single locate -look-for ml -position 15:18 \
  > -filename main.ml <main.ml | jq '.value.pos'
  {
    "line": 7,
    "col": 4
  }

Should jump to T's definition  (line 12)
  $ $MERLIN single locate -look-for ml -position 15:15 \
  > -filename main.ml <main.ml | jq '.value.pos'
  {
    "line": 12,
    "col": 2
  }

Should jump to F's definition (line 11)
  $ $MERLIN single locate -look-for ml -position 15:13 \
  > -filename main.ml <main.ml | jq '.value.pos'
  {
    "line": 11,
    "col": 0
  }

Should jump to M's definition (line 6)
  $ $MERLIN single locate -look-for ml -position 15:11 \
  > -filename main.ml <main.ml | jq '.value.pos'
  {
    "line": 6,
    "col": 2
  }

Should jump to N's definition (line 5)
  $ $MERLIN single locate -look-for ml -position 15:9 \
  > -filename main.ml <main.ml | jq '.value.pos'
  {
    "line": 5,
    "col": 0
  }

It also works as expected when the user inputs the expression manually
  $ $MERLIN single locate -prefix 'N.M(F.T).t' -look-for ml -position 15:18 \
  > -filename main.ml <main.ml | jq '.value.pos'
  {
    "line": 7,
    "col": 4
  }
