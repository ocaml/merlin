In Merlin there is a notion of "extended loc", that extends the normal loc in different scenarios. For instance, the loc of x is extended to the following ranges:
- To account for the space between "in" and the expression:

let _ = 1 in[ x]

(And similarly in many cases, as ifthenelse, while, ::, ...)

- To account for the lack of a node for "extra constraints":

let a = [( x : int)]

And similarly for coercion, pattern constraints and coercion

However, when in the two situation at the same time, the first one is ignored. Let's test that with a file containing both situations:

  $ cat > extras2.ml <<EOF
  > let a =
  >   let x = 1 in
  >   (x : int)
  > let a' =
  >   let x = 1 in
  >   x
  > EOF

and putting our cursor inbetween "in" and "x" and looking at the corresponding node:

  $ export LOC=3:0
  $ show_location extras2.ml $LOC
  let a =
    let x = 1 in
  █ (x : int)
  let a' =
    let x = 1 in
    x
  $ $MERLIN single type-enclosing -position $LOC -filename extras2.ml <extras2.ml | jq '[.value[0]]' | extract_ranges extras2.ml
  ---------- Range 0 ----------
  ··let x = 1 in
    (x : int)···

  $ export LOC=6:0
  $ show_location extras2.ml $LOC
  let a =
    let x = 1 in
    (x : int)
  let a' =
    let x = 1 in
  █ x
  $ $MERLIN single type-enclosing -position $LOC -filename extras2.ml <extras2.ml | jq '[.value[0]]' | extract_ranges extras2.ml
  ---------- Range 0 ----------
  ··x···

