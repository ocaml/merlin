Here we test destruction on patterns lhs of value bindings.

  $ cat > letbindings.ml <<EOF
  > type t = { x : int option ; y : float}
  > let record = {x = None; y = 2.}
  > type u = A | B of t
  > let variant = A
  > 
  > let a =
  >   let _ = record in
  >   ()
  > 
  > let b =
  >   let _ = variant in
  >   ()
  > 
  > let b =
  >   let A | B _ = variant in
  >   ()
  > 
  > let b =
  >   let {x = _; y} = record in
  >   ()
  > EOF

(FIXME:) Trying to destruct a pattern that's the lhs of a value binding whose
rhs is a record:

  $ $MERLIN single case-analysis -start 7:7 -end 7:7 -filename letbindings.ml < letbindings.ml
  {
    "class": "error",
    "value": "Destruct not allowed on value_binding",
    "notifications": []
  }

(FIXME:) Trying to destruct a pattern that's the lhs of a value binding whose
rhs is a variant:

  $ $MERLIN single case-analysis -start 11:7 -end 11:7 -filename letbindings.ml < letbindings.ml
  {
    "class": "error",
    "value": "Destruct not allowed on value_binding",
    "notifications": []
  }

Trying to desctruct a **subcase** of a pattern that's the lhs of a value binding
whose rhs is a variant.
Note that, internally this is stored in the typedtree as a match (to check for exhaustiveness of pattern matching), so we are not really checking anything about let bindings.

  $ $MERLIN single case-analysis -start 15:13 -end 15:13 -filename letbindings.ml < letbindings.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 15,
          "col": 12
        },
        "end": {
          "line": 15,
          "col": 13
        }
      },
      "{ x; y }"
    ],
    "notifications": []
  }

(FIXME:) Trying to desctruct a **subcase** of a pattern that's the lhs of a value binding
whose rhs is a record
  $ $MERLIN single case-analysis -start 19:12 -end 19:12 -filename letbindings.ml < letbindings.ml
  {
    "class": "error",
    "value": "Destruct not allowed on value_binding",
    "notifications": []
  }
