  $ cat >main.ml <<'EOF'
  > class c () = object
  >   method m = ()
  > end
  > 
  > class o = object
  >   inherit c () as super
  > end
  > 
  > module type M = sig
  >   class c : unit -> object
  >     method m : unit
  >   end
  > end
  > 
  > let () = (new c ())#m
  > EOF

class c: 
We expect 1:6 1:7
  $ $MERLIN single type-enclosing -position 1:6 \
  > -filename main.ml <main.ml | jq '.value[0]'
  {
    "start": {
      "line": 1,
      "col": 6
    },
    "end": {
      "line": 1,
      "col": 7
    },
    "type": "unit -> object method m : unit end",
    "tail": "no"
  }

method m:
We expect 2:9 2:10
  $ $MERLIN single type-enclosing -position 2:9 \
  > -filename main.ml <main.ml | jq '.value[0]'
  {
    "start": {
      "line": 2,
      "col": 9
    },
    "end": {
      "line": 2,
      "col": 10
    },
    "type": "unit",
    "tail": "no"
  }

inherit c:
We expect 6:10 6:11
  $ $MERLIN single type-enclosing -position 6:10 \
  > -filename main.ml <main.ml | jq '.value[0]'
  {
    "start": {
      "line": 6,
      "col": 10
    },
    "end": {
      "line": 6,
      "col": 11
    },
    "type": "unit -> c",
    "tail": "no"
  }

as super: FIXME location is missing from the Typedtree
  $ $MERLIN single type-enclosing -position 6:20 \
  > -filename main.ml <main.ml | jq '.value[0]'
  null

sig class c:
we expect 10:8 10:9
  $ $MERLIN single type-enclosing -position 10:8 \
  > -filename main.ml <main.ml | jq '.value[0]'
  {
    "start": {
      "line": 10,
      "col": 8
    },
    "end": {
      "line": 10,
      "col": 9
    },
    "type": "unit -> object method m : unit end",
    "tail": "no"
  }

sig method m: FIXME unsatisfying location:
We could handle these the same as fields, but the location
information is missing from the Typedtree
  $ $MERLIN single type-enclosing -position 11:11 \
  > -filename main.ml <main.ml | jq '.value[0]'
  {
    "start": {
      "line": 11,
      "col": 4
    },
    "end": {
      "line": 11,
      "col": 19
    },
    "type": "unit",
    "tail": "no"
  }

new c:
we expect 15:14 15:15
  $ $MERLIN single type-enclosing -verbosity 0 -position 15:14 \
  > -filename main.ml <main.ml | jq '.value[0]'
  {
    "start": {
      "line": 15,
      "col": 14
    },
    "end": {
      "line": 15,
      "col": 15
    },
    "type": "unit -> c",
    "tail": "no"
  }

new c with verbosity 1
  $ $MERLIN single type-enclosing -verbosity 1 -position 15:14 \
  > -filename main.ml <main.ml | jq '.value[0]'
  {
    "start": {
      "line": 15,
      "col": 14
    },
    "end": {
      "line": 15,
      "col": 15
    },
    "type": "unit -> < m : unit >",
    "tail": "no"
  }

#m: FIXME unsatisfying location: missing from the typedtree
  $ $MERLIN single type-enclosing -position 15:21 \
  > -filename main.ml <main.ml | jq '.value[0]'
  {
    "start": {
      "line": 15,
      "col": 9
    },
    "end": {
      "line": 15,
      "col": 21
    },
    "type": "unit",
    "tail": "no"
  }
