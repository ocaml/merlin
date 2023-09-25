  $ cat >main.ml <<EOF
  > let (let+) x f = f x
  > let (and+) x f = f x
  > 
  > let _ =
  >   let+ x, _ = 42, 43
  >   and+ y = 36 and+ _ = 37 in 
  >   x + y
  > EOF

Calling `holes` on the code fragment was raising an exception as
described in issue #1683.
  $ $MERLIN single holes -filename main.ml <main.ml
  {
    "class": "return",
    "value": [],
    "notifications": []
  }


  $ $MERLIN single type-enclosing \
  > -position 5:7 \
  > -filename main.ml <main.ml | jq '.value[0]'
  {
    "start": {
      "line": 5,
      "col": 7
    },
    "end": {
      "line": 5,
      "col": 8
    },
    "type": "int",
    "tail": "no"
  }
