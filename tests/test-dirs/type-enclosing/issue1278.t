  $ cat >test.ml <<EOF
  > module C = struct end
  > 
  > module M = struct
  >   type t = C
  > end
  > 
  > let (_ : M.t) = C
  > EOF

  $ ocamlmerlin single type-enclosing -protocol jquery -verbosity 0 \
  > -filename test.ml -position 7:17 -index 0 < test.ml | jq '.value[0]'
  {
    "start": {
      "line": 7,
      "col": 16
    },
    "end": {
      "line": 7,
      "col": 17
    },
    "type": "M.t",
    "tail": "no"
  }

  $ ocamlmerlin single type-enclosing -protocol jquery -verbosity 0 \
  > -filename test.ml -position 7:16 -index 0 < test.ml | jq '.value[0]'
  {
    "start": {
      "line": 7,
      "col": 16
    },
    "end": {
      "line": 7,
      "col": 17
    },
    "type": "M.t",
    "tail": "no"
  }

  $ cat >>test.ml << EOF
  > 
  > let (_ : M.t) = C
  > EOF

  $ ocamlmerlin single type-enclosing -protocol jquery -verbosity 0 \
  > -filename test.ml -position 9:17 -index 0 < test.ml | jq '.value[0]'
  {
    "start": {
      "line": 9,
      "col": 16
    },
    "end": {
      "line": 9,
      "col": 17
    },
    "type": "M.t",
    "tail": "no"
  }
