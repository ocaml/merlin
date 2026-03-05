  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ cat >dune <<EOF
  > (executable (name foo))
  > EOF

  $ cat > foo.re <<EOF
  > let _x = List.map;
  > EOF

  $ dune build
  $ $MERLIN single type-enclosing -position 1:15 -verbosity 0 -filename foo.re < foo.re | jq '.value[0]'
  {
    "start": {
      "line": 1,
      "col": 9
    },
    "end": {
      "line": 1,
      "col": 17
    },
    "type": "('a => 'b, list('a)) => list('b)",
    "tail": "no"
  }
