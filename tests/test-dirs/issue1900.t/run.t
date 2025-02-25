  $ dune exec ./main.exe
  test

FIXME: There should be no error.
  $ $MERLIN single errors -filename main.ml <main.ml | jq '.value'
  [
    {
      "start": {
        "line": 1,
        "col": 14
      },
      "end": {
        "line": 1,
        "col": 17
      },
      "type": "typer",
      "sub": [],
      "valid": true,
      "message": "Unbound value A.x"
    }
  ]

FIXME: Dune should communicate the -open Dune__exe flag after the others.
  $ $MERLIN single dump-configuration -filename main.ml <main.ml | \
  > jq '.value.merlin.flags_applied'
  [
    {
      "workdir": "$TESTCASE_ROOT",
      "workval": [
        "-open",
        "Dune__exe"
      ]
    },
    {
      "workdir": "$TESTCASE_ROOT",
      "workval": [
        "-w",
        "@1..3@5..28@30..39@43@46..47@49..57@61..62-40",
        "-strict-sequence",
        "-strict-formats",
        "-short-paths",
        "-keep-locs",
        "-open",
        "Lib",
        "-g"
      ]
    }
  ]

Using a .merlin with the FLG flags in the correct order works:
  $ cat >.merlin <<'EOF'
  > B _build/default/.main.eobjs/byte
  > B _build/default/lib/.lib.objs/byte
  > FLG -open Lib
  > FLG -open Dune__exe
  > EOF

  $ $MERLIN single dump-configuration -filename main.ml <main.ml | \
  > jq '.value.merlin.flags_applied'
  [
    {
      "workdir": "$TESTCASE_ROOT",
      "workval": [
        "-open",
        "Lib"
      ]
    },
    {
      "workdir": "$TESTCASE_ROOT",
      "workval": [
        "-open",
        "Dune__exe"
      ]
    }
  ]

  $ $MERLIN single errors -filename main.ml <main.ml | jq '.value'
  []
