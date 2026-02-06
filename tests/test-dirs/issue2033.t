  $ cat >main.mli <<EOF
  > type typ =
  >   | A
  >   | B
  >   | C
  > 
  > val string_of_type : typ -> string
  > EOF

Work as intended:
  $ $MERLIN single locate -prefix typ -position 5:0 -context unknown -look-for mli -filename main.mli <main.mli
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/main.mli",
      "pos": {
        "line": 1,
        "col": 5
      }
    },
    "notifications": []
  }

TODO: trying to find the `typ` declaration with the cursor positioned on a signature item located after the type definition fails:
  $ $MERLIN single locate -prefix typ -position 6:10 -context unknown -look-for mli -filename main.mli <main.mli
  {
    "class": "return",
    "value": "Already at definition point",
    "notifications": []
  }
